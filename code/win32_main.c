#include "parser.c"

/*
TODO:
[ ] make a git repo
[ ] split win32_main into typechcker and emitter
[ ] parse expressions like foo := >bar (is this a pointer type of address_of bar?)
 [ ] resolve ambiguious decls like foo := bar (is bar a type of a variable?)
[ ] all basic data types
[ ] sizeof
[ ] fixed length arrays
[ ] casts
[ ] partial resolve and emit cleanup
[ ] implicit context
[ ] dynamic arrays
[ ] array views
[ ] enums
[ ] initializers for structs
[ ] default parameters for funcs
*/

Arena _scratch_arena;
Arena *scratch_arena = &_scratch_arena;

Parser *parser;



#if 0
Type_Info *builtin_Type = null;
Type_Info *builtin_f32 = null;
Type_Info *builtin_i32 = null;

Type_Info *code_type_to_type_info(Arena *arena, Code_Type *node) {
  Type_Info *info = arena_push_struct(arena, Type_Info);
  
  switch (node->kind) {
    case Type_Kind_FUNC: {
      info->kind = Type_Info_Kind_FUNC;
      info->func.params = sb_new(arena, Type_Info *, 8);
      for (u32 i = 0; i < sb_count(node->func.params); i++) {
        Type_Info *param_info = code_type_to_type_info(arena, node->func.params[i]->type);
        sb_push(info->func.params, param_info);
      }
      info->func.return_type = code_type_to_type_info(arena, node->func.return_type);
    } break;
    
    case Type_Kind_PTR: {
      info->kind = Type_Info_Kind_POINTER;
      info->pointer.base = code_type_to_type_info(arena, node->pointer.base);
    } break;
    
    case Type_Kind_STRUCT: {
      info->kind = Type_Info_Kind_STRUCT;
      info->struct_t.members = sb_new(arena, Type_Info_Struct_Member, 8);
      for (u32 i = 0; i < sb_count(node->struct_t.members); i++) {
        Code_Stmt_Decl *decl = node->struct_t.members[i];
        Type_Info_Struct_Member member;
        member.name = decl->name;
        member.type = code_type_to_type_info(arena, decl->type);
        
        sb_push(info->struct_t.members, member);
      }
    } break;
    
    case Type_Kind_ARRAY: {
      info->kind = Type_Info_Kind_STRUCT;
      info->array.count = node->array.count;
      info->array.item_type = code_type_to_type_info(arena, node->array.item_type);
    } break;
    
    case Type_Kind_ALIAS: {
      // TODO(lvl5): this needs to be a scope lookup
      if (string_compare(node->alias.name, const_string("i32"))) {
        info = builtin_i32;
      } else if (string_compare(node->alias.name, const_string("f32"))) {
        info = builtin_f32;
      } else if (string_compare(node->alias.name, const_string("Type"))) {
        info = builtin_Type;
      }
    } break;
    
    default: assert(false);
  }
  
  return info;
}
#endif

typedef struct {
  Code_Stmt_Decl **decls;
} Parse_Result;

Parse_Result parse(Parser *p, String src, Token *tokens) {
  Parse_Result result = {0};
  result.decls = parse_program(p);
  
  assert(p->error_count == 0);
  
  return result;
}

Scope *alloc_scope(Arena *arena, Scope *parent) {
  Scope *result = arena_push_struct(arena, Scope);
  result->entries = sb_new(arena, Scope_Entry, 32);
  result->parent = parent;
  return result;
}

Scope_Entry *scope_get(Scope *scope, String name) {
  Scope_Entry *result = 0;
  for (u32 i = 0; i < sb_count(scope->entries); i++) {
    Scope_Entry *test = scope->entries + i;
    if (string_compare(test->decl->name, name)) {
      result = test;
      break;
    }
  }
  if (!result && scope->parent) {
    result = scope_get(scope->parent, name);
  }
  
  return result;
}

Scope_Entry *scope_add(Scope *scope, Code_Stmt_Decl *decl) {
  Scope_Entry *entry = scope_get(scope, decl->name);
  if (!entry) {
    sb_push(scope->entries, (Scope_Entry){0});
    entry = sb_peek(scope->entries);
  }
  
  entry->decl = decl;
  
  return entry;
}

typedef struct {
  Code_Stmt_Decl **top_decls;
  Arena *arena;
  Resolve_State need_state;
  Code_Stmt_Decl *decl;
} Resolver;

b32 need_resolve(Resolver *res, Resolve_State state) {
  b32 result = res->decl->resolve_state < state &&
    res->need_state == state;
  return result;
}

typedef struct {
  i32 indent;
  Resolver *res;
} Emitter;

b32 check_types(Code_Type *a, Code_Type *b) {
  b32 result = false;
  
  if (a->kind == b->kind) {
    switch (a->kind) {
      case Type_Kind_STRUCT: {
        result = false;
      } break;
      case Type_Kind_ENUM: {
        result = false;
      } break;
      case Type_Kind_PTR: {
        result = check_types(a->pointer.base, b->pointer.base);
      } break;
      case Type_Kind_ARRAY: {
        result = false;
        assert(false);
      } break;
      case Type_Kind_FUNC: {
        if (sb_count(a->func.params) == sb_count(b->func.params)) {
          result = true;
          for (u32 i = 0; i < sb_count(a->func.params); i++) {
            Code_Stmt_Decl *a_param = a->func.params[i];
            Code_Stmt_Decl *b_param = b->func.params[i];
            if (!check_types(a_param->type, b_param->type)) {
              result = false;
              break;
            }
          }
          if (!check_types(a->func.return_type, b->func.return_type)) {
            result = false;
          }
        }
      } break;
      case Type_Kind_ALIAS: {
        result = string_compare(a->alias.name, b->alias.name);
      } break;
      
      default: assert(false);
    }
  }
  
  assert(result);
  return result;
}

Scope *get_global_scope(Scope *child) {
  Scope *result = child;
  while (result->parent) result = child->parent;
  return result;
}

void resolve_decl(Resolver *, Scope *, Code_Stmt_Decl *);
void resolve_expr(Resolver *, Scope *, Code_Expr *);
void resolve_stmt(Resolver *, Scope *, Code_Stmt *);
void resolve_stmt_block(Resolver *, Scope *, Code_Stmt_Block *, b32);
void resolve_and_emit_decl(Resolver *, Scope *, Code_Stmt_Decl *);

void resolve_name(Resolver *res, Scope *scope, String name, Resolve_State state) {
  for (u32 i = 0; i < sb_count(res->top_decls); i++) {
    Code_Stmt_Decl *decl = res->top_decls[i];
    if (string_compare(decl->name, name)) {
      Resolve_State old_state = res->need_state;
      res->need_state = state;
      resolve_and_emit_decl(res, get_global_scope(scope), decl);
      res->need_state = old_state;
      break;
    }
  }
}

void resolve_stmt(Resolver *res, Scope *scope, Code_Stmt *stmt) {
  switch (stmt->kind) {
    case Stmt_Kind_DECL: {
      resolve_decl(res, scope, &stmt->decl);
    } break;
    case Stmt_Kind_ASSIGN: {
      resolve_expr(res, scope, stmt->assign.left);
      resolve_expr(res, scope, stmt->assign.right);
      // TODO(lvl5): make sure the operator makes sense
      // no += for structs, etc
      check_types(stmt->assign.left->type, stmt->assign.right->type);
    } break;
    case Stmt_Kind_EXPR: {
      resolve_expr(res, scope, stmt->expr.expr);
    } break;
    case Stmt_Kind_IF: {
      resolve_expr(res, scope, stmt->if_s.cond);
      resolve_stmt(res, scope, stmt->if_s.then_branch);
      if (stmt->if_s.else_branch) {
        resolve_stmt(res, scope, stmt->if_s.else_branch);
      }
    } break;
    case Stmt_Kind_BLOCK: {
      resolve_stmt_block(res, scope, &stmt->block, /*is_func_body*/ false);
    } break;
    case Stmt_Kind_FOR: {
      resolve_decl(res, scope, stmt->for_s.init);
      resolve_expr(res, scope, stmt->for_s.cond);
      resolve_stmt(res, scope, stmt->for_s.post);
      resolve_stmt(res, scope, stmt->for_s.body);
    } break;
    case Stmt_Kind_KEYWORD: {
      // TODO(lvl5): make sure the keyword makes sense?
      resolve_expr(res, scope, stmt->keyword.expr);
    } break;
    
    default: assert(false);
  }
}

void resolve_stmt_block(Resolver *res, Scope *scope, Code_Stmt_Block *block, b32 is_func_body) {
  Scope *child_scope = scope;
  if (!is_func_body) {
    child_scope = alloc_scope(res->arena, scope);
  }
  for (u32 i = 0; i < sb_count(block->statements); i++) {
    Code_Stmt *stmt = block->statements[i];
    resolve_stmt(res, scope, stmt);
  }
}

void resolve_type(Resolver *res, Scope *scope, Code_Type *type) {
  switch (type->kind) {
    case Type_Kind_STRUCT: {
      Scope *child_scope = alloc_scope(res->arena, scope);
      type->struct_t.scope = child_scope;
      for (u32 i = 0; i < sb_count(type->struct_t.members); i++) {
        Code_Stmt_Decl *member = type->struct_t.members[i];
        resolve_decl(res, child_scope, member);
      }
    } break;
    case Type_Kind_ENUM: {
      
    } break;
    case Type_Kind_PTR: {
      if (type->pointer.base->kind == Type_Kind_ALIAS) {
        resolve_name(res, scope, type->pointer.base->alias.name, Resolve_State_PARTIAL);
      } else {
        resolve_type(res, scope, type->pointer.base);
      }
    } break;
    case Type_Kind_ARRAY: {
      resolve_type(res, scope, type->array.item_type);
    } break;
    case Type_Kind_FUNC: {
      for (u32 i = 0; i < sb_count(type->func.params); i++) {
        Code_Stmt_Decl *param = type->func.params[i];
        resolve_decl(res, scope, param);
      }
      resolve_type(res, scope, type->func.return_type);
    } break;
    case Type_Kind_ALIAS: {
      resolve_name(res, scope, type->alias.name, Resolve_State_FULL);
    } break;
    
    
    default: assert(false);
  }
}

void resolve_expr(Resolver *res, Scope *scope, Code_Expr *expr) {
  // TODO(lvl5): set type type_info on the expression
  switch (expr->kind) {
    case Expr_Kind_UNARY: {
      // TODO(lvl5): make sure the operator makes sense
      resolve_expr(res, scope, expr->unary.val);
      
      if (expr->unary.op == T_REF) {
        expr->type = (Code_Type *)code_type_pointer(parser, 
                                                    expr->unary.val->type);
      } else if (expr->unary.op == T_DEREF) {
        expr->type = expr->unary.val->type->pointer.base;
      } else {
        expr->type = expr->unary.val->type;
      }
    } break;
    case Expr_Kind_BINARY: {
      // TODO(lvl5): make sure the operator makes sense
      resolve_expr(res, scope, expr->binary.left);
      
      if (expr->binary.op == T_SUBSCRIPT) {
        resolve_expr(res, scope, expr->binary.right);
        check_types(expr->binary.right->type, scope_get(scope, const_string("i32"))->decl->type);
        
        Code_Type *left_type = expr->binary.left->type;
        assert(left_type->kind == Type_Kind_ARRAY);
        expr->type = left_type->array.item_type;
      } else if (expr->binary.op == T_MEMBER) {
        Code_Type *left_type = expr->binary.left->type;
        // TODO(lvl5): left_type can be an alias for a struct
        // or a pointer
        if (left_type->kind == Type_Kind_PTR) {
          left_type = left_type->pointer.base;
        }
        assert(left_type->kind == Type_Kind_ALIAS);
        Code_Type_Struct *struct_t = &scope_get(scope, left_type->alias.name)->decl->value->type.struct_t;
        Scope *struct_scope = struct_t->scope;
        
        assert(expr->binary.right->kind == Expr_Kind_NAME);
        resolve_expr(res, struct_scope, expr->binary.right);
        
        String member_name = expr->binary.right->name.name;
        Code_Type *member_type = 0;
        for (u32 i = 0; i < sb_count(struct_t->members); i++) {
          Code_Stmt_Decl *member = struct_t->members[i];
          if (string_compare(member->name, member_name)) {
            member_type = member->type;
            break;
          }
        }
        assert(member_type);
        check_types(member_type, expr->binary.right->type);
        expr->type = member_type;
      } else {
        resolve_expr(res, scope, expr->binary.right);
        check_types(expr->binary.left->type, expr->binary.right->type);
        expr->type = expr->binary.left->type;
      }
      
      // TODO(lvl5): implicit conversions?
    } break;
    case Expr_Kind_CALL: {
      // NOTE(lvl5): only requires signature resolve
      if (expr->call.func->kind == Expr_Kind_NAME) {
        String func_name = expr->call.func->name.name;
        resolve_name(res, scope, func_name, Resolve_State_PARTIAL);
        expr->call.func->type = scope_get(scope, func_name)->decl->type;
      } else {
        resolve_expr(res, scope, expr->call.func);
      }
      
      for (u32 i = 0; i < sb_count(expr->call.args); i++) {
        Code_Expr *arg = expr->call.args[i];
        resolve_expr(res, scope, arg);
        
        Code_Stmt_Decl *param = expr->call.func->type->func.params[i];
        check_types(arg->type, param->type);
      }
      
      expr->type = expr->call.func->type->func.return_type;
    } break;
    case Expr_Kind_INT: {
      expr->type = &scope_get(scope, const_string("i32"))->decl->value->type;
    } break;
    case Expr_Kind_FLOAT: {
      expr->type = &scope_get(scope, const_string("f32"))->decl->value->type;
    } break;
    case Expr_Kind_STRING: {
      resolve_name(res, scope, const_string("string"), Resolve_State_FULL);
      resolve_name(res, scope, const_string("__string_make"), Resolve_State_FULL);
      expr->type = (Code_Type *)code_type_alias(parser, const_string("string"));
    } break;
    case Expr_Kind_NAME: {
      resolve_name(res, scope, expr->name.name, Resolve_State_FULL);
      
      expr->type = scope_get(scope, expr->name.name)->decl->type;
    } break;
    
    default: assert(false);
  }
  assert(expr->type);
}

void resolve_func(Resolver *res, Scope *scope, Code_Func *func) {
  Scope *child_scope = alloc_scope(res->arena, scope);
  func->scope = child_scope;
  
  if (res->decl->resolve_state < Resolve_State_PARTIAL &&
      res->need_state >= Resolve_State_PARTIAL) {
    resolve_type(res, child_scope, (Code_Type *)func->type);
  }
  if (need_resolve(res, Resolve_State_FULL)) {
    resolve_stmt_block(res, child_scope, func->body, /* is_func_body */ true);
  }
}

char *tcstring(String str) {
  char *result = to_c_string(scratch_arena, str);
  return result;
}

void builder_write(String str) {
  printf(tcstring(str));
}


void emit_indent(Emitter *e) {
  for (i32 indent_index = 0; indent_index < e->indent; indent_index++) {
    builder_write(const_string("  "));
  }
}

void emit_int(Emitter *e, i64 value) {
  printf(tcstring(i64_to_string(scratch_arena, value)));
}

void emit_type_postfix(Emitter *, Code_Type *);
void emit_type_prefix(Emitter *, Code_Type *);

void emit_struct_body(Emitter *e, Code_Type_Struct *str) {
  builder_write(const_string("{\n"));
  
  e->indent++;
  for (u32 i = 0; i < sb_count(str->members); i++) {
    emit_indent(e);
    Code_Stmt_Decl *member = str->members[i];
    emit_type_prefix(e, member->type);
    builder_write(const_string(" "));
    builder_write(member->name);
    emit_type_postfix(e, member->type);
    builder_write(const_string(";\n"));
  }
  e->indent--;
  
  builder_write(const_string("}"));
}

void emit_type_prefix(Emitter *e, Code_Type *type) {
  switch (type->kind) {
    case Type_Kind_FUNC: {
      Code_Type_Func *func = &type->func;
      emit_type_prefix(e, func->return_type);
    } break;
    
    case Type_Kind_ARRAY: {
      Code_Type_Array *arr = &type->array;
      emit_type_prefix(e, arr->item_type);
    } break;
    
    case Type_Kind_STRUCT: {
      builder_write(const_string("struct "));
      Code_Type_Struct *str = &type->struct_t;
      emit_struct_body(e, str);
    } break;
    
    case Type_Kind_PTR: {
      Code_Type_Pointer *ptr = &type->pointer;
      emit_type_prefix(e, ptr->base);
      builder_write(const_string("*"));
    } break;
    
    case Type_Kind_ALIAS: {
      builder_write(type->alias.name);
    } break;
    
    case Type_Kind_ENUM: {
      Code_Type_Enum *str = &type->enum_t;
      builder_write(const_string("enum {\n"));
      
      e->indent++;
      for (u32 i = 0; i < sb_count(str->members); i++) {
        emit_indent(e);
        String member = str->members[i];
        builder_write(member);
        builder_write(const_string(",\n"));
      }
      e->indent--;
      
      builder_write(const_string("}"));
    } break;
    
    default: assert(false);
  }
}

void emit_type_postfix(Emitter *e, Code_Type *type) {
  switch (type->kind) {
    case Type_Kind_FUNC: {
      Code_Type_Func *func = &type->func;
      builder_write(const_string("("));
      for (u32 i = 0; i < sb_count(func->params); i++) {
        Code_Stmt_Decl *param = func->params[i];
        emit_type_prefix(e, param->type);
        builder_write(const_string(" "));
        builder_write(param->name);
        emit_type_postfix(e, param->type);
        
        if (i < sb_count(func->params) - 1) {
          builder_write(const_string(", "));
        }
      }
      builder_write(const_string(")"));
    } break;
    
    case Type_Kind_ARRAY: {
      Code_Type_Array *arr = &type->array;
      
      builder_write(const_string("["));
      emit_int(e, arr->count);
      builder_write(const_string("]"));
    } break;
    
    case Type_Kind_STRUCT: {
      
    } break;
    
    case Type_Kind_PTR: {
      
    } break;
    
    case Type_Kind_ALIAS: {
      
    } break;
    
    case Type_Kind_ENUM: {
      
    } break;
    
    default: assert(false);
  }
}

void emit_expr(Emitter *e, Code_Expr *expr) {
  switch (expr->kind) {
    case Expr_Kind_NAME: {
      builder_write(expr->name.name);
    } break;
    case Expr_Kind_UNARY: {
      Token_Kind op = expr->unary.op;
      String op_string = Token_Kind_To_String[op];
      assert(op_string.count);
      if (op == T_REF) {
        builder_write(const_string("&"));
      } else if (op == T_DEREF) {
        builder_write(const_string("*"));
      } else  {
        builder_write(op_string);
      }
      builder_write(const_string("("));
      emit_expr(e, expr->unary.val);
      builder_write(const_string(")"));
    } break;
    case Expr_Kind_BINARY: {
      String op = Token_Kind_To_String[expr->binary.op];
      assert(op.count);
      // TODO(lvl5): remove unneccessary parens by dealing with precedence
      if (expr->binary.op == T_MEMBER) {
        emit_expr(e, expr->binary.left);
        if (expr->binary.left->type->kind == Type_Kind_PTR) {
          builder_write(const_string("->"));
        } else {
          builder_write(op);
        }
        emit_expr(e, expr->binary.right);
      } else {
        builder_write(const_string("("));
        emit_expr(e, expr->binary.left);
        builder_write(const_string(" "));
        builder_write(op);
        builder_write(const_string(" "));
        emit_expr(e, expr->binary.right);
        builder_write(const_string(")"));
      }
    } break;
    case Expr_Kind_CALL: {
      emit_expr(e, expr->call.func);
      builder_write(const_string("("));
      for (u32 i = 0; i < sb_count(expr->call.args); i++) {
        Code_Expr *arg = expr->call.args[i];
        emit_expr(e, arg);
        if (i < sb_count(expr->call.args) - 1) {
          builder_write(const_string(", "));
        }
      }
      builder_write(const_string(")"));
    } break;
    case Expr_Kind_INT: {
      builder_write(i64_to_string(scratch_arena, expr->int_e.value));
    } break;
    case Expr_Kind_FLOAT: {
      builder_write(f64_to_string(scratch_arena, expr->float_e.value, 10));
    } break;
    case Expr_Kind_STRING: {
      builder_write(const_string("__string_make(\""));
      builder_write(expr->string.value);
      builder_write(const_string("\", "));
      builder_write(i64_to_string(scratch_arena, expr->string.value.count));
      builder_write(const_string(")"));
    } break;
    
    default: assert(false);
  }
}

void emit_decl(Emitter *, Code_Stmt_Decl *);
void emit_stmt(Emitter *, Code_Stmt *);

void emit_stmt_block(Emitter *e, Code_Stmt_Block *block) {
  builder_write(const_string("{\n"));
  e->indent++;
  for (u32 i = 0; i < sb_count(block->statements); i++) {
    emit_indent(e);
    emit_stmt(e, block->statements[i]);
    builder_write(const_string("\n"));
  }
  e->indent--;
  
  emit_indent(e);
  builder_write(const_string("}\n"));
}

void emit_stmt(Emitter *e, Code_Stmt *stmt) {
  switch (stmt->kind) {
    case Stmt_Kind_ASSIGN: {
      String op_string = Token_Kind_To_String[stmt->assign.op];
      emit_expr(e, stmt->assign.left);
      builder_write(const_string(" "));
      builder_write(op_string);
      builder_write(const_string(" "));
      emit_expr(e, stmt->assign.right);
      builder_write(const_string(";"));
    } break;
    case Stmt_Kind_EXPR: {
      emit_expr(e, stmt->expr.expr);
      builder_write(const_string(";"));
    } break;
    case Stmt_Kind_IF: {
      builder_write(const_string("if ("));
      emit_expr(e, stmt->if_s.cond);
      builder_write(const_string(") "));
      emit_stmt(e, stmt->if_s.then_branch);
      if (stmt->if_s.else_branch) {
        builder_write(const_string(" else "));
        emit_stmt(e, stmt->if_s.else_branch);
      }
    } break;
    case Stmt_Kind_BLOCK: {
      emit_stmt_block(e, &stmt->block);
    } break;
    case Stmt_Kind_FOR: {
      builder_write(const_string("for ("));
      emit_decl(e, stmt->for_s.init);
      builder_write(const_string("; "));
      emit_expr(e, stmt->for_s.cond);
      builder_write(const_string("; "));
      emit_stmt(e, stmt->for_s.post);
      builder_write(const_string(") "));
      emit_stmt(e, stmt->for_s.body);
    } break;
    case Stmt_Kind_KEYWORD: {
      builder_write(Token_Kind_To_String[stmt->keyword.keyword]);
      builder_write(const_string(" "));
      emit_expr(e, stmt->keyword.expr);
      builder_write(const_string(";"));
    } break;
    case Stmt_Kind_DECL: {
      emit_decl(e, &stmt->decl);
      builder_write(const_string(";"));
    } break;
    default: assert(false);
  }
}


void emit_decl(Emitter *e, Code_Stmt_Decl *decl) {
  if (decl->type->kind == Type_Kind_ALIAS && 
      string_compare(decl->type->alias.name, const_string("Type"))) {
    assert(decl->value->kind == Code_Kind_TYPE);
    Code_Type *type = &decl->value->type;
    
    if (decl->resolve_state == Resolve_State_UNRESOLVED &&
        e->res->need_state == Resolve_State_FULL) {
      builder_write(const_string("typedef "));
      
      emit_type_prefix(e, type);
      builder_write(const_string(" "));
      
      if (type->kind == Type_Kind_FUNC) {
        builder_write(const_string("(*"));
        builder_write(decl->name);
        builder_write(const_string(")"));
      } else {
        builder_write(decl->name);
      }
      emit_type_postfix(e, type);
    } else if (decl->resolve_state == Resolve_State_UNRESOLVED &&
               e->res->need_state == Resolve_State_PARTIAL) {
      assert(decl->value->type.kind == Type_Kind_STRUCT);
      builder_write(const_string("typedef struct "));
      builder_write(decl->name);
      builder_write(const_string(" "));
      builder_write(decl->name);
    } else if (decl->resolve_state == Resolve_State_PARTIAL &&
               e->res->need_state == Resolve_State_FULL) {
      assert(decl->value->type.kind == Type_Kind_STRUCT);
      builder_write(const_string("struct "));
      builder_write(decl->name);
      builder_write(const_string(" "));
      emit_struct_body(e, &type->struct_t);
    }
  } else {
    emit_type_prefix(e, decl->type);
    builder_write(const_string(" "));
    builder_write(decl->name);
    emit_type_postfix(e, decl->type);
    
    if (decl->value) {
      if (decl->value->kind == Code_Kind_EXPR) {
        builder_write(const_string(" = "));
        emit_expr(e, (Code_Expr *)decl->value);
      } else if (decl->value->kind == Code_Kind_FUNC &&
                 need_resolve(e->res, Resolve_State_FULL)) {
        builder_write(const_string(" "));
        emit_stmt_block(e, decl->value->func.body);
      } else {
        builder_write(const_string(";\n"));
      }
    }
  }
}

void resolve_decl(Resolver *res, Scope *scope, Code_Stmt_Decl *decl) {
  if (decl->resolve_state >= res->need_state) {
    return;
  }
  
  if (decl->type) {
    resolve_type(res, scope, decl->type);
  }
  
  if (decl->value) {
    switch (decl->value->kind) {
      case Code_Kind_TYPE: {
        if (need_resolve(res, Resolve_State_PARTIAL)) {
          assert(decl->value->type.kind == Type_Kind_STRUCT);
        } else {
          resolve_type(res, scope, &decl->value->type);
        }
        
        Code_Type *type = scope_get(scope, const_string("Type"))->decl->type;
        if (decl->type) {
          check_types(decl->type, type);
        } else {
          decl->type = type;
        }
      } break;
      
      case Code_Kind_EXPR: {
        resolve_expr(res, scope, &decl->value->expr);
        if (decl->type) {
          check_types(decl->type, decl->value->expr.type);
        } else {
          decl->type = decl->value->expr.type;
        }
      } break;
      
      case Code_Kind_FUNC: {
        Code_Type *func_type = (Code_Type *)decl->value->func.type;
        if (need_resolve(res, Resolve_State_PARTIAL)) {
          resolve_type(res, scope, func_type);
        } else {
          resolve_func(res, scope, &decl->value->func);
        }
        
        if (decl->type) {
          check_types(decl->type, func_type);
        } else {
          decl->type = func_type;
        }
      } break;
      
      default: assert(false);
    }
  }
  scope_add(scope, decl);
}

void resolve_and_emit_decl(Resolver *res, Scope *scope, Code_Stmt_Decl *decl) {
  if (decl->resolve_state >= res->need_state) {
    return;
  }
  
  resolve_decl(res, scope, decl);
  Emitter emitter = {0};
  emitter.indent = 0;
  emitter.res = res;
  emit_decl(&emitter, decl);
  decl->resolve_state = res->need_state;
  
  if (decl->value && decl->type->kind == Type_Kind_FUNC) {
    builder_write(const_string("\n"));
  } else {
    builder_write(const_string(";\n\n"));
  }
}


void resolve_decls(Resolver *res, Parse_Result parsed, Scope *global_scope) {
  for (u32 decl_index = 0; decl_index < sb_count(parsed.decls); decl_index++) {
    Code_Stmt_Decl *decl = parsed.decls[decl_index];
    res->decl = decl;
    res->need_state = Resolve_State_FULL;
    resolve_and_emit_decl(res, global_scope, decl);
  }
}

int main() {
  Arena _arena;
  Arena *arena = &_arena;
  arena_init(arena, malloc(megabytes(10)), megabytes(10));
  arena_init(scratch_arena, malloc(megabytes(1)), megabytes(1));
  
  
#if 0  
  builtin_Type = arena_push_struct(arena, Type_Info);
  builtin_Type->kind = Type_Info_Kind_TYPE;
  builtin_Type->size = 8;
  
  builtin_f32 = arena_push_struct(arena, Type_Info);
  builtin_f32->kind = Type_Info_Kind_FLOAT;
  builtin_f32->size = 4;
  
  builtin_i32 = arena_push_struct(arena, Type_Info);
  builtin_i32->kind = Type_Info_Kind_INT;
  builtin_i32->size = 4;
  builtin_i32->int_t.is_signed = true;
#endif
  
  
  
  
  Buffer file = read_entire_file(arena, const_string("test.lang"));
  file.data[file.size++] = 0;
  String src = make_string((char *)file.data, (u32)file.size);
  Token *tokens = tokenize(arena, src);
  
  Parser _p = {0};
  Parser *p = &_p;
  p->arena = arena;
  p->tokens = tokens;
  p->i = 0;
  p->src = src;
  
  parser = p;
  
  Scope *global_scope = alloc_scope(arena, null);
  
#define add_default_type(name) \
  scope_add(global_scope, code_stmt_decl(p, const_string(name),(Code_Type *)code_type_alias(p, const_string("Type")),(Code_Node *)code_type_alias(p, const_string(name))));
  add_default_type("Type");
  add_default_type("void");
  add_default_type("u8");
  add_default_type("u16");
  add_default_type("u32");
  add_default_type("u64");
  add_default_type("i8");
  add_default_type("i16");
  add_default_type("i32");
  add_default_type("i64");
  add_default_type("f32");
  add_default_type("f64");
  add_default_type("b32");
  add_default_type("b8");
  add_default_type("byte");
  add_default_type("char");
  
  Parse_Result parse_result = parse(p, src, tokens);
  
  Resolver res;
  res.top_decls = parse_result.decls;
  res.arena = arena;
  resolve_decls(&res, parse_result, global_scope);
  
  return 0;
}
