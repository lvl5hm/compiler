#include "typechecker.c"

/*
TODO:
[ ] lvalues, rvalues, assignment, referencing and dereferencing
[ ] think about cast syntax, probably should be something like cast(val, type)
[ ] functions should be emitted as pointers except for constant declarations
[ ] constant declarations
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


typedef struct {
  Code_Stmt_Decl **decls;
} Parse_Result;

Parse_Result parse(Parser *p, String src, Token *tokens) {
  Parse_Result result = {0};
  result.decls = parse_program(p);
  
  assert(p->error_count == 0);
  
  return result;
}

typedef struct {
  i32 indent;
  Resolver *res;
} Emitter;

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
    case Expr_Kind_CAST: {
      builder_write(const_string("("));
      emit_type_prefix(e, expr->cast.cast_type);
      emit_type_postfix(e, expr->cast.cast_type);
      builder_write(const_string(")("));
      emit_expr(e, expr->cast.expr);
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
  res.parser = p;
  
  resolve_decls(&res, parse_result, global_scope);
  
  return 0;
}
