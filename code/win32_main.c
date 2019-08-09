#include "typechecker.c"

/*
        // it would be cool to check if param type is unsigned, then
        // check if arg >= 0 and wrap it into implicit cast.
        // should also check arg for size and implicit cast it
        // if it fits
        
        // automatically convert ints to floats
        // TODO(lvl5): also need to think about parsing, how do you decide
        // that the storage value needs to be u64 or i64?
        // TODO(lvl5): add a null pointer value
        // TODO(lvl5): make a function like
        // Code_Expr *maybe_implicit_cast(expr, type)
        // that maybe wraps the expression into implicit cast
        // possibly have special implicit casts inside if statements
        // like pointer to bool
        // TODO(lvl5): implicit cast to *void
        // TODO(lvl5): implicit cast int and float expressions
        
        
        [ ] automatically cast int and float literals if type of declaration is specified
        [ ] detect if int literal overflows type / is unsigned 
[ ] automatically cast to *void
 [ ] disallow pointer arithmetic on *void
[ ] defer
[ ] functions should be emitted as pointers except for constant declarations
[ ] sizeof
[ ] error reporting in the typechecker
[ ] compile an actual executable
[ ] redefinition should be an error

[ ] fixed length arrays
[ ] dynamic arrays
[ ] array views

[ ] decide what casts are allowed
[ ] lvalues, rvalues, assignment, referencing and dereferencing
[ ] if else block formatting
[ ] typedefs and functions inside functions
[ ] better for loop
-iterate int ranges
-iterate arrays
-iterate enums
[ ] while loop
[ ] enum to string, enum count
[ ] static struct members (enum names are essentially static members already)
[ ] initializers for structs
[ ] default parameters for funcs
[ ] allow omitting names in functions without bodies/typedefs??
[ ] some kind of short syntax for functions when type already has an alias (what do you do with typedefs that omit param names?)

[ ] implicit conversions
[ ] runtime Type_Info
[ ] varargs (Any type??)
[ ] modules
[ ] using
[ ] polymorphic functions????
[ ] compile time execution?????????????????
[ ] preserve comments
*/

typedef struct {
  Code_Stmt_Decl **decls;
  b32 success;
} Parse_Result;

Parse_Result parse(Parser *p, String src, Token *tokens) {
  Parse_Result result = {0};
  result.decls = parse_program(p);
  if (sb_count(p->errors) > 0) {
    for (u32 i = 0; i < sb_count(p->errors); i++) {
      printf("Parser error: %s\n\n", tcstring(p->errors[i]));
    }
    result.success = false;
  } else {
    result.success = true;
  }
  
  return result;
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
        builder_write(e->enum_name);
        builder_write(const_string("_"));
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
    case Expr_Kind_TYPE: {
      emit_type_prefix(e, &expr->type_e);
      emit_type_postfix(e, &expr->type_e);
    } break;
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
      switch (expr->binary.op) {
        case T_MEMBER: {
          emit_expr(e, expr->binary.left);
          if (expr->binary.is_enum_member) {
            builder_write(const_string("_"));
          } else if (expr->binary.left->type->kind == Type_Kind_PTR) {
            builder_write(const_string("->"));
          } else {
            builder_write(const_string("."));
          }
          emit_expr(e, expr->binary.right);
        } break;
        case T_SUBSCRIPT: {
          emit_expr(e, expr->binary.left);
          builder_write(const_string("["));
          emit_expr(e, expr->binary.right);
          builder_write(const_string("]"));
        } break;
        default: {
          builder_write(const_string("("));
          emit_expr(e, expr->binary.left);
          builder_write(const_string(" "));
          builder_write(op);
          builder_write(const_string(" "));
          emit_expr(e, expr->binary.right);
          builder_write(const_string(")"));
        } break;
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
      if (expr->cast.implicit) {
        emit_expr(e, expr->cast.expr);
      } else {
        builder_write(const_string("("));
        emit_type_prefix(e, expr->cast.cast_type);
        emit_type_postfix(e, expr->cast.cast_type);
        builder_write(const_string(")("));
        emit_expr(e, expr->cast.expr);
        builder_write(const_string(")"));
      }
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
      builder_write(const_string(", ctx)"));
    } break;
    case Expr_Kind_NULL: {
      builder_write(const_string("NULL"));
    } break;
    
    default: assert(false);
  }
}

void emit_stmt(Emitter *, Code_Stmt *, b32);

void emit_stmt_block(Emitter *e, Code_Stmt_Block *block) {
  builder_write(const_string("{\n"));
  e->indent++;
  for (u32 i = 0; i < sb_count(block->statements); i++) {
    emit_indent(e);
    emit_stmt(e, block->statements[i], true);
    builder_write(const_string("\n"));
  }
  e->indent--;
  
  emit_indent(e);
  builder_write(const_string("}"));
}

void emit_stmt(Emitter *e, Code_Stmt *stmt, b32 semi) {
  switch (stmt->kind) {
    case Stmt_Kind_ASSIGN: {
      String op_string = Token_Kind_To_String[stmt->assign.op];
      emit_expr(e, stmt->assign.left);
      builder_write(const_string(" "));
      builder_write(op_string);
      builder_write(const_string(" "));
      emit_expr(e, stmt->assign.right);
      
      if (semi) {
        builder_write(const_string(";"));
      }
    } break;
    case Stmt_Kind_EXPR: {
      emit_expr(e, stmt->expr.expr);
      if (semi) {
        builder_write(const_string(";"));
      }
    } break;
    case Stmt_Kind_IF: {
      builder_write(const_string("if ("));
      emit_expr(e, stmt->if_s.cond);
      builder_write(const_string(") "));
      emit_stmt(e, stmt->if_s.then_branch, true);
      if (stmt->if_s.else_branch) {
        builder_write(const_string(" else "));
        emit_stmt(e, stmt->if_s.else_branch, true);
      }
    } break;
    case Stmt_Kind_BLOCK: {
      emit_stmt_block(e, &stmt->block);
    } break;
    case Stmt_Kind_FOR: {
      builder_write(const_string("for ("));
      emit_stmt(e, stmt->for_s.init, false);
      builder_write(const_string("; "));
      emit_expr(e, stmt->for_s.cond);
      builder_write(const_string("; "));
      emit_stmt(e, stmt->for_s.post, /*semi*/ false);
      builder_write(const_string(") "));
      emit_stmt(e, stmt->for_s.body, true);
    } break;
    case Stmt_Kind_KEYWORD: {
      switch (stmt->keyword.keyword) {
        case T_RETURN: {
          builder_write(Token_Kind_To_String[stmt->keyword.keyword]);
          builder_write(const_string(" "));
          emit_stmt(e, stmt->keyword.stmt, true);
        } break;
        
        case T_PUSH_CONTEXT: {
          emit_stmt(e, stmt->keyword.stmt, true);
        } break;
        
        case T_DEFER: break;
        
        default: assert(false);
      }
    } break;
    case Stmt_Kind_DECL: {
      emit_decl(e, &stmt->decl, Resolve_State_FULL);
      if (semi) {
        builder_write(const_string(";"));
      }
    } break;
    default: assert(false);
  }
}

void emit_decl_full(Emitter *e, Code_Stmt_Decl *decl) {
  if (decl->type == builtin_Type) {
    Code_Type *type = &decl->value->expr.type_e;
    
    builder_write(const_string("typedef "));
    
    e->enum_name = decl->name;
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
    e->enum_name = (String){0};
  } else {
    emit_type_prefix(e, decl->type);
    builder_write(const_string(" "));
    builder_write(decl->name);
    emit_type_postfix(e, decl->type);
    
    if (decl->value) {
      if (decl->value->kind == Code_Kind_EXPR) {
        builder_write(const_string(" = "));
        emit_expr(e, (Code_Expr *)decl->value);
      } else if (decl->value->kind == Code_Kind_FUNC) {
        builder_write(const_string(" "));
        emit_stmt_block(e, decl->value->func.body);
      }
    }
  }
}

void emit_decl(Emitter *e, Code_Stmt_Decl *decl, Resolve_State need_state) {
  switch (need_state) {
    case Resolve_State_PARTIAL: {
      if (decl->value->kind == Code_Kind_FUNC) {
        emit_type_prefix(e, decl->type);
        builder_write(const_string(" "));
        builder_write(decl->name);
        emit_type_postfix(e, decl->type);
        
        builder_write(const_string(";"));
      } else if (decl->type == builtin_Type &&
                 decl->value->expr.type_e.kind == Type_Kind_STRUCT) {
        builder_write(const_string("typedef struct "));
        builder_write(decl->name);
        builder_write(const_string(" "));
        builder_write(decl->name);
        decl->emit_state = Resolve_State_PARTIAL;
      } else {
        emit_decl_full(e, decl);
        decl->emit_state = Resolve_State_FULL;
      }
    } break;
    
    case Resolve_State_FULL: {
      if (decl->emit_state == Resolve_State_PARTIAL &&
          decl->type == builtin_Type &&
          decl->value->expr.type_e.kind == Type_Kind_STRUCT) {
        builder_write(const_string("struct "));
        builder_write(decl->name);
        builder_write(const_string(" "));
        
        Code_Type *type = &decl->value->expr.type_e;
        emit_struct_body(e, &type->struct_t);
      } else {
        emit_decl_full(e, decl);
      }
      decl->emit_state = Resolve_State_FULL;
    } break;
  }
}

void resolve_decls(Resolver res, Parse_Result parsed, Scope *global_scope) {
  for (u32 decl_index = 0; decl_index < sb_count(parsed.decls); decl_index++) {
    Code_Stmt_Decl *decl = parsed.decls[decl_index];
    res.decl = decl;
    resolve_and_emit_decl(res, global_scope, decl, Resolve_State_FULL);
  }
}

Code_Type *add_default_type(Parser *p, Scope *global_scope, String name) {
  Code_Type_Alias *type = code_type_alias(p, name);
  type->is_builtin = true;
  
  Code_Type *type_of_type = string_compare(name, const_string("Type"))
    ? (Code_Type *)type
    : builtin_Type;
  
  scope_add(global_scope, code_stmt_decl(p, name, type_of_type, (Code_Node *)type, true));
  return (Code_Type *)type;
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
  p->errors = sb_new(arena, String, 16);
  
  Scope *global_scope = alloc_scope(arena, null);
  
  // TODO(lvl5): interning types somehow?
  builtin_Type = add_default_type(p, global_scope, const_string("Type"));
  builtin_u8 = add_default_type(p, global_scope, const_string("u8"));
  builtin_u16 = add_default_type(p, global_scope, const_string("u16"));
  builtin_u32 = add_default_type(p, global_scope, const_string("u32"));
  builtin_u64 = add_default_type(p, global_scope, const_string("u64"));
  builtin_i8 = add_default_type(p, global_scope, const_string("i8"));
  builtin_i16 = add_default_type(p, global_scope, const_string("i16"));
  builtin_i32 = add_default_type(p, global_scope, const_string("i32"));
  builtin_i64 = add_default_type(p, global_scope, const_string("i64"));
  
  add_default_type(p, global_scope, const_string("b8"));
  add_default_type(p, global_scope, const_string("b32"));
  add_default_type(p, global_scope, const_string("byte"));
  add_default_type(p, global_scope, const_string("char"));
  builtin_f32 = add_default_type(p, global_scope, const_string("f32"));
  add_default_type(p, global_scope, const_string("f64"));
  builtin_void = add_default_type(p, global_scope, const_string("void"));
  builtin_voidptr = (Code_Type *)code_type_pointer(p, builtin_void);
  
  
  Parse_Result parse_result = parse(p, src, tokens);
  if (parse_result.success) {
    Resolver res = {0};
    res.top_decls = parse_result.decls;
    res.arena = arena;
    res.parser = p;
    
    resolve_decls(res, parse_result, global_scope);
  }
  
  return 0;
}
