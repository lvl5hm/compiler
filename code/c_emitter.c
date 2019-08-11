#include "typechecker.c"


void emit_string(Emitter *e, String str) {
  builder_write(e->builder, str);
}

void emit_indent(Emitter *e) {
  for (i32 indent_index = 0; indent_index < e->indent; indent_index++) {
    emit_string(e, const_string("  "));
  }
}

void emit_int(Emitter *e, i64 value) {
  printf(tcstring(i64_to_string(scratch_arena, value)));
}

void emit_type_postfix(Emitter *, Code_Type *);
void emit_type_prefix(Emitter *, Code_Type *);
void emit_decl(Emitter *, Code_Stmt_Decl *, Resolve_State);

void emit_struct_body(Emitter *e, Code_Type_Struct *str) {
  emit_string(e, const_string("{\n"));
  
  e->indent++;
  for (u32 i = 0; i < sb_count(str->members); i++) {
    emit_indent(e);
    Code_Stmt_Decl *member = str->members[i];
    emit_type_prefix(e, member->type);
    emit_string(e, const_string(" "));
    emit_string(e, member->name);
    emit_type_postfix(e, member->type);
    emit_string(e, const_string(";\n"));
  }
  e->indent--;
  
  emit_string(e, const_string("}"));
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
      emit_string(e, const_string("struct "));
      Code_Type_Struct *str = &type->struct_t;
      emit_struct_body(e, str);
    } break;
    
    case Type_Kind_PTR: {
      Code_Type_Pointer *ptr = &type->pointer;
      emit_type_prefix(e, ptr->base);
      emit_string(e, const_string("*"));
    } break;
    
    case Type_Kind_ALIAS: {
      emit_string(e, type->alias.name);
    } break;
    
    case Type_Kind_ENUM: {
      Code_Type_Enum *str = &type->enum_t;
      emit_string(e, const_string("enum {\n"));
      
      e->indent++;
      for (u32 i = 0; i < sb_count(str->members); i++) {
        emit_indent(e);
        String member = str->members[i];
        emit_string(e, e->enum_name);
        emit_string(e, const_string("_"));
        emit_string(e, member);
        emit_string(e, const_string(",\n"));
      }
      e->indent--;
      
      emit_string(e, const_string("}"));
    } break;
    
    default: assert(false);
  }
}

void emit_type_postfix(Emitter *e, Code_Type *type) {
  switch (type->kind) {
    case Type_Kind_FUNC: {
      Code_Type_Func *func = &type->func;
      emit_string(e, const_string("("));
      for (u32 i = 0; i < sb_count(func->params); i++) {
        Code_Stmt_Decl *param = func->params[i];
        emit_type_prefix(e, param->type);
        emit_string(e, const_string(" "));
        emit_string(e, param->name);
        emit_type_postfix(e, param->type);
        
        if (i < sb_count(func->params) - 1) {
          emit_string(e, const_string(", "));
        }
      }
      emit_string(e, const_string(")"));
    } break;
    
    case Type_Kind_ARRAY: {
      Code_Type_Array *arr = &type->array;
      
      emit_string(e, const_string("["));
      emit_int(e, arr->count);
      emit_string(e, const_string("]"));
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
      emit_string(e, expr->name.name);
    } break;
    case Expr_Kind_UNARY: {
      Token_Kind op = expr->unary.op;
      String op_string = Token_Kind_To_String[op];
      assert(op_string.count);
      if (op == T_REF) {
        emit_string(e, const_string("&"));
      } else if (op == T_DEREF) {
        emit_string(e, const_string("*"));
      } else  {
        emit_string(e, op_string);
      }
      emit_string(e, const_string("("));
      emit_expr(e, expr->unary.val);
      emit_string(e, const_string(")"));
    } break;
    case Expr_Kind_BINARY: {
      String op = Token_Kind_To_String[expr->binary.op];
      assert(op.count);
      // TODO(lvl5): remove unneccessary parens by dealing with precedence
      switch (expr->binary.op) {
        case T_MEMBER: {
          emit_expr(e, expr->binary.left);
          if (expr->binary.is_enum_member) {
            emit_string(e, const_string("_"));
          } else if (expr->binary.left->type->kind == Type_Kind_PTR) {
            emit_string(e, const_string("->"));
          } else {
            emit_string(e, const_string("."));
          }
          emit_expr(e, expr->binary.right);
        } break;
        case T_SUBSCRIPT: {
          emit_string(e, const_string("("));
          emit_expr(e, expr->binary.left);
          emit_string(e, const_string(")"));
          emit_string(e, const_string("["));
          emit_expr(e, expr->binary.right);
          emit_string(e, const_string("]"));
        } break;
        default: {
          emit_string(e, const_string("("));
          emit_expr(e, expr->binary.left);
          emit_string(e, const_string(" "));
          emit_string(e, op);
          emit_string(e, const_string(" "));
          emit_expr(e, expr->binary.right);
          emit_string(e, const_string(")"));
        } break;
      }
    } break;
    case Expr_Kind_CALL: {
      emit_expr(e, expr->call.func);
      emit_string(e, const_string("("));
      for (u32 i = 0; i < sb_count(expr->call.args); i++) {
        Code_Expr *arg = expr->call.args[i];
        emit_expr(e, arg);
        if (i < sb_count(expr->call.args) - 1) {
          emit_string(e, const_string(", "));
        }
      }
      emit_string(e, const_string(")"));
    } break;
    case Expr_Kind_CAST: {
      if (expr->cast.implicit) {
        emit_expr(e, expr->cast.expr);
      } else {
        emit_string(e, const_string("("));
        emit_type_prefix(e, expr->cast.cast_type);
        emit_type_postfix(e, expr->cast.cast_type);
        emit_string(e, const_string(")("));
        emit_expr(e, expr->cast.expr);
        emit_string(e, const_string(")"));
      }
    } break;
    case Expr_Kind_INT: {
      emit_string(e, i64_to_string(scratch_arena, expr->int_e.value));
    } break;
    case Expr_Kind_FLOAT: {
      emit_string(e, f64_to_string(scratch_arena, expr->float_e.value, 10));
    } break;
    case Expr_Kind_STRING: {
      emit_string(e, const_string("__string_const(\""));
      emit_string(e, expr->string.value);
      emit_string(e, const_string("\", "));
      emit_string(e, i64_to_string(scratch_arena, expr->string.value.count));
      emit_string(e, const_string(", ctx)"));
    } break;
    case Expr_Kind_NULL: {
      emit_string(e, const_string("NULL"));
    } break;
    
    default: assert(false);
  }
}

void emit_stmt(Emitter *, Code_Stmt *, b32);

void emit_stmt_block(Emitter *e, Code_Stmt_Block *block) {
  emit_string(e, const_string("{\n"));
  e->indent++;
  for (u32 i = 0; i < sb_count(block->statements); i++) {
    emit_indent(e);
    emit_stmt(e, block->statements[i], true);
    emit_string(e, const_string("\n"));
  }
  e->indent--;
  
  emit_indent(e);
  emit_string(e, const_string("}"));
}

void emit_stmt(Emitter *e, Code_Stmt *stmt, b32 semi) {
  switch (stmt->kind) {
    case Stmt_Kind_ASSIGN: {
      String op_string = Token_Kind_To_String[stmt->assign.op];
      emit_expr(e, stmt->assign.left);
      emit_string(e, const_string(" "));
      emit_string(e, op_string);
      emit_string(e, const_string(" "));
      emit_expr(e, stmt->assign.right);
      
      if (semi) {
        emit_string(e, const_string(";"));
      }
    } break;
    case Stmt_Kind_EXPR: {
      emit_expr(e, stmt->expr.expr);
      if (semi) {
        emit_string(e, const_string(";"));
      }
    } break;
    case Stmt_Kind_IF: {
      emit_string(e, const_string("if ("));
      emit_expr(e, stmt->if_s.cond);
      emit_string(e, const_string(") "));
      emit_stmt(e, stmt->if_s.then_branch, true);
      if (stmt->if_s.else_branch) {
        emit_string(e, const_string(" else "));
        emit_stmt(e, stmt->if_s.else_branch, true);
      }
    } break;
    case Stmt_Kind_BLOCK: {
      emit_stmt_block(e, &stmt->block);
    } break;
    case Stmt_Kind_FOR: {
      emit_string(e, const_string("for ("));
      emit_stmt(e, stmt->for_s.init, false);
      emit_string(e, const_string("; "));
      emit_expr(e, stmt->for_s.cond);
      emit_string(e, const_string("; "));
      emit_stmt(e, stmt->for_s.post, /*semi*/ false);
      emit_string(e, const_string(") "));
      emit_stmt(e, stmt->for_s.body, true);
    } break;
    case Stmt_Kind_KEYWORD: {
      switch (stmt->keyword.keyword) {
        case T_RETURN: {
          emit_string(e, Token_Kind_To_String[stmt->keyword.keyword]);
          emit_string(e, const_string(" "));
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
        emit_string(e, const_string(";"));
      }
    } break;
    default: assert(false);
  }
}

void emit_decl_full(Emitter *e, Code_Stmt_Decl *decl) {
  if (decl->type == builtin_Type) {
    Code_Type *type = &decl->value->expr.type_e;
    
    emit_string(e, const_string("typedef "));
    
    e->enum_name = decl->name;
    emit_type_prefix(e, type);
    emit_string(e, const_string(" "));
    
    if (type->kind == Type_Kind_FUNC) {
      emit_string(e, const_string("(*"));
      emit_string(e, decl->name);
      emit_string(e, const_string(")"));
    } else {
      emit_string(e, decl->name);
    }
    emit_type_postfix(e, type);
    e->enum_name = (String){0};
  } else {
    if (decl->value && decl->value->kind == Code_Kind_FUNC) {
      if (decl->value->func.foreign) {
        emit_string(e, const_string("extern "));
      }
    }
    
    emit_type_prefix(e, decl->type);
    emit_string(e, const_string(" "));
    emit_string(e, decl->name);
    emit_type_postfix(e, decl->type);
    
    if (decl->value) {
      if (decl->value->kind == Code_Kind_EXPR) {
        emit_string(e, const_string(" = "));
        emit_expr(e, (Code_Expr *)decl->value);
      } else if (decl->value->kind == Code_Kind_FUNC) {
        if (decl->value->func.foreign) {
          emit_string(e, const_string(";"));
        } else {
          emit_string(e, const_string(" "));
          emit_stmt_block(e, decl->value->func.body);
        }
      }
    }
  }
}

void emit_decl(Emitter *e, Code_Stmt_Decl *decl, Resolve_State need_state) {
  switch (need_state) {
    case Resolve_State_PARTIAL: {
      if (decl->value->kind == Code_Kind_FUNC) {
        emit_type_prefix(e, decl->type);
        emit_string(e, const_string(" "));
        emit_string(e, decl->name);
        emit_type_postfix(e, decl->type);
        
        emit_string(e, const_string(";"));
        decl->emit_state = Resolve_State_PARTIAL;
      } else if (decl->type == builtin_Type &&
                 decl->value->expr.type_e.kind == Type_Kind_STRUCT) {
        emit_string(e, const_string("typedef struct "));
        emit_string(e, decl->name);
        emit_string(e, const_string(" "));
        emit_string(e, decl->name);
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
        emit_string(e, const_string("struct "));
        emit_string(e, decl->name);
        emit_string(e, const_string(" "));
        
        Code_Type *type = &decl->value->expr.type_e;
        emit_struct_body(e, &type->struct_t);
      } else {
        emit_decl_full(e, decl);
      }
      decl->emit_state = Resolve_State_FULL;
    } break;
  }
}

void emit_top_decl(Emitter *e, Code_Stmt_Decl *decl, Resolve_State need_state) {
  emit_decl(e, decl, need_state);
  if (decl->value && decl->type->kind == Type_Kind_FUNC) {
    emit_string(e, const_string("\n\n"));
  } else if (decl->value && decl->value->kind == Code_Kind_EXPR &&
             decl->value->expr.kind == Expr_Kind_TYPE &&
             (decl->value->expr.type_e.kind == Type_Kind_STRUCT ||
              decl->value->expr.type_e.kind == Type_Kind_ENUM)) {
    emit_string(e, const_string(";\n\n"));
  } else {
    emit_string(e, const_string(";\n"));
  }
}