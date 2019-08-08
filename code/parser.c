#include "parser.h"

typedef enum {
  Dep_Kind_NONE,
  
  Dep_Kind_EXISTS,
  Dep_Kind_SIGNATURE,
  Dep_Kind_HARD,
} Dep_Kind;


typedef struct Dep_Node Dep_Node;

struct Dep_Node {
  Code_Stmt_Decl *decl;
  Dep_Kind kind;
  Dep_Node **dependents;
  i32 dep_count;
};

Token ERROR_TOKEN = {0};

Token parser_get(Parser *p, i32 offset) {
  Token result = {0};
  u32 index = p->i + offset;
  if (index < sb_count(p->tokens) && index >= 0) {
    result = p->tokens[index];
  } else {
    result = ERROR_TOKEN;
  }
  return result;
}

b32 parser_peek(Parser *p, i32 offset, Token_Kind kind) {
  b32 result = parser_get(p, offset).kind == kind;
  return result;
}

void parser_next(Parser *p) {
  p->i++;
}

b32 parser_peek_range(Parser *p, i32 offset, Token_Kind kind_first, Token_Kind kind_last) {
  Token_Kind kind = parser_get(p, offset).kind;
  b32 result = (kind >= kind_first) && (kind <= kind_last);
  return result;
}

b32 parser_accept(Parser *p, Token_Kind kind) {
  Token t = parser_get(p, 0);
  b32 result = false;
  if (t.kind == kind) {
    result = true;
    assert(p->i < sb_count(p->tokens));
    p->i++;
  }
  return result;
}

b32 parser_accept_range(Parser *p, Token_Kind first, Token_Kind last) {
  Token t = parser_get(p, 0);
  b32 result = false;
  if (t.kind >= first && t.kind <= last) {
    result = true;
    assert(p->i < sb_count(p->tokens));
    p->i++;
  }
  return result;
}

Token parser_prev(Parser *p) {
  Token result = parser_get(p, -1);
  return result;
}

Token parser_expect(Parser *p, Token_Kind kind) {
  Token result = { .kind = T_ERROR };
  if (parser_accept(p, kind)) {
    result = parser_get(p, -1);
  } else {
    Token t = parser_get(p, 0);
    char *expected = Token_Kind_To_String[kind].data;
    char *got = to_c_string(p->arena, t.value);
    
    String line_str = get_line_from_index(p->src, (u32)(t.value.data - p->src.data));
    char *line = to_c_string(p->arena, line_str);
    
    syntax_error("Expected token \"%s\", got \"%s\" at %d:%d\n%s",
                 expected, got, t.line, t.col, line);
    p->i++;
    p->error_count++;
  }
  return result;
}


Code_Expr *parse_expr(Parser *p);
Code_Type *parse_type(Parser *p);
Code_Stmt_Decl *parse_decl(Parser *p);
Code_Stmt_Block *parse_stmt_block(Parser *p);


Code_Type_Func *parse_type_func(Parser *p) {
  Code_Type_Func *result = 0;
  parser_expect(p, T_FUNC);
  parser_expect(p, T_LPAREN);
  
  Code_Stmt_Decl **params = sb_new(p->arena, Code_Stmt_Decl *, 8);
  while (!parser_accept(p, T_RPAREN)) {
    Code_Stmt_Decl *param = parse_decl(p);
    sb_push(params, param);
    
    if (!parser_accept(p, T_COMMA)) {
      parser_expect(p, T_RPAREN);
      break;
    }
  }
  
  Code_Type *return_type = parse_type(p);
  
  result = code_type_func(p, params, return_type);
  return result;
}

Code_Type *parse_type(Parser *p) {
  Code_Type *result = 0;
  if (parser_accept(p, T_NAME)) {
    String name = parser_prev(p).value;
    result = (Code_Type *)code_type_alias(p, name);
  } else if (parser_accept(p, T_REF)) {
    Code_Type *base = parse_type(p);
    result = (Code_Type *)code_type_pointer(p, base);
  } else if (parser_accept(p, T_STRUCT)) {
    parser_expect(p, T_LCURLY);
    
    Code_Stmt_Decl **members = sb_new(p->arena, Code_Stmt_Decl *, 8);
    while (!parser_accept(p, T_RCURLY)) {
      Code_Stmt_Decl *member = parse_decl(p);
      parser_expect(p, T_SEMI);
      sb_push(members, member);
    }
    
    result = (Code_Type *)code_type_struct(p, members);
  } else if (parser_accept(p, T_ENUM)) {
    parser_expect(p, T_LCURLY);
    
    String *members = sb_new(p->arena, String, 16);
    while (!parser_accept(p, T_RCURLY)) {
      Token member = parser_expect(p, T_NAME);
      parser_expect(p, T_SEMI);
      sb_push(members, member.value);
    }
    result = (Code_Type *)code_type_enum(p, members);
  } else if (parser_accept(p, T_LBRACKET)) {
    Token t_count = parser_expect(p, T_INT);
    u64 count = string_to_u64(t_count.value);
    parser_expect(p, T_RBRACKET);
    Code_Type *element_type = parse_type(p);
    
    result = (Code_Type *)code_type_array(p, element_type, count);
  } else if (parser_peek(p, 0, T_FUNC)) {
    result = (Code_Type *)parse_type_func(p);
  } else if (parser_accept(p, T_LPAREN)) {
    result = (Code_Type *)parse_type(p);
    parser_expect(p, T_RPAREN);
  }
  return result;
}

Code_Stmt_Decl *parse_stmt_decl(Parser *p) {
  Code_Stmt_Decl *decl = parse_decl(p);
  
  Code_Node *value = decl->value;
  if (value && 
      (value->kind == Code_Kind_FUNC ||
       (value->kind == Code_Kind_EXPR &&
        value->expr.kind == Expr_Kind_TYPE &&
        (value->expr.type_e.kind == Type_Kind_STRUCT ||
         value->expr.type_e.kind == Type_Kind_ENUM)))) {
    
  } else {
    parser_expect(p, T_SEMI);
  }
  
  
  return decl;
}

Code_Stmt *parse_stmt(Parser *p, b32 expect_semi) {
  Code_Stmt *result = 0;
  if (parser_peek(p, 0, T_NAME) && parser_peek(p, 1, T_COLON)) {
    result = (Code_Stmt *)parse_stmt_decl(p);
  } else if (parser_accept(p, T_IF)) {
    Code_Expr *cond = parse_expr(p);
    Code_Stmt *then_branch = parse_stmt(p, true);
    Code_Stmt *else_branch = 0;
    if (parser_accept(p, T_ELSE)) {
      else_branch = parse_stmt(p, true);
    }
    result = (Code_Stmt *)code_stmt_if(p, cond, then_branch, else_branch);
  } else if (parser_accept(p, T_FOR)) {
    Code_Stmt_Decl *init = parse_decl(p);
    parser_expect(p, T_SEMI);
    Code_Expr *cond = parse_expr(p);
    parser_expect(p, T_SEMI);
    Code_Stmt *post = parse_stmt(p, false);
    Code_Stmt *body = parse_stmt(p, true);
    
    result = (Code_Stmt *)code_stmt_for(p, init, cond, post, body);
  } else if (parser_peek(p, 0, T_RETURN) ||
             parser_peek(p, 0, T_BREAK) ||
             parser_peek(p, 0, T_CONTINUE) ||
             parser_peek(p, 0, T_DEFER)) {
    Token_Kind keyword = parser_get(p, 0).kind;
    parser_expect(p, keyword);
    
    Code_Stmt *stmt = 0;
    if (!parser_peek(p, 0, T_SEMI)) {
      stmt = parse_stmt(p, true);
    }
    result = (Code_Stmt *)code_stmt_keyword(p, keyword, stmt, null);
  } else if (parser_peek(p, 0, T_PUSH_CONTEXT)) {
    parser_expect(p, T_PUSH_CONTEXT);
    Code_Expr *extra = parse_expr(p);
    Code_Stmt *stmt = parse_stmt(p, true);
    result = (Code_Stmt *)code_stmt_keyword(p, T_PUSH_CONTEXT, stmt, extra);
  } else if (parser_peek(p, 0, T_LCURLY)) {
    result = (Code_Stmt *)parse_stmt_block(p);
  } else {
    Code_Expr *left = parse_expr(p);
    if (parser_accept(p, T_SEMI)) {
      result = (Code_Stmt *)code_stmt_expr(p, left);
    } else if (parser_peek_range(p, 0, T_ASSIGN_FIRST, T_ASSIGN_LAST)) {
      Token_Kind op = parser_get(p, 0).kind;
      parser_expect(p, op);
      Code_Expr *right = parse_expr(p);
      if (expect_semi) {
        parser_expect(p, T_SEMI);
      }
      result = (Code_Stmt *)code_stmt_assign(p, left, op, right);
    } else {
      syntax_error("Expected an assignment statement or an expression.\n");
    }
  }
  
  return result;
}

Code_Expr *parse_expr_atom(Parser *p) {
  Code_Expr *result = 0;
  
  if (parser_accept(p, T_LPAREN)) {
    Code_Expr *inner = parse_expr(p);
    parser_expect(p, T_RPAREN);
    result = inner;
  } else if (parser_accept(p, T_NAME)) {
    String name = parser_prev(p).value;
    result = (Code_Expr *)code_expr_name(p, name);
  } else if (parser_accept(p, T_INT)) {
    i64 value = string_to_i64(parser_prev(p).value);
    result = (Code_Expr *)code_expr_int(p, value);
  } else if (parser_accept(p, T_FLOAT)) {
    f64 value = string_to_f64(parser_prev(p).value);
    result = (Code_Expr *)code_expr_float(p, value);
  } else if (parser_accept(p, T_STRING)) {
    String value = parser_prev(p).value;
    value.data++;
    value.count -= 2;
    result = (Code_Expr *)code_expr_string(p, value);
  } else {
    result = (Code_Expr *)parse_type(p);
  }
  return result;
}

Code_Expr *parse_expr_call(Parser *p) {
  Code_Expr *result = 0;
  
  result = parse_expr_atom(p);
  while (true) {
    if (parser_accept(p, T_DOT)) {
      Code_Expr *right = parse_expr_atom(p);
      result = (Code_Expr *)code_expr_binary(p, result, T_MEMBER, right);
    } else if (parser_accept(p, T_LBRACKET)) {
      Code_Expr *right = parse_expr(p);
      parser_expect(p, T_RBRACKET);
      result = (Code_Expr *)code_expr_binary(p, result, T_SUBSCRIPT, right);
    } else if (parser_accept(p, T_LPAREN)) {
      Code_Expr **args = sb_new(p->arena, Code_Expr *, 8);
      while (!parser_accept(p, T_RPAREN)) {
        Code_Expr *arg = parse_expr(p);
        sb_push(args, arg);
        if (!parser_accept(p, T_COMMA)) {
          parser_expect(p, T_RPAREN);
          break;
        }
      }
      result = (Code_Expr *)code_expr_call(p, result, args);
    } else {
      break;
    }
  }
  return result;
}

Code_Expr *parse_expr_unary(Parser *p) {
  Code_Expr *result = 0;
  if (parser_accept(p, T_SUB) ||
      parser_accept(p, T_NOT) ||
      parser_accept(p, T_BIT_NOT) ||
      parser_accept(p, T_LESS) ||
      parser_accept(p, T_REF)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Expr *expr = parse_expr_unary(p);
    result = (Code_Expr *)code_expr_unary(p, op, expr);
  } else {
    result = parse_expr_call(p);
  }
  return result;
}

Code_Expr *parse_expr_mul(Parser *p) {
  Code_Expr *result = parse_expr_unary(p);
  if (parser_accept_range(p, T_STAR_FIRST, T_STAR_LAST)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Expr *right = parse_expr_mul(p);
    result = (Code_Expr *)code_expr_binary(p, result, op, right);
  }
  return result;
}

Code_Expr *parse_expr_plus(Parser *p) {
  Code_Expr *result = parse_expr_mul(p);
  if (parser_accept_range(p, T_PLUS_FIRST, T_PLUS_LAST)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Expr *right = parse_expr_plus(p);
    result = (Code_Expr *)code_expr_binary(p, result, op, right);
  }
  return result;
}

Code_Expr *parse_expr_comp(Parser *p) {
  Code_Expr *result = parse_expr_plus(p);
  if (parser_accept_range(p, T_COMP_FIRST, T_COMP_LAST)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Expr *right = parse_expr_comp(p);
    result = (Code_Expr *)code_expr_binary(p, result, op, right);
  }
  return result;
}

Code_Expr *parse_expr_and(Parser *p) {
  Code_Expr *result = parse_expr_comp(p);
  if (parser_accept(p, T_AND)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Expr *right = parse_expr_and(p);
    result = (Code_Expr *)code_expr_binary(p, result, op, right);
  }
  return result;
}

Code_Expr *parse_expr(Parser *p) {
  Code_Expr *result = parse_expr_and(p);
  if (parser_accept(p, T_OR)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Expr *right = parse_expr(p);
    result = (Code_Expr *)code_expr_binary(p, result, op, right);
  }
  return result;
}

Code_Stmt_Block *parse_stmt_block(Parser *p) {
  Code_Stmt_Block *result = 0;
  parser_expect(p, T_LCURLY);
  Code_Stmt **statements = sb_new(p->arena, Code_Stmt *, 32);
  while (!parser_accept(p, T_RCURLY)) {
    Code_Stmt *st = parse_stmt(p, true);
    sb_push(statements, st);
  }
  result = code_stmt_block(p, statements);
  return result;
}


Code_Stmt_Decl *parse_decl(Parser *p) {
  Code_Stmt_Decl *result = 0;
  if (parser_peek(p, 0, T_NAME) && parser_peek(p, 1, T_COLON)) {
    Token t_name = parser_expect(p, T_NAME);
    parser_expect(p, T_COLON);
    
    String name = t_name.value;
    
    Code_Type *type = 0;
    Code_Node *value = 0;
    
    if (!parser_peek(p, 0, T_ASSIGN) &&
        !parser_peek(p, 0, T_COLON)) {
      type = parse_type(p);
    }
    
    b32 is_const = false;
    if (parser_accept(p, T_ASSIGN) ||
        parser_accept(p, T_COLON)) {
      Token_Kind prev = parser_prev(p).kind;
      if (prev == T_COLON) {
        is_const = true;
      }
      
      if (parser_peek(p, 0, T_FUNC)) {
        Code_Type_Func *sig = parse_type_func(p);
        if (parser_peek(p, 0, T_LCURLY)) {
          Code_Stmt_Block *body = parse_stmt_block(p);
          value = (Code_Node *)code_func(p, sig, body);
        } else {
          value = (Code_Node *)sig;
        }
      } else {
        value = (Code_Node *)parse_expr(p);
      }
    }
    
    result = code_stmt_decl(p, name, type, value, is_const);
  }
  
  return result;
}

Code_Stmt_Decl **parse_program(Parser *p) {
  Code_Stmt_Decl **result = sb_new(p->arena, Code_Stmt_Decl *, 64);
  while (p->i < sb_count(p->tokens) && p->error_count == 0) {
    Code_Stmt_Decl *decl = parse_stmt_decl(p);
    
    sb_push(result, decl);
  }
  return result;
}

