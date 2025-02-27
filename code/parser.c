#include "parser.h"



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


void compiler_error(Parser *p, Token t, char *fmt, ...) {
#define BUF_COUNT 256
  
  va_list args;
  va_start(args, fmt);
  
  char *buf = arena_push_array(scratch_arena, char, BUF_COUNT);
  i32 count = vsprintf_s(buf, BUF_COUNT, fmt, args);
  va_end(args);
  assert(count > 0 && count <= BUF_COUNT);
  
  
  String line_str = get_line_from_index(p->src, (u32)(t.value.data - p->src.data));
  char *line = to_c_string(p->arena, line_str);
  
  char *buf2 = arena_push_array(scratch_arena, char, BUF_COUNT);
  char *line_pointer = arena_push_array(scratch_arena, char, line_str.count);
  i32 i = 0;
  for (; i < t.col; i++) {
    line_pointer[i] = ' ';
  }
  line_pointer[i-1] = '^';
  line_pointer[i] = '\0';
  
  i32 count2 = sprintf_s(buf2, BUF_COUNT, " at %d:%d\n%04d   %s\n       %s", t.line, t.col, t.line, line, line_pointer);
  assert(count2 > 0 && count2 <= BUF_COUNT);
  
  
  String err_string = concat(scratch_arena, make_string(buf, (u32)count-1), make_string(buf2, (u32)count2));
  
  p->error = err_string;
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
    compiler_error(p, t, "Expected token \"%s\", got \"%s\" ", expected, got);
    p->i++;
  }
  return result;
}


Code_Node *parse_expr(Parser *p);
Code_Node *parse_type(Parser *p);
Code_Node *parse_decl(Parser *p);
Code_Node *parse_stmt_block(Parser *p);

void set_begin_end(void *nd, i32 begin, i32 end) {
  if (nd) {
    Code_Node *node = (Code_Node *)nd;
    node->first_token = begin;
    node->last_token = end;
  }
}

Code_Node *parse_type_func(Parser *p) {
  Code_Node *result = 0;
  i32 begin = p->i;
  parser_expect(p, T_FUNC);
  parser_expect(p, T_LPAREN);
  
  Code_Node **params = sb_new(p->arena, Code_Stmt_Decl *, 8);
  while (!parser_accept(p, T_RPAREN)) {
    Code_Node *param = parse_decl(p);
    sb_push(params, param);
    
    if (!parser_accept(p, T_COMMA)) {
      parser_expect(p, T_RPAREN);
      break;
    }
  }
  
  Code_Node *return_type = parse_type(p);
  if (!return_type) {
    return_type = code_type_alias(p, const_string("void"));
  }
  
  result = code_type_func(p, params, return_type);
  set_begin_end(result, begin, p->i-1);
  
  return result;
}

Code_Node *parse_type(Parser *p) {
  Code_Node *result = 0;
  i32 begin = p->i;
  
  if (parser_accept(p, T_NAME)) {
    String name = parser_get(p, -1).value;
    
    // NOTE(lvl5): if this is one of the basic types, just set it
    Scope_Entry *entry = scope_get(p->global_scope, name);
    if (entry) {
      result = entry->decl->s_decl.value;
    } else {
      result = code_type_alias(p, name);
    }
  } else if (parser_accept(p, T_REF)) {
    Code_Node *base = parse_type(p);
    result = code_type_pointer(p, base);
  } else if (parser_accept(p, T_STRUCT)) {
    parser_expect(p, T_LCURLY);
    
    Code_Node **members = sb_new(p->arena, Code_Stmt_Decl *, 8);
    while (!parser_accept(p, T_RCURLY)) {
      Code_Node *member = parse_decl(p);
      parser_expect(p, T_SEMI);
      sb_push(members, member);
    }
    
    result = code_type_struct(p, members);
  } else if (parser_accept(p, T_ENUM)) {
    parser_expect(p, T_LCURLY);
    
    String *members = sb_new(p->arena, String, 16);
    while (!parser_accept(p, T_RCURLY)) {
      Token member = parser_expect(p, T_NAME);
      parser_expect(p, T_SEMI);
      sb_push(members, member.value);
    }
    result = code_type_enum(p, members);
  } else if (parser_accept(p, T_LBRACKET)) {
    Token t_count = parser_expect(p, T_INT);
    u64 count = string_to_u64(t_count.value);
    parser_expect(p, T_RBRACKET);
    Code_Node *element_type = parse_type(p);
    
    result = code_type_array(p, element_type, count);
  } else if (parser_peek(p, 0, T_FUNC)) {
    result = parse_type_func(p);
  } else if (parser_accept(p, T_LPAREN)) {
    result = parse_type(p);
    parser_expect(p, T_RPAREN);
  }
  
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_stmt_decl(Parser *p, b32 expect_semi) {
  i32 begin = p->i;
  Code_Node *decl = parse_decl(p);
  
  if (expect_semi) {
    Code_Node *value = decl->s_decl.value;
    if (value && 
        (value->kind == Code_Kind_FUNC ||
         value->kind == Code_Kind_TYPE_STRUCT ||
         value->kind == Code_Kind_TYPE_ENUM)) {
      
    } else {
      parser_expect(p, T_SEMI);
    }
  }
  
  set_begin_end(decl, begin, p->i);
  return decl;
}

b32 is_expression(Code_Node *node) {
  b32 result = node->kind >= Code_Kind_EXPR_FIRST &&
    node->kind <= Code_Kind_EXPR_LAST;
  return result;
}

b32 is_type(Code_Node *node) {
  b32 result = node->kind >= Code_Kind_TYPE_FIRST &&
    node->kind <= Code_Kind_TYPE_LAST;
  return result;
}

b32 is_statement(Code_Node *node) {
  b32 result = node->kind >= Code_Kind_STMT_FIRST &&
    node->kind <= Code_Kind_STMT_LAST;
  return result;
}

Code_Node *parse_stmt(Parser *p, b32 expect_semi) {
  Code_Node *result = 0;
  i32 begin = p->i;
  
  if (parser_peek(p, 0, T_NAME) && parser_peek(p, 1, T_COLON)) {
    result = parse_stmt_decl(p, expect_semi);
  } else if (parser_accept(p, T_IF)) {
    Code_Node *cond = parse_expr(p);
    Code_Node *then_branch = parse_stmt(p, true);
    Code_Node *else_branch = 0;
    if (parser_accept(p, T_ELSE)) {
      else_branch = parse_stmt(p, true);
    }
    result = code_stmt_if(p, cond, then_branch, else_branch);
  } else if (parser_accept(p, T_WHILE)) {
    Code_Node *cond = parse_expr(p);
    Code_Node *body = parse_stmt(p, true);
    result = code_stmt_while(p, cond, body);
  } else if (parser_accept(p, T_FOR)) {
    Code_Node *init = parse_stmt(p, false);
    
    if (init->kind == Code_Kind_STMT_EXPR &&
        parser_accept(p, T_DOUBLE_DOT)) {
      Code_Node *min = init;
      Code_Node *max = parse_expr(p);
      init = code_stmt_decl(p, const_string("it"), null, min, false);
      
      Code_Node *it_name = code_expr_name(p, const_string("it"));
      Code_Node *cond = code_expr_binary(p, it_name, T_LESS, max);
      Code_Node *post = code_stmt_assign(p, it_name, T_ADD_ASSIGN,
                                         code_expr_int(p, 1));
      
      Code_Node *body = parse_stmt(p, true);
      result = code_stmt_for(p, init, cond, post, body);
    } else {
      parser_expect(p, T_SEMI);
      Code_Node *cond = parse_expr(p);
      parser_expect(p, T_SEMI);
      Code_Node *post = parse_stmt(p, false);
      Code_Node *body = parse_stmt(p, true);
      
      result = code_stmt_for(p, init, cond, post, body);
    }
  } else if (parser_peek(p, 0, T_RETURN) ||
             parser_peek(p, 0, T_BREAK) ||
             parser_peek(p, 0, T_CONTINUE) ||
             parser_peek(p, 0, T_DEFER)) {
    Token_Kind keyword = parser_get(p, 0).kind;
    parser_expect(p, keyword);
    
    Code_Node *stmt = 0;
    if (!parser_peek(p, 0, T_SEMI)) {
      stmt = parse_stmt(p, true);
    }
    result = code_stmt_keyword(p, keyword, stmt, null);
  } else if (parser_peek(p, 0, T_PUSH_CONTEXT)) {
    parser_expect(p, T_PUSH_CONTEXT);
    Code_Node *extra = parse_expr(p);
    Code_Node *stmt = parse_stmt(p, true);
    result = code_stmt_keyword(p, T_PUSH_CONTEXT, stmt, extra);
  } else if (parser_peek(p, 0, T_LCURLY)) {
    result = parse_stmt_block(p);
  } else {
    Code_Node *left = parse_expr(p);
    if (parser_peek_range(p, 0, T_ASSIGN_FIRST, T_ASSIGN_LAST)) {
      Token_Kind op = parser_get(p, 0).kind;
      parser_expect(p, op);
      Code_Node *right = parse_expr(p);
      result = code_stmt_assign(p, left, op, right);
    } else {
      result = code_stmt_expr(p, left);
    }
    if (expect_semi) {
      parser_expect(p, T_SEMI);
    }
  }
  
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_expr_atom(Parser *p) {
  Code_Node *result = 0;
  i32 begin = p->i;
  
  if (parser_accept(p, T_LPAREN)) {
    Code_Node *inner = parse_expr(p);
    parser_expect(p, T_RPAREN);
    result = inner;
  } else if (parser_accept(p, T_NAME)) {
    String name = parser_prev(p).value;
    result = code_expr_name(p, name);
  } else if (parser_accept(p, T_INT)) {
    u64 value = string_to_u64(parser_prev(p).value);
    result = code_expr_int(p, value);
  } else if (parser_accept(p, T_FLOAT)) {
    f64 value = string_to_f64(parser_prev(p).value);
    result = code_expr_float(p, value);
  } else if (parser_accept(p, T_STRING)) {
    String value = parser_prev(p).value;
    value.data++;
    value.count -= 2;
    result = code_expr_string(p, value);
  } else if (parser_accept(p, T_NULL)) {
    result = code_expr_null(p);
  } else if (parser_accept(p, T_CHAR)) {
    Token tok_char = parser_prev(p);
    result = code_expr_char(p, tok_char.value.data[1]);
  } else {
    result = parse_type(p);
  }
  
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_expr_call(Parser *p) {
  Code_Node *result = 0;
  i32 begin = p->i;
  
  result = parse_expr_atom(p);
  while (true) {
    if (parser_accept(p, T_DOT)) {
      Code_Node *right = parse_expr_atom(p);
      result = code_expr_binary(p, result, T_MEMBER, right);
    } else if (parser_accept(p, T_LBRACKET)) {
      Code_Node *right = parse_expr(p);
      parser_expect(p, T_RBRACKET);
      result = code_expr_binary(p, result, T_SUBSCRIPT, right);
    } else if (parser_accept(p, T_LPAREN)) {
      Code_Node **args = sb_new(p->arena, Code_Expr *, 8);
      while (!parser_accept(p, T_RPAREN)) {
        Code_Node *arg = parse_expr(p);
        sb_push(args, arg);
        if (!parser_accept(p, T_COMMA)) {
          parser_expect(p, T_RPAREN);
          break;
        }
      }
      result = code_expr_call(p, result, args);
    } else {
      break;
    }
  }
  
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_expr_unary(Parser *p) {
  Code_Node *result = 0;
  i32 begin = p->i;
  
  if (parser_accept(p, T_SUB) ||
      parser_accept(p, T_NOT) ||
      parser_accept(p, T_BIT_NOT) ||
      parser_accept(p, T_LESS) ||
      parser_accept(p, T_REF)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Node *expr = parse_expr_unary(p);
    result = code_expr_unary(p, op, expr);
  } else {
    result = parse_expr_call(p);
  }
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_expr_mul(Parser *p) {
  i32 begin = p->i;
  Code_Node *result = parse_expr_unary(p);
  if (parser_accept_range(p, T_STAR_FIRST, T_STAR_LAST)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Node *right = parse_expr_mul(p);
    result = code_expr_binary(p, result, op, right);
  }
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_expr_plus(Parser *p) {
  i32 begin = p->i;
  Code_Node *result = parse_expr_mul(p);
  if (parser_accept_range(p, T_PLUS_FIRST, T_PLUS_LAST)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Node *right = parse_expr_plus(p);
    result = code_expr_binary(p, result, op, right);
  }
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_expr_comp(Parser *p) {
  i32 begin = p->i;
  Code_Node *result = parse_expr_plus(p);
  if (parser_accept_range(p, T_COMP_FIRST, T_COMP_LAST)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Node *right = parse_expr_comp(p);
    result = code_expr_binary(p, result, op, right);
  }
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_expr_and(Parser *p) {
  i32 begin = p->i;
  Code_Node *result = parse_expr_comp(p);
  if (parser_accept(p, T_AND)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Node *right = parse_expr_and(p);
    result = code_expr_binary(p, result, op, right);
  }
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_expr(Parser *p) {
  i32 begin = p->i;
  Code_Node *result = parse_expr_and(p);
  if (parser_accept(p, T_OR)) {
    Token_Kind op = parser_prev(p).kind;
    Code_Node *right = parse_expr(p);
    result = code_expr_binary(p, result, op, right);
  }
  set_begin_end(result, begin, p->i-1);
  return result;
}

Code_Node *parse_stmt_block(Parser *p) {
  Code_Node *result = 0;
  i32 begin = p->i;
  parser_expect(p, T_LCURLY);
  Code_Node **statements = sb_new(p->arena, Code_Stmt *, 32);
  while (!parser_accept(p, T_RCURLY)) {
    Code_Node *st = parse_stmt(p, true);
    sb_push(statements, st);
  }
  result = code_stmt_block(p, statements);
  set_begin_end(result, begin, p->i-1);
  return result;
}


Code_Node *parse_decl(Parser *p) {
  i32 begin = p->i;
  Code_Node *result = 0;
  Token t_name = parser_expect(p, T_NAME);
  parser_expect(p, T_COLON);
  
  String name = t_name.value;
  
  Code_Node *type = 0;
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
      Code_Node *sig = parse_type_func(p);
      if (parser_peek(p, 0, T_LCURLY)) {
        Code_Node *body = parse_stmt_block(p);
        value = code_func(p, sig, body, false);
      } else if (parser_accept(p, T_POUND)) {
        Token t = parser_get(p, -1);
        if (string_compare(t.value, const_string("foreign"))) {
          String foreign_name = {0};
          if (parser_accept(p, T_NAME)) {
            foreign_name = parser_prev(p).value;
          }
          Token tok_module = parser_expect(p, T_STRING);
          Code_Func *func = &code_func(p, sig, null, true)->func;
          func->module = tok_module.value;
          // NOTE(lvl5): trim the string
          func->module.data++;
          func->module.count -= 2;
          func->foreign_name = foreign_name;
          value = (Code_Node *)func;
          parser_expect(p, T_SEMI);
        } else {
          assert(false);
        }
      } else {
        value = (Code_Node *)sig;
      }
    } else {
      value = (Code_Node *)parse_expr(p);
    }
  }
  result = code_stmt_decl(p, name, type, value, is_const);
  set_begin_end(result, begin, p->i-1);
  
  return result;
}

Code_Node **parse_program(Parser *p) {
  Code_Node **result = sb_new(p->arena, Code_Node *, 64);
  while (p->i < sb_count(p->tokens) && string_is_empty(p->error)) {
    Code_Node *decl = parse_stmt_decl(p, true);
    
    sb_push(result, decl);
  }
  return result;
}

