#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#define LVL5_DEBUG
#include "lvl5_string.h"
#include "lvl5_stretchy_buffer.h"

typedef enum {
  T_NONE,
  T_ERROR,
  T_NAME,
  T_INT,
  T_FLOAT,
  T_STRING,
  T_CHAR,
  
  T_LPAREN,
  T_RPAREN,
  T_LBRACKET,
  T_RBRACKET,
  T_LCURLY,
  T_RCURLY,
  T_NOT,
  T_BIT_NOT,
  T_SEMI,
  T_COMMA,
  T_DOUBLE_QUOTE,
  T_SINGLE_QUOTE,
  T_DOT,
  T_MEMBER = T_DOT,
  
  T_MUL,
  T_STAR_FIRST = T_MUL,
  T_DIV,
  T_MOD,
  T_RSHIFT,
  T_LSHIFT,
  T_BIT_AND,
  T_STAR_LAST = T_BIT_AND,
  
  T_ADD,
  T_PLUS_FIRST = T_ADD,
  T_SUB,
  T_BIT_OR,
  T_BIT_XOR,
  T_PLUS_LAST = T_BIT_XOR,
  
  T_NOT_EQUALS,
  T_COMP_FIRST = T_NOT_EQUALS,
  T_EQUALS,
  T_LESS,
  T_LESS_EQUALS,
  T_GREATER,
  T_GREATER_EQUALS,
  T_COMP_LAST = T_GREATER_EQUALS,
  
  T_OR,
  T_AND,
  T_QUESTION,
  T_POUND,
  T_BACKSLASH,
  T_COLON,
  T_ARROW,
  
  T_ASSIGN,
  T_ASSIGN_FIRST = T_ASSIGN,
  T_BIT_OR_ASSIGN,
  T_BIT_AND_ASSIGN,
  T_ADD_ASSIGN,
  T_MUL_ASSIGN,
  T_DIV_ASSIGN,
  T_SUB_ASSIGN,
  T_MOD_ASSIGN,
  T_BIT_XOR_ASSIGN,
  T_ASSIGN_LAST = T_BIT_XOR_ASSIGN,
  
  T_RETURN,
  T_KEYWORD_FIRST = T_RETURN,
  T_BREAK,
  T_FUNC,
  T_STRUCT,
  T_ENUM,
  T_ELSE,
  T_CONTINUE,
  T_FOR,
  T_PUSH_CONTEXT,
  T_DEFER,
  T_NULL,
  T_WHILE,
  T_IF,
  T_KEYWORD_LAST = T_IF,
  
  T_DOUBLE_DOT,
  T_TRIPLE_DOT,
  T_SUBSCRIPT = T_LBRACKET,
  T_DEREF = T_LESS,
  T_REF = T_MUL,
} Token_Kind;

String Token_Kind_To_String[] = {
  [T_RETURN] = arr_string("return"),
  [T_NULL] = arr_string("null"),
  [T_BREAK] = arr_string("break"),
  [T_FUNC] = arr_string("func"),
  [T_STRUCT] = arr_string("struct"),
  [T_ENUM] = arr_string("enum"),
  [T_ELSE] = arr_string("else"),
  [T_IF] = arr_string("if"),
  [T_FOR] = arr_string("for"),
  [T_CONTINUE] = arr_string("continue"),
  [T_PUSH_CONTEXT] = arr_string("push_context"),
  [T_DEFER] = arr_string("defer"),
  [T_WHILE] = arr_string("while"),
  
  [T_NONE] = arr_string("NONE"),
  [T_ERROR] = arr_string("ERROR"),
  [T_NAME] = arr_string("NAME"),
  [T_INT] = arr_string("INT"),
  [T_CHAR] = arr_string("CHAR"),
  [T_FLOAT] = arr_string("FLOAT"),
  [T_LPAREN] = arr_string("("),
  [T_RPAREN] = arr_string(")"),
  [T_LBRACKET] = arr_string("["),
  [T_RBRACKET] = arr_string("]"),
  [T_LCURLY] = arr_string("{"),
  [T_RCURLY] = arr_string("}"),
  [T_NOT] = arr_string("!"),
  [T_BIT_NOT] = arr_string("~"),
  [T_SEMI] = arr_string(";"),
  [T_COMMA] = arr_string(","),
  [T_DOUBLE_QUOTE] = arr_string("\""),
  [T_SINGLE_QUOTE] = arr_string("'"),
  [T_DOT] = arr_string("."),
  [T_DOUBLE_DOT] = arr_string(".."),
  [T_TRIPLE_DOT] = arr_string("..."),
  
  [T_MUL] = arr_string("*"),
  [T_DIV] = arr_string("/"),
  [T_MOD] = arr_string("%"),
  [T_RSHIFT] = arr_string(">>"),
  [T_LSHIFT] = arr_string("<<"),
  [T_BIT_AND] = arr_string("&"),
  
  [T_ADD] = arr_string("+"),
  [T_SUB] = arr_string("-"),
  [T_BIT_OR] = arr_string("||"),
  [T_BIT_XOR] = arr_string("^"),
  
  [T_NOT_EQUALS] = arr_string("!="),
  [T_EQUALS] = arr_string("=="),
  [T_LESS] = arr_string("<"),
  [T_LESS_EQUALS] = arr_string("<="),
  [T_GREATER] = arr_string(">"),
  [T_GREATER_EQUALS] = arr_string(">="),
  
  [T_OR] = arr_string("||"),
  [T_AND] = arr_string("&&"),
  [T_QUESTION] = arr_string("?"),
  [T_POUND] = arr_string("#"),
  [T_BACKSLASH] = arr_string("\\"),
  [T_COLON] = arr_string(":"),
  [T_ARROW] = arr_string("->"),
  
  [T_ASSIGN] = arr_string("="),
  [T_BIT_OR_ASSIGN] = arr_string("|="),
  [T_BIT_AND_ASSIGN] = arr_string("&="),
  [T_ADD_ASSIGN] = arr_string("+="),
  [T_MUL_ASSIGN] = arr_string("*="),
  [T_DIV_ASSIGN] = arr_string("/="),
  [T_SUB_ASSIGN] = arr_string("-="),
  [T_MOD_ASSIGN] = arr_string("%="),
  [T_BIT_XOR_ASSIGN] = arr_string("^="),
};


b32 is_digit(char c) {
  b32 result = c >= '0' && c <= '9';
  return result;
}

b32 is_alpha(char c) {
  b32 result = (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') || (c == '_');
  return result;
}

b32 is_whitespace(char c) {
  b32 result = c == ' ' || c == '\n' || c == '\t' || c == '\r';
  return result;
}

typedef struct {
  Token_Kind kind;
  i32 line;
  i32 col;
  String value;
} Token;


Token_Kind get_keyword_kind(String str) {
  i32 result = 0;
  for (i32 i = T_KEYWORD_FIRST; i <= T_KEYWORD_LAST; i++) {
    String keyword_string = Token_Kind_To_String[i];
    if (string_compare(keyword_string, str)) {
      result = i;
      break;
    }
  }
  return result;
}

String get_line_from_index(String str, u32 index) {
  i32 line_start = index;
  while (line_start > 0 && str.data[line_start-1] != '\n') line_start--;
  //while (str.data[line_start] == ' ') line_start++;
  
  i32 line_end = index;
  while (line_end < str.count && str.data[line_end] != '\n') line_end++;
  
  String result = substring(str, line_start, line_end);
  return result;
}


void syntax_error(char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}

Token *tokenize(Arena *arena, String src) {
  Token *tokens = sb_new(arena, Token, 1024);
  
  i32 line = 1;
  i32 col = 1;
  i32 token_start = 0;
  
  Token t = {0};
  i32 i = 0;
  
  char *stream = src.data;
  
#define next(n) { \
    col += n; \
    i += n; \
    stream += n; \
  }
#define skip(n) { \
    next(n); \
    token_start += n; \
  }
#define eat() { \
    next(1); \
  }
#define end(tok_kind) { \
    t.line = line; \
    t.col = col - (i - token_start); \
    t.kind = tok_kind; \
    t.value = substring(src, token_start, i); \
    token_start = i; \
    sb_push(tokens, t); \
    continue; \
  }
  
#define case1(ch0, kind0) \
  case ch0: { \
    eat(); \
    end(kind0); \
  } break;
#define case2(ch0, kind0, ch1, kind1) \
  case ch0: { \
    eat(); \
    if (*stream == ch1) { \
      eat(); \
      end(kind1); \
    } else { \
      end(kind0); \
    } \
  } break;
#define case3(ch0, kind0, ch1, kind1, ch2, kind2) \
  case ch0: { \
    eat(); \
    if (*stream == ch1) { \
      eat(); \
      end(kind1); \
    } else if (*stream == ch2) { \
      eat(); \
      end(kind2); \
    } else { \
      end(kind0); \
    } \
  } break;
  
  while (true) {
    switch (*stream) {
      case 0: {
        goto end;
      } break;
      case ' ': {
        skip(1);
        continue;
      } break;
      case '\n': {
        skip(1);
        line++;
        col = 1;
      } break;
      case '"': {
        eat();
        // TODO(lvl5): deal with escape sequences
        while ((*stream != '"') ||
               (*(stream-1) == '\\')) {
          eat();
        }
        eat();
        end(T_STRING);
      } break;
      case '\'': {
        eat();
        eat();
        eat();
        end(T_CHAR);
      } break;
      
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9': {
        eat();
        while (is_digit(*stream)) eat();
        
        // float
        if (*stream == '.') {
          eat();
          if (!is_digit(*stream)) syntax_error("Unexpected symbol %c", *stream);
          
          while (is_digit(*stream)) eat();
          end(T_FLOAT);
        } else {
          end(T_INT);
        }
      } break;
      case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
      case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
      case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
      case 'v': case 'w': case 'x': case 'y': case 'z':
      
      case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
      case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
      case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
      case 'V': case 'W': case 'X': case 'Y': case 'Z':
      
      case '_': {
        eat();
        while (is_digit(*stream) || is_alpha(*stream)) eat();
        String value = substring(src, token_start, i);
        Token_Kind kind = get_keyword_kind(value);
        if (kind) {
          t.line = line; 
          t.col = col - (i - token_start); 
          t.kind = kind; 
          t.value = substring(src, token_start, i); 
          token_start = i; 
          sb_push(tokens, t); 
          continue;
        } else {
          end(T_NAME);
        }
      } break;
      
      case '/': {
        if (*(stream+1) == '/') {
          skip(2);
          while (*stream != '\n') {
            skip(1);
          }
        } else if (*(stream+1) == '*') {
          skip(2);
          while (!(*stream == '*' && *(stream+1) == '/')) {
            skip(1);
          }
          skip(2);
        } else {
          eat();
          if (*stream == '=') {
            end(T_DIV_ASSIGN);
          } else {
            end(T_DIV);
          }
        }
      } break;
      
      case '#': {
        skip(1);
        while (is_digit(*stream) || is_alpha(*stream)) eat();
        end(T_POUND);
      } break;
      
      case1('\\', T_BACKSLASH);
      case1(',', T_COMMA);
      case1(';', T_SEMI);
      case1('(', T_LPAREN);
      case1(')', T_RPAREN);
      case1('[', T_LBRACKET);
      case1(']', T_RBRACKET);
      case1('{', T_LCURLY);
      case1('}', T_RCURLY);
      case1('~', T_BIT_NOT);
      case1(':', T_COLON);
      
      case2('=', T_ASSIGN, '=', T_EQUALS);
      case2('!', T_NOT, '=', T_NOT_EQUALS);
      case2('+', T_ADD, '=', T_ADD_ASSIGN);
      case2('*', T_MUL, '=', T_MUL_ASSIGN);
      case2('^', T_BIT_XOR, '=', T_BIT_XOR_ASSIGN);
      case2('%', T_MOD, '=', T_MOD_ASSIGN);
      
      case3('.', T_DOT, '.', T_DOUBLE_DOT, '.', T_TRIPLE_DOT);
      case3('-', T_SUB, '=', T_SUB_ASSIGN, '>', T_ARROW);
      case3('>', T_GREATER, '=', T_GREATER_EQUALS, '>', T_RSHIFT);
      case3('<', T_LESS, '=', T_LESS_EQUALS, '>', T_LSHIFT);
      case3('|', T_BIT_OR, '|', T_OR, '=', T_BIT_OR_ASSIGN);
      case3('&', T_BIT_AND, '&', T_AND, '=', T_BIT_AND_ASSIGN);
      
      default: assert(false);
    }
  }
  
  end:
  
  return tokens;
}
