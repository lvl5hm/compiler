#include "lexer.h"



Arena _scratch_arena;
Arena *scratch_arena = &_scratch_arena;


typedef struct Code_Node Code_Node;
typedef struct Code_Type Code_Type;
typedef struct Code_Stmt Code_Stmt;
typedef struct Code_Stmt_Decl Code_Stmt_Decl;
typedef struct Code_Expr Code_Expr;
typedef struct Code_Stmt_Block Code_Stmt_Block;


Code_Node *builtin_Type = 0;

Code_Node *builtin_i8 = 0;
Code_Node *builtin_i16 = 0;
Code_Node *builtin_i32 = 0;
Code_Node *builtin_i64 = 0;

Code_Node *builtin_u8 = 0;
Code_Node *builtin_u16 = 0;
Code_Node *builtin_u32 = 0;
Code_Node *builtin_u64 = 0;

Code_Node *builtin_f32 = 0;
Code_Node *builtin_f64 = 0;

Code_Node *builtin_void = 0;
Code_Node *builtin_voidptr = 0;
Code_Node *builtin_string = 0;

Code_Node *builtin_statement_type = 0;



typedef struct {
  Code_Node *decl;
} Scope_Entry;

typedef struct Scope Scope;
struct Scope {
  Scope_Entry *entries;
  Code_Stmt **deferred_statements;
  Scope *parent;
};

typedef struct {
  Code_Node **members;
  Scope *scope;
  i32 size;
} Code_Type_Struct;

typedef struct {
  String *members;
  Scope *scope;
  Code_Node *item_type;
} Code_Type_Enum;

typedef struct {
  Code_Node *item_type;
  u64 count;
} Code_Type_Array;

typedef struct {
  Code_Node *base;
} Code_Type_Pointer;

typedef struct {
  Code_Node **params;
  Code_Node *return_type;
} Code_Type_Func;

typedef struct {
  String name;
  Code_Node *base;
  b32 is_builtin;
} Code_Type_Alias;

typedef struct {
  i32 size;
  b32 is_signed;
} Code_Type_Int;

typedef struct {
  i32 _;
} Code_Type_Void;

typedef struct {
  i32 size;
} Code_Type_Float;

typedef struct {
  String name;
  Code_Node *decl;
} Code_Expr_Name;

typedef struct {
  Code_Node *left;
  Code_Node *right;
  Token_Kind op;
  b32 is_enum_member;
} Code_Expr_Binary;

typedef struct {
  Code_Node *val;
  Token_Kind op;
} Code_Expr_Unary;

typedef struct {
  Code_Node *func;
  Code_Node **args;
} Code_Expr_Call;

typedef struct {
  Code_Node *cast_type;
  Code_Node *expr;
  b32 implicit;
} Code_Expr_Cast;


typedef enum {
  Storage_Kind_NONE,
  //Storage_Kind_FUNC,
  Storage_Kind_BSS,
  Storage_Kind_STACK,
} Storage_Kind;

typedef struct {
  Storage_Kind storage_kind;
  i32 offset;
  void *data;
  i32 size;
} Placeholder;

typedef struct {
  u64 value;
  Placeholder *placeholder;
} Code_Expr_Int;

typedef struct {
  i32 _;
} Code_Expr_Null;

typedef struct {
  f64 value;
} Code_Expr_Float;

typedef struct {
  String value;
  i32 offset;
} Code_Expr_String;

typedef struct {
  char value;
} Code_Expr_Char;

typedef struct {
  Code_Node **members;
  String name;
} Code_Expr_Struct;

typedef struct {
  Code_Node *sig;
  Code_Node *body;
  
  b32 foreign;
  String module;
  String foreign_name;
  
  Scope *scope;
  i32 stack_size;
} Code_Func;

typedef struct {
  Code_Node *left;
  Code_Node *right;
  
  Token_Kind op;
} Code_Stmt_Assign;

typedef struct {
  Code_Node *expr;
} Code_Stmt_Expr;

typedef struct {
  Code_Node *cond;
  Code_Node *then_branch;
  Code_Node *else_branch;
} Code_Stmt_If;

typedef struct {
  Code_Node *init;
  Code_Node *cond;
  Code_Node *post;
  Code_Node *body;
} Code_Stmt_For;

typedef struct {
  Token_Kind keyword;
  Code_Node *stmt;
  Code_Node *extra;
} Code_Stmt_Keyword;

struct Code_Stmt_Block {
  Code_Node **statements;
  Scope *scope;
};

struct Code_Stmt_Decl {
  String name;
  Code_Node *type;
  Code_Node *value;
  b32 is_const;
  
  Storage_Kind storage_kind;
  u32 offset; // offset from the BSS or the stack (or beginning of the struct)
};

typedef struct {
  Code_Node *cond;
  Code_Node *body;
} Code_Stmt_While;

typedef struct {
  int _;
} Code_Expr_Array;

typedef enum {
  Code_Kind_NONE,
  
  Code_Kind_FUNC,
  
  Code_Kind_TYPE_STRUCT,
  Code_Kind_TYPE_FIRST = Code_Kind_TYPE_STRUCT,
  Code_Kind_TYPE_ENUM,
  Code_Kind_TYPE_POINTER,
  Code_Kind_TYPE_ARRAY,
  Code_Kind_TYPE_FUNC,
  Code_Kind_TYPE_ALIAS,
  Code_Kind_TYPE_INT,
  Code_Kind_TYPE_FLOAT,
  Code_Kind_TYPE_VOID,
  Code_Kind_TYPE_LAST = Code_Kind_TYPE_VOID,
  
  Code_Kind_EXPR_CAST,
  Code_Kind_EXPR_FIRST = Code_Kind_EXPR_CAST,
  Code_Kind_EXPR_UNARY,
  Code_Kind_EXPR_BINARY,
  Code_Kind_EXPR_CALL,
  Code_Kind_EXPR_INT,
  Code_Kind_EXPR_FLOAT,
  Code_Kind_EXPR_STRING,
  Code_Kind_EXPR_ARRAY,
  Code_Kind_EXPR_STRUCT,
  Code_Kind_EXPR_NAME,
  Code_Kind_EXPR_TYPE,
  Code_Kind_EXPR_NULL,
  Code_Kind_EXPR_CHAR,
  Code_Kind_EXPR_LAST = Code_Kind_EXPR_CHAR,
  
  Code_Kind_STMT_ASSIGN,
  Code_Kind_STMT_FIRST = Code_Kind_STMT_ASSIGN,
  Code_Kind_STMT_EXPR,
  Code_Kind_STMT_IF,
  Code_Kind_STMT_BLOCK,
  Code_Kind_STMT_FOR,
  Code_Kind_STMT_KEYWORD,
  Code_Kind_STMT_DECL,
  Code_Kind_STMT_WHILE,
  Code_Kind_STMT_MULTI,
  Code_Kind_STMT_LAST = Code_Kind_STMT_MULTI,
} Code_Kind;

struct Code_Node {
  union {
    Code_Func func;
    
    Code_Type_Struct t_struct;
    Code_Type_Enum t_enum;
    Code_Type_Pointer t_pointer;
    Code_Type_Array t_array;
    Code_Type_Func t_func;
    Code_Type_Alias t_alias;
    Code_Type_Int t_int;
    Code_Type_Float t_float;
    Code_Type_Void t_void;
    
    Code_Expr_Cast e_cast;
    Code_Expr_Unary e_unary;
    Code_Expr_Binary e_binary;
    Code_Expr_Call e_call;
    Code_Expr_Int e_int;
    Code_Expr_Float e_float;
    Code_Expr_String e_string;
    Code_Expr_Array e_array;
    Code_Expr_Struct e_struct;
    Code_Expr_Name e_name;
    Code_Expr_Null e_null;
    Code_Expr_Char e_char;
    
    Code_Stmt_Assign s_assign;
    Code_Stmt_Expr s_expr;
    Code_Stmt_If s_if;
    Code_Stmt_Block s_block;
    Code_Stmt_For s_for;
    Code_Stmt_Keyword s_keyword;
    Code_Stmt_Decl s_decl;
    Code_Stmt_While s_while;
  };
  
  Code_Node *type;
  Code_Kind kind;
  i32 first_token;
  i32 last_token;
};

typedef struct {
  String src;
  Arena *arena;
  Token *tokens;
  u32 i;
  
  String error;
  Scope *global_scope;
} Parser;


Scope *alloc_scope(Arena *arena, Scope *parent) {
  Scope *result = arena_push_struct(arena, Scope);
  Scope zero_scope = {0};
  *result = zero_scope;
  result->entries = sb_new(arena, Scope_Entry, 32);
  result->parent = parent;
  return result;
}

Scope_Entry *scope_get(Scope *scope, String name) {
  Scope_Entry *result = 0;
  for (u32 i = 0; i < sb_count(scope->entries); i++) {
    Scope_Entry *test = scope->entries + i;
    if (string_compare(test->decl->s_decl.name, name)) {
      result = test;
      break;
    }
  }
  if (!result && scope->parent) {
    result = scope_get(scope->parent, name);
  }
  
  return result;
}

Scope_Entry *scope_add(Scope *scope, Code_Node *decl) {
  Scope_Entry *entry = scope_get(scope, decl->s_decl.name);
  assert(!entry);
  sb_push(scope->entries, (Scope_Entry){0});
  entry = sb_peek(scope->entries);
  
  entry->decl = decl;
  
  return entry;
}


Code_Node *code_node(Parser *p, Code_Kind kind) {
  Code_Node *node = arena_push_struct(p->arena, Code_Node);
  Code_Node zero_node = {0};
  *node = zero_node;
  node->kind = kind;
  
  return node;
}

Code_Node *code_stmt_decl(Parser *p, String name, Code_Node *type, Code_Node *value, b32 is_const) {
  Code_Node *node = code_node(p, Code_Kind_STMT_DECL);
  node->s_decl.name = name;
  node->s_decl.type = type;
  node->s_decl.value = value;
  node->s_decl.is_const = is_const;
  return node;
}

Code_Node *code_type_func(Parser *p, Code_Node **params, 
                          Code_Node *return_type) {
  Code_Node *node = code_node(p, Code_Kind_TYPE_FUNC);
  node->t_func.params = params;
  node->t_func.return_type = return_type;
  return node;
}

Code_Node *code_type_int(Parser *p, i32 size, b32 is_signed) {
  Code_Node *node = code_node(p, Code_Kind_TYPE_INT);
  node->t_int.size = size;
  node->t_int.is_signed = is_signed;
  node->type = builtin_Type;
  return node;
}

Code_Node *code_type_float(Parser *p, i32 size) {
  Code_Node *node = code_node(p, Code_Kind_TYPE_FLOAT);
  node->t_float.size = size;
  node->type = builtin_Type;
  return node;
}

Code_Node *code_type_pointer(Parser *p, Code_Node *base) {
  Code_Node *node = code_node(p, Code_Kind_TYPE_POINTER);
  node->t_pointer.base = base;
  return node;
}

Code_Node *code_expr_name(Parser *p, String name) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_NAME);
  node->e_name.name = name;
  return node;
}

Code_Node *code_expr_null(Parser *p) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_NULL);
  return node;
}

Code_Node *code_expr_string(Parser *p, String value) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_STRING);
  node->e_string.value = value;
  return node;
}

Code_Node *code_expr_char(Parser *p, char value) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_CHAR);
  node->e_char.value = value;
  return node;
}

Code_Node *code_type_struct(Parser *p, Code_Node **members) {
  Code_Node *node = code_node(p, Code_Kind_TYPE_STRUCT);
  node->t_struct.members = members;
  return node;
}

Code_Node *code_type_array(Parser *p, Code_Node *item_type, u64 count) {
  Code_Node *node = code_node(p, Code_Kind_TYPE_ARRAY);
  node->t_array.item_type = item_type;
  node->t_array.count = count;
  return node;
}

Code_Node *code_type_enum(Parser *p, String *members) {
  Code_Node *node = code_node(p, Code_Kind_TYPE_ENUM);
  node->t_enum.members = members;
  return node;
}

Code_Node *code_type_alias(Parser *p, String alias) {
  Code_Node *node = code_node(p, Code_Kind_TYPE_ALIAS);
  node->t_alias.name = alias;
  return node;
}

Code_Node *code_type_void(Parser *p) {
  Code_Node *node = code_node(p, Code_Kind_TYPE_VOID);
  node->type = builtin_Type;
  return node;
}

Code_Node *code_func(Parser *p, Code_Node *sig, Code_Node *body, b32 foreign) {
  Code_Node *node = code_node(p, Code_Kind_FUNC);
  node->func.sig = sig;
  node->func.body = body;
  node->func.foreign = foreign;
  return node;
}

Code_Node *code_expr_binary(Parser *p, Code_Node *left, Token_Kind op, Code_Node *right) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_BINARY);
  node->e_binary.left = left;
  node->e_binary.right = right;
  node->e_binary.op = op;
  return node;
}

Code_Node *code_expr_call(Parser *p, Code_Node *func, Code_Node **args) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_CALL);
  node->e_call.func = func;
  node->e_call.args = args;
  return node;
}

Code_Node *code_expr_unary(Parser *p, Token_Kind op, Code_Node *val) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_UNARY);
  node->e_unary.val = val;
  node->e_unary.op = op;
  return node;
}

Code_Node *code_expr_cast(Parser *p, Code_Node *cast_type, Code_Node *expr, b32 implicit) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_CAST);
  node->e_cast.cast_type = cast_type;
  node->e_cast.expr = expr;
  node->e_cast.implicit = implicit;
  return node;
}

Code_Node *code_expr_int(Parser *p, u64 value) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_INT);
  node->e_int.value = value;
  return node;
}

Code_Node *code_expr_float(Parser *p, f64 value) {
  Code_Node *node = code_node(p, Code_Kind_EXPR_FLOAT);
  node->e_float.value = value;
  return node;
}

Code_Node *code_stmt_assign(Parser *p, Code_Node *left, Token_Kind op, Code_Node *right) {
  Code_Node *node = code_node(p, Code_Kind_STMT_ASSIGN);
  node->s_assign.left = left;
  node->s_assign.right = right;
  node->s_assign.op = op;
  return node;
}

Code_Node *code_stmt_while(Parser *p, Code_Node *cond, Code_Node *body) {
  Code_Node *node = code_node(p, Code_Kind_STMT_WHILE);
  node->s_while.cond = cond;
  node->s_while.body = body;
  return node;
}

Code_Node *code_stmt_if(Parser *p, Code_Node *cond, Code_Node *then_branch, Code_Node *else_branch) {
  Code_Node *node = code_node(p, Code_Kind_STMT_IF);
  node->s_if.cond = cond;
  node->s_if.then_branch = then_branch;
  node->s_if.else_branch = else_branch;
  return node;
}

Code_Node *code_stmt_for(Parser *p, Code_Node *init, Code_Node *cond, Code_Node *post, Code_Node *body) {
  Code_Node *node = code_node(p, Code_Kind_STMT_FOR);
  node->s_for.init = init;
  node->s_for.cond = cond;
  node->s_for.post = post;
  node->s_for.body = body;
  return node;
}

Code_Node *code_stmt_keyword(Parser *p, Token_Kind keyword, Code_Node *stmt, Code_Node *extra) {
  Code_Node *node = code_node(p, Code_Kind_STMT_KEYWORD);
  node->s_keyword.keyword = keyword;
  node->s_keyword.stmt = stmt;
  node->s_keyword.extra = extra;
  return node;
}

Code_Node *code_stmt_block(Parser *p, Code_Node **statements) {
  Code_Node *node = code_node(p, Code_Kind_STMT_BLOCK);
  node->s_block.statements = statements;
  return node;
}

Code_Node *code_stmt_expr(Parser *p, Code_Node *expr) {
  Code_Node *node = code_node(p, Code_Kind_STMT_EXPR);
  node->s_expr.expr = expr;
  return node;
}
