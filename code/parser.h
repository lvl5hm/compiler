#include "lexer.h"

typedef enum {
  Type_Info_Kind_NONE,
  
  Type_Info_Kind_ARRAY,
  Type_Info_Kind_POINTER,
  Type_Info_Kind_STRUCT,
  Type_Info_Kind_INT,
  Type_Info_Kind_FLOAT,
  Type_Info_Kind_TYPE,
  Type_Info_Kind_ALIAS,
  Type_Info_Kind_FUNC,
} Type_Info_Kind;

typedef struct Type_Info Type_Info;

typedef struct {
  Type_Info *item_type;
  u64 count;
} Type_Info_Array;

typedef struct {
  Type_Info *base;
} Type_Info_Pointer;

typedef struct {
  String name;
  Type_Info *type;
} Type_Info_Struct_Member;

typedef struct {
  Type_Info_Struct_Member *members;
} Type_Info_Struct;

typedef struct {
  b32 __;
} Type_Info_Float;

typedef struct {
  b32 is_signed;
} Type_Info_Int;

typedef struct {
  b32 __;
} Type_Info_Type;

typedef struct {
  Type_Info **params;
  Type_Info *return_type;
} Type_Info_Func;

struct Type_Info {
  union {
    Type_Info_Array array;
    Type_Info_Pointer pointer;
    Type_Info_Float float_t;
    Type_Info_Int int_t;
    Type_Info_Type type;
    Type_Info_Func func;
    Type_Info_Struct struct_t;
  };
  Type_Info_Kind kind;
  i32 size;
};


typedef struct Code_Node Code_Node;
typedef struct Code_Type Code_Type;
typedef struct Code_Stmt Code_Stmt;
typedef struct Code_Stmt_Decl Code_Stmt_Decl;
typedef struct Code_Expr Code_Expr;
typedef struct Code_Stmt_Block Code_Stmt_Block;

typedef struct {
  Code_Stmt_Decl *decl;
} Scope_Entry;

typedef struct Scope Scope;
struct Scope {
  Scope_Entry *entries;
  Scope *parent;
};




typedef struct {
  Code_Stmt_Decl **members;
  
  Scope *scope;
} Code_Type_Struct;

typedef struct {
  String *members;
} Code_Type_Enum;

typedef struct {
  Code_Type *item_type;
  u64 count;
} Code_Type_Array;

typedef struct {
  Code_Type *base;
} Code_Type_Pointer;


typedef struct {
  Code_Stmt_Decl **params;
  Code_Type *return_type;
} Code_Type_Func;

typedef struct {
  String name;
} Code_Type_Alias;


typedef enum {
  Type_Kind_STRUCT,
  Type_Kind_ENUM,
  Type_Kind_PTR,
  Type_Kind_ARRAY,
  Type_Kind_FUNC,
  Type_Kind_ALIAS,
} Type_Kind;

struct Code_Type {
  union {
    Code_Type_Struct struct_t;
    Code_Type_Enum enum_t;
    Code_Type_Array array;
    Code_Type_Pointer pointer;
    Code_Type_Func func;
    Code_Type_Alias alias;
  };
  Type_Kind kind;
};


typedef struct {
  String name;
} Code_Expr_Name;

typedef struct {
  Code_Expr *left;
  Code_Expr *right;
  Token_Kind op;
} Code_Expr_Binary;

typedef struct {
  Code_Expr *val;
  Token_Kind op;
} Code_Expr_Unary;

typedef struct {
  Code_Expr *func;
  Code_Expr **args;
} Code_Expr_Call;

typedef struct {
  i64 value;
} Code_Expr_Int;

typedef struct {
  f64 value;
} Code_Expr_Float;

typedef struct {
  String value;
} Code_Expr_String;

typedef enum {
  Expr_Kind_NONE,
  
  Expr_Kind_UNARY,
  Expr_Kind_BINARY,
  Expr_Kind_CALL,
  Expr_Kind_INT,
  Expr_Kind_FLOAT,
  Expr_Kind_STRING,
  Expr_Kind_ARRAY,
  Expr_Kind_STRUCT,
  Expr_Kind_NAME,
} Expr_Kind;

struct Code_Expr {
  union {
    Code_Expr_Name name;
    Code_Expr_Unary unary;
    Code_Expr_Binary binary;
    Code_Expr_Call call;
    Code_Expr_Int int_e;
    Code_Expr_Float float_e;
    Code_Expr_String string;
    //Code_Expr_Array array;
    //Code_Expr_Struct struct_e;
  };
  
  Code_Type *type;
  Expr_Kind kind;
};

typedef struct {
  Code_Type_Func *type;
  Code_Stmt_Block *body;
  Scope *scope;
} Code_Func;

typedef struct {
  Code_Expr *left;
  Code_Expr *right;
  
  Token_Kind op;
} Code_Stmt_Assign;

typedef struct {
  Code_Expr *expr;
} Code_Stmt_Expr;

typedef struct {
  Code_Expr *cond;
  Code_Stmt *then_branch;
  Code_Stmt *else_branch;
} Code_Stmt_If;

typedef struct {
  Code_Stmt_Decl *init;
  Code_Expr *cond;
  Code_Stmt *post;
  Code_Stmt *body;
} Code_Stmt_For;

typedef struct {
  Token_Kind keyword;
  Code_Expr *expr;
} Code_Stmt_Keyword;

struct Code_Stmt_Block {
  Code_Stmt **statements;
  Scope *scope;
};

typedef enum {
  Resolve_State_UNRESOLVED,
  Resolve_State_PARTIAL,
  Resolve_State_FULL,
} Resolve_State;

struct Code_Stmt_Decl {
  String name;
  Code_Type *type;
  Code_Node *value;
  Scope *scope;
  
  Resolve_State resolve_state;
};

typedef enum {
  Stmt_Kind_NONE,
  
  Stmt_Kind_ASSIGN,
  Stmt_Kind_EXPR,
  Stmt_Kind_IF,
  Stmt_Kind_BLOCK,
  Stmt_Kind_FOR,
  Stmt_Kind_KEYWORD,
  Stmt_Kind_DECL,
} Stmt_Kind;

struct Code_Stmt {
  union {
    Code_Stmt_Assign assign;
    Code_Stmt_Expr expr;
    Code_Stmt_If if_s;
    Code_Stmt_Block block;
    Code_Stmt_For for_s;
    Code_Stmt_Keyword keyword;
    Code_Stmt_Decl decl;
  };
  Stmt_Kind kind;
};


typedef enum {
  Code_Kind_NONE,
  Code_Kind_ERROR,
  
  Code_Kind_TYPE,
  Code_Kind_EXPR,
  Code_Kind_STMT,
  Code_Kind_FUNC,
} Code_Kind;

struct Code_Node {
  union {
    Code_Func func;
    Code_Stmt stmt;
    Code_Expr expr;
    Code_Type type;
  };
  
  Code_Kind kind;
};

typedef enum {
  Parser_Flag_NONE = 0,
} Parser_Flag;

typedef struct {
  String src;
  Arena *arena;
  Token *tokens;
  u32 i;
  i32 error_count;
  
  Parser_Flag flags;
} Parser;

Code_Node *code_node(Parser *p, Code_Kind kind) {
  Code_Node *node = arena_push_struct(p->arena, Code_Node);
  Code_Node zero_node = {0};
  *node = zero_node;
  node->kind = kind;
  
  return node;
}

Code_Stmt_Decl *code_stmt_decl(Parser *p, String name, Code_Type *type, Code_Node *value) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_DECL;
  node->stmt.decl.name = name;
  node->stmt.decl.type = type;
  node->stmt.decl.value = value;
  return (Code_Stmt_Decl *)node;
}

Code_Type_Func *code_type_func(Parser *p, Code_Stmt_Decl **params, Code_Type *return_type) {
  Code_Node *node = code_node(p, Code_Kind_TYPE);
  node->type.kind = Type_Kind_FUNC;
  node->type.func.params = params;
  node->type.func.return_type = return_type;
  return (Code_Type_Func *)node;
}

Code_Type_Pointer *code_type_pointer(Parser *p, Code_Type *base) {
  Code_Node *node = code_node(p, Code_Kind_TYPE);
  node->type.kind = Type_Kind_PTR;
  node->type.pointer.base = base;
  return (Code_Type_Pointer *)node;
}

Code_Expr_Name *code_expr_name(Parser *p, String name) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_NAME;
  node->expr.name.name = name;
  return (Code_Expr_Name *)node;
}

Code_Expr_String *code_expr_string(Parser *p, String value) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_STRING;
  node->expr.string.value = value;
  return (Code_Expr_String *)node;
}

Code_Type_Struct *code_type_struct(Parser *p, Code_Stmt_Decl **members) {
  Code_Node *node = code_node(p, Code_Kind_TYPE);
  node->type.kind = Type_Kind_STRUCT;
  node->type.struct_t.members = members;
  return (Code_Type_Struct *)node;
}

Code_Type_Array *code_type_array(Parser *p, Code_Type *item_type, u64 count) {
  Code_Node *node = code_node(p, Code_Kind_TYPE);
  node->type.kind = Type_Kind_ARRAY;
  node->type.array.item_type = item_type;
  node->type.array.count = count;
  return (Code_Type_Array *)node;
}

Code_Type_Enum *code_type_enum(Parser *p, String *members) {
  Code_Node *node = code_node(p, Code_Kind_TYPE);
  node->type.kind = Type_Kind_ENUM;
  node->type.enum_t.members = members;
  return (Code_Type_Enum *)node;
}

Code_Type_Alias *code_type_alias(Parser *p, String alias) {
  Code_Node *node = code_node(p, Code_Kind_TYPE);
  node->type.kind = Type_Kind_ALIAS;
  node->type.alias.name = alias;
  return (Code_Type_Alias *)node;
}

Code_Func *code_func(Parser *p, Code_Type_Func *sig, Code_Stmt_Block *body) {
  Code_Node *node = code_node(p, Code_Kind_FUNC);
  node->func.type = sig;
  node->func.body = body;
  return (Code_Func *)node;
}

Code_Expr_Binary *code_expr_binary(Parser *p, Code_Expr *left, Token_Kind op, Code_Expr *right) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_BINARY;
  node->expr.binary.left = left;
  node->expr.binary.right = right;
  node->expr.binary.op = op;
  return (Code_Expr_Binary *)node;
}

Code_Expr_Call *code_expr_call(Parser *p, Code_Expr *func, Code_Expr **args) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_CALL;
  node->expr.call.func = func;
  node->expr.call.args = args;
  return (Code_Expr_Call *)node;
}

Code_Expr_Unary *code_expr_unary(Parser *p, Token_Kind op, Code_Expr *val) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_UNARY;
  node->expr.unary.val = val;
  node->expr.unary.op = op;
  return (Code_Expr_Unary *)node;
}

Code_Expr_Int *code_expr_int(Parser *p, i64 value) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_INT;
  node->expr.int_e.value = value;
  return (Code_Expr_Int *)node;
}

Code_Expr_Float *code_expr_float(Parser *p, f64 value) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_FLOAT;
  node->expr.float_e.value = value;
  return (Code_Expr_Float *)node;
}

Code_Stmt_Assign *code_stmt_assign(Parser *p, Code_Expr *left, Token_Kind op, Code_Expr *right) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_ASSIGN;
  node->stmt.assign.left = left;
  node->stmt.assign.right = right;
  node->stmt.assign.op = op;
  return (Code_Stmt_Assign *)node;
}

Code_Stmt_If *code_stmt_if(Parser *p, Code_Expr *cond, Code_Stmt *then_branch, Code_Stmt *else_branch) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_IF;
  node->stmt.if_s.cond = cond;
  node->stmt.if_s.then_branch = then_branch;
  node->stmt.if_s.else_branch = else_branch;
  return (Code_Stmt_If *)node;
}

Code_Stmt_For *code_stmt_for(Parser *p, Code_Stmt_Decl *init, Code_Expr *cond, Code_Stmt *post, Code_Stmt *body) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_FOR;
  node->stmt.for_s.init = init;
  node->stmt.for_s.cond = cond;
  node->stmt.for_s.post = post;
  node->stmt.for_s.body = body;
  return (Code_Stmt_For *)node;
}

Code_Stmt_Keyword *code_stmt_keyword(Parser *p, Token_Kind keyword, Code_Expr *expr) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_KEYWORD;
  node->stmt.keyword.keyword = keyword;
  node->stmt.keyword.expr = expr;
  return (Code_Stmt_Keyword *)node;
}

Code_Stmt_Block *code_stmt_block(Parser *p, Code_Stmt **statements) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_BLOCK;
  node->stmt.block.statements = statements;
  return (Code_Stmt_Block *)node;
}

Code_Stmt_Expr *code_stmt_expr(Parser *p, Code_Expr *expr) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_EXPR;
  node->stmt.expr.expr = expr;
  return (Code_Stmt_Expr *)node;
}

