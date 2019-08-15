#include "lexer.h"


Arena _scratch_arena;
Arena *scratch_arena = &_scratch_arena;


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
  Code_Stmt **deferred_statements;
  Scope *parent;
};


typedef struct {
  Code_Stmt_Decl **members;
  Scope *scope;
  i32 size;
} Code_Type_Struct;

typedef struct {
  String *members;
  Scope *scope;
  Code_Type *item_type;
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
  Code_Type *base;
  b32 is_builtin;
} Code_Type_Alias;

typedef struct {
  i32 size;
  b32 is_signed;
} Code_Type_Int;

typedef struct {
  i32 size;
} Code_Type_Float;


typedef enum {
  Type_Kind_STRUCT,
  Type_Kind_ENUM,
  Type_Kind_PTR,
  Type_Kind_ARRAY,
  Type_Kind_FUNC,
  Type_Kind_ALIAS,
  Type_Kind_INT,
  Type_Kind_FLOAT,
} Type_Kind;

struct Code_Type {
  union {
    Code_Type_Struct struct_t;
    Code_Type_Enum enum_t;
    Code_Type_Array array;
    Code_Type_Pointer pointer;
    Code_Type_Func func;
    Code_Type_Alias alias;
    Code_Type_Int int_t;
    Code_Type_Float float_t;
  };
  Type_Kind kind;
};


typedef struct {
  String name;
  Code_Stmt_Decl *decl;
} Code_Expr_Name;

typedef struct {
  Code_Expr *left;
  Code_Expr *right;
  Token_Kind op;
  b32 is_enum_member;
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
  Code_Type *cast_type;
  Code_Expr *expr;
  b32 implicit;
} Code_Expr_Cast;

typedef struct {
  u64 value;
  i32 size;
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

typedef enum {
  Expr_Kind_NONE,
  
  Expr_Kind_CAST,
  Expr_Kind_CAST_OR_CALL,
  Expr_Kind_UNARY,
  Expr_Kind_BINARY,
  Expr_Kind_CALL,
  Expr_Kind_INT,
  Expr_Kind_FLOAT,
  Expr_Kind_STRING,
  Expr_Kind_ARRAY,
  Expr_Kind_STRUCT,
  Expr_Kind_NAME,
  Expr_Kind_TYPE,
  Expr_Kind_NULL,
  Expr_Kind_CHAR,
} Expr_Kind;

struct Code_Expr {
  union {
    Code_Expr_Null null_e;
    Code_Type type_e;
    Code_Expr_Cast cast;
    Code_Expr_Name name;
    Code_Expr_Unary unary;
    Code_Expr_Binary binary;
    Code_Expr_Call call;
    Code_Expr_Int int_e;
    Code_Expr_Float float_e;
    Code_Expr_String string;
    Code_Expr_Char char_e;
    //Code_Expr_Array array;
    //Code_Expr_Struct struct_e;
  };
  
  Code_Type *type;
  Expr_Kind kind;
};

typedef struct {
  Code_Type_Func *type;
  Code_Stmt_Block *body;
  
  b32 foreign;
  String module;
  String foreign_name;
  
  Scope *scope;
  i32 stack_size;
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
  Code_Stmt *init;
  Code_Expr *cond;
  Code_Stmt *post;
  Code_Stmt *body;
} Code_Stmt_For;

typedef struct {
  Token_Kind keyword;
  Code_Stmt *stmt;
  Code_Expr *extra;
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

typedef enum {
  Storage_Kind_NONE,
  Storage_Kind_FUNC,
  Storage_Kind_BSS,
  Storage_Kind_STACK,
} Storage_Kind;

struct Code_Stmt_Decl {
  String name;
  Code_Type *type;
  Code_Node *value;
  
  // TODO(lvl5): merge bools into flags
  b32 is_const;
  
  Storage_Kind storage_kind;
  u32 offset; // offset from the BSS or the stack (or beginning of the struct)
  
  // TODO(lvl5): only global decls should have a check_state
  // we can probably get rid of emit_state
  Resolve_State check_state;
  Resolve_State emit_state;
};

typedef struct {
  Code_Expr *cond;
  Code_Stmt *body;
} Code_Stmt_While;

typedef enum {
  Stmt_Kind_NONE,
  
  Stmt_Kind_ASSIGN,
  Stmt_Kind_EXPR,
  Stmt_Kind_IF,
  Stmt_Kind_BLOCK,
  Stmt_Kind_FOR,
  Stmt_Kind_KEYWORD,
  Stmt_Kind_DECL,
  Stmt_Kind_WHILE,
} Stmt_Kind;

struct Code_Stmt {
  union {
    Code_Stmt_While while_s;
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
  
  Code_Kind_EXPR,
  Code_Kind_STMT,
  Code_Kind_FUNC,
} Code_Kind;

struct Code_Node {
  union {
    Code_Func func;
    Code_Stmt stmt;
    Code_Expr expr;
  };
  
  Code_Kind kind;
  i32 first_token;
  i32 last_token;
};

typedef enum {
  Parser_Flag_NONE = 0,
} Parser_Flag;

typedef struct {
  String src;
  Arena *arena;
  Token *tokens;
  u32 i;
  
  String *errors;
  
  Parser_Flag flags;
} Parser;

Code_Node *code_node(Parser *p, Code_Kind kind) {
  Code_Node *node = arena_push_struct(p->arena, Code_Node);
  Code_Node zero_node = {0};
  *node = zero_node;
  node->kind = kind;
  
  return node;
}

Code_Stmt_Decl *code_stmt_decl(Parser *p, String name, Code_Type *type, Code_Node *value, b32 is_const) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_DECL;
  node->stmt.decl.name = name;
  node->stmt.decl.type = type;
  node->stmt.decl.value = value;
  node->stmt.decl.is_const = is_const;
  return (Code_Stmt_Decl *)node;
}

Code_Type_Func *code_type_func(Parser *p, Code_Stmt_Decl **params, Code_Type *return_type) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_TYPE;
  node->expr.type_e.kind = Type_Kind_FUNC;
  node->expr.type_e.func.params = params;
  node->expr.type_e.func.return_type = return_type;
  return (Code_Type_Func *)node;
}

Code_Type_Int *code_type_int(Parser *p, i32 size, b32 is_signed) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_TYPE;
  node->expr.type_e.kind = Type_Kind_INT;
  node->expr.type_e.int_t.size = size;
  node->expr.type_e.int_t.is_signed = is_signed;
  return (Code_Type_Int *)node;
}

Code_Type_Float *code_type_float(Parser *p, i32 size) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_TYPE;
  node->expr.type_e.kind = Type_Kind_FLOAT;
  node->expr.type_e.float_t.size = size;
  return (Code_Type_Float *)node;
}

Code_Type_Pointer *code_type_pointer(Parser *p, Code_Type *base) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_TYPE;
  node->expr.type_e.kind = Type_Kind_PTR;
  node->expr.type_e.pointer.base = base;
  return (Code_Type_Pointer *)node;
}

Code_Expr_Name *code_expr_name(Parser *p, String name) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_NAME;
  node->expr.name.name = name;
  return (Code_Expr_Name *)node;
}

Code_Expr_Null *code_expr_null(Parser *p) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_NULL;
  return (Code_Expr_Null *)node;
}

Code_Expr_String *code_expr_string(Parser *p, String value) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_STRING;
  node->expr.string.value = value;
  return (Code_Expr_String *)node;
}

Code_Expr_Char *code_expr_char(Parser *p, char value) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_CHAR;
  node->expr.char_e.value = value;
  return (Code_Expr_Char *)node;
}

Code_Type_Struct *code_type_struct(Parser *p, Code_Stmt_Decl **members) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_TYPE;
  node->expr.type_e.kind = Type_Kind_STRUCT;
  node->expr.type_e.struct_t.members = members;
  return (Code_Type_Struct *)node;
}

Code_Type_Array *code_type_array(Parser *p, Code_Type *item_type, u64 count) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_TYPE;
  node->expr.type_e.kind = Type_Kind_ARRAY;
  node->expr.type_e.array.item_type = item_type;
  node->expr.type_e.array.count = count;
  return (Code_Type_Array *)node;
}

Code_Type_Enum *code_type_enum(Parser *p, String *members) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_TYPE;
  node->expr.type_e.kind = Type_Kind_ENUM;
  node->expr.type_e.enum_t.members = members;
  return (Code_Type_Enum *)node;
}

Code_Type_Alias *code_type_alias(Parser *p, String alias) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_TYPE;
  node->expr.type_e.kind = Type_Kind_ALIAS;
  node->expr.type_e.alias.name = alias;
  return (Code_Type_Alias *)node;
}

Code_Func *code_func(Parser *p, Code_Type_Func *sig, Code_Stmt_Block *body, b32 foreign) {
  Code_Node *node = code_node(p, Code_Kind_FUNC);
  node->func.type = sig;
  node->func.body = body;
  node->func.foreign = foreign;
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

Code_Expr_Cast *code_expr_cast(Parser *p, Code_Type *cast_type, Code_Expr *expr, b32 implicit) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_CAST;
  node->expr.cast.cast_type = cast_type;
  node->expr.cast.expr = expr;
  node->expr.cast.implicit = implicit;
  return (Code_Expr_Cast*)node;
}

Code_Expr_Int *code_expr_int(Parser *p, u64 value, i32 size) {
  Code_Node *node = code_node(p, Code_Kind_EXPR);
  node->expr.kind = Expr_Kind_INT;
  node->expr.int_e.value = value;
  node->expr.int_e.size = size;
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

Code_Stmt_While *code_stmt_while(Parser *p, Code_Expr *cond, Code_Stmt *body) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_WHILE;
  node->stmt.while_s.cond = cond;
  node->stmt.while_s.body = body;
  return (Code_Stmt_While *)node;
}

Code_Stmt_If *code_stmt_if(Parser *p, Code_Expr *cond, Code_Stmt *then_branch, Code_Stmt *else_branch) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_IF;
  node->stmt.if_s.cond = cond;
  node->stmt.if_s.then_branch = then_branch;
  node->stmt.if_s.else_branch = else_branch;
  return (Code_Stmt_If *)node;
}

Code_Stmt_For *code_stmt_for(Parser *p, Code_Stmt *init, Code_Expr *cond, Code_Stmt *post, Code_Stmt *body) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_FOR;
  node->stmt.for_s.init = init;
  node->stmt.for_s.cond = cond;
  node->stmt.for_s.post = post;
  node->stmt.for_s.body = body;
  return (Code_Stmt_For *)node;
}

Code_Stmt_Keyword *code_stmt_keyword(Parser *p, Token_Kind keyword, Code_Stmt *stmt, Code_Expr *extra) {
  Code_Node *node = code_node(p, Code_Kind_STMT);
  node->stmt.kind = Stmt_Kind_KEYWORD;
  node->stmt.keyword.keyword = keyword;
  node->stmt.keyword.stmt = stmt;
  node->stmt.keyword.extra = extra;
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

