//#include "c_emitter.c"
//#include "typechecker.c"
//#include "bytecode_emitter.c"
#include "parser.c"
#include "time.h"
#include <setjmp.h>


/*
TODO:
[ ] the problem: it makes sense to keep the type aliases, for loops, etc 
for the C backend to keep the code clean and idiomatic. it makes sense to transform
the code to make it as easy as possible for the bytecode backend
we should probably make a more clear separation between compiler phases
1) resolve parser ambigueties / build dependency graph?
2) infer types / typecheck
3) set sizes / locations
4.1) transform ast for bytecode
5.1) generate bytecode

[ ] aligning struct members
[ ] aligning stack variables
[ ] remove C-style for loops
[ ] add looping over strings
[ ] transform for loops AST into while loops
[ ] redefinition should be an error
[ ] better typechecker error messages

maybe this should come after polymorphism??? probably not, because polymorphism should be hard
[ ] fixed length arrays
[ ] dynamic arrays
[ ] array views


[ ] function overloading
[ ] operator overloading
[ ] default initialization for all variables
[ ] sizeof
[ ] functions should be emitted as pointers except for constant declarations
[ ] decide what casts are allowed
[ ] lvalues, rvalues, assignment, referencing and dereferencing
[ ] typedefs and functions inside functions
[ ] better for loop
-iterate arrays
-iterate enums
-iterate strings
[ ] while loop
[ ] enum to string, enum count
[ ] static struct members (enum names are essentially static members already)
[ ] initializers for structs
[ ] default parameters for funcs
[ ] allow omitting type names in functions without bodies/typedefs??
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
  Code_Node **decls;
  b32 success;
} Parse_Result;

char *tcstring(String str) {
  char *result = to_c_string(scratch_arena, str);
  return result;
}

Parse_Result parse(Parser *p, String src, Token *tokens) {
  Parse_Result result = {0};
  result.decls = parse_program(p);
  if (!string_is_empty(p->error)) {
    printf("Parser error: %s\n\n", tcstring(p->error));
    result.success = false;
  } else {
    result.success = true;
  }
  
  return result;
}


typedef struct {
  byte *data;
  u64 size;
} Buffer;
Buffer read_entire_file(Arena *arena, String file_name) {
  Buffer result;
  FILE *file;
  char *c_file_name = to_c_string(arena, file_name);
  fopen_s(&file, c_file_name, "rb");
  fseek(file, 0, SEEK_END);
  result.size = ftell(file);
  fseek(file, 0, SEEK_SET);
  result.data = arena_push_array(arena, byte, result.size + 1);
  
  fread(result.data, result.size, 1, file);
  
  fclose(file);
  return result;
}

void builder_to_file(String file_name, String_Builder *builder) {
  FILE *file;
  char *c_file_name = to_c_string(scratch_arena, file_name);
  fopen_s(&file, c_file_name, "wb");
  
  for (String_Builder_Block *block = &builder->first; block; block = block->next) {
    u64 size = block == builder->last 
      ? builder->count_in_block 
      : STRING_BUILDER_BLOCK_MAX;
    fwrite(block->data, size, 1, file);
  }
  fclose(file);
}


typedef enum {
  Stage_NONE,
  Stage_TYPECHECK,
  Stage_SIZE,
  Stage_BYTECODE,
  Stage_RUN,
} Stage;

typedef struct {
  jmp_buf return_buf;
  Stage stage;
  Parser *parser;
} Check_State_Common;

typedef struct {
  Check_State_Common *common;
  Scope *scope;
} Check_State;

#define yield() longjmp(state.common->return_buf, 1)

b32 types_are_equal(Code_Node *a, Code_Node *b) {
  b32 result = false;
  
  if (a->kind == b->kind) {
    switch (a->kind) {
      case Code_Kind_TYPE_ALIAS: {
        result = string_compare(a->t_alias.name, b->t_alias.name);
      } break;
      
      case Code_Kind_TYPE_POINTER: {
        result = types_are_equal(a->t_pointer.base, b->t_pointer.base);
      } break;
      
      case Code_Kind_TYPE_ARRAY: {
        // TODO(lvl5): fixed sized arrays
        result = types_are_equal(a->t_array.item_type, b->t_array.item_type);
      } break;
      
      case Code_Kind_TYPE_FUNC: {
        if (types_are_equal(a->t_func.return_type, b->t_func.return_type)) {
          result = true;
          if (sb_count(a->t_func.params) == sb_count(a->t_func.params)) {
            for (u32 i = 0; i < sb_count(a->t_func.params); i++) {
              Code_Node *a_param = a->t_func.params[i];
              Code_Node *b_param = b->t_func.params[i];
              if (!types_are_equal(a_param, b_param)) {
                result = false;
                break;
              }
            }
          }
        }
      } break;
      
      case Code_Kind_TYPE_INT: {
        result = a->t_int.size == b->t_int.size &&
          a->t_int.is_signed == b->t_int.is_signed;
      } break;
      
      case Code_Kind_TYPE_FLOAT: {
        result = a->t_float.size == b->t_float.size;
      } break;
      
      case Code_Kind_TYPE_VOID: {
        result = true;
      } break;
      
      default: {
        result = false;
      } break;
    }
  }
  
  return result;
}


void typecheck_decl(Check_State, Code_Node *);
void typecheck_stmt(Check_State, Code_Node *);
void typecheck_expression(Check_State, Code_Node *);

void typecheck_type(Check_State state, Code_Node *node) {
  if (node->type) return;
  
  switch (node->kind) {
    case Code_Kind_TYPE_STRUCT: {
      if (!node->t_struct.scope) {
        node->t_struct.scope = alloc_scope(state.common->parser->arena,
                                           state.scope);
      }
      
      state.scope = node->t_struct.scope;
      for (u32 i = 0; i < sb_count(node->t_struct.members); i++) {
        Code_Node *member = node->t_struct.members[i];
        typecheck_decl(state, member);
      }
    } break;
    case Code_Kind_TYPE_ENUM: {
      if (!node->t_enum.scope) {
        node->t_enum.scope = alloc_scope(state.common->parser->arena,
                                         state.scope);
      }
      
      state.scope = node->t_enum.scope;
      for (u32 i = 0; i < sb_count(node->t_enum.members); i++) {
        String member = node->t_enum.members[i];
        
        Code_Node *decl = code_stmt_decl(
          state.common->parser,
          member,
          builtin_i64,
          code_expr_int(state.common->parser, (u64)i),
          true);
        scope_add(state.scope, decl);
      }
    } break;
    case Code_Kind_TYPE_POINTER: {
      typecheck_type(state, node->t_pointer.base);
    } break;
    case Code_Kind_TYPE_ARRAY: {
      typecheck_type(state, node->t_array.item_type);
    } break;
    case Code_Kind_TYPE_FUNC: {
      typecheck_type(state, node->t_func.return_type);
      for (u32 i = 0; i < sb_count(node->t_func.params); i++) {
        typecheck_type(state, node->t_func.params[i]);
      }
    } break;
    case Code_Kind_TYPE_ALIAS: {
      Scope_Entry *entry = scope_get(state.scope, node->t_alias.name);
      if (!entry) {
        yield();
      }
      node->t_alias.base = entry->decl->s_decl.value;
    } break;
    default: assert(false);
  }
  
  node->type = builtin_Type;
}


void typecheck_stmt_block(Check_State state, Code_Node *node, b32 is_function_body) {
  if (!is_function_body) {
    node->s_block.scope = alloc_scope(state.common->parser->arena, state.scope);
    state.scope = node->s_block.scope;
  }
  
  for (u32 i = 0; i < sb_count(node->s_block.statements); i++) {
    typecheck_stmt(state, node->s_block.statements[i]);
  }
}

void typecheck_stmt(Check_State state, Code_Node *node) {
  if (node->type) return;
  
  switch (node->kind) {
    case Code_Kind_STMT_ASSIGN: {
      // TODO(lvl5): check operator and types
      // TODO(lvl5): implicit conversions
      typecheck_expression(state, node->s_assign.left);
      typecheck_expression(state, node->s_assign.right);
      assert(types_are_equal(node->s_assign.left->type,
                             node->s_assign.right->type));
    } break;
    case Code_Kind_STMT_EXPR: {
      typecheck_expression(state, node->s_expr.expr);
    } break;
    case Code_Kind_STMT_IF: {
      typecheck_expression(state, node->s_if.cond);
      typecheck_stmt(state, node->s_if.then_branch);
      if (node->s_if.else_branch) {
        typecheck_stmt(state, node->s_if.else_branch);
      }
    } break;
    case Code_Kind_STMT_BLOCK: {
      typecheck_stmt_block(state, node, false);
    } break;
    case Code_Kind_STMT_FOR: {
      typecheck_stmt(state, node->s_for.init);
      typecheck_expression(state, node->s_for.cond);
      typecheck_stmt(state, node->s_for.post);
      typecheck_stmt(state, node->s_for.body);
    } break;
    case Code_Kind_STMT_KEYWORD: {
      assert(false);
    } break;
    case Code_Kind_STMT_DECL: {
      typecheck_decl(state, node);
    } break;
    case Code_Kind_STMT_WHILE: {
      typecheck_expression(state, node->s_while.cond);
      typecheck_stmt(state, node->s_while.body);
    } break;
    
    default: assert(false);
  }
  
  node->type = builtin_statement_type;
}

void typecheck_func(Check_State state, Code_Node *node) {
  Code_Func *func = &node->func;
  
  typecheck_type(state, func->sig);
  
  node->func.scope = alloc_scope(state.common->parser->arena, state.scope);
  
  state.scope = node->func.scope;
  typecheck_stmt_block(state, func->body, true);
}

Code_Node *get_final_type(Code_Node *type) {
  Code_Node *result = type;
  while (result->kind == Code_Kind_TYPE_ALIAS) {
    result = result->t_alias.base;
  }
  return result;
}

void typecheck_expression(Check_State state, Code_Node *node) {
  if (node->type) return;
  
  switch (node->kind) {
    case Code_Kind_EXPR_CAST: {
      // TODO(lvl5): check that the conversion makes sense
      node->type = node->e_cast.cast_type;
    } break;
    case Code_Kind_EXPR_UNARY: {
      Code_Expr_Unary *unary = &node->e_unary;
      typecheck_expression(state, unary->val);
      // NOTE(lvl5): may be an address expression OR a pointer type
      if (is_type(unary->val)) {
        Code_Node *ptr = code_type_pointer(state.common->parser,
                                           unary->val);
        *node = *ptr;
        typecheck_type(state, node);
      } else {
        if (unary->op == T_REF) {
          node->type = code_type_pointer(state.common->parser, 
                                         unary->val->type);
        } else if (unary->op == T_DEREF) {
          assert(unary->val->type->kind == Code_Kind_TYPE_POINTER);
          node->type = unary->val->type->t_pointer.base;
        } else {
          node->type = unary->val->type;
        }
      }
    } break;
    case Code_Kind_EXPR_BINARY: {
      Code_Expr_Binary *bin = &node->e_binary;
      
      switch (bin->op) {
        case T_MEMBER: {
          assert(bin->right->kind == Code_Kind_EXPR_NAME);
          typecheck_expression(state, bin->left);
          Code_Node *left_type = get_final_type(bin->left->type);
          
          if (left_type->kind == Code_Kind_TYPE_STRUCT) {
            Code_Node *expr_type = null;
            for (u32 i = 0; i < sb_count(left_type->t_struct.members); i++) {
              Code_Stmt_Decl *member = &left_type->t_struct.members[i]->s_decl;
              if (string_compare(member->name, bin->right->e_name.name)) {
                expr_type = member->type;
              }
            }
            assert(expr_type);
            node->type = expr_type;
          } else if (left_type->kind == Code_Kind_TYPE_ENUM) {
            b32 member_found = false;
            for (u32 i = 0; i < sb_count(left_type->t_enum.members); i++) {
              String member = left_type->t_enum.members[i];
              if (string_compare(member, bin->right->e_name.name)) {
                member_found = true;
              }
            }
            assert(member_found);
            node->type = left_type->t_enum.item_type;
          }
        } break;
        
        assert(false);
      }
      
      assert(types_are_equal(bin->left->type, bin->right->type));
      
      node->type = bin->left->type;
    } break;
    case Code_Kind_EXPR_CALL: {
      // NOTE(lvl5): may be a function call OR a cast
      typecheck_expression(state, node->e_call.func);
      if (is_type(node->e_call.func)) {
        Code_Node *cast = code_expr_cast(state.common->parser,
                                         node->e_call.func,
                                         node->e_call.args[0],
                                         false);
        *node = *cast;
        typecheck_expression(state, node);
      } else {
        assert(node->e_call.func->type->kind == Code_Kind_TYPE_FUNC);
        Code_Type_Func *sig = &node->e_call.func->type->t_func;
        
        for (u32 i = 0; i < sb_count(node->e_call.args); i++) {
          Code_Node *param = sig->params[i];
          Code_Node *arg = node->e_call.args[i];
          typecheck_expression(state, arg);
          assert(types_are_equal(arg->type, param->type));
        }
        
        node->type = sig->return_type;
      }
    } break;
    case Code_Kind_EXPR_INT: {
      node->type = builtin_i64;
    } break;
    case Code_Kind_EXPR_FLOAT: {
      node->type = builtin_f64;
    } break;
    case Code_Kind_EXPR_STRING: {
      // TODO(lvl5): the string alias needs its base to be set
      node->type = builtin_string;
    } break;
    case Code_Kind_EXPR_NAME: {
      Scope_Entry *entry = scope_get(state.scope, node->e_name.name);
      if (!entry || !entry->decl->s_decl.type) {
        yield();
      }
      
      if (entry->decl->s_decl.type == builtin_Type) {
        Code_Node *type = code_type_alias(state.common->parser, 
                                          node->e_name.name);
        *node = *type;
        typecheck_type(state, node);
      }
      
      node->type = entry->decl->s_decl.type;
    } break;
    case Code_Kind_EXPR_NULL: {
      node->type = builtin_voidptr;
    } break;
    case Code_Kind_EXPR_CHAR: {
      node->type = builtin_i8;
    } break;
    
    default: assert(false);
  }
}

void typecheck_decl(Check_State state, Code_Node *node) {
  Code_Stmt_Decl *decl = &node->s_decl;
  
  if (!scope_get(state.scope, decl->name)) {
    scope_add(state.scope, node);
  }
  
  if (decl->type) {
    typecheck_type(state, decl->type);
  }
  
  if (decl->value) {
    if (decl->value->kind == Code_Kind_FUNC) {
      typecheck_func(state, decl->value);
    } else if (is_expression(decl->value)) {
      typecheck_expression(state, decl->value);
    } else if (is_type(decl->value)) {
      typecheck_type(state, decl->value);
    } else {
      assert(false);
    }
    
    if (decl->type) {
      assert(types_are_equal(decl->value->type, decl->type));
    } else {
      decl->type = decl->value->type;
    }
  }
}

void typecheck_top_decl(Check_State state, Code_Node *node) {
  typecheck_decl(state, node);
  state.common->stage = Stage_TYPECHECK;
}

int main() {
  clock_t front_start = clock();
  
  Arena _arena;
  Arena *arena = &_arena;
  arena_init(arena, malloc(megabytes(100)), megabytes(100));
  arena_init(scratch_arena, malloc(megabytes(1)), megabytes(1));
  
  //bytecode_test(arena);
  
  Scope *global_scope = alloc_scope(arena, null);
  
  Buffer file = read_entire_file(arena, const_string("code\\test.lang"));
  file.data[file.size++] = 0;
  String src = make_string((char *)file.data, (u32)file.size);
  Token *tokens = tokenize(arena, src);
  
  Parser _p = {0};
  Parser *p = &_p;
  p->arena = arena;
  p->tokens = tokens;
  p->i = 0;
  p->src = src;
  p->global_scope = global_scope;
  
  {
    builtin_Type = code_type_alias(p, const_string("Type"));
    scope_add(global_scope, code_stmt_decl(p, const_string("Type"), 
                                           builtin_Type, builtin_Type, true));
    
    builtin_void = code_type_void(p);
    scope_add(global_scope, code_stmt_decl(p, const_string("void"), builtin_Type,
                                           (Code_Node *)builtin_void, true));
    
#define ADD_BUILTIN_INT(name, size, is_signed) \
    builtin_##name = code_type_int(p, size, is_signed); \
    scope_add(global_scope, code_stmt_decl(p, const_string(#name), builtin_Type, \
    (Code_Node *)builtin_##name, true));
    
    ADD_BUILTIN_INT(u8, 1, false);
    ADD_BUILTIN_INT(u16, 2, false);
    ADD_BUILTIN_INT(u32, 4, false);
    ADD_BUILTIN_INT(u64, 8, false);
    ADD_BUILTIN_INT(i8, 1, true);
    ADD_BUILTIN_INT(i16, 2, true);
    ADD_BUILTIN_INT(i32, 4, true);
    ADD_BUILTIN_INT(i64, 8, true);
    
#define ADD_BUILTIN_FLOAT(name, size) \
    builtin_##name = code_type_float(p, size); \
    scope_add(global_scope, code_stmt_decl(p, const_string(#name), builtin_Type, \
    (Code_Node *)builtin_##name, true));
    ADD_BUILTIN_FLOAT(f32, 4);
    ADD_BUILTIN_FLOAT(f64, 8);
    
    builtin_string = code_type_alias(p, const_string("string"));
    builtin_voidptr = code_type_pointer(p, builtin_void);
    
    Code_Node _builtint_statement_type;
    builtin_statement_type = &_builtint_statement_type;
  }
  
  Parse_Result parse_result = parse(p, src, tokens);
  if (parse_result.success) {
    u32 top_decl_count = sb_count(parse_result.decls);
    Check_State *states = sb_new(arena, Check_State, 
                                 top_decl_count);
    zero_memory_slow(states, sizeof(Check_State)*top_decl_count);
    Check_State_Common *commons = sb_new(arena, Check_State_Common,
                                         top_decl_count);
    zero_memory_slow(commons, sizeof(Check_State_Common)*top_decl_count);
    
    for (u32 i = 0; i < top_decl_count; i++) {
      Check_State *state = states + i;
      state->common = commons + i;
      state->scope = global_scope;
      state->common->parser = p;
    }
    
    u32 completed_count = 0;
    while (completed_count < top_decl_count) {
      for (u32 top_decl_index = 0; 
           top_decl_index < sb_count(parse_result.decls);
           top_decl_index++) {
        Code_Node *top_decl = parse_result.decls[top_decl_index];
        Check_State state = states[top_decl_index];
        
        Stage old_stage = state.common->stage;
        if (!setjmp(state.common->return_buf)) {
          switch (state.common->stage) {
            case Stage_NONE: {
              typecheck_top_decl(state, top_decl);
            } break;
          }
        }
        if (state.common->stage != old_stage) {
          completed_count++;
        }
      }
    }
  }
  
  
  
  
  
#if 0
  if (parse_result.success) {
    Resolver_Common common = {0};
    Resolver res = {0};
    res.common = &common;
    res.common->top_decls = parse_result.decls;
    res.common->parser = p;
    
    Bc_Emitter emitter = {0};
    emitter.bss_segment = arena_push_array(arena, byte, megabytes(2));
    emitter.instructions = arena_push_array(arena, Bc_Instruction, 2048);
    emitter.parser = p;
    res.common->bc_emitter = &emitter;
    
    
    resolve_decls(res, parse_result, global_scope);
    
    
    clock_t front_end = clock();
    
    f64 front_time = (f64)(front_end - front_start)/(f64)(CLOCKS_PER_SEC);
    printf("front time: %0.3f s\n", front_time);
    
    if (sb_count(p->errors) == 0) {
      bytecode_print(&emitter);
      __debugbreak();
      bytecode_run(arena, &emitter);
      return;
    }
  }
  
  if (sb_count(p->errors)) {
    
  } else {
    char compiler_flags[] = "-Od -MTd -nologo -Oi -GR- -EHa- -WX -W4 -wd4101 -wd4702 -wd4005 -wd4505 -wd4456 -wd4201 -wd4100 -wd4189 -wd4204 -wd4459 -Zi -FC";
    
    char linker_flags[] = "-incremental:no -opt:ref OpenGL32.lib Winmm.lib user32.lib Gdi32.lib";
    
    char buffer[1024];
    sprintf_s(buffer, 1024, "cl %s code\\out.c /link %s", compiler_flags, linker_flags);
    
    clock_t front_end = clock();
    
    f64 front_time = (f64)(front_end - front_start)/(f64)(CLOCKS_PER_SEC);
    printf("front time: %0.3f s\n", front_time);
    
    clock_t cl_start = clock();
    system(buffer);
    
    clock_t cl_end = clock();
    f64 cl_time = (f64)(cl_end - cl_start)/(f64)(CLOCKS_PER_SEC);
    printf("cl time: %0.3f s\n", cl_time);
  }
#endif
  
  getchar();
  return 0;
}
