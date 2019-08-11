//#include "c_emitter.c"
#include "typechecker.c"
#include "bytecode_emitter.c"
#include "time.h"

/*
TODO:
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
    u64 size = block == builder->cur ? builder->count_in_block : STRING_BUILDER_BLOCK_MAX;
    fwrite(block->data, size, 1, file);
    
    int foo = 43;
  }
  fclose(file);
}

void resolve_decls(Resolver res, Parse_Result parsed, Scope *global_scope) {
  for (u32 decl_index = 0; decl_index < sb_count(parsed.decls); decl_index++) {
    Code_Stmt_Decl *decl = parsed.decls[decl_index];
    resolve_and_emit_decl(res, global_scope, decl, Resolve_State_FULL);
  }
}

int main() {
  clock_t front_start = clock();
  
  Arena _arena;
  Arena *arena = &_arena;
  arena_init(arena, malloc(megabytes(10)), megabytes(10));
  arena_init(scratch_arena, malloc(megabytes(1)), megabytes(1));
  
  bytecode_test(arena);
  
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
  p->errors = sb_new(arena, String, 16);
  
  Scope *global_scope = alloc_scope(arena, null);
  
  builtin_Type = (Code_Type *)code_type_alias(p, const_string("Type"));
  scope_add(global_scope, code_stmt_decl(p, const_string("Type"), builtin_Type, (Code_Node *)builtin_Type, true));
  
  builtin_void = (Code_Type *)code_type_alias(p, const_string("void"));
  scope_add(global_scope, code_stmt_decl(p, const_string("void"), builtin_Type,
                                         (Code_Node *)builtin_void, true));
  
#define ADD_BUILTIN_INT(name, size, is_signed) \
  builtin_##name = (Code_Type *)code_type_int(p, size, is_signed); \
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
  builtin_##name = (Code_Type *)code_type_float(p, size); \
  scope_add(global_scope, code_stmt_decl(p, const_string(#name), builtin_Type, \
  (Code_Node *)builtin_##name, true));
  ADD_BUILTIN_FLOAT(f32, 32);
  ADD_BUILTIN_FLOAT(f64, 64);
  
  builtin_voidptr = (Code_Type *)code_type_pointer(p, builtin_void);
  
  
  
  Parse_Result parse_result = parse(p, src, tokens);
  if (parse_result.success) {
    Resolver_Common common = {0};
    Resolver res = {0};
    res.common = &common;
    res.common->top_decls = parse_result.decls;
    res.common->parser = p;
    
#if 0    
    Emitter emitter = {0};
    emitter.indent = 0;
    emitter.res = &res;
    emitter.builder = arena_push_struct(arena, String_Builder);
    builder_init(emitter.builder, arena);
    res.emitter = &emitter;
#endif
    
    Bc_Emitter emitter = {0};
    emitter.instructions = arena_push_array(arena, Bc_Instruction, 2048);
    res.common->bc_emitter = &emitter;
    
    // NOTE(lvl5): c header
#if 0
    emit_string(&emitter, const_string(
      "#define NULL 0\n"
      "typedef char i8;\n"
      "typedef short i16;\n"
      "typedef int i32;\n"
      "typedef long long i64;\n"
      "typedef unsigned char u8;\n"
      "typedef unsigned short u16;\n"
      "typedef unsigned int u32;\n"
      "typedef unsigned long long u64;\n"
      "\n"
      ));
#endif
    
    resolve_decls(res, parse_result, global_scope);
    
#if 0    
    emit_string(&emitter, const_string(
      "int main() {\n"
      "  Context dummy = {0};\n"
      "  __entry(dummy);\n"
      "}\n"
      ));
#endif
    
    if (sb_count(p->errors) == 0) {
      bytecode_print(&emitter);
      __debugbreak();
      //bytecode_run(arena, &emitter);
      //builder_to_file(const_string("code\\out.c"), emitter.builder);
    }
  }
  
  if (sb_count(p->errors)) {
    
  } else {
    char compiler_flags[] = "-Od -MTd -nologo -Oi -GR- -EHa- -WX -W4 -wd4101 -wd4702 -wd4005 -wd4505 -wd4456 -wd4201 -wd4100 -wd4189 -wd4204 -wd4459 -Zi -FC -I\"C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.21.27702\\include\" -I\"C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.17763.0\\ucrt\" -I\"C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.17763.0\\um\" -I\"C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.17763.0\\shared\"";
    
    char linker_flags[] = "-incremental:no -opt:ref OpenGL32.lib Winmm.lib user32.lib Gdi32.lib /LIBPATH:\"C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.21.27702\\lib\\x64\" /LIBPATH:\"C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.17763.0\\um\\x64\" /LIBPATH:\"C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.17763.0\\ucrt\\x64\"";
    
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
  
  getchar();
  return 0;
}
