#include "c_emitter.c"
#include "time.h"

/*
TODO:
[ ] redefinition should be an error

[ ] import external c functions
[ ] fixed length arrays
[ ] dynamic arrays
[ ] array views

[ ] default initialization for all variables
[ ] sizeof
[ ] functions should be emitted as pointers except for constant declarations
[ ] decide what casts are allowed
[ ] lvalues, rvalues, assignment, referencing and dereferencing
[ ] typedefs and functions inside functions
[ ] better for loop
-iterate arrays
-iterate enums
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
  clock_t front_start = clock();
  
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
  builtin_f32 = add_default_type(p, global_scope, const_string("f32"));
  builtin_f64 = add_default_type(p, global_scope, const_string("f64"));
  builtin_void = add_default_type(p, global_scope, const_string("void"));
  builtin_voidptr = (Code_Type *)code_type_pointer(p, builtin_void);
  scope_add(global_scope, code_stmt_decl(p, const_string("char"), builtin_Type, (Code_Node *)builtin_i8, true));
  
  
  Parse_Result parse_result = parse(p, src, tokens);
  if (parse_result.success) {
    Resolver res = {0};
    res.top_decls = parse_result.decls;
    res.arena = arena;
    res.parser = p;
    
    Emitter emitter = {0};
    emitter.indent = 0;
    emitter.res = &res;
    emitter.builder = arena_push_struct(arena, String_Builder);
    builder_init(emitter.builder, arena);
    
    res.emitter = &emitter;
    
    // NOTE(lvl5): c header
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
    
    resolve_decls(res, parse_result, global_scope);
    
    emit_string(&emitter, const_string(
      "int main() {\n"
      "  Context dummy = {0};\n"
      "  __entry(dummy);\n"
      "}\n"
      ));
    
    if (sb_count(p->errors) == 0) {
      builder_to_file(const_string("code\\out.c"), emitter.builder);
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
