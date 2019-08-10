#define NULL 0
typedef char i8;
typedef short i16;
typedef int i32;
typedef long long i64;
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

typedef i32 b32;
typedef i8 b8;
typedef u8 byte;
typedef enum {
  Alloc_Op_ALLOC,
  Alloc_Op_REALLOC,
  Alloc_Op_FREE,
  Alloc_Op_FREE_ALL,
} Alloc_Op;

typedef struct Context Context;

typedef byte* (*Allocator)(u64 size, Alloc_Op op, void* allocator_data, void* old_ptr, u64 old_size, u64 align, Context ctx);
typedef struct {
  byte* data;
  u64 capacity;
  u64 size;
} Arena;

struct Context {
  Allocator allocator;
  void* allocator_data;
  Arena scratch;
};

Arena arena_make(void* data, u64 capacity, Context ctx) {
  Arena result;
  result.data = (byte*)(data);
  result.capacity = capacity;
  return result;
}

void* alloc(u64 size, Context ctx) {
  byte* result = ctx.allocator(size, Alloc_Op_ALLOC, ctx.allocator_data, NULL, 0, 4, ctx);
  return result;
}

typedef struct {
  char* data;
  u64 count;
} string;

void memory_copy(void* dst, void* src, u64 size, Context ctx) {
  for (u64 i = 0; (i < size); i += 1) {
    ((byte*)(dst))[i] = ((byte*)(src))[i];
  }
}

byte* arena_allocator(u64 size, Alloc_Op op, void* allocator_data, void* old_ptr, u64 old_size, u64 align, Context ctx) {
  byte* result = NULL;
  Arena* arena = (Arena*)(allocator_data);
  if ((op == Alloc_Op_ALLOC)) {
    result = (arena->data + arena->size);
    arena->size += size;
  } else if ((op == Alloc_Op_FREE_ALL)) {
    arena->size = 0;
  } else if ((op == Alloc_Op_REALLOC)) {
    result = (byte*)(alloc(size, ctx));
    memory_copy(result, old_ptr, old_size, ctx);
  }
  return result;
}

extern void* malloc(u64 size, Context ctx);

extern void free(void* ptr, Context ctx);

extern void realloc(void* old_ptr, u64 new_size, Context ctx);

byte* system_allocator(u64 size, Alloc_Op op, void* allocator_data, void* old_ptr, u64 old_size, u64 align, Context ctx) {
  byte* result = NULL;
  if ((op == Alloc_Op_ALLOC)) {
    result = (byte*)(malloc(size, ctx));
  } else if ((op == Alloc_Op_FREE_ALL)) {
    free(old_ptr, ctx);
  } else if ((op == Alloc_Op_REALLOC)) {
    realloc(old_ptr, size, ctx);
  }
  return result;
}

string __string_make(char* data, u64 count, Context ctx) {
  string result;
  result.data = (char*)(alloc(count, ctx));
  memory_copy(result.data, data, count, ctx);
  result.count = count;
  return result;
}

extern i32 putchar(char c, Context ctx);

void print_string(string s, Context ctx) {
  for (u64 i = 0; (i < s.count); i += 1) {
    putchar((s.data)[i], ctx);
  }
}

void __main(Context ctx) {
  print_string(__string_make("hello world", 11, ctx), ctx);
}

void __entry(Context ctx) {
  ctx.allocator = system_allocator;
  __main(ctx);
}

int main() {
  Context dummy = {0};
  __entry(dummy);
}
