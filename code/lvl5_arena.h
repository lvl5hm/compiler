#ifndef LVL5_ARENA
#define LVL5_ARENA_VERSION 0

#include "lvl5_types.h"

typedef struct {
  byte *data;
  u64 size;
  u64 capacity;
  
#ifdef LVL5_DEBUG
  u32 marks_taken;
#endif
} Arena;


void copy_memory_slow(void *dst, void *src, u64 size) {
  for (u64 i = 0; i < size; i++) {
    ((byte *)dst)[i] = ((byte *)src)[i];
  }
}

void zero_memory_slow(void *dst, u64 size) {
  for (u64 i = 0; i < size; i++) {
    ((byte *)dst)[i] = 0;
  }
} 

void arena_init(Arena *arena, void *data, u64 capacity) {
  arena->data = (byte *)data;
  arena->capacity = capacity;
  arena->size = 0;
}

#define __DEFAULT_ALIGN 16

#define arena_push_array(arena, T, count) \
(T *)arena_push_memory(arena, sizeof(T)*count, __DEFAULT_ALIGN)

#define arena_push_struct(arena, T) \
(T *)arena_push_memory(arena, sizeof(T), __DEFAULT_ALIGN)

#define arena_push_size(arena, size) \
arena_push_memory(arena, size, __DEFAULT_ALIGN)

byte *arena_push_memory(Arena *arena, u64 size, u64 align) {
  byte *result = 0;
  assert(arena->size + size <= arena->capacity);
  
  u64 data_u64 = (u64)(arena->data + arena->size);
  u64 data_u64_aligned = align_pow_2(data_u64, align);
  result = (byte *)data_u64_aligned;
  arena->size += align_pow_2(size, align);
  
  return result;
}

u64 arena_get_mark(Arena *arena) {
  u64 result = arena->size;
  
#ifdef LVL5_DEBUG
  arena->marks_taken++;
#endif
  
  return result;
}

void arena_set_mark(Arena *arena, u64 mark) {
  assert(mark <= arena->capacity);
  arena->size = mark;
  
#ifdef LVL5_DEBUG
  arena->marks_taken--;
#endif
}

void arena_check_no_marks(Arena *arena) {
#ifdef LVL5_DEBUG
  assert(arena->marks_taken == 0);
#endif
}

void arena_init_subarena(Arena *parent, Arena *child, u64 capacity) {
  byte *child_memory = arena_push_array(parent, byte, capacity);
  arena_init(child, child_memory, capacity);
}

#define LVL5_ARENA
#endif