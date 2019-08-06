#ifndef LVL5_STRETCHY_BUFFER
#define LVL5_STRETCHY_BUFFER_VERSION 0

#include "lvl5_arena.h"

typedef struct {
  u32 count;
  u32 capacity;
  
  Arena *arena;
} sb_Header;



#define __get_header(arr) ((sb_Header *)(arr)-1)
#define sb_count(arr) (__get_header(arr)->count)
#define sb_capacity(arr) (__get_header(arr)->capacity)

#define __need_grow(arr) (!arr || (sb_count(arr) == sb_capacity(arr)))

#define sb_new(arena, T, capacity) __sb_new(arena, capacity, sizeof(T))
void *__sb_new(Arena *arena, u32 capacity, u32 item_size) {
  sb_Header header = {0};
  header.capacity = capacity;
  header.arena = arena;
  header.count = 0;
  
  byte *memory = arena_push_size(arena, sizeof(sb_Header) + item_size*capacity);
  *(sb_Header *)memory = header;
  
  byte *result = memory + sizeof(sb_Header);
  return result;
}

#define sb_push(arr, item) __need_grow(arr) ? __grow(&(arr), sizeof(item)) : 0, (arr)[sb_count(arr)++] = item

#define sb_peek(arr) (&((arr)[sb_count(arr)-1]))

void *__grow(void *arr_ptr_, u64 item_size) {
  void **arr_ptr = (void **)arr_ptr_;
  void *arr = *arr_ptr;
  
  assert(arr);
  sb_Header *header = __get_header(arr);
  assert(header->arena);
  
  i32 new_capacity = header->capacity*2;
  
  u32 header_size = sizeof(sb_Header);
  void *data = arena_push_size(header->arena, 
                               new_capacity*item_size + header_size) + header_size;
  copy_memory_slow(data, arr, header->capacity*item_size);
  
  sb_Header *new_header = __get_header(data);
  new_header->arena = header->arena;
  new_header->count = header->count;
  new_header->capacity = new_capacity;
  
  *arr_ptr = data;
  
  return 0;
}

#define LVL5_STRETCHY_BUFFER
#endif