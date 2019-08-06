#ifndef LVL5_RANDOM_H

#include "lvl5_math.h"

// NOTE(lvl5): random numbers
/*
this is xoroshiro180+
*/
u64 rotl(const u64 x, i32 k) {
  return (x << k) | (x >> (64 - k));
}

typedef struct {
  u64 seed[2];
} Rand;

Rand make_random_sequence(u64 seed) {
  Rand result;
  result.seed[0] = seed;
  result.seed[1] = seed*seed;
  return result;
}

u64 random_u64(Rand *s) {
  const u64 s0 = s->seed[0];
  u64 s1 = s->seed[1];
  const u64 result = s0 + s1;
  
  s1 ^= s0;
  s->seed[0] = rotl(s0, 24) ^ s1 ^ (s1 << 16); // a, b
  s->seed[1] = rotl(s1, 37); // c
  
  return result;
}


f32 random(Rand *s) {
  u64 r_u64 = random_u64(s);
  f32 result = (f32)r_u64/(f32)U64_MAX;
  return result;
}

f32 random_bilateral(Rand *s) {
  f32 r = random(s);
  f32 result = r*2 - 1.0f;
  return result;
}

f32 random_range(Rand *s, f32 min, f32 max) {
  f32 r = random(s);
  f32 range = max - min;
  f32 result = r*range + min;
  return result;
}

i32 random_range_i32(Rand *s, i32 min, i32 max) {
  f32 r = random_range(s, (f32)min, (f32)max);
  i32 result = (i32)roundf(r);
  return result;
}


#define LVL5_RANDOM_H
#endif