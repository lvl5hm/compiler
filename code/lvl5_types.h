#ifndef LVL5_TYPES

typedef char i8;
typedef short i16;
typedef int i32;
typedef long long i64;

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

typedef float f32;
typedef double f64;

typedef i32 b32;
typedef u8 byte;

#define false 0
#define true 1
#define null 0

#define kilobytes(n) (1024LL*(n))
#define megabytes(n) (1024LL*kilobytes(n))
#define gigabytes(n) (1024LL*megabytes(n))
#define terabytes(n) (1024LL*gigabytes(n))

#ifdef LVL5_DEBUG
#define assert(expr) if (!(expr)) *(i32 *)0 = 0xFF;
#else
#define assert(expr)
#endif

#define align_pow_2(value, align) (((value) + (align)-1) & ~((align)-1))

#define array_count(arr) (sizeof(arr)/sizeof(*(arr)))

#define offsetof(T, m) ((u64)(void *)&(((T *)0)->m))


#define LVL5_TYPES
#endif