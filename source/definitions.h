#ifndef INCLUDE_GUARD_DEFINITIONS_H
#define INCLUDE_GUARD_DEFINITIONS_H

#include <cstdint>


#if defined(WIN32) || defined(_WIN32)
#define OS_WINDOWS
#elif defined(linux)
#define OS_LINUX
#else
#error "Platform not supported"
#endif


#define INTERNAL static

#define KILOBYTES(x) ((x) * 1024)
#define MEGABYTES(x) (KILOBYTES(x) * 1024)
#define GIGABYTES(x) (MEGABYTES(x) * 1024)

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

#define STRUCT_OFFSET(type, member) ((u64)&(((type*)0)->member))
#define STRUCT_OFFSET_PTR(type, member) (&(((type*)0)->member))

#define FOR_RANGE(var, start, end, step) for (s32 var = (start); var != end; var += (step))

typedef enum Ordering {
	CMP_LESSER  = -1,
	CMP_EQUAL   =  0,
	CMP_GREATER =  1
} Ordering;

#define PI 3.14159265358979

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef float  r32;
typedef double r64;

typedef s8  b8;
typedef s32 b32;


void fire_assert(char const *msg, char const *func, char const *file, int line); // NOTE: defined in platform.c
void die(const char *msg);


#ifdef DEVELOPER
#define assert(expr) (void)((expr) || (fire_assert(#expr, __func__, __FILE__, __LINE__),0))

#else
#define assert(expr)

#endif // DEVELOPER

#ifdef BOUNDS_CHECKING
#define BOUNDS_CHECK(low, high, index, msg) {if ((index) < (low) || (index) > (high)) die(msg);}

#else
#define BOUNDS_CHECK(low, high, index, msg)

#endif // BOUNDS_CHECKING

#endif // INCLUDE_GUARD_DEFINITIONS_H

