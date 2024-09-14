#pragma once

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
#define FOR(collection, it) for (auto it = begin(collection); it < end(collection); it += 1)
#define FOR_INDEX(collection, it) ((it) - begin(collection))

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

struct V2i {
    union {
        struct {
            s32 x;
            s32 y;
        };
        struct {
            s32 width;
            s32 height;
        };
    };
};

void fire_assert(char const *msg, char const *func, char const *file, int line);
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


typedef void*(AllocatorFunc)(void *data, s64 size, void *old, s64 old_size);
struct Allocator {
    AllocatorFunc *allocate;
    void *data;
};

inline constexpr s64 c_string_length(char const *str) {
    if (str == 0) return 0;

    s64 size = 0;
    for (; str[size]; size += 1);

    return size;
}

struct String {
    u8 *data;
    s64 size;

    String() = default;
    String(u8 *buffer, s64 length) {
        data = buffer;
        size = length;
    }

    //NOTE: we never touch a reference and always copy before a change so this cast should be fine
    constexpr String(char const *str)
    : data((u8*)str), size(c_string_length(str)) {}

    String &operator=(char const *str) {
        data = (u8*)str;
        size = c_string_length(str);

        return *this;
    }

    u8 &operator[](s64 index) {
        BOUNDS_CHECK(0, size - 1, index, "String indexing out of bounds");

        return data[index];
    }

};

template<typename Type>
struct Array {
    Type *data;
    s64   size;

    Type &operator[](s64 index) {
        BOUNDS_CHECK(0, size - 1, index, "Array indexing out of bounds");

        return data[index];
    }

    Type const &operator[](s64 index) const {
        BOUNDS_CHECK(0, size - 1, index, "Array indexing out of bounds");

        return data[index];
    }
};

template<typename Type>
Type *begin(Array<Type> array) {
    return array.data;
}

template<typename Type>
Type *end(Array<Type> array) {
    return array.data + array.size;
}


template<typename Type>
struct DArray {
    Allocator allocator;

    Type *memory;
    s64   size;
    s64   alloc;

    Type &operator[](s64 index) {
        BOUNDS_CHECK(0, size - 1, index, "Array indexing out of bounds");

        return memory[index];
    }

    Type const &operator[](s64 index) const {
        BOUNDS_CHECK(0, size - 1, index, "Array indexing out of bounds");

        return memory[index];
    }

    operator Array<Type>() {
        return {memory, size};
    }
};

template<typename Type>
Type *begin(DArray<Type> array) {
    return array.data;
}

template<typename Type>
Type *end(DArray<Type> array) {
    return array.data + array.size;
}


template<typename Functor>
struct DeferGuardBase {
    Functor functor;

    DeferGuardBase(Functor f): functor(f) {}
    ~DeferGuardBase() {functor();}
};

template<typename Functor>
DeferGuardBase<Functor> make_defer_guard_base(Functor f) {return f;}

#define CONCAT(l, r) CONCAT2(l, r)
#define CONCAT2(l, r) l ## r

#define UNIQUE_NAME(base) CONCAT(base, __COUNTER__)

#define DEFER(stuff) DEFER2(UNIQUE_NAME(DeferGuardUniqueName), stuff)
#define DEFER2(name, stuff) auto name = make_defer_guard_base([&](){stuff;});

