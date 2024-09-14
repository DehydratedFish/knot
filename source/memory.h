#pragma once

#include "definitions.h"
#include <initializer_list>
#include <cstddef>


INTERNAL void *allocate(Allocator allocator, s64 bytes) {
    if (bytes == 0) return 0;

    return allocator.allocate(allocator.data, bytes, 0, 0);
}
INTERNAL void *reallocate(Allocator allocator, s64 bytes, void *old, s64 old_bytes) {
    return allocator.allocate(allocator.data, bytes, old, old_bytes);
}
INTERNAL void deallocate(Allocator allocator, void *ptr, s64 old_size) {
    if (!ptr) return;

    allocator.allocate(allocator.data, 0, ptr, old_size);
}

#define ALLOC(alloc, type, count)  (type*)allocate((alloc), (count) * sizeof(type))
#define REALLOC(alloc, ptr, old_count, new_count) (decltype(ptr))reallocate((alloc), (new_count) * sizeof(*ptr), (ptr), (old_count) * sizeof(*ptr))
#define DEALLOC(alloc, ptr, count) deallocate((alloc), (ptr), (count) * sizeof(*ptr))

#define ALLOCATE_ARRAY(type, size) {ALLOC(default_allocator(), type, (size)), (size)}
#define DESTROY_ARRAY(arr) DEALLOC(default_allocator(), (arr).data, (arr).size)


Allocator default_allocator();

Allocator temporary_allocator();
s64  temporary_storage_mark();
void temporary_storage_rewind(s64 mark);
void reset_temporary_storage();


#define INIT_STRUCT(ptr) zero_memory(ptr, sizeof(*ptr))

inline void zero_memory(void *data, u64 bytes) {
    u8 *tmp = (u8*)data;
    for (u64 i = 0; i < bytes; i += 1) {
        tmp[i] = 0;
    }
}

inline void copy_memory(void *dest, void const *src, u64 size) {
    u8 *d = (u8*)dest;
    u8 *s = (u8*)src;

    if (d < s) {
        for (u64 i = 0; i < size; i += 1) {
            d[i] = s[i];
        }
    } else {
        for (u64 i = size; i > 0; i -= 1) {
            d[i - 1] = s[i - 1];
        }
    }
}



struct MemoryBuffer {
    Allocator allocator;

    u8 *memory;
    s64 used;
    s64 alloc;
};

inline MemoryBuffer allocate_memory_buffer(Allocator alloc, s64 size) {
    MemoryBuffer buffer = {};
    buffer.allocator = alloc;
    buffer.memory = ALLOC(alloc, u8, size);
    buffer.used   = 0;
    buffer.alloc  = size;

    return buffer;
}

inline void free_memory_buffer(MemoryBuffer *buffer) {
    DEALLOC(buffer->allocator, buffer->memory, buffer->alloc);

    INIT_STRUCT(buffer);
}

template<typename Type>
void destroy(Array<Type> array, Allocator alloc = default_allocator()) {
    DEALLOC(alloc, array.data, array.size);
}

template<typename Type, typename Prev>
Array<Type> cast_array(Array<Prev> array) {
    Array<Type> new_array = {};
    new_array.data = (Type*)array.data;
    new_array.size = array.size;

    return new_array;
}


template<typename Type>
void init(DArray<Type> &array, s64 size, Allocator alloc = default_allocator()) {
    if (array.allocator.allocate == 0) array.allocator = alloc;

    if (array.alloc < size) {
        array.memory = REALLOC(array.allocator, array.memory, array.alloc, size);
        array.alloc  = size;
    }
    array.size = 0;
}

template<typename Type>
void init(DArray<Type> &array, Type *buffer, s64 size, Allocator alloc = default_allocator()) {
    prealloc(array, size, alloc);

    copy_memory(array.memory, buffer, size * sizeof(Type));
}

template<typename Type>
void prealloc(DArray<Type> &array, s64 size, Allocator alloc = default_allocator()) {
    init(array, size, alloc);
    array.size = size;
}

template<typename Type>
void ensure_space(DArray<Type> &array, s64 size, Allocator alloc = default_allocator()) {
    if (array.allocator.allocate == 0) array.allocator = alloc;

    s64 total_size = array.size + size;
    if (total_size > array.alloc) {
        s64 new_alloc = array.size * 2;
        if (total_size > array.alloc) array.alloc = total_size;

        array.memory = REALLOC(array.allocator, array.memory, array.alloc, new_alloc);
        array.alloc  = new_alloc;
    }
}

template<typename Type>
Type *last(DArray<Type> &array) {
    if (array.size) return &array[array.size - 1];

    return 0;
}

template<typename Type>
Type *append(DArray<Type> &array, Type element, Allocator alloc = default_allocator()) {
    if (array.allocator.allocate == 0) array.allocator = alloc;

    if (array.alloc == 0) {
        u32 const default_size = 4;
        array.memory = ALLOC(array.allocator, Type, default_size);
        array.alloc = default_size;
    } else if (array.size == array.alloc) {
        s64 new_alloc = array.alloc * 2;
        array.memory  = REALLOC(array.allocator, array.memory, array.alloc, new_alloc);
        array.alloc   = new_alloc;
    }

    array.memory[array.size] = element;
    array.size += 1;

    return &array.memory[array.size - 1];
}

template<typename Type>
void append(DArray<Type> &array, Type *elements, s64 count, Allocator alloc = default_allocator()) {
    if (array.allocator.allocate == 0) array.allocator = alloc;

    s64 space = array.alloc - array.size;
    if (space < count) {
        array.memory = REALLOC(array.allocator, array.memory, array.alloc, array.size + count);
        array.alloc  = array.size + count;
    }

    copy_memory(array.memory + array.size, elements, count * sizeof(Type));
    array.size += count;
}

template<typename Type>
Type *insert(DArray<Type> &array, s64 index, Type element, Allocator alloc = default_allocator()) {
	BOUNDS_CHECK(0, array.size, index, "Array insertion out of bounds.");

    if (array.allocator.allocate == 0) array.allocator = alloc;

    if (array.alloc == 0) {
        u32 const default_size = 4;
        array.memory = ALLOC(array.allocator, Type, default_size);
        array.alloc = default_size;
    } else if (array.size == array.alloc) {
        s64 new_alloc = array.alloc * 2;
        array.memory  = REALLOC(array.allocator, array.memory, array.alloc, new_alloc);
        array.alloc   = new_alloc;
    }

    copy_memory(array.memory + index + 1, array.memory + index, (array.size - index) * sizeof(Type));
    array.memory[index] = element;
    array.size += 1;

    return &array.memory[index];
}

template<typename Type>
void insert(DArray<Type> &array, s64 index, Type *elements, s64 count, Allocator alloc = default_allocator()) {
	BOUNDS_CHECK(0, array.size, index, "Array insertion out of bounds.");

    if (array.allocator.allocate == 0) array.allocator = alloc;

    if (array.alloc == 0) {
        array.memory = ALLOC(array.allocator, Type, count);
        array.alloc = count;
    } else if (array.size + count > array.alloc) {
        s64 new_alloc = array.alloc * 2;
        if (new_alloc < array.size + count) new_alloc = array.size + count;
        array.memory  = REALLOC(array.allocator, array.memory, array.alloc, new_alloc);
        array.alloc   = new_alloc;
    }

    copy_memory(array.memory + index, array.memory + index + count, (array.size - index) * sizeof(Type));
    copy_memory(array.memory + index, elements, count * sizeof(Type));
    array.size += count;
}

// NOTE: does not preserve order
template<typename Type>
void remove(DArray<Type> &array, s64 index) {
    if (array.size == 0) return;

    BOUNDS_CHECK(0, array.size - 1, index, "Array remove out of bounds.");

    array.size -= 1;
    array.memory[index] = array.memory[array.size]; // NOTE: Skipping bounds check with .memory.
}

template<typename Type>
void stable_remove(DArray<Type> &array, s64 index, s64 count = 1) {
    if (array.size == 0 || count == 0) return;

    BOUNDS_CHECK(0, array.size - 1, index, "Array remove out of bounds.");

    if (count > array.size) count = array.size;

    array.size -= count;
    copy_memory(array.memory + index, array.memory + index + count, (array.size - index) * sizeof(Type));
}

template<typename Type>
Array<Type> allocate_array(s64 size, Allocator alloc = default_allocator()) {
    Array<Type> result = {};
    result.data = ALLOC(alloc, Type, size);
    result.size = size;

    return result;
}

// TODO: This is too much C++ness for my taste.
//       If it is convenient enough I will keep it though.
template<typename Type>
INTERNAL Array<Type> make_array(std::initializer_list<Type> list) {
    Array<Type> result = allocate_array<Type>(list.size());

    Type *it = result.data;
    for (auto elem: list) {
        *it = elem;

        it += 1;
    }

    return result;
}


template<typename Type>
Array<Type> allocate_array(DArray<Type> &array) {
    Array<Type> result = ALLOCATE_ARRAY(Type, array.size);
    copy_memory(result.data, array.memory, array.size * sizeof(Type));

    return result;
}

template<typename Type>
void destroy(DArray<Type> &array) {
    if (array.alloc == 0) return;

    assert(array.allocator.allocate);

    deallocate(array.allocator, array.memory, array.alloc * sizeof(Type));

    array.memory = 0;
    array.size   = 0;
    array.alloc  = 0;
}


struct MemoryArena {
    Allocator allocator;
    u8 *memory;
    s64 used;
    s64 alloc;

    s64 all_time_high;
};

inline void destroy(MemoryArena *arena) {
    DEALLOC(arena->allocator, arena->memory, arena->alloc);

    INIT_STRUCT(arena);
}

inline MemoryArena allocate_arena(s64 size) {
    MemoryArena arena = {};
    arena.allocator  = default_allocator();
    arena.memory = ALLOC(arena.allocator, u8, size);
    arena.alloc  = size;

    return arena;
}

inline void *allocate_from_arena(MemoryArena *arena, s64 size, void *old, s64 old_size) {
    u32 const alignment = alignof(void*);

    u8 *current_ptr = arena->memory + arena->used;

    if (old) {
        if (size) {
            if (current_ptr - old_size == old) {
                s64 additional_size = size - old_size;
                if (arena->used + additional_size > arena->alloc) die("Could not allocate from arena.");

                arena->used += additional_size;
                
                if (arena->used > arena->all_time_high) arena->all_time_high = arena->used;
                return old;
            } else {
                s64 space = arena->alloc - arena->used;

#pragma warning( suppress : 4146 )
                s64 padding = -(u64)current_ptr & (alignment - 1);

                if (size > (space - padding)) die("Could not allocate from arena.");

                void *result = arena->memory + padding + arena->used;
                zero_memory(result, size);
                arena->used += padding + size;

                copy_memory(result, old, old_size);

                if (arena->used > arena->all_time_high) arena->all_time_high = arena->used;
                return result;
            }
        } else {
            if (current_ptr - old_size == old) {
                arena->used -= old_size;
            }

            return 0;
        }
    }

    s64 space = arena->alloc - arena->used;

#pragma warning( suppress : 4146 )
    s64 padding = -(u64)current_ptr & (alignment - 1);

    if (size > (space - padding)) die("Could not allocate from arena.");

    void *result = arena->memory + padding + arena->used;
    zero_memory(result, size);
    arena->used += padding + size;

    if (arena->used > arena->all_time_high) arena->all_time_high = arena->used;
    return result;
}

inline Allocator make_arena_allocator(MemoryArena *arena) {
    return {(AllocatorFunc*)allocate_from_arena, arena};
}

