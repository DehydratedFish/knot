#pragma once

#include "memory.h"


inline bool equal(String lhs, String rhs) {
    if (lhs.size == rhs.size) {
        for (s64 i = 0; i < lhs.size; i += 1) {
            if (lhs[i] != rhs[i]) return false;
        }

        return true;
    }

    return false;
}

inline bool operator==(String lhs, String rhs) {
    return equal(lhs, rhs);
}

inline bool operator!=(String lhs, String rhs) {
    return !(equal(lhs, rhs));
}

inline bool contains(String str, String search) {
    for (s64 i = 0; i < str.size; i += 1) {
        if (i + search.size > str.size) return false;

        String tmp = {str.data + i, search.size};
        if (equal(tmp, search)) return true;
    }

    return false;
}

inline b32 starts_with(String str, String begin) {
    if (str.size < begin.size) return false;

    str.size = begin.size;
    return str == begin;
}

inline bool ends_with(String str, String search) {
    if (str.size < search.size) return false;

    str.data += str.size - search.size;
    str.size  = search.size;

    return str == search;
}

inline String shrink_front(String str, s64 amount = 1) {
#ifdef BOUNDS_CHECKING
    if (str.size < amount) die("String is too small to shrink.");
#endif
    str.data += amount;
    str.size -= amount;

    return str;
}

inline String shrink_back(String str, s64 amount = 1) {
#ifdef BOUNDS_CHECKING
    if (str.size < amount) die("String is too small to shrink.");
#endif
    str.size -= amount;

    return str;
}

inline String shrink(String str, s64 amount = 1) {
#ifdef BOUNDS_CHECKING
    if (str.size < amount * 2) die("String is too small to shrink.");
#endif
    return shrink_back(shrink_front(str, amount), amount);
}

inline String trim(String str) {
    s64 num_whitespaces = 0;

    for (s64 i = 0; i < str.size; i += 1) {
        if (str[i] > ' ') break;

        num_whitespaces += 1;
    }
    str.data += num_whitespaces;
    str.size -= num_whitespaces;

    num_whitespaces = 0;
    for (s64 i = str.size; i > 0; i -= 1) {
        if (str[i - 1] > ' ') break;

        num_whitespaces += 1;
    }
    str.size -= num_whitespaces;

    return str;
}

inline String sub_string(String buffer, s64 offset, s64 size) {
#ifdef BOUNDS_CHECKING
    if (buffer.size < offset + size) die("String read out of bounds.");
#endif
    String result = {buffer.data + offset, size};

    return result;
}

inline String allocate_string(s64 size, Allocator alloc = default_allocator()) {
    String result = {};
    result.data = ALLOC(alloc, u8, size);
    result.size = size;

    return result;
}

inline void destroy(String *str, Allocator alloc = default_allocator()) {
    DEALLOC(alloc, str->data, str->size);
    INIT_STRUCT(str);
}

inline String allocate_temp_string(s64 size) {
    String result = {};
    result.data = (u8*)allocate(temporary_allocator(), size);
    result.size = size;

    return result;
}

inline String allocate_string(String str, Allocator alloc = default_allocator()) {
    String result = allocate_string(str.size, alloc);

    copy_memory(result.data, str.data, str.size);

    return result;
}

inline String allocate_temp_string(String str) {
    String result = allocate_temp_string(str.size);

    copy_memory(result.data, str.data, str.size);

    return result;
}


#ifndef STRING_BUILDER_BLOCK_SIZE
#define STRING_BUILDER_BLOCK_SIZE KILOBYTES(4)
#endif

struct StringBuilderBlock {
    StringBuilderBlock *next;

    u8 buffer[STRING_BUILDER_BLOCK_SIZE];
    s64 used;
};
struct StringBuilder {
    Allocator allocator;

    StringBuilderBlock  first;
    StringBuilderBlock *current;

    s64 total_size;
};

inline void reset(StringBuilder *builder) {
    StringBuilderBlock *block = &builder->first;

    while (block) {
        block->used = 0;
        block = block->next;
    }

    builder->total_size = 0;
}

inline void append(StringBuilder *builder, u8 c, Allocator alloc = default_allocator()) {
    if (builder->current == 0) builder->current = &builder->first;
    if (builder->allocator.allocate == 0) builder->allocator = alloc;

    if (builder->current->used + 1 > STRING_BUILDER_BLOCK_SIZE) {
        if (builder->current->next == 0) {
            builder->current->next = ALLOC(builder->allocator, StringBuilderBlock, 1);
        } else {
            // NOTE: buffer not initialized to zero for speed
            builder->current->next->used = 0;
        }
        builder->current = builder->current->next;
    }

    builder->current->buffer[builder->current->used] = c;
    builder->current->used += 1;
    builder->total_size    += 1;
}

inline void append(StringBuilder *builder, String str, Allocator alloc = default_allocator()) {
    if (builder->current == 0) builder->current = &builder->first;
    if (builder->allocator.allocate == 0) builder->allocator = alloc;

    s64 space = STRING_BUILDER_BLOCK_SIZE - builder->current->used;
    while (space < str.size) {
        copy_memory(builder->current->buffer + builder->current->used, str.data, space);
        builder->current->used += space;
        builder->total_size    += space;
        str = shrink_front(str, space);

        if (builder->current->next == 0) {
            builder->current->next = ALLOC(builder->allocator, StringBuilderBlock, 1);
        } else {
            builder->current->next->used = 0;
        }
        builder->current = builder->current->next;

        space = STRING_BUILDER_BLOCK_SIZE;
    }

    copy_memory(builder->current->buffer + builder->current->used, str.data, str.size);
    builder->current->used += str.size;
    builder->total_size    += str.size;
}

inline void append_raw(StringBuilder *builder, void *buffer, s64 size, Allocator alloc = default_allocator()) {
    String str = {(u8*)buffer, size};
    append(builder, str, alloc);
}
#define APPEND_RAW(builder, value) append_raw((builder), (void*)&(value), sizeof(value))

inline void destroy(StringBuilder *builder) {
    StringBuilderBlock *next = builder->first.next;

    while (next) {
        StringBuilderBlock *tmp = next->next;
        DEALLOC(builder->allocator, next, 1);

        next = tmp;
    }
}

inline String to_allocated_string(StringBuilder *builder, Allocator alloc = default_allocator()) {
    String result = allocate_string(builder->total_size, alloc);

    s64 size = 0;
    StringBuilderBlock *block = &builder->first;
    while (block) {
        copy_memory(result.data + size, block->buffer, block->used);
        size += block->used;

        block = block->next;
    }

    return result;
}

inline String temp_string(StringBuilder *builder) {
    assert(builder->total_size < STRING_BUILDER_BLOCK_SIZE);

    return {builder->first.buffer, builder->first.used};
}

