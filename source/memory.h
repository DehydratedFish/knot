#ifndef INCLUDE_GUARD_MEMORY_H
#define INCLUDE_GUARD_MEMORY_H

#include "definitions.h"


#define INIT_STRUCT(ptr) zero_memory(ptr, sizeof(*ptr))

#define ALLOCATE_ARRAY(type, size) get_memory_context()->allocate(0, sizeof(type) * size)
#define REALLOCATE(ptr, size) get_memory_context()->allocate((ptr), (size))
#define DEALLOCATE(ptr) get_memory_context()->allocate((ptr), 0)

#define ARENA_ALLOCATE(arena, type) (type*)memory_arena_allocate(arena, sizeof(type))
#define ARENA_ALLOCATE_ARRAY(arena, type, size) (type*)memory_arena_allocate(arena, sizeof(type) * (size))


typedef void*(AllocatorFunc)(void*, u64, void*);
struct Allocator {
	AllocatorFunc *allocate;
	void *data;
};

struct MemoryContext {
	Allocator allocator;
};

MemoryContext *get_memory_context();

void *allocate(u64 bytes);
#define ALLOCATE(type) (type*)allocate(sizeof(type));

inline void *allocate_memory(Allocator allocator, u64 bytes) {
	return allocator.allocate(0, bytes, allocator.data);
}
inline void *reallocate_memory(Allocator allocator, void *ptr, u64 bytes) {
	return allocator.allocate(ptr, bytes, allocator.data);
}
inline void deallocate_memory(Allocator allocator, void *ptr) {
	allocator.allocate(ptr, 0, allocator.data);
}

void zero_memory(void *data, u64 size);
void copy_memory(void *dest, void *src, u64 size);
void copy_memory_reverse(void *dest, void *src, u64 size);
void move_memory(void *dest, void *src, u64 size);

#endif // INCLUDE_GUARD_MEMORY_H

