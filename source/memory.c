#include "memory.h"

#include "utf.h"


b32 compare_memory(void *lhs, void *rhs, u64 length) {
	u8 *l = lhs;
	u8 *r = rhs;

	for (s32 i = 0; i < length; i += 1) {
		if (l[i] != r[i]) {
			return false;
		}
	}
	return true;
}


u16 read_le_u16(u8 *data) {
	return data[0] | (data[1] << 8);
}

u32 read_le_u32(u8 *data) {
	return data[0] | (data[1] << 8) | (data[2] << 16) | (data[3] << 24);
}

u64 read_le_u64(u8 *data) {
	return data[0] | (data[1] << 8) | (data[2] << 16) | (data[3] << 24) | ((u64)data[4] << 32) | ((u64)data[5] << 40) | ((u64)data[6] << 48) | ((u64)data[7] << 56);
}

r32 read_le_r32(u8 *data) {
	u32 tmp = read_le_u32(data);

	return *(r32*)&tmp;
}

r64 read_le_r64(u8 *data) {
	u64 tmp = read_le_u32(data);

	return *(r64*)&tmp;
}

MemoryChunk memory_chunk_advance(MemoryChunk *chunk, u64 size) {
	if (chunk->size < size) size = chunk->size;

	MemoryChunk skipped = {chunk->data, size};

	chunk->data += size;
	chunk->size -= size;

	return skipped;
}

b32 memory_chunk_validate_bounds(MemoryChunk *chunk, u64 offset, u64 size) {
	if (chunk->size < offset) return false;
	if (chunk->size < offset + size) return false;

	return true;
}

u64 memory_chunk_extract(MemoryChunk *chunk, void *buffer, u64 size) {
	if (chunk->size < size) {
		size = chunk->size;
	}

	copy_memory(buffer, chunk->data, size);
	chunk->data += size;
	chunk->size -= size;

	return size;
}

String memory_chunk_extract_until(MemoryChunk *chunk, u8 delim) {
	String ref = {chunk->data, 0};

	for (; ref.size < chunk->size; ref.size += 1) {
		if (chunk->data[ref.size] == delim) break;
	}

	memory_chunk_advance(chunk, ref.size + 1);

	return ref;
}

b32 memory_chunk_extract_cp(MemoryChunk *chunk, u32 *cp) {
	if (chunk->size < 1) return false;

	s32 size = utf8_length(chunk->data[0]);
	u8 buffer[4];
	if (memory_chunk_extract(chunk, buffer, size) != size) return false;

	*cp = utf32_from_utf8(buffer);

	return true;
}

u64 memory_chunk_read(MemoryChunk *chunk, u64 pos, void *buffer, u64 size) {
	if (chunk->size < pos) return 0;

	u8 *data = chunk->data + pos;
	if (chunk->size < pos + size) size = chunk->size - pos;

	copy_memory(buffer, data, size);

	return size;
}

b32 memory_chunk_peek_cp(MemoryChunk *chunk, u32 *cp) {
	if (chunk->size < 1) return false;

	s32 size = utf8_length(chunk->data[0]);
	if (chunk->size < size) return false;

	u8 buffer[4];
	*cp = utf32_from_utf8(chunk->data);

	return true;
}

MemoryArena memory_arena_create(u32 size) {
	MemoryArena arena;
	arena.buffer = ALLOCATE_ARRAY(u8, size);
	arena.used = 0;
	arena.alloc = size;

	return arena;
}

void memory_arena_free(MemoryArena *arena) {
	DEALLOCATE(arena->buffer);
	arena->buffer = 0;
	arena->used = 0;
	arena->alloc = 0;
}

MemoryArena memory_arena_sub_arena(MemoryArena *arena, u32 size) {
	MemoryArena sub;
	sub.buffer = memory_arena_allocate(arena, size);

	if (sub.buffer == 0) die("Not enough memory in MemoryArena.");
	sub.used = 0;
	sub.alloc = size;

	return sub;
}

u8 *memory_arena_allocate(MemoryArena *arena, u32 size) {
	if (arena->alloc < arena->used + size) die("MemoryArena is out of memory.");

	u8 *result = (u8*)arena->buffer + arena->used;
	arena->used += size;

	return result;
}

ArenaString memory_arena_allocate_string(MemoryArena *arena, String string) {
	ArenaString result = {
		memory_arena_allocate(arena, string.size),
		string.size,
		string.hash
	};
	copy_memory(result.data, string.data, string.size);

	return result;
}

void memory_arena_reset(MemoryArena *arena) {
	arena->used = 0;
}

void zero_memory(void *data, u64 size) {
	u8 *ptr = data;
	for (u64 i = 0; i < size; i += 1) {
		ptr[i] = 0;
	}
}

