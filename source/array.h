#ifndef INCLUDE_GUARD_ARRAY_H
#define INCLUDE_GUARD_ARRAY_H

#include "memory.h"

s32 print(char const *, ...);


s32 const ArrayDefaultSize = 16;
r32 const ArrayGrowFactor = 2.0f;

template<typename Type>
struct Array {
	Allocator allocator;

	Type *data;
	s32 size;
	s32 alloc;

	Array() {
		allocator = get_memory_context()->allocator;

		data = 0;
		size = 0;
		alloc = 0;
	}

	Array(Array const &array) {
		allocator = array.allocator;

		data = array.data;
		size = array.size;
		alloc = 0;
	}

	Array(Array &&array) {
		allocator = array.allocator;

		data = array.data;
		size = array.size;
		alloc = array.alloc;

		array.alloc = 0;
	}

	~Array() {
		if (alloc) {
			for (s32 i = 0; i < size; i += 1) {
				data[i].Type::~Type();
			}

			allocator.allocate(data, 0, allocator.data);
			data = 0;
			size = 0;
			alloc = 0;
		}
	}

	Type &operator[](s32 index) {
		BOUNDS_CHECK(0, size - 1, index, "Array<> indexing out of bounds");

		return data[index];
	}
};


template<typename Type>
void array_alloc(Array<Type> *array, s32 size) {
	array->data = (Type*)allocate_memory(array->allocator, size * sizeof(Type));
	array->size = 0;
	array->alloc = size;
}

template<typename Type>
void append(Array<Type> *array, Type const &elem) {
	if (array->alloc == 0) {
		if (array->size == 0) {
			array->allocator = get_memory_context()->allocator;

			array_alloc(array, ArrayDefaultSize);
			array->data[array->size] = elem;
			array->size += 1;

			return;
		} else {
			Type *tmp = array->data;
			array->alloc = (s32)(array->size * ArrayGrowFactor);
			array->data = (Type*)allocate_memory(array->allocator, array->alloc * sizeof(Type));

			for (s32 i = 0; i < array->size; i += 1) array->data[i] = tmp[i];
		}
	}

	if (array->alloc < array->size + 1) {
		array->alloc = (s32)(array->alloc * ArrayGrowFactor);
		array->data = (Type*)reallocate_memory(array->allocator, array->data, array->alloc * sizeof(Type));
	}

	array->data[array->size] = elem;
	array->size += 1;
}


#endif // INCLUDE_GUARD_ARRAY_H

