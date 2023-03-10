#ifndef INCLUDE_GUARD_STRING2_H
#define INCLUDE_GUARD_STRING2_H

#include "memory.h"


inline s32 c_string_length(char const *str) {
	if (str == 0) return 0;

	s32 size = 0;
	for (; str[size]; size += 1);

	return size;
}


r32 const StringGrowFactor = 2.0f;
struct String {
	Allocator allocator;

	u8 *data;
	s32 size;
	s32 alloc;


	String() {
		allocator = get_memory_context()->allocator;

		data = 0;
		size = 0;
		alloc = 0;
	}

	String(String const &string) {
		allocator = string.allocator;

		data = string.data;
		size = string.size;
		alloc = 0;
	}

	String(String &&string) {
		allocator = string.allocator;

		data = string.data;
		size = string.size;
		alloc = string.alloc;

		string.alloc = 0;
	}

	String(char const *str) {
		allocator = get_memory_context()->allocator;

		//NOTE: we never touch a reference and always copy before a change so this cast should be fine
		data = (u8*)str;
		size = c_string_length(str);
		alloc = 0;
	}

	String(u8 *str, s32 length) {
		allocator = get_memory_context()->allocator;

		data = str;
		size = length;
		alloc = 0;
	}

	~String() {
		if (alloc) {
			allocator.allocate(data, 0, allocator.data);
			data = 0;
			size = 0;
			alloc = 0;
		}
	}

	String &operator=(String const &string) {
		allocator = string.allocator;

		data = string.data;
		size = string.size;
		alloc = 0;

		return *this;
	}

	String &operator=(String &&string) {
		allocator = string.allocator;

		data = string.data;
		size = string.size;
		alloc = string.alloc;

		string.alloc = 0;

		return *this;
	}

	String &operator=(char const *str) {
		s32 length = c_string_length(str);
		if (alloc == 0) {
			data = (u8*)str;
			size = length;
		} else {
			if (alloc < length) {
				data = (u8*)reallocate_memory(allocator, data, length);
				alloc = length;
			}

			copy_memory(data, (u8*)str, length);
			size = length;
		}

		return *this;
	}
};


inline void prealloc(String *string, s32 size) {
	string->data = (u8*)allocate_memory(string->allocator, size);
	string->alloc = size;
}

inline void string_alloc(String *string, u8 *buffer, s32 length) {
	// TODO: check if it is necessary to deallocate first

	string->data = (u8*)allocate_memory(string->allocator, length);
	string->size = length;
	string->alloc = length;

	copy_memory(string->data, buffer, length);
}

inline void append(String *string, u8 *buffer, s32 length) {
	if (string->alloc == 0) {
		if (string->size == 0) {
			string_alloc(string, buffer, length);
			return;
		} else {
			u8 *tmp = string->data;
			s32 size = string->size;
			prealloc(string, size + length);
			copy_memory(string->data, tmp, size);
			string->size = size;
		}
	}

	s32 new_size = string->size + length;
	if (string->alloc < new_size) {
		s32 new_alloc = (s32)(string->alloc * StringGrowFactor);
		if (new_alloc < length) new_alloc = string->alloc + length;

		string->data = (u8*)reallocate_memory(string->allocator, string->data, new_alloc);
		string->alloc = new_alloc;
	}

	copy_memory(string->data + string->size, buffer, length);
	string->size += length;
}
inline void append(String *string, String ref) {
	append(string, ref.data, ref.size);
}

// TODO: utf8
inline void put(String *string, u8 c) {
	append(string, &c, 1);
}

template<typename Type>
inline bool get(String *string, Type *value) {
	assert(string->alloc == 0);

	s32 const size = sizeof(Type);

	if (string->size < size) return false;

	copy_memory(value, string->data, size);
	string->data += size;
	string->size -= size;

	return false;
}

inline String copy(String *string) {
	String result;
	append(&result, string->data, string->size);

	return result;
}

inline bool equal(String lhs, String rhs) {
	if (lhs.size == rhs.size) {
		for (s32 i = 0; i < lhs.size; i += 1) {
			if (lhs.data[i] != rhs.data[i]) return false;
		}

		return true;
	}

	return false;
}


// TODO: rename
s64 convert_string_to_s64(u8 *buffer, s32 buffer_size);
String convert_signed_to_string(u8 *buffer, s32 buffer_size, s64 number, s32 base, b32 uppercase, b32 keep_sign);
s32 convert_unsigned_to_string(u8 *buffer, s32 buffer_size, u64 number, s32 base, b32 uppercase);
void convert_to_ptr_string(u8 *buffer, s32 buffer_size, void *address);

#endif // INCLUDE_GUARD_STRING2_H

