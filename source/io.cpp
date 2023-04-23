#include "io.h"


INTERNAL u8 CharacterLookup[] = "0123456789abcdefghijklmnopqrstuvwxyz";
INTERNAL u8 CharacterLookupUppercase[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

s64 to_s64(String str) {
	s64 result = 0;
	b32 is_negative = false;

	if (str.size == 0) {
		return result;
	}

	if (str[0] == '-') {
		is_negative = true;
		str.data += 1;
		str.size -= 1;
	}

	for (s32 i = 0; i < str.size; i += 1) {
		result *= 10;
		result += str[i] - '0';
	}

	if (is_negative) {
		result *= -1;
	}

	return result;
}

u64 to_u64(String str) {
	u64 result = 0;

	if (str.size == 0) {
		return result;
	}

	for (s32 i = 0; i < str.size; i += 1) {
		result *= 10;
		result += str[i] - '0';
	}

	return result;
}

