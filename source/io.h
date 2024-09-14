#pragma once

#include "definitions.h"


s64 to_s64(String str);
u64 to_u64(String str);

String convert_signed_to_string(u8 *buffer, s32 buffer_size, s64 signed_number, s32 base = 10, b32 uppercase = false, b32 keep_sign = false);
String convert_unsigned_to_string(u8 *buffer, s32 buffer_size, u64 number, s32 base = 10, b32 uppercase = false);
String convert_double_to_string(u8 *buffer, s32 size, r64 number, s32 precision = 6, b32 scientific = false, b32 hex = false, b32 uppercase = false, b32 keep_sign = false);
void convert_to_ptr_string(u8 *buffer, s32 buffer_size, void *address);

s64 print(char const *fmt, ...);
s64 format(struct StringBuilder *builder, char const *fmt, ...);
s64 format(struct StringBuilder *builder, char const *fmt, va_list args);
s64 format(struct PlatformFile *file, char const *fmt, ...);
s64 format(struct PlatformFile *file, char const *fmt, va_list args);

String format(char const *fmt, ...);
String t_format(char const *fmt, ...);

