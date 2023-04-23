#ifndef INCLUDE_GUARD_KNOT_IO_H
#define INCLUDE_GUARD_KNOT_IO_H

#include "string2.h"


s64 to_s64(String str);
u64 to_u64(String str);

String convert_signed_to_string(u8 *buffer, s32 buffer_size, s64 number, s32 base, b32 uppercase, b32 keep_sign);
s32 convert_unsigned_to_string(u8 *buffer, s32 buffer_size, u64 number, s32 base, b32 uppercase);
void convert_to_ptr_string(u8 *buffer, s32 buffer_size, void *address);

#endif // INCLUDE_GUARD_KNOT_IO_H

