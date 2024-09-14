#pragma once

#include "definitions.h"


enum {
    GET_OK    = 0x00,
    GET_EMPTY = 0x01,
    GET_ERROR = 0x02,
};

struct String16 {
    u16 *data;
    s64  size;
};

struct String32 {
    u32 *data;
    s64  size;
};

String to_utf8(Allocator alloc, String16 string);
String to_utf8(Allocator alloc, String32 string);

String16 to_utf16(Allocator alloc, String string, b32 add_null_terminator = false);


struct UTF8Iterator {
    String string;

    s64 index;
    u32 utf8_size;
    u32 cp;
    b32 valid;
};

enum IterationStart {
    ITERATE_FROM_START,
    ITERATE_FROM_END
};
UTF8Iterator make_utf8_it(String string, IterationStart from = ITERATE_FROM_START);
void next(UTF8Iterator *it);
void prev(UTF8Iterator *it);

struct UTF8CharResult {
    u32 cp;
    u8  byte[4];
    s32 length;
    u32 status;
};
UTF8CharResult utf8_peek(String str);

UTF8CharResult to_utf8(u32 cp);


struct CodePointInfo {
    s64 index;
    u32 length;
};

struct UTF8String {
    DArray<u8> bytes;
    DArray<CodePointInfo> code_points;
};


void reset(UTF8String *string);

void append(UTF8String *string, u32 code_point);
void append(UTF8String *string, String code_points);
void insert(UTF8String *string, s64 index, String code_points);
void remove(UTF8String *string, s64 index, s64 count = 1);

