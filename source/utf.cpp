#include "utf.h"
#include "memory.h"
#include "string2.h"
        


struct UTF16GetResult {
    u16 value[2];
    b32 is_surrogate;
    b32 status;
};

INTERNAL UTF16GetResult get_utf16(String16 *string) {
    if (string->size == 0) return {{}, 0, GET_EMPTY};
    if (string->data[0] > 0xDBFF && string->data[0] < 0xE000) return {{}, 0, GET_ERROR};

    UTF16GetResult result = {};
    if (string->data[0] > 0xD7FF && string->data[0] < 0xDC00) {
        if (string->size < 2) return {{}, 0, GET_ERROR};

        result.value[0] = string->data[0];
        result.value[1] = string->data[1];
        result.is_surrogate = true;
        result.status   = GET_OK;

        string->data += 2;
        string->size -= 2;
    } else {
        result.value[0] = string->data[0];
        result.is_surrogate = false;
        result.status   = GET_OK;

        string->data += 1;
        string->size -= 1;
    }

    return result;
}

String to_utf8(Allocator alloc, String16 string) {
    if (string.size == 0) return {};

    DArray<u8> buffer = {};
    buffer.allocator = alloc;

    UTF16GetResult c = get_utf16(&string);
    while (c.status == GET_OK) {
        if (c.is_surrogate) {
            u32 cp = ((c.value[0] - 0xD800) >> 10) + (c.value[1] - 0xDC00) + 0x10000;

            u8 byte;
            byte = 0xF0 | (cp >> 18);
            append(buffer, byte);
            byte = 0x80 | ((cp >> 12) & 0x3F);
            append(buffer, byte);
            byte = 0x80 | ((cp >> 6) & 0x3F);
            append(buffer, byte);
            byte = 0x80 | (cp & 0x3F);
            append(buffer, byte);
        } else {
            u16 cp = c.value[0];

            u8 byte;
            if (cp < 0x80) {
                byte = (u8)cp;
                append(buffer, byte);
            } else if (cp < 0x800) {
                byte = 0xC0 | (cp >> 6);
                append(buffer, byte);
                byte = 0x80 | (cp & 0x3F);
                append(buffer, byte);
            } else {
                byte = 0xE0 | (cp >> 12);
                append(buffer, byte);
                byte = 0x80 | ((cp >> 6) & 0x3F);
                append(buffer, byte);
                byte = 0x80 | (cp & 0x3F);
                append(buffer, byte);
            }
        }

        c = get_utf16(&string);
    }

    if (c.status == GET_ERROR) {
        die("Malformed utf16 string.");
    }

    return {buffer.memory, buffer.size};
}

String to_utf8(Allocator alloc, String32 string) {
    if (string.size == 0) return {};

    StringBuilder builder = {};
    DEFER(destroy(&builder));

    for (s64 i = 0; i < string.size; i += 1) {
        u32 cp = string.data[i];

        u8 byte;
        if (cp < 0x80) {
            byte = cp;
            append(&builder, byte);
        } else if (cp < 0x800) {
            byte = 0xC0 | (cp >> 6);
            append(&builder, byte);
            byte = 0x80 | (cp & 0x3F);
            append(&builder, byte);
        } else {
            byte = 0xE0 | (cp >> 12);
            append(&builder, byte);
            byte = 0x80 | ((cp >> 6) & 0x3F);
            append(&builder, byte);
            byte = 0x80 | (cp & 0x3F);
            append(&builder, byte);
        }
    }

    return to_allocated_string(&builder, alloc);
}

INTERNAL s32 utf8_length(u8 c) {
    if      ((c & 0x80) == 0x00) return 1;
    else if ((c & 0xE0) == 0xC0) return 2;
    else if ((c & 0xF0) == 0xE0) return 3;
    else if ((c & 0xF8) == 0xF0) return 4;

    return -1;
}


// TODO: Maybe lookup table?
INTERNAL s32 utf8_length(String str, s64 offset) {
    if (str.size <= offset) return 0;

    return utf8_length(str[offset]);
}

struct UTF8GetResult {
    u8  byte[4];
    s32 size;
    b32 status;
};

INTERNAL UTF8GetResult get_utf8(String string, s64 offset) {
    UTF8GetResult result = {};

    s32 size = utf8_length(string, offset);
    if (size == -1) {
        String tmp = sub_string(string, offset, string.size - offset);
        result.status = GET_ERROR;

        return result;
    }
    if (size == 0) {
        result.status = GET_EMPTY;
    }

    copy_memory(result.byte, string.data + offset, size);
    result.size = size;

    return result;
}

INTERNAL UTF8GetResult get_utf8_backwards(String string, s64 offset) {
    UTF8GetResult result = {};

    if (offset - 1 < 0) {
        result.status = GET_EMPTY;

        return result;
    }

    while (offset >= 0) {
        offset -= 1;

        if ((string[offset] & 0xC0) != 0x80) {

            break;
        }
    }

    result = get_utf8(string, offset);

    return result;
}

/*
 * Note: Helpful stackoverflow question
 * https://stackoverflow.com/questions/73758747/looking-for-the-description-of-the-algorithm-to-convert-utf8-to-utf16
 */
String16 to_utf16(Allocator alloc, String string, b32 add_null_terminator) {
    if (string.size == 0) return {};

    DArray<u16> buffer = {};
    buffer.allocator = alloc;

    s64 offset = 0x00;
    u32 status = GET_OK;
    while (status == GET_OK) {
        UTF8GetResult c = get_utf8(string, offset);
        status = c.status;
        if (c.status != GET_OK) break;

        if (c.size == 1) {
            u16 value = c.byte[0] & 0x7F;
            append(buffer, value);
        } else if (c.size == 2) {
            u16 value = ((c.byte[0] & 0x1F) << 6) | (c.byte[1] & 0x3F);
            append(buffer, value);
        } else if (c.size == 3) {
            u16 value = ((c.byte[0] & 0x0F) << 12) | ((c.byte[1] & 0x3F) << 6) | (c.byte[2] & 0x3F);
            append(buffer, value);
        } else if (c.size == 4) {
            u32 cp = ((c.byte[0] & 0x07) << 18) | ((c.byte[1] & 0x3F) << 12) | ((c.byte[2] & 0x3F) << 6) | (c.byte[3] & 0x3F);
            cp -= 0x10000;

            u16 high = 0xD800 + ((cp >> 10) & 0x3FF);
            u16 low  = 0xDC00 + (cp & 0x3FF);

            append(buffer, high);
            append(buffer, low );
        }

        offset += c.size;
    }

    if (status == GET_ERROR) {
        // TODO: better error handling
        die("Malformed utf8");
    }

    if (add_null_terminator) {
        u16 n = L'\0';
        append(buffer, n);
    }

    return {buffer.memory, buffer.size};
}

UTF8Iterator make_utf8_it(String string, IterationStart from) {
    UTF8Iterator it = {};
    it.string = string;

    if (from == ITERATE_FROM_START) {
        next(&it);
    } else {
        it.index = string.size;
        prev(&it);
    }

    return it;
}

void next(UTF8Iterator *it) {
    assert(it);

    it->index += it->utf8_size;
    UTF8GetResult c = get_utf8(it->string, it->index);
    if (c.status != GET_OK) {
        INIT_STRUCT(it);

        return;
    }

    if (c.size == 1) {
        it->cp = c.byte[0] & 0x7F;
    } else if (c.size == 2) {
        it->cp = ((c.byte[0] & 0x1F) << 6) | (c.byte[1] & 0x3F);
    } else if (c.size == 3) {
        it->cp = ((c.byte[0] & 0x0F) << 12) | ((c.byte[1] & 0x3F) << 6) | (c.byte[2] & 0x3F);
    } else if (c.size == 4) {
        it->cp = ((c.byte[0] & 0x07) << 18) | ((c.byte[1] & 0x3F) << 12) | ((c.byte[2] & 0x3F) << 6) | (c.byte[3] & 0x3F);
    }
    
    it->utf8_size = c.size;
    it->valid = true;
}

void prev(UTF8Iterator *it) {
    assert(it);

    UTF8GetResult c = get_utf8_backwards(it->string, it->index);
    if (c.status != GET_OK) {
        assert(c.status == GET_EMPTY);
        INIT_STRUCT(it);

        return;
    }

    if (c.size == 1) {
        it->cp = c.byte[0] & 0x7F;
    } else if (c.size == 2) {
        it->cp = ((c.byte[0] & 0x1F) << 6) | (c.byte[1] & 0x3F);
    } else if (c.size == 3) {
        it->cp = ((c.byte[0] & 0x0F) << 12) | ((c.byte[1] & 0x3F) << 6) | (c.byte[2] & 0x3F);
    } else if (c.size == 4) {
        it->cp = ((c.byte[0] & 0x07) << 18) | ((c.byte[1] & 0x3F) << 12) | ((c.byte[2] & 0x3F) << 6) | (c.byte[3] & 0x3F);
    }
    
    it->index -= c.size;
    it->utf8_size = c.size;
    it->valid = true;
}

UTF8CharResult utf8_peek(String str) {
    UTF8CharResult result = {};

    if (str.size == 0) {
        result.status = GET_EMPTY;

        return result;
    }

    s32 length = utf8_length(str.data[0]);
    if (str.size < length) {
        result.status = GET_ERROR;

        return result;
    }

    if (length == 1) {
        result.cp = str[0] & 0x7F;
    } else if (length == 2) {
        result.cp = ((str[0] & 0x1F) << 6) | (str[1] & 0x3F);
    } else if (length == 3) {
        result.cp = ((str[0] & 0x0F) << 12) | ((str[1] & 0x3F) << 6) | (str[2] & 0x3F);
    } else if (length == 4) {
        result.cp = ((str[0] & 0x07) << 18) | ((str[1] & 0x3F) << 12) | ((str[2] & 0x3F) << 6) | (str[3] & 0x3F);
    }

    copy_memory(result.byte, str.data, length);
    result.length = length;
    result.status = GET_OK;

    return result;
}

UTF8CharResult to_utf8(u32 cp) {
    UTF8CharResult result = {};
    result.cp = cp;

    if (cp < 0x80) {
        result.byte[0] = cp;
        result.length = 1;
    } else if (cp < 0x800) {
        result.byte[0] = 0xC0 | (cp >> 6);
        result.byte[1] = 0x80 | (cp & 0x3F);
        result.length = 2;
    } else if (cp < 0x10000) {
        result.byte[0] = 0xE0 |  (cp >> 12);
        result.byte[1] = 0x80 | ((cp >>  6) & 0x3F);
        result.byte[2] = 0x80 | (cp & 0x3F);
        result.length = 3;
    } else if (cp < 0x110000) {
        result.byte[0] = 0xF0 |  (cp >> 18);
        result.byte[1] = 0x80 | ((cp >> 12) & 0x3F);
        result.byte[2] = 0x80 | ((cp >>  6) & 0x3F);
        result.byte[3] = 0x80 | (cp & 0x3F);
        result.length = 4;
    } else {
        result.status = -1;
    }
    // TODO: Check for overlong stuff and so on.

    return result;
}


void reset(UTF8String *string) {
    string->bytes.size = 0;
    string->code_points.size = 0;
}

void append(UTF8String *string, u32 code_point) {
    UTF8CharResult cp = to_utf8(code_point);
    assert(cp.status == 0);
    
    append(string, {cp.byte, cp.length});
}

void append(UTF8String *string, String code_points) {
    s64 offset = 0x00;
    s64 index  = string->bytes.size;
    while (offset < code_points.size) {
        CodePointInfo info = {};
        info.index  = index;
        info.length = utf8_length(code_points, offset);

        append(string->code_points, info);
        offset += info.length;
        index  += info.length;
    }

    BOUNDS_CHECK(0, code_points.size, offset, "Malformed UTF8 data.");

    append(string->bytes, code_points.data, code_points.size);
}

// TODO: Two options here...
//       [USED] Iterate string and pre determine the amount of code points to insert them without moving each time
//              Insert code point on each iteration, moving the back again and again
void insert(UTF8String *string, s64 index, String code_points) {
    s64 offset = 0x00;
    s64 count  = 0;
    while (offset < code_points.size) {
        s32 length = utf8_length(code_points, offset);

        offset += length;
        count  += 1;
    }

    BOUNDS_CHECK(0, code_points.size, offset, "Malformed UTF8 data.");

    CodePointInfo byte_index = string->code_points[index];
    u8 *bytes_ptr = string->bytes.memory + byte_index.index;

    ensure_space(string->bytes, code_points.size);
    copy_memory(bytes_ptr, bytes_ptr + code_points.size, string->bytes.size - (bytes_ptr - string->bytes.memory));

    for (s64 i = index; i < string->code_points.size; i += 1) {
        string->code_points[i].index += offset;
    }

    CodePointInfo *cp_index = &string->code_points[index];

    ensure_space(string->code_points, count);
    copy_memory(cp_index, cp_index + count, (string->code_points.size - index) * sizeof(CodePointInfo));

    offset = 0x00;
    s64 cp = index;
    while (offset < code_points.size) {
        CodePointInfo info = {};
        info.index  = (bytes_ptr + offset) - string->bytes.memory;
        info.length = utf8_length(code_points, offset);

        copy_memory(bytes_ptr + offset, code_points.data + offset, info.length);
        cp_index[cp] = info;

        offset += info.length;
        cp     += 1;
    }
}

void remove(UTF8String *string, s64 index, s64 count) {
    BOUNDS_CHECK(0,string->code_points.size, index + count, "UTF8String removal out of bounds");

    s64 remove_length = 0;
    for (s64 i = index; i < index + count; i += 1) {
        remove_length += string->code_points[index].length;
    }

    s64 byte_index = string->code_points[index].index;

    s64 bytes_to_move = string->bytes.size - byte_index - remove_length;
    copy_memory(string->bytes.memory + index, string->bytes.memory + index + remove_length, bytes_to_move);
    string->bytes.size -= remove_length;

    copy_memory(string->code_points.memory + index, string->code_points.memory + index + count, string->code_points.size - index - count);
    string->code_points.size -= count;

    for (s64 i = index; i < string->code_points.size; i += 1) {
        string->code_points[index].index -= remove_length;
    }
}

