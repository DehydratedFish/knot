#pragma once

#include "memory.h"
#include "io.h"


enum {
    PLATFORM_FILE_READ,
    PLATFORM_FILE_WRITE,
    PLATFORM_FILE_APPEND,
};
struct PlatformFile {
    void *handle;
    MemoryBuffer read_buffer;
    MemoryBuffer write_buffer;

    b32 open;
};

PlatformFile platform_create_file_handle(String filename, u32 mode);
void   platform_close_file_handle(PlatformFile *file);
s64    platform_file_size(PlatformFile *file);
String platform_read(PlatformFile *file, u64 offset, void *buffer, s64 size);
s64    platform_write(PlatformFile *file, void const *buffer, s64 size);
s64    platform_write(PlatformFile *file, u64 offset, void const *buffer, s64 size);
b32    platform_flush_write_buffer(PlatformFile *file);


struct PlatformConsole {
    PlatformFile *out;
};

extern PlatformConsole Console;

enum {
    PLATFORM_READ_OK,
    PLATFORM_READ_ERROR
};
struct PlatformReadResult {
    String content;
    s32 status;
};
PlatformReadResult platform_read_entire_file(String file, Allocator alloc = default_allocator());

s32 application_main(Array<String> args);

