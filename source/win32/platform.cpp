#include "sane_win32.h"

#include "string2.h"
#include "platform.h"
#include "utf.h"


INTERNAL void *ProcessHandle;

PlatformConsole Console;

#ifndef PLATFORM_CONSOLE_BUFFER_SIZE
#define PLATFORM_CONSOLE_BUFFER_SIZE 4096
#endif


#include <cstdlib>

INTERNAL void *cstd_alloc_func(void *, s64 size, void *old, s64 old_size) {
    void *result = 0;

    if (old) {
        if (size) {
            result = realloc(old, size);
        } else {
            free(old);
        }
    } else {
        result = calloc(1, size);
    }

    return result;
}
Allocator CStdAllocator = {cstd_alloc_func, 0};


// TODO: Thread safety... make TLS?
Allocator DefaultAllocator;
MemoryArena TemporaryStorage;

Allocator default_allocator() {
    return DefaultAllocator;
}

Allocator temporary_allocator() {
    return make_arena_allocator(&TemporaryStorage);
}

s64 temporary_storage_mark() {
    return TemporaryStorage.used;
}

void temporary_storage_rewind(s64 mark) {
    TemporaryStorage.used = mark;
}

void reset_temporary_storage() {
    TemporaryStorage.used = 0;
}


int main_main() {
    ProcessHandle = GetCurrentProcess();

    DefaultAllocator = CStdAllocator;
    TemporaryStorage = allocate_arena(KILOBYTES(32));

    PlatformFile standard_out_handle = {};
    standard_out_handle.handle = GetStdHandle(STD_OUTPUT_HANDLE);
    standard_out_handle.write_buffer = allocate_memory_buffer(DefaultAllocator, PLATFORM_CONSOLE_BUFFER_SIZE);
    standard_out_handle.open = true;

    Console.out = &standard_out_handle;

    wchar_t *cmd_line = GetCommandLineW();

    int cmd_arg_count;
    wchar_t **cmd_args = CommandLineToArgvW(cmd_line, &cmd_arg_count);
    DEFER(LocalFree(cmd_args));

    Array<String> args = ALLOCATE_ARRAY(String, cmd_arg_count);
    DEFER(DESTROY_ARRAY(args));

    for (int i = 0; i < cmd_arg_count; i += 1) {
        String16 arg = {(u16*)cmd_args[i], (s64)wcslen(cmd_args[i])};
        args[i] = to_utf8(default_allocator(), arg);
    }

    s32 status = application_main(args);

    for (int i = 0; i < cmd_arg_count; i += 1) {
        destroy(&args[i]);
    }

    platform_flush_write_buffer(Console.out);

    destroy(&TemporaryStorage);
    free_memory_buffer(&Console.out->write_buffer);

    return status;
}

int APIENTRY WinMain(HINSTANCE, HINSTANCE, char*, int) {
    return main_main();
}

int main() {
    return main_main();
}


INTERNAL String16 widen(String str, Allocator alloc = temporary_allocator()) {
    return to_utf16(alloc, str, true);
}


INTERNAL void convert_backslash_to_slash(wchar_t *buffer, s64 size) {
    for (s64 i = 0; i < size; i += 1) {
        if (buffer[i] == L'\\') buffer[i] = L'/';
    }
}
INTERNAL void convert_slash_to_backslash(wchar_t *buffer, s64 size) {
    for (s64 i = 0; i < size; i += 1) {
        if (buffer[i] == L'/') buffer[i] = L'\\';
    }
}
INTERNAL void convert_slash_to_backslash(String16 str) {
    return convert_slash_to_backslash((wchar_t*)str.data, str.size);
}


PlatformFile platform_create_file_handle(String filename, u32 mode) {
    u32 win32_mode = 0;
    u32 win32_open_mode = 0;

    switch (mode) {
    case PLATFORM_FILE_READ: {
        win32_mode = GENERIC_READ;
        win32_open_mode = OPEN_ALWAYS;
    } break;

    case PLATFORM_FILE_WRITE: {
        win32_mode = GENERIC_WRITE;
        win32_open_mode = CREATE_ALWAYS;
    } break;

    case PLATFORM_FILE_APPEND: {
        win32_mode = GENERIC_READ | GENERIC_WRITE;
        win32_open_mode = OPEN_ALWAYS;
    } break;

    default:
        die("Unknown file mode.");
    }

    PlatformFile result = {};

    String16 wide_filename = widen(filename);
    convert_slash_to_backslash(wide_filename);

    void *handle = CreateFileW((wchar_t*)wide_filename.data, win32_mode, 0, 0, win32_open_mode, FILE_ATTRIBUTE_NORMAL, 0);
    if (handle == INVALID_HANDLE_VALUE) {
        return result;
    }

    if (mode == PLATFORM_FILE_APPEND) {
        SetFilePointer(handle, 0, 0, FILE_END);
    }

    result.handle = handle;
    result.open   = true;

    return result;
}

void platform_close_file_handle(PlatformFile *file) {
    if (file->handle != INVALID_HANDLE_VALUE && file->handle != 0) {
        CloseHandle(file->handle);
    }

    INIT_STRUCT(file);
}

s64 platform_file_size(PlatformFile *file) {
    s64 size;
    GetFileSizeEx(file->handle, &size);

    return size;
}

String platform_read(PlatformFile *file, u64 offset, void *buffer, s64 size) {
    // TODO: split reads if they are bigger than 32bit
    assert(size <= INT_MAX);

    if (!file->open) return {};

    OVERLAPPED ov = {0};
    ov.offset      = offset & 0xFFFFFFFF;
    ov.offset_high = (offset >> 32) & 0xFFFFFFFF;

    ReadFile(file->handle, buffer, (u32)size, 0, &ov);

    u32 bytes_read = 0;
    GetOverlappedResult(file->handle, &ov, &bytes_read, true);

    return {(u8*)buffer, bytes_read};
}

s64 platform_write(PlatformFile *file, u64 offset, void const *buffer, s64 size) {
    // TODO: split writes if they are bigger than 32bit
    assert(size <= INT_MAX);

    if (!file->open) return 0;

    OVERLAPPED ov = {0};
    ov.offset      = offset & 0xFFFFFFFF;
    ov.offset_high = (offset >> 32) & 0xFFFFFFFF;

    WriteFile(file->handle, buffer, (u32)size, 0, &ov);

    u32 bytes_written = 0;
    b32 status = GetOverlappedResult(file->handle, &ov, &bytes_written, true);

    return bytes_written;
}

s64 platform_write(PlatformFile *file, void const *buffer, s64 size) {
    return platform_write(file, ULLONG_MAX, buffer, size);
}

b32 platform_flush_write_buffer(PlatformFile *file) {
    if (platform_write(file, file->write_buffer.memory, file->write_buffer.used) != file->write_buffer.used) return false;
    file->write_buffer.used = 0;

    return true;
}

PlatformReadResult platform_read_entire_file(String file, Allocator alloc) {
    PlatformReadResult result = {};

    String16 wide_file = widen(file);
    void *handle = CreateFileW((wchar_t*)wide_file.data, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (handle == INVALID_HANDLE_VALUE) {
        result.status = PLATFORM_READ_ERROR;

        return result;
    }
    DEFER(CloseHandle(handle));

    s64 size;
    GetFileSizeEx(handle, &size);

    String content = {};

    if (size) {
        content = allocate_string(size, alloc);

        s64 total = 0;
        while (total != content.size) {
            u32 bytes_read = 0;

            if (!ReadFile(handle, content.data + total, (u32)size, &bytes_read, 0)) {
                result.status = PLATFORM_READ_ERROR;

                destroy(&content, alloc);
                return result;
            }

            total += bytes_read;
            size -= bytes_read;
        }
    }
    result.content = content;

    return result;
}

Array<String> platform_directory_listing(String path) {
    DArray<String> listing = {};

    String search = path;
    if (path.size == 0) {
        search = "*";
    } else if (!ends_with(path, "/") || !ends_with(path, "\\")) {
        search = t_format("%S/%s", path, "*");
    } else if (!ends_with(path, "/*") || !ends_with(path, "\\*")) {
        search = t_format("%S/%s", path, "\\*");
    }

    String16 wide_folder = to_utf16(temporary_allocator(), search, true);
    convert_slash_to_backslash((wchar_t*)wide_folder.data, wide_folder.size);

    WIN32_FIND_DATAW data;
    void *handle = FindFirstFileW((wchar_t*)wide_folder.data, &data);
    if (handle == INVALID_HANDLE_VALUE) return listing;

    Allocator alloc = default_allocator();
    append(listing, to_utf8(alloc, {(u16*)data.file_name, (s64)wcslen(data.file_name)}));

    while (FindNextFileW(handle, &data)) {
        append(listing, to_utf8(alloc, {(u16*)data.file_name, (s64)wcslen(data.file_name)}));
    }

    FindClose(handle);

    return listing;
}

void platform_destroy_directory_listing(Array<String> *listing) {
    for (s64 i = 0; i < listing->size; i += 1) {
        destroy(&listing->data[i]);
    }
    DESTROY_ARRAY(*listing);
}


u32 const STACK_TRACE_SIZE = 64;
u32 const SYMBOL_NAME_LENGTH = 1024;

INTERNAL void print_stack_trace() {
    u8 InfoStorage[sizeof(SYMBOL_INFO) + SYMBOL_NAME_LENGTH];

    void *stack[STACK_TRACE_SIZE];
    HPROCESS process = GetCurrentProcess();

    if (!SymInitialize(process, 0, true)) {
        print("Could not retrieve stack trace.");
        return;
    }

    s32 const skipped_frames = 2;
    u16 frames = RtlCaptureStackBackTrace(skipped_frames, STACK_TRACE_SIZE, stack, 0);
    SYMBOL_INFO *info = (SYMBOL_INFO *)InfoStorage;
    info->size_of_struct = sizeof(SYMBOL_INFO);
    info->max_name_len   = SYMBOL_NAME_LENGTH - 1;

    u32 displacement;
    IMAGEHLP_LINE64 line = {};
    line.size_of_struct = sizeof(line);

    for (int i = 0; i < frames - 8; i += 1) {
        if (SymFromAddr(process, (u64)stack[i], 0, info)) {
            SymGetLineFromAddr64(process, (u64)stack[i], &displacement, &line);
            print("%s(%d): %s\n", line.file_name, line.line_number, info->name);
        } else {
            print("0x000000000000: ???");
        }
    }

    SymCleanup(process);
}

void die(char const *msg) {
    print("Fatal Error: %s\n\n", msg);
    print_stack_trace();

    DebugBreak();
    ExitProcess(-1);
}

void fire_assert(char const *msg, char const *func, char const *file, int line) {
    print("Assertion failed: %s\n", msg);
    print("\t%s\n\t%s:%d\n\n", file, func, line);

    print_stack_trace();

    DebugBreak();
    ExitProcess(-1);
}

