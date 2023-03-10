#ifndef INCLUDE_GUARD_PLATFORM_H
#define INCLUDE_GUARD_PLATFORM_H

#include "string2.h"
#include "array.h"


s32 application_main(Array<String> args);

s32 print(char const *fmt, ...);




typedef struct PlatformFile {
	String name;
	void *handle;
} PlatformFile;

PlatformFile platform_file_open(String filename);
void platform_file_close(PlatformFile *file);

u64 platform_file_size(PlatformFile *file);
u32 platform_file_read(PlatformFile *file, u64 offset, void *buffer, u32 size);

String platform_read_entire_file(String filename);


#endif // INCLUDE_GUARD_PLATFORM_H

