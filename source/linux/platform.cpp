#include "platform.h"

#include "stdarg.h"
#include "stdlib.h"
#include "errno.h"
#include "signal.h"
#include "unistd.h"
#include "fcntl.h"
#include "execinfo.h"
#include "sys/stat.h"



INTERNAL void print_stack_trace() {
	u32 const max_entry_count = 128;
	void *entries[max_entry_count];

	u32 entry_count = backtrace(entries, max_entry_count);
	print("Stack trace: \n");
	backtrace_symbols_fd(entries, entry_count, STDOUT_FILENO);
}

void fire_assert(char const *msg, char const *func, char const *file, int line) {
	print("Assertion failed: %s\n", msg);
	print("\t%s\n\t%s:%d\n\n", file, func, line);

	print_stack_trace();

	_exit(-1);
}

void die(char const *msg) {
	print("Programm requested to close because of: %s\n\n", msg);
	print_stack_trace();

	_exit(-1);
}

INTERNAL void segfault_stacktrace(int signal) {
	print_stack_trace();
	_exit(-1);
}


void *default_allocator_func(void *ptr, u64 bytes, void *) {
	void *result = 0;

	if (ptr == 0) {
		if (bytes == 0) return 0; // NOTE: no error condition

		result = malloc(bytes);
		zero_memory(result, bytes);
	} else {
		if (bytes == 0) {
			free(ptr);
			return 0;
		} else {
			result = realloc(ptr, bytes);
		}
	}

	if (result == 0)
		die("Could not allocate memory. Aborting...\n");

	return result;
}


thread_local MemoryContext ThreadMemoryContext;
Allocator DefaultAllocator = {default_allocator_func, 0};

MemoryContext *get_memory_context() {
	return &ThreadMemoryContext;
}

int main(int argc, char **argv) {
	s32 result = 0;

	signal(SIGSEGV, segfault_stacktrace);
	ThreadMemoryContext.allocator = DefaultAllocator;

	Array<String> args;
	for (s32 i = 0; i < argc; i += 1) {
		append(&args, String((u8*)argv[i], c_string_length(argv[i])));
	}
	result = application_main(args);
}


void *allocate(u64 bytes) {
	return allocate_memory(DefaultAllocator, bytes);
}
void deallocate(void *ptr) {
	deallocate_memory(DefaultAllocator, ptr);
}


void copy_memory(void *dest, void *src, u64 size) {
	u8 *lhs = (u8*)dest;
	u8 *rhs = (u8*)src;

	for (u64 i = 0; i < size; i += 1) {
		lhs[i] = rhs[i];
	}
}

void copy_memory_reverse(void *dest, void *src, u64 size) {
	u8 *lhs = (u8*)dest;
	u8 *rhs = (u8*)src;

	while (size) {
		size -= 1;

		lhs[size] = rhs[size];
	}
}

void move_memory(void *dest, void *src, u64 size) {
	if (dest < src)
		copy_memory(dest, src, size);
	else
		copy_memory_reverse(dest, src, size);
}

void zero_memory(void *data, u64 bytes) {
	u8 *tmp = (u8*)data;
	for (u64 i = 0; i < bytes; i += 1) {
		tmp[i] = 0;
	}
}


// TODO: read and write stuff, filename
PlatformFile platform_file_open(String filename) {
	PlatformFile file = {0};

	put(&filename, '\0');

	s32 handle = open((char const*)filename.data, O_RDONLY);
	if (handle == -1) {
		print("Error: could not open file %S\n", pr(filename));
		return file;
	}

	file.name = copy(&filename);
	file.handle = (void*)(u64)handle;

	return file;
}

void platform_file_close(PlatformFile *file) {
	close((s32)(u64)file->handle); // silence warning
}

u64 platform_file_size(PlatformFile *file) {
	struct stat info;
	if (fstat((s32)(u64)file->handle, &info) == 0) {
		return info.st_size;
	}

	return 0;
}

String platform_read_entire_file(String filename) {
	String result;

	PlatformFile file = platform_file_open(filename);
	u64 size = platform_file_size(&file);
	if (size == 0) return result;

	prealloc(&result, size);
	result.size = size;
	platform_file_read(&file, 0, result.data, size);

	platform_file_close(&file);

	return result;
}

u32 platform_file_read(PlatformFile *file, u64 offset, void *buffer, u32 size) {
	u32 bytes_read = 0;
	u32 pending = size;

	do {
		offset += bytes_read;
		pending -= bytes_read;
		bytes_read = pread((s32)(u64)file->handle, buffer, pending, offset);
	} while (bytes_read > 0);

	return size - pending;
}

#define FILE_BUFFER_SIZE KILOBYTES(4)

enum FileBufferMode {
	FB_MODE_FLUSH_ON_NEW_LINE,
	FB_MODE_FLUSH_ON_FULL_BUFFER,
	FB_MODE_UNBUFFERED
};
typedef struct FileBuffer {
	u8 buffer[FILE_BUFFER_SIZE];
	s32 used;
	u32 mode;
} FileBuffer;

FileBuffer ConsoleFileBuffer;
s32 ConsoleFileHandle;


void platform_basic_file_write(s32 file, void *buffer, s32 size) {
	ssize_t written = write(file, buffer, size);
	if (written == -1) die("WriteFile failed with error code.\n");
	assert(written == size);
}


void file_buffer_flush(FileBuffer *fb, s32 file) {
	if (fb->used) {
		platform_basic_file_write(file, fb->buffer, fb->used);
		fb->used = 0;
	}
}

void file_buffer_basic_put(FileBuffer *fb, s32 file, u8 c) {
	if (fb->used == FILE_BUFFER_SIZE) file_buffer_flush(fb, file);

	fb->buffer[fb->used] = c;
	fb->used += 1;
}

s32 file_buffer_basic_write(FileBuffer *fb, s32 file, u8 *data, s64 size) {
	s32 space = FILE_BUFFER_SIZE - fb->used;
	if (space) {
		if (size < space) space = size;
		copy_memory(fb->buffer + fb->used, data, space);
		fb->used += space;
	}
	if (fb->used == FILE_BUFFER_SIZE) file_buffer_flush(fb, file);

	return space;
}

void file_buffer_put(FileBuffer *fb, s32 file, u8 c) {
	if (fb->mode == FB_MODE_UNBUFFERED) {
		platform_basic_file_write(file, &c, 1);
		return;
	}

	file_buffer_basic_put(fb, file, c);
	if(fb->mode == FB_MODE_FLUSH_ON_NEW_LINE && c == '\n') {
		file_buffer_flush(fb, file);
	}
}

void file_buffer_append(FileBuffer *fb, s32 file, u8 *data, s64 size) {
	switch (fb->mode) {
	case FB_MODE_UNBUFFERED: {
		platform_basic_file_write(file, data, size);
	} break;

	case FB_MODE_FLUSH_ON_FULL_BUFFER: {
		while (size) {
			s32 written = file_buffer_basic_write(fb, file, data, size);
			size -= written;
			data += written;
		}
	} break;

	case FB_MODE_FLUSH_ON_NEW_LINE: {
		for (s32 i = 0; i < size; i += 1) {
			file_buffer_basic_put(fb, file, data[i]);

			if (data[i] == '\n') file_buffer_flush(fb, file);
		}
	} break;
	}
}

INTERNAL String convert_double_to_string(u8 *buffer, s32 size, r64 number, s32 precision, b32 scientific, b32 hex, b32 uppercase, b32 keep_sign);
void convert_to_ptr_string(u8 *buffer, s32 buffer_size, void *address);
String convert_signed_to_string(u8 *buffer, s32 buffer_size, s64 signed_number, s32 base, b32 uppercase, b32 keep_sign);
s32 convert_unsigned_to_string(u8 *buffer, s32 buffer_size, u64 number, s32 base, b32 uppercase);
s64 convert_string_to_s64(u8 *buffer, s32 buffer_size);


String format(char const *fmt, ...) {
	va_list args;
	va_start(args, fmt);

	String result; // TODO: prealloc
	while (fmt[0]) {
		if (fmt[0] == '%' && fmt[1]) {
			fmt += 1;

			switch (fmt[0]) {
			case 'd': {
				u8 buffer[128];
				String ref = convert_signed_to_string(buffer, 128, va_arg(args, s32), 10, false, false);

				append(&result, ref.data, ref.size);
			} break;

			case 'u': {
				u8 buffer[128];
				s32 length = convert_unsigned_to_string(buffer, 128, va_arg(args, u32), 10, false);

				append(&result, buffer, length);
			} break;

			case 'l': {
				if (fmt[1] == 'x') {
					u8 buffer[128];
					String ref = convert_signed_to_string(buffer, 128, va_arg(args, s64), 16, false, false);

					append(&result, ref.data, ref.size);
					break;
				} else {
					die("Unknown format specifier.\n");
				}
				die("Unknown format specifier %l\n");
			} break;

			case 'x': {
				u8 buffer[128];
				s32 length = convert_unsigned_to_string(buffer, 128, va_arg(args, u32), 16, false);

				append(&result, buffer, length);
			} break;

			case 'p': {
				u8 buffer[18];
				convert_to_ptr_string(buffer, 18, va_arg(args, void*));

				append(&result, buffer, 18);
			} break;

			case 'f': {
				u8 buffer[512];
				String res = convert_double_to_string(buffer, 512, va_arg(args, r64), 6, false, false, false, false);

				append(&result, res.data, res.size);
			} break;

			case 'c': {
				u8 c = va_arg(args, int);
				append(&result, &c, 1);
			} break;

			case 's': {
				char *str = va_arg(args, char *);
				s64 length = c_string_length(str);

				append(&result, (u8*)str, length);
			} break;

			case 'S': {
				PrintRef str = va_arg(args, PrintRef);

				append(&result, str.data, str.size);
			} break;
			}

			fmt += 1;
			continue;
		}
		put(&result, fmt[0]);
		fmt += 1;
	}

	va_end(args);

	return result;
}

s32 print(char const *fmt, ...) {
	va_list args;
	va_start(args, fmt);

	s32 out = STDOUT_FILENO;

	while (fmt[0]) {
		if (fmt[0] == '%' && fmt[1]) {
			fmt += 1;

			switch (fmt[0]) {
			case 'd': {
				u8 buffer[128];
				String ref = convert_signed_to_string(buffer, 128, va_arg(args, s32), 10, false, false);

				file_buffer_append(&ConsoleFileBuffer, out, ref.data, ref.size);
			} break;

			case 'u': {
				u8 buffer[128];
				s32 length = convert_unsigned_to_string(buffer, 128, va_arg(args, u32), 10, false);

				file_buffer_append(&ConsoleFileBuffer, out, buffer, length);
			} break;

			case 'l': {
				if (fmt[1] == 'x') {
					u8 buffer[128];
					String ref = convert_signed_to_string(buffer, 128, va_arg(args, s64), 16, false, false);

					file_buffer_append(&ConsoleFileBuffer, out, ref.data, ref.size);
					break;
				} else {
					die("Unknown format specifier.\n");
				}
				die("Unknown format specifier %l\n");
			} break;

			case 'x': {
				u8 buffer[128];
				s32 length = convert_unsigned_to_string(buffer, 128, va_arg(args, u32), 16, false);

				file_buffer_append(&ConsoleFileBuffer, out, buffer, length);
			} break;

			case 'p': {
				u8 buffer[18];
				convert_to_ptr_string(buffer, 18, va_arg(args, void*));

				file_buffer_append(&ConsoleFileBuffer, out, buffer, 18);
			} break;

			case 'f': {
				u8 buffer[512];
				String res = convert_double_to_string(buffer, 512, va_arg(args, r64), 6, false, false, false, false);

				file_buffer_append(&ConsoleFileBuffer, out, res.data, res.size);
			} break;

			case 'c': {
				u8 c = va_arg(args, int);
				file_buffer_append(&ConsoleFileBuffer, out, &c, 1);
			} break;

			case 's': {
				char *str = va_arg(args, char *);
				s64 length = c_string_length(str);

				file_buffer_append(&ConsoleFileBuffer, out, (u8*)str, length);
			} break;

			case 'S': {
				PrintRef str = va_arg(args, PrintRef);

				file_buffer_append(&ConsoleFileBuffer, out, str.data, str.size);
			} break;
			}

			fmt += 1;
			continue;
		}
		file_buffer_put(&ConsoleFileBuffer, out, fmt[0]);
		fmt += 1;
	}

	va_end(args);

	return 0;
}

/*
 * implementation from io.h
 *
 * io functions for basic console output
 *
 */


#define stbsp__uint32 unsigned int
#define stbsp__int32 signed int

#ifdef _MSC_VER
#define stbsp__uint64 unsigned __int64
#define stbsp__int64 signed __int64
#else
#define stbsp__uint64 unsigned long long
#define stbsp__int64 signed long long
#endif

#define stbsp__uint16 unsigned short

#define STBSP__SPECIAL 0x7000

static stbsp__int32 stbsp__real_to_str(char const **start, stbsp__uint32 *len, char *out, stbsp__int32 *decimal_pos, double value, stbsp__uint32 frac_digits);
static stbsp__int32 stbsp__real_to_parts(stbsp__int64 *bits, stbsp__int32 *expo, double value);

static void append_buffer(u8 *buffer, s32 *size, u32 c) {
		buffer[*size] = c;
			*size += 1;
}

static struct
{
	short temp; // force next field to be 2-byte aligned
	char pair[201];
} stbsp__digitpair =
{
	0,
	"00010203040506070809101112131415161718192021222324"
	"25262728293031323334353637383940414243444546474849"
	"50515253545556575859606162636465666768697071727374"
	"75767778798081828384858687888990919293949596979899"
};

// copies d to bits w/ strict aliasing (this compiles to nothing on /Ox)
#define STBSP__COPYFP(dest, src)			\
{							\
	int cn;						\
	for (cn = 0; cn < 8; cn++)			\
		((char *)&dest)[cn] = ((char *)&src)[cn];\
}

// get float info
static stbsp__int32 stbsp__real_to_parts(stbsp__int64 *bits, stbsp__int32 *expo, double value)
{
	double d;
	stbsp__int64 b = 0;

	// load value and round at the frac_digits
	d = value;

	STBSP__COPYFP(b, d);

	*bits = b & ((((stbsp__uint64)1) << 52) - 1);
	*expo = (stbsp__int32)(((b >> 52) & 2047) - 1023);

	return (stbsp__int32)((stbsp__uint64) b >> 63);
}

static double const stbsp__bot[23] = {
	1e+000, 1e+001, 1e+002, 1e+003, 1e+004, 1e+005, 1e+006, 1e+007, 1e+008, 1e+009, 1e+010, 1e+011,
	1e+012, 1e+013, 1e+014, 1e+015, 1e+016, 1e+017, 1e+018, 1e+019, 1e+020, 1e+021, 1e+022
};
static double const stbsp__negbot[22] = {
	1e-001, 1e-002, 1e-003, 1e-004, 1e-005, 1e-006, 1e-007, 1e-008, 1e-009, 1e-010, 1e-011,
	1e-012, 1e-013, 1e-014, 1e-015, 1e-016, 1e-017, 1e-018, 1e-019, 1e-020, 1e-021, 1e-022
};
static double const stbsp__negboterr[22] = {
	-5.551115123125783e-018,  -2.0816681711721684e-019, -2.0816681711721686e-020, -4.7921736023859299e-021, -8.1803053914031305e-022, 4.5251888174113741e-023,
	4.5251888174113739e-024,  -2.0922560830128471e-025, -6.2281591457779853e-026, -3.6432197315497743e-027, 6.0503030718060191e-028,  2.0113352370744385e-029,
	-3.0373745563400371e-030, 1.1806906454401013e-032,  -7.7705399876661076e-032, 2.0902213275965398e-033,  -7.1542424054621921e-034, -7.1542424054621926e-035,
	2.4754073164739869e-036,  5.4846728545790429e-037,  9.2462547772103625e-038,  -4.8596774326570872e-039
};
static double const stbsp__top[13] = {
	1e+023, 1e+046, 1e+069, 1e+092, 1e+115, 1e+138, 1e+161, 1e+184, 1e+207, 1e+230, 1e+253, 1e+276, 1e+299
};
static double const stbsp__negtop[13] = {
	1e-023, 1e-046, 1e-069, 1e-092, 1e-115, 1e-138, 1e-161, 1e-184, 1e-207, 1e-230, 1e-253, 1e-276, 1e-299
};
static double const stbsp__toperr[13] = {
	8388608,
	6.8601809640529717e+028,
	-7.253143638152921e+052,
	-4.3377296974619174e+075,
	-1.5559416129466825e+098,
	-3.2841562489204913e+121,
	-3.7745893248228135e+144,
	-1.7356668416969134e+167,
	-3.8893577551088374e+190,
	-9.9566444326005119e+213,
	6.3641293062232429e+236,
	-5.2069140800249813e+259,
	-5.2504760255204387e+282
};
static double const stbsp__negtoperr[13] = {
	3.9565301985100693e-040,  -2.299904345391321e-063,  3.6506201437945798e-086,  1.1875228833981544e-109,
	-5.0644902316928607e-132, -6.7156837247865426e-155, -2.812077463003139e-178,  -5.7778912386589953e-201,
	7.4997100559334532e-224,  -4.6439668915134491e-247, -6.3691100762962136e-270, -9.436808465446358e-293,
	8.0970921678014997e-317
};

#if defined(_MSC_VER) && (_MSC_VER <= 1200)
static stbsp__uint64 const stbsp__powten[20] = {
	1,
	10,
	100,
	1000,
	10000,
	100000,
	1000000,
	10000000,
	100000000,
	1000000000,
	10000000000,
	100000000000,
	1000000000000,
	10000000000000,
	100000000000000,
	1000000000000000,
	10000000000000000,
	100000000000000000,
	1000000000000000000,
	10000000000000000000U
};
#define stbsp__tento19th ((stbsp__uint64)1000000000000000000)
#else
static stbsp__uint64 const stbsp__powten[20] = {
	1,
	10,
	100,
	1000,
	10000,
	100000,
	1000000,
	10000000,
	100000000,
	1000000000,
	10000000000ULL,
	100000000000ULL,
	1000000000000ULL,
	10000000000000ULL,
	100000000000000ULL,
	1000000000000000ULL,
	10000000000000000ULL,
	100000000000000000ULL,
	1000000000000000000ULL,
	10000000000000000000ULL
};
#define stbsp__tento19th (1000000000000000000ULL)
#endif

#define stbsp__ddmulthi(oh, ol, xh, yh)                            \
{                                                               \
	double ahi = 0, alo, bhi = 0, blo;                           \
	stbsp__int64 bt;                                             \
	oh = xh * yh;                                                \
	STBSP__COPYFP(bt, xh);                                       \
	bt &= ((~(stbsp__uint64)0) << 27);                           \
	STBSP__COPYFP(ahi, bt);                                      \
	alo = xh - ahi;                                              \
	STBSP__COPYFP(bt, yh);                                       \
	bt &= ((~(stbsp__uint64)0) << 27);                           \
	STBSP__COPYFP(bhi, bt);                                      \
	blo = yh - bhi;                                              \
	ol = ((ahi * bhi - oh) + ahi * blo + alo * bhi) + alo * blo; \
}

#define stbsp__ddtoS64(ob, xh, xl)          \
{                                        \
	double ahi = 0, alo, vh, t;           \
	ob = (stbsp__int64)xh;                \
	vh = (double)ob;                      \
	ahi = (xh - vh);                      \
	t = (ahi - xh);                       \
	alo = (xh - (ahi - t)) - (vh + t);    \
	ob += (stbsp__int64)(ahi + alo + xl); \
}

#define stbsp__ddrenorm(oh, ol) \
{                            \
	double s;                 \
	s = oh + ol;              \
	ol = ol - (s - oh);       \
	oh = s;                   \
}

#define stbsp__ddmultlo(oh, ol, xh, xl, yh, yl) ol = ol + (xh * yl + xl * yh);

#define stbsp__ddmultlos(oh, ol, xh, yl) ol = ol + (xh * yl);

static void stbsp__raise_to_power10(double *ohi, double *olo, double d, stbsp__int32 power) // power can be -323 to +350
{
	double ph, pl;
	if ((power >= 0) && (power <= 22)) {
		stbsp__ddmulthi(ph, pl, d, stbsp__bot[power]);
	} else {
		stbsp__int32 e, et, eb;
		double p2h, p2l;

		e = power;
		if (power < 0)
			e = -e;
		et = (e * 0x2c9) >> 14; /* %23 */
		if (et > 13)
			et = 13;
		eb = e - (et * 23);

		ph = d;
		pl = 0.0;
		if (power < 0) {
			if (eb) {
				--eb;
				stbsp__ddmulthi(ph, pl, d, stbsp__negbot[eb]);
				stbsp__ddmultlos(ph, pl, d, stbsp__negboterr[eb]);
			}
			if (et) {
				stbsp__ddrenorm(ph, pl);
				--et;
				stbsp__ddmulthi(p2h, p2l, ph, stbsp__negtop[et]);
				stbsp__ddmultlo(p2h, p2l, ph, pl, stbsp__negtop[et], stbsp__negtoperr[et]);
				ph = p2h;
				pl = p2l;
			}
		} else {
			if (eb) {
				e = eb;
				if (eb > 22)
					eb = 22;
				e -= eb;
				stbsp__ddmulthi(ph, pl, d, stbsp__bot[eb]);
				if (e) {
					stbsp__ddrenorm(ph, pl);
					stbsp__ddmulthi(p2h, p2l, ph, stbsp__bot[e]);
					stbsp__ddmultlos(p2h, p2l, stbsp__bot[e], pl);
					ph = p2h;
					pl = p2l;
				}
			}
			if (et) {
				stbsp__ddrenorm(ph, pl);
				--et;
				stbsp__ddmulthi(p2h, p2l, ph, stbsp__top[et]);
				stbsp__ddmultlo(p2h, p2l, ph, pl, stbsp__top[et], stbsp__toperr[et]);
				ph = p2h;
				pl = p2l;
			}
		}
	}
	stbsp__ddrenorm(ph, pl);
	*ohi = ph;
	*olo = pl;
}

// given a float value, returns the significant bits in bits, and the position of the
//   decimal point in decimal_pos.  +/-INF and NAN are specified by special values
//   returned in the decimal_pos parameter.
// frac_digits is absolute normally, but if you want from first significant digits (got %g and %e), or in 0x80000000
static stbsp__int32 stbsp__real_to_str(char const **start, stbsp__uint32 *len, char *out, stbsp__int32 *decimal_pos, double value, stbsp__uint32 frac_digits)
{
	double d;
	stbsp__int64 bits = 0;
	stbsp__int32 expo, e, ng, tens;

	d = value;
	STBSP__COPYFP(bits, d);
	expo = (stbsp__int32)((bits >> 52) & 2047);
	ng = (stbsp__int32)((stbsp__uint64) bits >> 63);
	if (ng)
		d = -d;

	if (expo == 2047) // is nan or inf?
	{
		*start = (bits & ((((stbsp__uint64)1) << 52) - 1)) ? "NaN" : "Inf";
		*decimal_pos = STBSP__SPECIAL;
		*len = 3;
		return ng;
	}

	if (expo == 0) // is zero or denormal
	{
		if (((stbsp__uint64) bits << 1) == 0) // do zero
		{
			*decimal_pos = 1;
			*start = out;
			out[0] = '0';
			*len = 1;
			return ng;
		}
		// find the right expo for denormals
		{
			stbsp__int64 v = ((stbsp__uint64)1) << 51;
			while ((bits & v) == 0) {
				--expo;
				v >>= 1;
			}
		}
	}

	// find the decimal exponent as well as the decimal bits of the value
	{
		double ph, pl;

		// log10 estimate - very specifically tweaked to hit or undershoot by no more than 1 of log10 of all expos 1..2046
		tens = expo - 1023;
		tens = (tens < 0) ? ((tens * 617) / 2048) : (((tens * 1233) / 4096) + 1);

		// move the significant bits into position and stick them into an int
		stbsp__raise_to_power10(&ph, &pl, d, 18 - tens);

		// get full as much precision from double-double as possible
		stbsp__ddtoS64(bits, ph, pl);

		// check if we undershot
		if (((stbsp__uint64)bits) >= stbsp__tento19th)
			++tens;
	}

	// now do the rounding in integer land
	frac_digits = (frac_digits & 0x80000000) ? ((frac_digits & 0x7ffffff) + 1) : (tens + frac_digits);
	if ((frac_digits < 24)) {
		stbsp__uint32 dg = 1;
		if ((stbsp__uint64)bits >= stbsp__powten[9])
			dg = 10;
		while ((stbsp__uint64)bits >= stbsp__powten[dg]) {
			++dg;
			if (dg == 20)
				goto noround;
		}
		if (frac_digits < dg) {
			stbsp__uint64 r;
			// add 0.5 at the right position and round
			e = dg - frac_digits;
			if ((stbsp__uint32)e >= 24)
				goto noround;
			r = stbsp__powten[e];
			bits = bits + (r / 2);
			if ((stbsp__uint64)bits >= stbsp__powten[dg])
				++tens;
			bits /= r;
		}
noround:;
	}

	// kill long trailing runs of zeros
	if (bits) {
		stbsp__uint32 n;
		for (;;) {
			if (bits <= 0xffffffff)
				break;
			if (bits % 1000)
				goto donez;
			bits /= 1000;
		}
		n = (stbsp__uint32)bits;
		while ((n % 1000) == 0)
			n /= 1000;
		bits = n;
donez:;
	}

	// convert to string
	out += 64;
	e = 0;
	for (;;) {
		stbsp__uint32 n;
		char *o = out - 8;
		// do the conversion in chunks of U32s (avoid most 64-bit divides, worth it, constant denomiators be damned)
		if (bits >= 100000000) {
			n = (stbsp__uint32)(bits % 100000000);
			bits /= 100000000;
		} else {
			n = (stbsp__uint32)bits;
			bits = 0;
		}
		while (n) {
			out -= 2;
			*(stbsp__uint16 *)out = *(stbsp__uint16 *)&stbsp__digitpair.pair[(n % 100) * 2];
			n /= 100;
			e += 2;
		}
		if (bits == 0) {
			if ((e) && (out[0] == '0')) {
				++out;
				--e;
			}
			break;
		}
		while (out != o) {
			*--out = '0';
			++e;
		}
	}

	*decimal_pos = tens;
	*start = out;
	*len = e;
	return ng;
}

INTERNAL u8 CharacterLookup[] = "0123456789abcdefghijklmnopqrstuvwxyz";
INTERNAL u8 CharacterLookupUppercase[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
//
// TODO: Check the size of the specified buffer. Currently the function expects it to just be large enough.
INTERNAL String convert_double_to_string(u8 *buffer, s32 size, r64 number, s32 precision, b32 scientific, b32 hex, b32 uppercase, b32 keep_sign) {
	stbsp__int32 pos;
	char const *out;
	char tmp[512];
	stbsp__uint32 tmp_size = 512;

	u8 *lookup = uppercase ? CharacterLookupUppercase : CharacterLookup;

	stbsp__int32 sign = stbsp__real_to_str(&out, &tmp_size, tmp, &pos, number, precision);

	s32 written = 0;
	if (sign) append_buffer(buffer, &written, '-');
	else if (keep_sign) append_buffer(buffer, &written, '+');

	if (pos == STBSP__SPECIAL) {
		append_buffer(buffer, &written, out[0]);
		append_buffer(buffer, &written, out[1]);
		append_buffer(buffer, &written, out[2]);

		return {buffer, written};
	}

	if (hex) {
		stbsp__uint64 bits;
		stbsp__int32 expo;
		sign = stbsp__real_to_parts((stbsp__int64*)&bits, &expo, number);

		if (expo == -1023)
			expo = bits ? -1022 : 0;
		else
			bits |= ((stbsp__uint64)1) << 52;

		bits <<= 64 - 56;
		if (precision < 15)
			bits += (((stbsp__uint64)8) << 56) >> (precision * 4);

		append_buffer(buffer, &written, '0');
		append_buffer(buffer, &written, 'x');

		append_buffer(buffer, &written, lookup[(bits >> 60) & 15]);
		append_buffer(buffer, &written, '.');

		bits <<= 4;
		while (precision) {
			append_buffer(buffer, &written, lookup[(bits >> 60) & 15]);
			bits <<= 4;
			precision -= 1;
		}
		append_buffer(buffer, &written, uppercase ? 'P' : 'p');

		u8 exp_buffer[4];
		String exp_str = convert_signed_to_string(exp_buffer, 4, expo, 10, uppercase, true);

		for (s32 i = 0; i < exp_str.size; i += 1) {
			append_buffer(buffer, &written, exp_str.data[i]);
		}
	} else if (scientific) {
		s32 exp = pos - 1;
		u8 exp_buffer[4];
		String exp_str = convert_signed_to_string(exp_buffer, 4, exp, 10, 0, true);

		append_buffer(buffer, &written, out[0]);
		tmp_size -= 1;
		out += 1;

		append_buffer(buffer, &written, '.');
		while (tmp_size && precision) {
			append_buffer(buffer, &written, out[0]);
			out += 1;
			tmp_size -= 1;
			precision -= 1;
		}
		while (precision) {
			append_buffer(buffer, &written, '0');
			precision -= 1;
		}
		append_buffer(buffer, &written, uppercase ? 'E' : 'e');
		for (s32 i = 0; i < exp_str.size; i += 1) {
			append_buffer(buffer, &written, exp_str.data[i]);
		}
	} else {
		if (pos == 0) {
			append_buffer(buffer, &written, '0');
			append_buffer(buffer, &written, '.');
		} else if (pos > 0) {
			while (pos && tmp_size) {
				append_buffer(buffer, &written, out[0]);
				out += 1;
				tmp_size -= 1;
				pos -= 1;
			}
			while (pos) {
				append_buffer(buffer, &written, '0');
				pos -= 1;
			}
			append_buffer(buffer, &written, '.');
		} else {
			append_buffer(buffer, &written, '0');
			append_buffer(buffer, &written, '.');

			while (pos < 0 && precision) {
				append_buffer(buffer, &written, '0');
				pos += 1;
				precision -= 1;
			}
		}

		while (precision && tmp_size) {
			append_buffer(buffer, &written, out[0]);
			out += 1;
			tmp_size -= 1;
			precision -= 1;
		}

		while (precision) {
			append_buffer(buffer, &written, '0');
			precision -= 1;
		}
	}

	return {buffer, written};
}

void convert_to_ptr_string(u8 *buffer, s32 buffer_size, void *address) {
	assert(buffer_size >= 18);

	u64 number = (u64)address;

	buffer[0] = '0';
	buffer[1] = 'x';

	s32 end = 17;
	do {
		s64 quot = number / 16;
		s64 rem  = number % 16;

		buffer[end] = CharacterLookupUppercase[rem];
		end -= 1;
		number = quot;
	} while (number);

	while (end > 1) {
		buffer[end] = '0';
		end -= 1;
	}
}

String convert_signed_to_string(u8 *buffer, s32 buffer_size, s64 signed_number, s32 base, b32 uppercase, b32 keep_sign) {
	assert(buffer_size);

	b32 is_negative;
	u64 number;

	if (signed_number < 0) {
		is_negative = true;
		number = -signed_number;
	} else {
		is_negative = false;
		number = signed_number;
	}

	u8 *lookup = uppercase ? CharacterLookupUppercase : CharacterLookup;

	u8 *ptr = buffer + (buffer_size - 1);
	s32 written = 0;
	do {
		s64 quot = number / base;
		s64 rem  = number % base;

		*ptr = lookup[rem];
		ptr -= 1;
		written += 1;
		number = quot;
	} while (number && written < buffer_size);

	if (is_negative || keep_sign) {
		ptr[0] = is_negative ? '-' : '+';
		written += 1;
	} else {
		ptr += 1;
	}

	String result = {ptr, written};
	return result;
}

s32 convert_unsigned_to_string(u8 *buffer, s32 buffer_size, u64 number, s32 base, b32 uppercase) {
	assert(buffer_size);

	u8 *ptr = buffer + (buffer_size - 1);
	s32 written = 0;
	do {
		s64 quot = number / base;
		s64 rem  = number % base;

		*ptr = CharacterLookup[rem];
		ptr -= 1;
		written += 1;
		number = quot;
	} while (number && written < buffer_size);

	ptr += 1;
	copy_memory(buffer, ptr, written);

	return written;
}

s64 convert_string_to_s64(u8 *buffer, s32 buffer_size) {
	s64 result = 0;
	b32 is_negative = false;

	if (buffer_size == 0) {
		return result;
	}

	if (buffer[0] == '-') {
		is_negative = true;
		buffer += 1;
		buffer_size -= 1;
	}

	for (s32 i = 0; i < buffer_size; i += 1) {
		result *= 10;
		result += buffer[i] - '0';
	}

	if (is_negative) {
		result *= -1;
	}

	return result;
}

