#include "io.h"

#include <cstdarg>

#include "string2.h"
#include "platform.h"


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

String convert_signed_to_string(u8 *buffer, s32 buffer_size, s64 signed_number, s32 base, b32 uppercase, b32 keep_sign) {
    assert(buffer_size >= 20);

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

    u8 *ptr = buffer + buffer_size;
    s32 written = 0;
    do {
        s64 quot = number / base;
        s64 rem  = number % base;

        ptr -= 1;
        *ptr = lookup[rem];
        written += 1;
        number = quot;
    } while (number);

    if (is_negative || keep_sign) {
        ptr -= 1;
        *ptr = is_negative ? '-' : '+';
        written += 1;
    }

    String result = {ptr, written};
    return result;
}

String convert_unsigned_to_string(u8 *buffer, s32 buffer_size, u64 number, s32 base, b32 uppercase) {
    assert(buffer_size >= 20);

    u8 *ptr = buffer + buffer_size;
    s32 written = 0;

    u8 *lookup = uppercase ? CharacterLookupUppercase : CharacterLookup;

    do {
        u64 quot = number / base;
        u64 rem  = number % base;

        ptr -= 1;
        *ptr = lookup[rem];
        written += 1;
        number = quot;
    } while (number);

    String result = {ptr, written};
    return result;
}

s64 format(StringBuilder *builder, char const *fmt, va_list args) {
    while (fmt[0]) {
        if (fmt[0] == '%' && fmt[1]) {
            fmt += 1;

            switch (fmt[0]) {
            case 'd': {
                s32 const buffer_size = 32;
                u8 buffer[buffer_size];
                String ref = convert_signed_to_string(buffer, buffer_size, va_arg(args, s32));

                append(builder, ref);
            } break;

            case 'D': {
                u32 const buffer_size = 32;
                u8 buffer[buffer_size];
                String ref = convert_signed_to_string(buffer, buffer_size, va_arg(args, s64));

                append(builder, ref);
            } break;

            case 'u': {
                u32 const buffer_size = 32;
                u8 buffer[buffer_size];
                String ref = convert_unsigned_to_string(buffer, buffer_size, va_arg(args, u32));

                append(builder, ref);
            } break;

            case 'U': {
                u32 const buffer_size = 32;
                u8 buffer[buffer_size];
                String ref = convert_unsigned_to_string(buffer, buffer_size, va_arg(args, u64));

                append(builder, ref);
            } break;

            case 'f': {
                u32 const buffer_size = 128; // TODO: is this big enough?
                u8 buffer[buffer_size];
                String ref = convert_double_to_string(buffer, buffer_size, va_arg(args, r64), 6, false, false, false, false);

                append(builder, ref);
            } break;

            case 'p': {
                u32 const buffer_size = 32;
                u8 buffer[buffer_size];

                void *ptr = va_arg(args, void*);
                String ref = convert_unsigned_to_string(buffer, buffer_size, (u64)ptr, 16);

                while (ref.size < 16) {
                    ref.data -= 1;
                    ref.size += 1;
                    ref[0] = '0';
                }

                ref.data -= 2;
                ref.size += 2;
                ref[0] = '0';
                ref[1] = 'x';

                append(builder, ref);
            } break;

            case 's': {
                char *str = va_arg(args, char *);
                s64 length = c_string_length(str);

                append(builder, {(u8*)str, length});
            } break;

            case 'S': {
                String str = va_arg(args, String);

                append(builder, str);
            } break;
            }

            fmt += 1;
            continue;
        }
        append(builder, (u8)fmt[0]);

        fmt += 1;
    }

    return builder->total_size;
}

s64 format(StringBuilder *builder, char const *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    s64 result = format(builder, fmt, args);
    va_end(args);
    
    return result;
}

String format(char const *fmt, ...) {
    StringBuilder builder = {};
    DEFER(destroy(&builder));

    va_list args;
    va_start(args, fmt);
    format(&builder, fmt, args);
    va_end(args);

    return to_allocated_string(&builder);
}

String t_format(char const *fmt, ...) {
    StringBuilder builder = {};
    DEFER(destroy(&builder));

    va_list args;
    va_start(args, fmt);
    format(&builder, fmt, args);
    va_end(args);

    return to_allocated_string(&builder, temporary_allocator());
}

INTERNAL s64 write(PlatformFile *file, void const *buffer, s64 size) {
    s64 written = 0;

    if (file->write_buffer.memory) {
        s64 space = file->write_buffer.alloc - file->write_buffer.used;
        if (space < size) {
            platform_flush_write_buffer(file);

            written += platform_write(file, buffer, size);
        } else {
            copy_memory(file->write_buffer.memory + file->write_buffer.used, buffer, size);
            file->write_buffer.used += size;

            written += size;
        }
    } else {
        written = platform_write(file, buffer, size);
    }

    return written;
}

INTERNAL b32 write_byte(PlatformFile *file, u8 value) {
    if (file->write_buffer.memory) {
        if (file->write_buffer.used == file->write_buffer.alloc) {
            platform_flush_write_buffer(file);
        }
        file->write_buffer.memory[file->write_buffer.used] = value;
        file->write_buffer.used += 1;

        if (value == '\n') {
            platform_flush_write_buffer(file);
        }
    } else {
        if (platform_write(file, &value, 1) != 1) return false;
    }

    return true;
}

INTERNAL s64 print_internal(PlatformFile *file, char const *fmt, va_list args) {
    assert(file);

    s64 written = 0;
    if (!file->open) return written;

    while (fmt[0]) {
        if (fmt[0] == '%' && fmt[1]) {
            fmt += 1;

            switch (fmt[0]) {
            case 'd': {
                u32 const buffer_size = 32;
                u8 buffer[buffer_size];
                String ref = convert_signed_to_string(buffer, buffer_size, va_arg(args, s32));

                s64 this_write = write(file, ref.data, ref.size);
                written += this_write;
                if (this_write < ref.size) return written;
            } break;

            case 'D': {
                u32 const buffer_size = 32;
                u8 buffer[buffer_size];
                String ref = convert_signed_to_string(buffer, buffer_size, va_arg(args, s64));

                s64 this_write = write(file, ref.data, ref.size);
                written += this_write;
                if (this_write < ref.size) return written;
            } break;

            case 'u': {
                u32 const buffer_size = 32;
                u8 buffer[buffer_size];
                String ref = convert_unsigned_to_string(buffer, buffer_size, va_arg(args, u32));

                s64 this_write = write(file, ref.data, ref.size);
                written += this_write;
                if (this_write < ref.size) return written;
            } break;

            case 'U': {
                u32 const buffer_size = 32;
                u8 buffer[buffer_size];

                u64 value = va_arg(args, u64);
                String ref = convert_unsigned_to_string(buffer, buffer_size, value);

                s64 this_write = write(file, ref.data, ref.size);
                written += this_write;
                if (this_write < ref.size) return written;
            } break;

            case 'p': {
                u32 const buffer_size = 32;
                u8 buffer[buffer_size];

                void *ptr = va_arg(args, void*);
                String ref = convert_unsigned_to_string(buffer, buffer_size, (u64)ptr, 16);

                while (ref.size < 16) {
                    ref.data -= 1;
                    ref.size += 1;
                    ref[0] = '0';
                }

                ref.data -= 2;
                ref.size += 2;
                ref[0] = '0';
                ref[1] = 'x';

                s64 this_write = write(file, ref.data, ref.size);
                written += this_write;
                if (this_write < ref.size) return written;
            } break;

            case 'f': {
                u32 const buffer_size = 128; // TODO: is this big enough?
                u8 buffer[buffer_size];
                String ref = convert_double_to_string(buffer, buffer_size, va_arg(args, r64), 6, false, false, false, false);

                s64 this_write = write(file, ref.data, ref.size);
                written += this_write;
                if (this_write < ref.size) return written;
            } break;

            case 'c': {
                u8 c = va_arg(args, int);

                if (!write_byte(file, c)) return written;
            } break;

            case 's': {
                char *str = va_arg(args, char *);
                s64 size  = c_string_length(str);

                s64 this_write = write(file, str, size);
                written += this_write;
                if (this_write < size) return written;
            } break;

            case 'S': {
                String str = va_arg(args, String);

                s64 this_write = write(file, str.data, str.size);
                written += this_write;
                if (this_write < str.size) return written;
            } break;
            }
        } else {
            if (!write_byte(file, fmt[0])) return written;
            written += 1;
        }
        fmt += 1;
    }

    return written;
}

s64 print(char const *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    s64 written = print_internal(Console.out, fmt, args);

    va_end(args);

    return written;
}


/******************************************************************************
 *
 * Section for the floating point conversion stuff... this is very complex and
 * for now I just use the implementation from Sean Barrett's STB libraries.
 *
 *****************************************************************************/

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

static void append_buffer(u8 *buffer, s64 *size, u32 c) {
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

String convert_double_to_string(u8 *buffer, s32 size, r64 number, s32 precision, b32 scientific, b32 hex, b32 uppercase, b32 keep_sign) {
    stbsp__int32 pos;
    char const *out;
    char tmp[512];
    stbsp__uint32 tmp_size = 512;

    u8 *lookup = uppercase ? CharacterLookupUppercase : CharacterLookup;

    stbsp__int32 sign = stbsp__real_to_str(&out, &tmp_size, tmp, &pos, number, precision);

    s64 written = 0;
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
