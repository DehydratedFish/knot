/*******************************************************************************
 *
 ******************************************************************************/


#pragma once

#include "definitions.h"

#include <cstring>


#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

#define WINAPI   __stdcall
#define APIENTRY __stdcall
#define CALLBACK __stdcall

typedef struct WND      *HWND;
typedef struct DC       *HDC;
typedef struct INSTANCE *HINSTANCE;
typedef struct ICON     *HICON;
typedef struct CURSOR   *HCURSOR;
typedef struct BRUSH    *HBRUSH;
typedef struct MENU     *HMENU;
typedef struct PROCESS  *HPROCESS;
typedef struct GLRC     *HGLRC;

typedef int (__stdcall *PROC)();
typedef s64 (__stdcall *FARPROC)();

#ifdef _WIN64
typedef s64 sPtr;
typedef u64 uPtr;
#else
typedef s32 sPtr;
typedef u32 uPtr;
#endif // _WIN64

u32 const MAX_PATH = 260;
u32 const INFINITE = 0xFFFFFFFF;
void const *INVALID_HANDLE_VALUE = (void const*)-1;

u32 const WM_DESTROY = 0x0002;
u32 const WM_SIZE    = 0x0005;
u32 const WM_CLOSE   = 0x0010;
u32 const WM_QUIT    = 0x0012;

u32 const WM_KEYDOWN = 0x0100;
u32 const WM_KEYUP   = 0x0101;
u32 const WM_CHAR    = 0x0102;

u32 const WM_MOUSEMOVE  = 0x0200;
u32 const WM_MOUSEWHEEL = 0x020A;

u32 const VK_BACK    = 0x08;
u32 const VK_RETURN  = 0x0D;
u32 const VK_SHIFT   = 0x10;
u32 const VK_CONTROL = 0x11;
u32 const VK_MENU    = 0x12;
u32 const VK_END     = 0x23;
u32 const VK_HOME    = 0x24;
u32 const VK_LEFT    = 0x25;
u32 const VK_UP      = 0x26;
u32 const VK_RIGHT   = 0x27;
u32 const VK_DOWN    = 0x28;
u32 const VK_DELETE  = 0x2E;

#define LOWORD(v) ((u32)((v) & 0xFFFF))
#define HIWORD(v) ((u32)(((v) >> 16) & 0xFFFF))

#define GET_X_LPARAM(v) ((s16)LOWORD(v))
#define GET_Y_LPARAM(v) ((s16)HIWORD(v))

typedef sPtr (CALLBACK *WNDPROC)(HWND window, u32 msg, uPtr w_param, sPtr l_param);

struct OVERLAPPED {
    uPtr internal;
    uPtr internal_high;

    union {
        struct {
            u32 offset;
            u32 offset_high;
        };
        void *pointer;
    };
    void *event;
};

struct SECURITY_ATTRIBUTES {
    u32 length;
    void *security_descriptor;
    b32 inherit_handle;
};


#define WIN32_FUNC_DEF(ret) __declspec(dllimport) ret __stdcall

WIN32_FUNC_DEF(HDC)       GetDC(HWND wnd);
WIN32_FUNC_DEF(HPROCESS)  GetCurrentProcess();
WIN32_FUNC_DEF(u32)       GetModuleFileNameW(HINSTANCE module, wchar_t *file_name, u32 size);
WIN32_FUNC_DEF(HINSTANCE) GetModuleHandleW(wchar_t const *module_name);
WIN32_FUNC_DEF(void)      ExitProcess(u32);
WIN32_FUNC_DEF(b32)       QueryPerformanceFrequency(s64 *frequency);
WIN32_FUNC_DEF(wchar_t*)  GetCommandLineW();
WIN32_FUNC_DEF(wchar_t**) CommandLineToArgvW(wchar_t *cmd_line, s32 *num_args);
WIN32_FUNC_DEF(b32)       CloseHandle(void *object);
WIN32_FUNC_DEF(u32)       WaitForSingleObject(void *handle, u32 milliseconds);
WIN32_FUNC_DEF(void)      PostQuitMessage(int exit_code);
WIN32_FUNC_DEF(sPtr)      DefWindowProcW(HWND wnd, u32 msg, uPtr w_param, sPtr l_param);
WIN32_FUNC_DEF(u32)       GetLastError();

// LoadCursor
wchar_t const *IDC_ARROW = (wchar_t const*)(u16)32512;
WIN32_FUNC_DEF(HCURSOR) LoadCursorW(HINSTANCE instance, wchar_t const *cursor_name);

// RegisterClassW
u32 const CS_OWNDC = 0x0020;

struct WNDCLASSW {
    u32       style;
    WNDPROC   wnd_proc;
    int       cls_extra;
    int       wnd_extra;
    HINSTANCE instance;
    HICON     icon;
    HCURSOR   cursor;
    HBRUSH    background;
    wchar_t const *menu_name;
    wchar_t const *class_name;
};
WIN32_FUNC_DEF(u16) RegisterClassW(WNDCLASSW *wnd_class);

// CreateWindowExW
u32 const WS_OVERLAPPED  = 0x00000000;
u32 const WS_MAXIMIZEBOX = 0x00010000;
u32 const WS_MINIMIZEBOX = 0x00020000;
u32 const WS_THICKFRAME  = 0x00040000;
u32 const WS_SYSMENU     = 0x00080000;
u32 const WS_CAPTION     = 0x00C00000;
u32 const WS_VISIBLE     = 0x10000000;
u32 const WS_OVERLAPPEDWINDOW = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX;

s32 const CW_USEDEFAULT = 0x80000000;
WIN32_FUNC_DEF(HWND) CreateWindowExW(u32 ex_style, wchar_t const *class_name, wchar_t const *window_name, u32 style, int x, int y, int width, int height,
                                     HWND parent, HMENU menu, HINSTANCE instance, void *param);

// PeekMessageW
u32 const PM_REMOVE = 0x0001;

struct MSG {
    HWND wnd;
    u32  message;
    u32  w_param;
    s32  l_param;
    u32  time;
    V2i  pt;
    u32  private_;
};
WIN32_FUNC_DEF(b32) PeekMessageW(MSG *msg, HWND wnd, u32 msg_filter_min, u32 msg_filter_max, u32 remove_msg);
WIN32_FUNC_DEF(b32) TranslateMessage(MSG const *msg);
WIN32_FUNC_DEF(s32) DispatchMessageW(MSG const *msg);


WIN32_FUNC_DEF(HINSTANCE) LoadLibraryW(wchar_t const *lib_file_name);
WIN32_FUNC_DEF(FARPROC)   GetProcAddress(HINSTANCE module, char const *proc_name);

WIN32_FUNC_DEF(void*) LocalFree(void *mem);

// CreateThread
typedef u32 (WINAPI THREAD_START_ROUTINE)(void *thread_parameter);
// TODO: Missing __drv_aliasesMem on parameter here... does this do anything meaningful?
WIN32_FUNC_DEF(void*) CreateThread(SECURITY_ATTRIBUTES *thread_attributes, u32 stack_size, THREAD_START_ROUTINE *start_address, void *parameter, u32 creation_flags, u32 *thread_id);
WIN32_FUNC_DEF(b32)   TerminateThread(void *handle, u32 exit_code);

// VirtualAlloc
u32 const MEM_COMMIT  = 0x00001000;
u32 const MEM_RESERVE = 0x00002000;
u32 const MEM_RELEASE = 0x00008000;
u32 const PAGE_READWRITE = 0x04;
WIN32_FUNC_DEF(void*) VirtualAlloc(void *address, uPtr size, u32 allocation_type, u32 protect);
WIN32_FUNC_DEF(b32)   VirtualFree(void *adress, uPtr size, u32 free_type);

// GetStdHandle
u32 const STD_INPUT_HANDLE  = -10;
u32 const STD_OUTPUT_HANDLE = -11;
u32 const STD_ERROR_HANDLE  = -12;
WIN32_FUNC_DEF(void*) GetStdHandle(u32 std_handle);

// MessageBoxW
u32 const MB_OK        = 0x00000000;
u32 const MB_ICONERROR = 0x00000010;
WIN32_FUNC_DEF(int) MessageBoxW(HWND wnd, wchar_t const *text, wchar_t const *caption, u32 type);

// CreateFileW
u32 const GENERIC_WRITE = 0x40000000;
u32 const GENERIC_READ  = 0x80000000;

u32 const CREATE_ALWAYS = 2;
u32 const OPEN_EXISTING = 3;
u32 const OPEN_ALWAYS   = 4;

u32 const FILE_ATTRIBUTE_NORMAL = 128;
WIN32_FUNC_DEF(void*) CreateFileW(wchar_t *file_name, u32 desired_access, u32 share_mode, SECURITY_ATTRIBUTES *security_attributes, u32 creation_disposition, u32 flags_and_attributes, void *template_file);

WIN32_FUNC_DEF(b32) ReadFile(void *file, void *buffer, u32 number_of_bytes_to_read, u32 *number_of_bytes_read, OVERLAPPED *overlapped);
WIN32_FUNC_DEF(b32) WriteFile(void *file, void const*buffer, u32 number_of_bytes_to_write, u32 *number_of_bytes_written, OVERLAPPED *overlapped);

// SetFilePointer
u32 const FILE_BEGIN   = 0;
u32 const FILE_CURRENT = 1;
u32 const FILE_END     = 2;
WIN32_FUNC_DEF(u32) SetFilePointer(void *file, s32 distance_to_move, s32 *distance_to_move_high, u32 move_method);

// FindFirstFileW
struct WIN32_FIND_DATAW {
    u32 file_attributes;
    // NOTE: u64 instead of FILETIME
    // TODO: Does this work?
    u64 creation_time;
    u64 last_access_time;
    u64 last_write_time;
    // NOTE: u64 instead of two DWORDS with file size low and high.
    // TODO: Does this work?
    u32 file_size_high;
    u32 file_size_low;
    u32 reserved0;
    u32 reserved1;
    wchar_t file_name[MAX_PATH];
    wchar_t alternate_file_name[14];
    u32 file_type;
    u32 creator_type;
    u16 finder_flags;
};
WIN32_FUNC_DEF(void*) FindFirstFileW(wchar_t *file_name, WIN32_FIND_DATAW *find_file_data);
WIN32_FUNC_DEF(b32)   FindNextFileW(void *handle, WIN32_FIND_DATAW *find_file_data);
WIN32_FUNC_DEF(b32)   FindClose(void *handle);

WIN32_FUNC_DEF(b32) GetFileSizeEx(void *file, s64 *file_size);
WIN32_FUNC_DEF(b32) GetOverlappedResult(void *file, OVERLAPPED *overlapped, u32 *number_of_bytes_transferred, b32 wait);

WIN32_FUNC_DEF(PROC)  wglGetProcAddress(char const *unnamed);
WIN32_FUNC_DEF(HGLRC) wglCreateContext(HDC unnamed);
WIN32_FUNC_DEF(b32)   wglMakeCurrent(HDC unnamed1, HGLRC unnamed2);
WIN32_FUNC_DEF(b32)   wglDeleteContext(HGLRC unnamed);

WIN32_FUNC_DEF(b32) SwapBuffers(HDC unnamed);

WIN32_FUNC_DEF(void) DebugBreak();


// TODO: SymInitialize, SymFromAddr and SymGetLineFromAddr64 should be the UNICODE W variant.
WIN32_FUNC_DEF(b32) SymInitialize(HPROCESS process, wchar_t const *user_search_path, b32 invade_process);
WIN32_FUNC_DEF(b32) SymCleanup(HPROCESS process);
WIN32_FUNC_DEF(u16) RtlCaptureStackBackTrace(u32 frames_to_skip, u32 frames_to_capture, void *back_trace, u32 *back_trace_hash);

// SymFromAddr
struct SYMBOL_INFO {
    u32 size_of_struct;
    u32 type_index;
    u64 reserved[2];
    u32 index;
    u32 size;
    u64 mod_base;
    u32 flags;
    u64 value;
    u64 address;
    u32 reg; // register
    u32 scope;
    u32 tag;
    u32 name_len;
    u32 max_name_len;
    char name[1];
};
WIN32_FUNC_DEF(s32) SymFromAddr(HPROCESS process, u64 address, u64 *displacement, SYMBOL_INFO *symbol);

// SymGetLineFromAddr64
struct IMAGEHLP_LINE64 {
    u32   size_of_struct;
    void *key;
    u32   line_number;
    char *file_name;
    u64   address;
};
WIN32_FUNC_DEF(b32) SymGetLineFromAddr64(HPROCESS process, u64 addr, u32 *displacement, IMAGEHLP_LINE64 *line64);

// ChoosePixelFormat
u32 const PFD_DOUBLEBUFFER   = 0x00000001;
u32 const PFD_DRAW_TO_WINDOW = 0x00000004;
u32 const PFD_SUPPORT_OPENGL = 0x00000020;

u32 const PFD_TYPE_RGBA  = 0;
u32 const PFD_MAIN_PLANE = 0;

struct PIXELFORMATDESCRIPTOR {
    u16 size;
    u16 version;
    u32 flags;
    u8  pixel_type;
    u8  color_bits;
    u8  red_bits;
    u8  red_shift;
    u8  green_bits;
    u8  green_shift;
    u8  blue_bits;
    u8  blue_shift;
    u8  alpha_bits;
    u8  alpha_shift;
    u8  accum_bits;
    u8  accum_red_bits;
    u8  accum_green_bits;
    u8  accum_blue_bits;
    u8  accum_aplha_bits;
    u8  depth_bits;
    u8  stelncil_bits;
    u8  aux_buffers;
    u8  layer_type;
    u8  reserved;
    u32 layer_mask;
    u32 visible_mask;
    u32 damage_mask;
};
WIN32_FUNC_DEF(int) ChoosePixelFormat(HDC hdc, PIXELFORMATDESCRIPTOR const *pfd);
WIN32_FUNC_DEF(b32) SetPixelFormat(HDC hdc, int format, PIXELFORMATDESCRIPTOR *pfd);

#ifdef __cplusplus
}
#endif // __cplusplus

