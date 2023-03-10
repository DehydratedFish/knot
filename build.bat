@echo off

IF NOT EXIST "build" mkdir build
pushd build

SET libs=User32.lib Kernel32.lib Shlwapi.lib DbgHelp.lib

SET sources=..\source\knot.cpp ..\source\win32\platform.cpp ..\source\parser.cpp

IF NOT EXIST "build_files" mkdir build_files

cl /D"DEVELOPER" /D"BOUNDS_CHECKING" /I.\..\source /FC /Zi /nologo /W2 /GS- /permissive- /Fdbuild_files\ /Fobuild_files\ /Fe"knot.exe" %sources% /link /SUBSYSTEM:CONSOLE /NODEFAULTLIB /INCREMENTAL:NO %libs%

popd

