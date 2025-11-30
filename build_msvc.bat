@echo off

IF NOT EXIST "build" mkdir build
pushd build

SET libs=User32.lib Kernel32.lib Shell32.lib Gdi32.lib Ole32.lib Shlwapi.lib DbgHelp.lib

SET sources=..\..\source\knot.cpp ..\..\source\parser.cpp ..\..\source\bytecode.cpp
SET mountain=..\..\dependencies\mountain\source\win32\platform.cpp ..\..\dependencies\mountain\source\io.cpp ..\..\dependencies\mountain\source\utf.cpp

IF NOT EXIST "debug" mkdir debug
pushd debug

IF NOT EXIST "build_files" mkdir build_files

cl /D"DEVELOPER" /D"BOUNDS_CHECKING" /I.\..\..\dependencies\mountain\source /I.\..\..\source /FC /Zi /nologo /W2 /permissive- /Fdbuild_files\ /Fobuild_files\ /Fe"knot.exe" %sources% %mountain% /link /SUBSYSTEM:CONSOLE /INCREMENTAL:NO %libs%
IF %ERRORLEVEL% NEQ 0 EXIT /b %ERRORLEVEL%

popd
popd

