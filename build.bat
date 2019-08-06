@echo off


pushd build

set compilerFlags=-Od -MTd -nologo -Oi -GR- -EHa- -WX -W4 -wd4101 -wd4702 -wd4005 -wd4505 -wd4456 -wd4201 -wd4100 -wd4189 -wd4204 -wd4459 -Zi -FC -I"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.21.27702\include" -I"C:\Program Files (x86)\Windows Kits\10\Include\10.0.17763.0\ucrt" -I"C:\Program Files (x86)\Windows Kits\10\Include\10.0.17763.0\um" -I"C:\Program Files (x86)\Windows Kits\10\Include\10.0.17763.0\shared"

set linkerFlags=-incremental:no -opt:ref OpenGL32.lib Winmm.lib user32.lib Gdi32.lib /LIBPATH:"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.21.27702\lib\x64" /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.17763.0\um\x64" /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.17763.0\ucrt\x64"


cl %compilerFlags% ..\code\win32_main.c /link %linkerFlags%

popd


pushd data

rem ..\build\win32_main.exe
rem start test.bmp

popd