@echo off

clang-cl -fuse-ld=lld -Z7 -MTd /std:c++17 ..\lib.cpp hello.obj -o hello.exe

hello.exe