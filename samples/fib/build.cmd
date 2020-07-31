@echo off

clang-cl -fuse-ld=lld -Z7 -MTd /std:c++17 ..\lib.cpp fib.obj -o fib.exe

fib.exe