@echo off

cd build

cmake -G Ninja -DCMAKE_TOOLCHAIN_FILE=D:/Coding/vcpkg/scripts/buildsystems/vcpkg.cmake  -DVCPKG_TARGET_TRIPLET=x64-windows ..
cmake --build .

ctest

cd ..