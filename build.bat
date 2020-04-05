@echo off

cd build
cmake -G Ninja ..
cmake --build .

cd ..