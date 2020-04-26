#include <iostream>

extern "C"
{
    int retInt(int);
}

extern "C" void print(const char* t)
{
    std::cout << t << '\n';
}

int main()
{
    std::cout << retInt(42) << '\n';
}