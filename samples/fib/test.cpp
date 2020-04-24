#include <iostream>

extern "C"
{
    int retInt(int);
}

int main()
{
    std::cout << retInt(42) << '\n';
}