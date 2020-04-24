#include <iostream>

extern "C"
{
    bool retBool(bool);
    int retInt(int);
}

int main()
{
    std::cout << retBool(true) << '\n';
    std::cout << retInt(42) << '\n';
}