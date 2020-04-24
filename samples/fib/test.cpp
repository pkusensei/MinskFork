#include <iostream>

extern "C"
{
    bool retBool();
    int retInt();
}

int main()
{
    std::cout << retBool() << '\n';
    std::cout << retInt() << '\n';
}