#include <iostream>

extern "C"
{
    int test();
}

int main()
{
    std::cout << test() << '\n';
}