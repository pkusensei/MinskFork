#include <iostream>
#include <memory>
#include <string>
#include <vector>

extern "C"
{
    void Main();
}

static auto strs = std::vector<std::unique_ptr<std::string>>();

extern "C" const char *input()
{
    auto result = std::string();
    std::getline(std::cin, result);
    strs.push_back(std::make_unique<std::string>(std::move(result)));
    return strs.back()->c_str();
}

extern "C" void print(const char *t)
{
    std::cout << t << '\n';
}

int main()
{
    Main();
}