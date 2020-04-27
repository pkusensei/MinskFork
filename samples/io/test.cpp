#include <iostream>
#include <memory>
#include <string>
#include <unordered_set>

static auto strs = std::unordered_set<std::unique_ptr<std::string>>();

extern "C" const char *input()
{
    auto result = std::string();
    std::getline(std::cin, result);
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}

extern "C" void print(const char *t)
{
    std::cout << t << '\n';
}

extern "C" const char *strConcat(const char *s1, const char *s2)
{
    auto result = std::string(s1) + s2;
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}
