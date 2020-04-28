#include <iostream>
#include <memory>
#include <string>
#include <unordered_set>

struct UniqueStringHash
{
    size_t operator()(const std::unique_ptr<std::string> &s) const noexcept
    {
        return std::hash<std::string>{}(*s);
    }
};

struct UniqueStringEqual
{
    bool operator()(const std::unique_ptr<std::string> &s1,
                    const std::unique_ptr<std::string> &s2) const noexcept
    {
        return (*s1) == (*s2);
    }
};

static auto strs = std::unordered_set<std::unique_ptr<std::string>,
                                      UniqueStringHash, UniqueStringEqual>();

extern "C" const char *input()
{
    auto result = std::string();
    std::getline(std::cin, result);
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}

extern "C" const char *strConcat(const char *s1, const char *s2)
{
    auto result = std::string(s1) + s2;
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}
