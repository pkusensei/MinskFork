#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <unordered_set>

using IntegerType = int;

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

extern "C" const char *boolToStr(bool v)
{
    std::string result = v ? "True" : "False";
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}

extern "C" const char *intToStr(IntegerType v)
{
    auto result = std::to_string(v);
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}

extern "C" bool strToBool(const char *s) noexcept
{
    return std::strcmp(s, "true") == 0 || std::strcmp(s, "True") == 0;
}

extern "C" IntegerType strToInt(const char *s)
{
    return std::stoi(s);
}

extern "C" const char *input()
{
    auto result = std::string();
    std::getline(std::cin, result);
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}

extern "C" IntegerType rnd(IntegerType max)
{
    static auto rd = std::random_device();
    auto mt = std::mt19937(rd());
    auto dist = std::uniform_int_distribution<IntegerType>(0, max);

    return dist(mt);
}

extern "C" const char *strConcat(const char *s1, const char *s2)
{
    auto result = std::string(s1) + s2;
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}
