#pragma once

#include <cassert>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#ifdef MCF_EXPORTS
#ifdef _MSC_VER
#define MCF_API __declspec(dllexport)
#elif defined __GNUC__
#define MCF_API __attribute__ ((visibility("default")))
#endif // _MSC_VER
#else
#ifdef _MSC_VER
#define MCF_API __declspec(dllimport)
#elif defined __GNUC__
#define MCF_API __attribute__ ((visibility ("hidden")))
#endif // _MSC_VER
#endif // MCF_EXPORTS

namespace MCF {

using std::string;
using std::string_view;
using std::vector;

using std::unique_ptr;
using std::make_unique;
using std::shared_ptr;
using std::make_shared;

using IntegerType = int;

inline constexpr auto NEW_LINE = '\n';

inline IntegerType StringToInteger(string_view s)
{
	return std::stoi(string(s));
}

template<typename To, typename From>
requires std::derived_from<To, From>
unique_ptr<To> StaticUniquePtrCast(unique_ptr<From> from)noexcept
{
	return unique_ptr<To>{static_cast<To*>(from.release())};
}

template<typename Derived, typename Ret = Derived, typename T>
unique_ptr<Ret> UniqueClone(const T* p)
{
	assert(p && "Can only clone valid ptr.");
	return make_unique<Derived>(static_cast<const Derived&>(*p));
}

}//MCF