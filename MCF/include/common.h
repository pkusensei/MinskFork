#pragma once

#include <memory>
#include <string>
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
using std::vector;

using std::unique_ptr;
using std::make_unique;
using std::shared_ptr;
using std::make_shared;

using IntegerType = long; // HACK use long as interger type

inline IntegerType StringToInteger(const string& s)
{
	return std::stol(s);
}

constexpr auto NEW_LINE = '\n';

}//MCF