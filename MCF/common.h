#pragma once

#include <memory>
#include <string>
#include <vector>

#pragma warning(disable: 4251)

#ifdef MCF_EXPORTS
#define MCF_API __declspec(dllexport)
#else
#define MCF_API __declspec(dllimport)
#endif

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