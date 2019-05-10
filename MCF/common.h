#pragma once

#include <string>
#include <vector>

#ifdef MCF_EXPORTS
#define MCF_API __declspec(dllexport)
#else
#define MCF_API __declspec(dllimport)
#endif

namespace MCF {

using std::string;
using std::unique_ptr;
using std::make_unique;
using std::vector;

using IntegerType = long; // HACK use long as interger type

inline IntegerType StringToInteger(const string& s)
{
	return std::stol(s);
}

}//MCF