#pragma once
#include <tuple>
#include <any>

#ifdef MCF_EXPORTS
#define MCF_API __declspec(dllexport)
#else
#define MCF_API __declspec(dllimport)
#endif

namespace MCF {

using TextSpan = std::tuple<int, int, int>;
using ValueType = std::any;

}//MCF