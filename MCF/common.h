#pragma once
#include <tuple>
#include <any>

namespace MCF {

using TextSpan = std::tuple<int, int, int>;
using ValueType = std::any;

}