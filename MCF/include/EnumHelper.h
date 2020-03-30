#pragma once

#include <type_traits>

namespace MCF {

//HACK 
//since there's no reflection 

//these help to iterate over enums
template<typename T, typename = std::enable_if_t<std::is_enum_v<T>>>
T& operator++(T& value)
{
	auto tmp = std::underlying_type_t<T>(value);
	value = static_cast<T>(tmp + 1);
	return value;
}

template<typename T, typename = std::enable_if_t<std::is_enum_v<T>>>
T& operator++(T& value, int)
{
	return ++value;
}

//collects all enum values into one vector
template<typename T, size_t N, typename = std::enable_if_t<std::is_enum_v<T>>>
const auto& GetAllEnumValue(T start, T end)
{
	auto build = [start, end]()constexpr
	{
		auto result = std::array<T, N>();
		for (auto [i, e] = std::make_pair(0, start); e != end; ++i, ++e)
			result.at(i) = e;
		result.at(N - 1) = end;
		return result;
	};
	static const auto enums = build();
	return enums;
}

}//MCF