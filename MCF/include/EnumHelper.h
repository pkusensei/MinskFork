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
template<typename T, typename = std::enable_if_t<std::is_enum_v<T>>>
const auto& GetAllEnumValue(T start, T end)
{
	auto build = [start, end]()
	{
		auto result = std::vector<T>();
		for (auto i = start; i != end; i++)
			result.push_back(i);
		result.push_back(end);
		result.shrink_to_fit();
		return result;
	};
	static const auto vec = build();
	return vec;
}

}//MCF