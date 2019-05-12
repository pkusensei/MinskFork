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
T& operator++(T& value, int c)
{
	return ++value;
}

//collects all enum values into one vector
template<typename T, typename = std::enable_if_t<std::is_enum_v<T>>>
decltype(auto) GetAllEnumValue(const T& start, const T& end)
{
	auto result = vector<T>();
	for (auto i = start; i != end; i++)
		result.emplace_back(i);
	result.emplace_back(end);
	result.shrink_to_fit();
	return result;
}

}//MCF