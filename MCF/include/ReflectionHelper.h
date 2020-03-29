#pragma once

#include <type_traits>

namespace MCF {

//HACK 
//since there's no reflection 
//these help to reduce redundancy in GetChildren and like.

template<typename Base, typename Derived,
	typename = std::enable_if_t<std::is_base_of_v<Base, Derived>>>
	decltype(auto) MakeVecOfRaw(const unique_ptr<Derived>& ptr)
{
	return vector<const Base*>{ptr.get()};
}

template<typename Base, typename Derived,
	typename = std::enable_if_t<std::is_base_of_v<Base, Derived>>>
	decltype(auto) MakeVecOfRaw(const Derived& value)
{
	return vector<const Base*>{&value};
}

template<typename Base, typename T, typename...Args>
decltype(auto) MakeVecOfRaw(const T& t, Args&... args)
{
	auto result = MakeVecOfRaw<Base>(t);
	auto rest = MakeVecOfRaw<Base>(args...);
	result.insert(result.end(), rest.begin(), rest.end());
	return result;
}

//takes iterators begin and end from a container<unique_ptr<Derived>>
template<typename Base, typename Derived, typename It,
	typename = std::enable_if_t<
	std::is_same_v<Derived,
	typename std::iterator_traits<It>::value_type::element_type >>>
	decltype(auto) MakeVecOfRaw(It start, It end)
{
	auto result = vector<const Base*>();
	for (auto it = start; it != end; ++it)
		result.emplace_back(it->get());
	return result;
}

}