#pragma once

#include <type_traits>

namespace MCF {

//HACK 
//since there's no reflection 
//these help to reduce redundancy in GetChildren and like
template<typename Base, typename Derived,
	typename = std::enable_if_t<std::is_base_of_v<Base, Derived>>>
	decltype(auto) MakeVecOfRaw(const unique_ptr<Derived>& ptr)
{
	return vector<Base*>{ptr.get()};
}

template<typename Base, typename Derived,
	typename = std::enable_if_t<std::is_base_of_v<Base, Derived>>>
	decltype(auto) MakeVecOfRaw(const Derived& value)
{
	return vector<Base*>{&value};
}

template<typename Base, typename Derived, typename...Args,
	typename = std::enable_if_t<std::is_base_of_v<Base, Derived>>>
	decltype(auto) MakeVecOfRaw(const unique_ptr<Derived>& ptr, Args&... args)
{
	auto result = MakeVecOfRaw<Base>(ptr);
	auto rest = MakeVecOfRaw<Base>(args...);
	result.insert(result.end(), rest.begin(), rest.end());
	return result;
}

template<typename Base, typename Derived, typename...Args,
	typename = std::enable_if_t<std::is_base_of_v<Base, Derived>>>
	decltype(auto) MakeVecOfRaw(const Derived& value, Args&... args)
{
	auto result = MakeVecOfRaw<Base>(value);
	auto rest = MakeVecOfRaw<Base>(args...);
	result.insert(result.end(), rest.begin(), rest.end());
	return result;
}

//takes iterators begin and end from a container<unique_ptr<Derived>>
template<typename Base, typename Derived, typename Iter,
	typename = std::enable_if_t<std::is_base_of_v<Base, Derived>>>
	decltype(auto) MakeVecOfRaw(Iter start, Iter end)
{
	auto result = vector<Base*>();
	for (auto it = start; it != end; ++it)
		result.emplace_back(it->get());
	return result;
}

}