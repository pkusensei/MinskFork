#pragma once

#include <type_traits>

namespace MCF {

//HACK 
//since there's no reflection 
//these help to reduce redundancy in GetChildren and like.

template<typename Base, typename Derived, template<typename...>typename Ptr>
requires std::derived_from<Derived, Base>
decltype(auto) MakeVecOfRaw(const Ptr<Derived>& ptr)
{
	return ptr == nullptr ? vector<const Base*>{} : vector<const Base*>{ &(*ptr) };
}

template<typename Base, typename Derived>
requires std::derived_from<Derived, Base>
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
template<typename Base, typename Derived, typename It>
requires std::same_as<Derived, typename std::iterator_traits<It>::value_type::element_type>
&& std::derived_from<Derived, Base>
decltype(auto) MakeVecOfRaw(It start, It end)
{
	auto result = vector<const Base*>{};
	for (auto it = start; it != end; ++it)
		result.emplace_back(&**it);
	return result;
}

}