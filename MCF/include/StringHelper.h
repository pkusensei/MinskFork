#pragma once

#include <algorithm>
#include <cctype>
#include <sstream>

// HACK force intellisense to work with <concepts>
#ifndef __cpp_lib_concepts
#define __cpp_lib_concepts
#include <concepts>
#undef __cpp_lib_concepts
#endif

#include "common.h"

namespace MCF {

/// string helpers
MCF_API string_view TrimString(string_view text);
MCF_API string_view TrimStringStart(string_view text);
MCF_API string_view TrimStringEnd(string_view text);
string& StringReplaceAll(string& data, string_view from, string_view to);

template<typename... Args>
string BuildStringFrom(Args&&... args)
{
	std::stringstream ss;
	(ss << ... << std::forward<Args>(args));
	return ss.str();
}

template<typename T>
requires std::same_as<char, typename T::value_type>
&& requires (const T& t) { std::cbegin(t); }
bool StringIsBlank(const T& text)
{
	using It = decltype(std::cbegin(text));
	return text.empty() ||
		std::all_of<It, int(*)(int)>(std::cbegin(text), std::cend(text), std::isspace); // NOTE just why? 
}

template<typename It>
requires std::same_as<char, typename std::iterator_traits<It>::value_type::value_type>
string StringJoin(It begin, It end, char seperator = ' ')
{
	auto result = string();
	for (; begin != end; ++begin)
	{
		result += std::string(*begin) + seperator;
	}
	if (!result.empty())
		return result.erase(result.length() - 1);
	return result;
}


template<typename It>
requires std::same_as<char, typename std::iterator_traits<It>::value_type>
vector<string_view> StringSplit(It begin, It end, char delimiter = ' ')
{
	auto result = vector<string_view>();
	if (begin == end)
		return result;

	auto it = std::find(begin, end, delimiter);
	if (it == end)
	{
		result.emplace_back(&*begin, end - begin);
	} else
	{
		result.emplace_back(&*begin, it - begin);
		auto rest = StringSplit(++it, end, delimiter);
		result.insert(result.end(), rest.begin(), rest.end());
	}
	return result;
}

}//MCF