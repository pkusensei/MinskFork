#pragma once

#include <algorithm>
#include <cctype>
#include <sstream>

#include "common.h"

namespace MCF {

/// string helpers
MCF_API bool StringStartsWith(string_view sample, string_view beginning);
MCF_API bool StringEndsWith(string_view sample, string_view ending);
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
bool StringIsBlank(const T& text)
{
	using It = decltype(text.cbegin());
	static_assert(std::is_same_v<char,
		typename std::iterator_traits<It>::value_type>);

	return text.empty() ||
		std::all_of<It, int(*)(int)>(text.cbegin(), text.cend(), std::isspace); // NOTE just why? 
}

template<typename It>
string StringJoin(It begin, It end, char seperator = ' ')
{
	static_assert(std::is_same_v<char,
		typename std::iterator_traits<It>::value_type::value_type>);

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
vector<string_view> StringSplit(It begin, It end, char delimiter = ' ')
{
	static_assert(std::is_same_v<char,
		typename std::iterator_traits<It>::value_type>);

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