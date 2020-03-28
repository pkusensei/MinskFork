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
MCF_API string StringJoin(const vector<string>& strs, const char seperator = ' ');
string& StringReplaceAll(string& data, string_view from, string_view to);

template<typename... Args>
string BuildStringFrom(Args&&... args)
{
	std::stringstream ss;
	(ss << ... << args);
	return ss.str();
}

template<typename T>
bool StringIsBlank(const T& text)
{
	using It = decltype(text.cbegin()); // NOTE just why? 
	return text.empty() ||
		std::all_of<It, int(*)(int)>(text.cbegin(), text.cend(), std::isspace);
}

template<typename It>
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