#include "helpers.h"

#include <algorithm>
#include <cctype>
#include <sstream>

namespace MCF {

bool StringStartsWith(string_view sample, string_view beginning)
{
	if (sample.length() < beginning.length())return false;
	return std::equal(beginning.begin(), beginning.end(), sample.begin());
}

bool StringEndsWith(string_view sample, string_view ending)
{
	if (sample.length() < ending.length()) return false;
	return std::equal(ending.rbegin(), ending.rend(), sample.rbegin());
}

string_view TrimString(string_view text)
{
	return TrimStringStart(TrimStringEnd(text));
}

string_view TrimStringStart(string_view text)
{
	auto it = find_if(text.cbegin(), text.cend(),
		[](char c) { return !std::isspace(c); });
	if (it == text.cend())
		return string_view();
	text.remove_prefix(it - text.cbegin());
	return text;
}

string_view TrimStringEnd(string_view text)
{
	auto it = find_if(text.crbegin(), text.crend(),
		[](char c) { return !std::isspace(c); });
	if (it == text.crend())
		return string_view();
	text.remove_suffix(it - text.crbegin());
	return text;
}

string StringJoin(const vector<string>& strs, const char seperator)
{
	auto result = string();
	for (const auto& it : strs)
	{
		result += it + seperator;
	}
	if (!result.empty())
		result.erase(result.length() - 1);
	return result;
}

string& StringReplaceAll(string& data, string_view from, string_view to)
{
	if (data.empty()) return data;
	auto pos = data.find(from);
	while (pos != string::npos)
	{
		data.replace(pos, from.length(), to);
		pos = data.find(from, pos + to.length());
	}
	return data;
}

}//MCF