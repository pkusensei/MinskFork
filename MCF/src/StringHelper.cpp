#include "StringHelper.h"

#include <algorithm>
#include <sstream>

namespace MCF {

string_view TrimString(string_view text)
{
	return TrimStringStart(TrimStringEnd(text));
}

string_view TrimStringStart(string_view text)
{
	auto it = std::find_if(text.cbegin(), text.cend(),
						   [](char c) { return !std::isspace(c); });
	if (it == text.cend())
		return string_view();
	text.remove_prefix(it - text.cbegin());
	return text;
}

string_view TrimStringEnd(string_view text)
{
	auto it = std::find_if(text.crbegin(), text.crend(),
						   [](char c) { return !std::isspace(c); });
	if (it == text.crend())
		return string_view();
	text.remove_suffix(it - text.crbegin());
	return text;
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