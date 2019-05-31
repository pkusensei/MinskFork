#include "stdafx.h"
#include "helpers.h"

#include <algorithm>
#include <cctype>
#include <sstream>

namespace MCF {


bool StringStartsWith(const string& sample, const string& beginning)
{
	if (sample.length() < beginning.length())return false;
	return std::equal(beginning.begin(), beginning.end(), sample.begin());
}

bool StringEndsWith(const string& sample, const string& ending)
{
	if (sample.length() < ending.length()) return false;
	return std::equal(ending.rbegin(), ending.rend(), sample.rbegin());
}

string TrimString(const string& text)
{
	return TrimStringStart(TrimStringEnd(text));
}

string TrimStringStart(const string& text)
{
	auto result = text;
	result.erase(result.begin(), std::find_if(result.begin(), result.end(),
		[](char ch) { return !std::isspace(ch); }));
	return result;
}

string TrimStringEnd(const string& text)
{
	auto result = text;
	result.erase(std::find_if(result.rbegin(), result.rend(),
		[](char ch) { return !std::isspace(ch); }).base(), result.end());
	return result;
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

vector<string> StringSplit(const string& s, const char delimiter)
{
	auto result = vector<string>();
	auto token = string();
	auto isstream = std::istringstream(s);
	while (std::getline(isstream, token, delimiter))
	{
		result.emplace_back(token);
	}
	return result;
}

void StringReplaceAll(string& data, const string& from, const string& to)
{
	if (data.empty()) return;
	auto pos = data.find(from);
	while (pos != string::npos)
	{
		data.replace(pos, from.length(), to);
		pos = data.find(from, pos + to.length());
	}
}

}//MCF