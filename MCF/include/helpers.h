#pragma once

#include <sstream>

#include "common.h"

namespace MCF {

/// string helpers
MCF_API bool StringStartsWith(string_view sample, string_view beginning);
MCF_API bool StringEndsWith(string_view sample, string_view ending);
MCF_API string TrimString(const string& text);
MCF_API string TrimStringStart(const string& text);
MCF_API string TrimStringEnd(const string& text);
MCF_API string StringJoin(const vector<string>& strs, const char seperator = ' ');
MCF_API vector<string> StringSplit(const string& s, const char delimiter = ' ');
void StringReplaceAll(string& data, const string& from, const string& to);

template<typename... Args>
string BuildStringFrom(Args... args)
{
	std::stringstream ss;
	(ss << ... << args);
	return ss.str();
}

}//MCF