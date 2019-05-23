#pragma once

#include "common.h"

namespace MCF {

/// string helpers
MCF_API bool StringStartsWith(const string& sample, const string& beginning);
MCF_API bool StringEndsWith(const string& sample, const string& ending);
MCF_API string TrimString(const string& text);
MCF_API string TrimStringStart(const string& text);
MCF_API string TrimStringEnd(const string& text);
MCF_API string StringJoin(const vector<string>& strs, const char seperator = ' ');
MCF_API vector<string> StringSplit(const string& s, const char delimiter = ' ');

}//MCF