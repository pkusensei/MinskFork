#pragma once

#include "common.h"

namespace MCF {

class TextSpan final
{
private:
	std::tuple<size_t, size_t> _span;
public:
	TextSpan(size_t start = 0, size_t length = 0);
	~TextSpan() = default;
	TextSpan(const TextSpan&) = default;
	TextSpan(TextSpan&&) = default;
	TextSpan& operator=(const TextSpan&);
	TextSpan& operator=(TextSpan&&) = default;

	size_t Start()const { return std::get<0>(_span); }
	size_t Length()const { return std::get<1>(_span); }
	size_t End()const { return std::get<0>(_span) + std::get<1>(_span); }

	static TextSpan FromBounds(size_t start, size_t end);
};

class SourceText
{
public:
	SourceText();
	~SourceText();
};

}//MCF