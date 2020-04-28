#pragma once

#include <string_view>
#include <vector>

#include "SourceText.h"

class AnnotatedText final
{
private:
	std::string _text;
	std::vector<MCF::TextSpan> _spans;

	static std::string Dedent(std::string_view text);
public:
	AnnotatedText(std::string text, std::vector<MCF::TextSpan> spans)
		:_text(std::move(text)), _spans(std::move(spans))
	{
	}

	std::string_view Text()const { return _text; }
	constexpr const std::vector<MCF::TextSpan>& Spans()const { return _spans; }

	static std::vector<std::string_view> DedentLines(std::string_view text);
	static AnnotatedText Parse(std::string_view input);
};
