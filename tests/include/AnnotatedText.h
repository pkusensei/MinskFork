#pragma once

#include <string>
#include <vector>

#include "SourceText.h"

class AnnotatedText final
{
private:
	std::string _text;
	std::vector<MCF::TextSpan> _spans;

	static std::string Dedent(const std::string& text);
public:
	AnnotatedText(const std::string& text, const std::vector<MCF::TextSpan>& spans)
		:_text(text), _spans(spans)
	{
	}

	const std::string& Text()const { return _text; }
	const std::vector<MCF::TextSpan>& Spans()const { return _spans; }

	static std::vector<std::string> DedentLines(const std::string& text);
	static AnnotatedText Parse(const std::string& input);
};
