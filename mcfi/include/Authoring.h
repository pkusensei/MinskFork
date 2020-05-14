#pragma once

#include "SourceText.h"

namespace MCF {
class SyntaxTree;
}

enum class Classification
{
	Text,
	Keyword,
	Identifier,
	Number,
	String,
	Comment
};

struct ClassifiedSpan
{
	MCF::TextSpan Span;
	Classification ClassEnum;

	ClassifiedSpan(MCF::TextSpan span, Classification classEnum)
		:Span(std::move(span)), ClassEnum(classEnum)
	{
	}
};

std::vector<ClassifiedSpan> Classify(const MCF::SyntaxTree& tree, const MCF::TextSpan& span);