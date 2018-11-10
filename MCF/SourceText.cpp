#include "stdafx.h"
#include "SourceText.h"

namespace MCF {

TextSpan::TextSpan(size_t start, size_t length)
	:_span(start, length)
{
}

TextSpan & TextSpan::operator=(const TextSpan & other)
{
	_span = other._span;
	return *this;
}

TextSpan TextSpan::FromBounds(size_t start, size_t end)
{
	return TextSpan(start, end - start);
}

TextLine::TextLine(const SourceText & text, size_t start, size_t length, size_t lengthWithBreak)
	:_text(text), _start(start), _length(length), _lengthIncludingLineBreak(lengthWithBreak)
{
}

string TextLine::ToString() const
{
	return _text.ToString(Span());
}

SourceText::SourceText(const string & text)
	:_text(text), _lines(ParseLines(this, text))
{
}

void SourceText::AddLine(vector<TextLine>& result, const SourceText & sourceText, 
						 size_t position, size_t lineStart, size_t lineBreakWidth)
{
	auto lineLength = position - lineStart;
	auto lineLengthWithLineBreak = lineLength + lineBreakWidth;
	result.emplace_back(sourceText, lineStart, lineLength, lineLengthWithLineBreak);
}

size_t SourceText::GetLineBreakWidth(const string & text, size_t position)
{
	auto character = text[position];
	auto last = position + 1 >= text.length() ? '\0' : text[position + 1];
	if (character == '\r' && last == '\n')
		return 2;
	else if (character == '\r' || last == '\n')
		return 1;
	else return 0;
}

vector<TextLine> SourceText::ParseLines(SourceText * sourceText, const string & text)
{
	auto result = vector<TextLine>();
	size_t position = 0;
	size_t lineStart = 0;
	while (position < text.length())
	{
		auto lineBreakWidth = GetLineBreakWidth(text, position);
		if (lineBreakWidth == 0)
			++position;
		else
		{
			AddLine(result, *sourceText, position, lineStart, lineBreakWidth);
			position += lineBreakWidth;
			lineStart = position;
		}
	}
	if (position >= lineStart)
		AddLine(result, *sourceText, position, lineStart, 0);
	result.shrink_to_fit();
	return result;
}

size_t SourceText::GetLineIndex(size_t position) const
{
	size_t lower = 0;
	size_t upper = _lines.size() - 1;

	while (lower <= upper)
	{
		auto index = lower + (upper - lower) / 2;
		auto start = _lines[index].Start();
		if (position == start)
			return index;
		else if (start > position)
			upper = index - 1;
		else lower = index + 1;
	}
	return lower - 1;
}

}//MCF
