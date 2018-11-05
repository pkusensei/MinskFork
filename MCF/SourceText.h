#pragma once

#include "common.h"

namespace MCF {

class SourceText;

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

class TextLine final
{
private:
	const SourceText& _text;
	const size_t _start;
	const size_t _length;
	const size_t _lengthIncludingLineBreak;
public:
	TextLine(const SourceText& text, size_t start, size_t length, size_t lengthWithBreak);
	~TextLine() = default;

	const SourceText* Text()const { return &_text; }
	size_t Start()const { return _start; }
	size_t Length()const { return _length; }
	size_t End()const { return _start + _length; }
	size_t LengthIncludingLineBreak()const { return _lengthIncludingLineBreak; }
	TextSpan Span()const { return TextSpan(_start, _length); }
	TextSpan SpanIncludingLineBreak()const { return TextSpan(_start, _lengthIncludingLineBreak); }
	string ToString()const;
};

class SourceText final
{
private:
	const string _text;
	vector<TextLine> _lines;

	static void AddLine(vector<TextLine>& result, const SourceText& sourceText, 
						size_t position, size_t lineStart, size_t lineBreakWidth);
	static size_t GetLineBreakWidth(const string& text, size_t position);
	static vector<TextLine> ParseLines(SourceText* sourceText, const string& text);
public:
	SourceText(const string& text);
	~SourceText() = default;

	const vector<TextLine> Lines()const { return _lines; }
	size_t Length()const { return _text.length(); }
	const char operator[](size_t sub) { return _text[sub]; }
	size_t GetLineIndex(size_t position)const;

	string ToString()const { return _text; }
	string ToString(size_t start, size_t length)const { return _text.substr(start, length); }
	string ToString(const TextSpan& span)const { return ToString(span.Start(), span.Length()); }
	
	static SourceText From(const string& text) { return SourceText(text); }
};

}//MCF
