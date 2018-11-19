#pragma once

#include "common.h"

namespace MCF {

class SourceText;

class TextSpan final
{
private:
	std::pair<size_t, size_t> _span;
public:
	constexpr explicit TextSpan(size_t start = 0, size_t length = 0)
		:_span(start, length)
	{
	}
	~TextSpan() = default;
	TextSpan(const TextSpan&) = default;
	TextSpan(TextSpan&&) = default;
	TextSpan& operator=(const TextSpan&);
	TextSpan& operator=(TextSpan&&) = default;

	size_t Start()const noexcept { return _span.first; }
	size_t Length()const noexcept { return _span.second; }
	size_t End()const noexcept { return Start() + Length(); }
	string ToString() const { return std::to_string(Start()) + ".." + std::to_string(End()); }

	MCF_API constexpr bool operator==(const TextSpan& other)const { return _span == other._span; }
	MCF_API constexpr bool operator!=(const TextSpan& other)const { return _span != other._span; }
	MCF_API static TextSpan FromBounds(size_t start, size_t end);
};

class TextLine final
{
private:
	const SourceText* _text;
	const size_t _start;
	const size_t _length;
	const size_t _lengthIncludingLineBreak;
public:
	TextLine(const SourceText& text, size_t start, size_t length, size_t lengthWithBreak);
	~TextLine() = default;

	constexpr const SourceText* Text()const noexcept { return _text; }
	constexpr size_t Start()const noexcept { return _start; }
	constexpr size_t Length()const noexcept { return _length; }
	constexpr size_t End()const { return _start + _length; }
	constexpr size_t LengthIncludingLineBreak()const noexcept { return _lengthIncludingLineBreak; }
	TextSpan Span()const { return TextSpan(_start, _length); }
	TextSpan SpanIncludingLineBreak()const { return TextSpan(_start, _lengthIncludingLineBreak); }
	string ToString()const;
};

class MCF_API SourceText final
{
private:
	const string _text;
	vector<TextLine> _lines;

	static void AddLine(vector<TextLine>& result, const SourceText& sourceText,
						size_t position, size_t lineStart, size_t lineBreakWidth);
	static size_t GetLineBreakWidth(const string& text, size_t position);
	static vector<TextLine> ParseLines(const SourceText* sourceText, const string& text);

	explicit SourceText(const string& text);

public:
	~SourceText() = default;
	SourceText(const SourceText& other);
	SourceText(SourceText&& other) noexcept;

	const vector<TextLine> Lines()const { return _lines; }
	size_t Length()const noexcept { return _text.length(); }
	char operator[](size_t sub) const { return _text[sub]; }
	size_t GetLineIndex(size_t position)const noexcept;

	string ToString()const { return _text; }
	string ToString(size_t start, size_t length)const { return _text.substr(start, length); }
	string ToString(const TextSpan& span)const { return ToString(span.Start(), span.Length()); }

	static SourceText From(const string& text) { return SourceText(text); }
};

}//MCF
