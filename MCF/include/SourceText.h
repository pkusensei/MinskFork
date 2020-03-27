#pragma once

#include "common.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

class SourceText;

class TextSpan final
{
private:
	size_t _start;
	size_t _length;

public:
	constexpr explicit TextSpan(size_t start = 0, size_t length = 0) noexcept
		:_start(start), _length(length)
	{
	}

	constexpr size_t Start()const noexcept { return _start; }
	constexpr size_t Length()const noexcept { return _length; }
	constexpr size_t End()const noexcept { return Start() + Length(); }
	string ToString() const
	{
		return std::to_string(Start()) + ".." + std::to_string(End());
	}

	constexpr bool operator==(const TextSpan& other)const noexcept
	{
		return _start == other._start && _length == other._length;
	}
	constexpr bool operator!=(const TextSpan& other)const noexcept
	{
		return !(*this == other);
	}
	MCF_API static TextSpan FromBounds(size_t start, size_t end)
	{
		return TextSpan(start, end - start);
	}
};

class TextLine final
{
private:
	std::reference_wrapper<const SourceText> _text;
	size_t _start;
	size_t _length;
	size_t _lengthIncludingLineBreak;

public:
	TextLine(const SourceText& text, size_t start, size_t length,
		size_t lengthWithBreak)noexcept
		:_text(text), _start(start), _length(length),
		_lengthIncludingLineBreak(lengthWithBreak)
	{
	}

	~TextLine() = default;

	constexpr const SourceText& Text()const noexcept { return _text; }
	constexpr size_t Start()const noexcept { return _start; }
	constexpr size_t Length()const noexcept { return _length; }
	constexpr size_t End()const noexcept { return _start + _length; }
	constexpr size_t LengthIncludingLineBreak()const noexcept
	{
		return _lengthIncludingLineBreak;
	}
	constexpr TextSpan Span()const
	{
		return TextSpan(_start, _length);
	}
	constexpr TextSpan SpanIncludingLineBreak()const
	{
		return TextSpan(_start, _lengthIncludingLineBreak);
	}
	string_view ToString()const;

};

class MCF_API SourceText final
{
private:
	string _text;
	string _fileName;
	vector<TextLine> _lines;

	static void AddLine(vector<TextLine>& result, const SourceText& sourceText,
		size_t position, size_t lineStart, size_t lineBreakWidth);
	static size_t GetLineBreakWidth(string_view text, size_t position);
	static vector<TextLine> ParseLines(const SourceText& sourceText, string_view text);

	explicit SourceText(string_view text, string_view fileName = {})
		:_text(text), _fileName(fileName)
	{
		_lines = ParseLines(*this, _text);
	}

public:
	constexpr string_view FileName()const { return _fileName; }
	constexpr const vector<TextLine>& Lines()const { return _lines; }
	constexpr size_t Length()const noexcept { return _text.length(); }
	constexpr char operator[](size_t sub) const { return _text.at(sub); }
	size_t GetLineIndex(size_t position)const noexcept;

	constexpr string_view ToString()const { return _text; }
	constexpr string_view ToString(size_t start, size_t length)const
	{
		return ToString().substr(start, length);
	}
	constexpr string_view ToString(const TextSpan& span)const
	{
		return ToString(span.Start(), span.Length());
	}

	static unique_ptr<SourceText> From(string_view text, string_view fileName = {})
	{
		return unique_ptr<SourceText>(new SourceText(text, fileName));
	}
};

class TextLocation
{
private:
	std::reference_wrapper<const SourceText> _text;
	TextSpan _span;

public:
	TextLocation(const SourceText& text, TextSpan span)
		:_text(text), _span(std::move(span))
	{
	}

	constexpr const SourceText& Text()const noexcept { return _text; }
	constexpr const TextSpan& Span()const noexcept { return _span; }

	constexpr string_view FileName()const { return Text().FileName(); }
	constexpr size_t StartLine()const noexcept
	{
		return Text().GetLineIndex(Span().Start());
	}
	constexpr size_t EndLine()const noexcept
	{
		return Text().GetLineIndex(Span().End());
	}
	constexpr size_t StartCharacter() const
	{
		return Span().Start() - Text().Lines()[StartLine()].Start();
	}
	constexpr size_t EndCharacter() const
	{
		return Span().End() - Text().Lines()[EndLine()].End();
	}

};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
