#pragma once

#include <filesystem>

#include "common.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

namespace fs = std::filesystem;

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

	constexpr bool OverlapsWith(const TextSpan& span)const noexcept
	{
		return Start() < span.End() && span.Start() < End();
	}

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
	const SourceText* _text;
	size_t _start;
	size_t _length;
	size_t _lengthIncludingLineBreak;

public:
	TextLine(const SourceText& text, size_t start, size_t length,
		size_t lengthWithBreak)noexcept
		:_text(&text), _start(start), _length(length),
		_lengthIncludingLineBreak(lengthWithBreak)
	{
	}

	~TextLine() = default;

	constexpr const SourceText& Text()const noexcept { return *_text; }
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
	fs::path _filePath;
	vector<TextLine> _lines;

	static void AddLine(vector<TextLine>& result, const SourceText& sourceText,
		size_t position, size_t lineStart, size_t lineBreakWidth);
	static size_t GetLineBreakWidth(string_view text, size_t position);
	static vector<TextLine> ParseLines(const SourceText& sourceText, string_view text);

	explicit SourceText(string text, fs::path filePath = {})
		:_text(std::move(text)), _filePath(std::move(filePath))
	{
		_lines = ParseLines(*this, _text);
	}

public:
	constexpr const fs::path& FilePath()const { return _filePath; }
	constexpr const vector<TextLine>& Lines()const { return _lines; }
	size_t Length()const noexcept { return _text.length(); }
	char operator[](size_t sub) const { return _text.at(sub); }
	size_t GetLineIndex(size_t position)const noexcept;

	string_view ToString()const { return _text; }
	constexpr string_view ToString(size_t start, size_t length)const
	{
		return ToString().substr(start, length);
	}
	constexpr string_view ToString(const TextSpan& span)const
	{
		return ToString(span.Start(), span.Length());
	}

	static SourceText From(string text, fs::path path = {})
	{
		return SourceText(std::move(text), std::move(path));
	}
};

class TextLocation
{
private:
	const SourceText* _text;
	TextSpan _span;

public:
	TextLocation(const SourceText& text, TextSpan span)
		:_text(&text), _span(std::move(span))
	{
	}

	constexpr const SourceText& Text()const noexcept { return *_text; }
	constexpr const TextSpan& Span()const noexcept { return _span; }

	constexpr const fs::path& FilePath()const { return Text().FilePath(); }
	size_t StartLine()const noexcept
	{
		return Text().GetLineIndex(Span().Start());
	}
	size_t EndLine()const noexcept
	{
		return Text().GetLineIndex(Span().End());
	}
	constexpr size_t StartCharacter() const
	{
		return Span().Start() - Text().Lines()[StartLine()].Start();
	}
	constexpr size_t EndCharacter() const
	{
		return Span().End() > Text().Lines()[EndLine()].End() ?
			Span().End() - Text().Lines()[EndLine()].End()
			: 0;
	}

};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
