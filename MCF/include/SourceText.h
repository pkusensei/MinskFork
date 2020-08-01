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

struct SourceText;

struct TextSpan final
{
	size_t Start;
	size_t Length;

public:
	constexpr explicit TextSpan(size_t start = 0, size_t length = 0) noexcept
		:Start(start), Length(length)
	{
	}

	constexpr size_t End()const noexcept { return Start + Length; }
	constexpr bool OverlapsWith(const TextSpan& span)const noexcept
	{
		return Start < span.End() && span.Start < End();
	}

	string ToString() const
	{
		return std::to_string(Start) + ".." + std::to_string(End());
	}

	constexpr bool operator==(const TextSpan& other)const noexcept
	{
		return Start == other.Start && Length == other.Length;
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

struct TextLine final
{
	const SourceText* Text;
	size_t Start;
	size_t Length;
	size_t LengthIncludingLineBreak;

public:
	constexpr TextLine(const SourceText& text, size_t start, size_t length,
					   size_t lengthWithBreak)noexcept
		:Text(&text), Start(start),
		Length(length), LengthIncludingLineBreak(lengthWithBreak)
	{
	}

	constexpr size_t End()const noexcept { return Start + Length; }
	constexpr TextSpan Span()const noexcept
	{
		return TextSpan(Start, Length);
	}
	constexpr TextSpan SpanIncludingLineBreak()const noexcept
	{
		return TextSpan(Start, LengthIncludingLineBreak);
	}
	string_view ToString()const;

};

struct MCF_API [[nodiscard]] SourceText final
{
	string Text;
	fs::path FilePath;
	vector<TextLine> Lines;

private:
	static void AddLine(vector<TextLine>& result, const SourceText& sourceText,
						size_t position, size_t lineStart, size_t lineBreakWidth);
	static size_t GetLineBreakWidth(string_view text, size_t position);
	static vector<TextLine> ParseLines(const SourceText& sourceText, string_view text);

	explicit SourceText(string text, fs::path filePath = {})
		:Text(std::move(text)), FilePath(std::move(filePath)),
		Lines{ ParseLines(*this, Text) }
	{
	}

public:
	size_t Length()const noexcept { return Text.length(); }
	char operator[](size_t sub) const { return Text.at(sub); }
	size_t GetLineIndex(size_t position)const noexcept;

	string_view ToString()const noexcept { return Text; }
	constexpr string_view ToString(size_t start, size_t length)const
	{
		return ToString().substr(start, length);
	}
	constexpr string_view ToString(const TextSpan& span)const
	{
		return ToString(span.Start, span.Length);
	}

	static SourceText From(string text, fs::path path = {})noexcept
	{
		return SourceText(std::move(text), std::move(path));
	}
	static SourceText From(string_view text, fs::path path = {})
	{
		return SourceText(string(text), std::move(path));
	}

};

struct TextLocation final
{
	TextSpan Span;
	const SourceText* Text;

public:
	constexpr explicit TextLocation(const SourceText& text, TextSpan span)noexcept
		:Span(std::move(span)), Text(&text)
	{
	}

	constexpr const fs::path& FilePath()const noexcept { return Text->FilePath; }
	size_t StartLine()const noexcept
	{
		return Text->GetLineIndex(Span.Start);
	}
	size_t EndLine()const noexcept
	{
		return Text->GetLineIndex(Span.End());
	}
	size_t StartCharacter() const
	{
		return Span.Start - Text->Lines.at(StartLine()).Start;
	}
	size_t EndCharacter() const
	{
		return Span.End() > Text->Lines.at(EndLine()).End() ?
			Span.End() - Text->Lines.at(EndLine()).End() : 0;
	}

};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
