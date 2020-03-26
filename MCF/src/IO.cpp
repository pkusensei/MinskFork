#include "IO.h"

#include "ConsoleHelper.h"
#include "Diagnostic.h"
#include "Parsing.h"

namespace MCF {

void TextWriter::SetForeground(const ConsoleColor& color)
{
	if (IsConsoleOutput())
		SetConsoleColor(color);
}

void TextWriter::ResetColor()
{
	if (IsConsoleOutput())
		ResetConsoleColor();
}

void TextWriter::Write(string_view text)
{
	_out << text;
}

void TextWriter::WriteKeyword(const SyntaxKind& kind)
{
	WriteKeyword(GetText(kind));
}

void TextWriter::WriteLine()
{
	_out << NEW_LINE;
}

void TextWriter::WriteKeyword(string_view text)
{
	SetForeground(ConsoleColor::Blue);
	Write(text);
	ResetColor();
}

void TextWriter::WriteIdentifier(string_view text)
{
	SetForeground(ConsoleColor::DarkYellow);
	Write(text);
	ResetColor();
}

void TextWriter::WriteNumber(string_view text)
{
	SetForeground(ConsoleColor::Cyan);
	Write(text);
	ResetColor();
}

void TextWriter::WriteString(string_view text)
{
	SetForeground(ConsoleColor::Magenta);
	Write(text);
	ResetColor();
}

void TextWriter::WriteSpace()
{
	Write(" ");
}

void TextWriter::WritePunctuation(const SyntaxKind& kind)
{
	WritePunctuation(GetText(kind));
}

void TextWriter::WritePunctuation(string_view text)
{
	SetForeground(ConsoleColor::DarkGray);
	Write(text);
	ResetColor();
}

void TextWriter::WriteDiagnostics(DiagnosticBag& diagnostics, const SyntaxTree& tree)
{
	for (const auto& diag : diagnostics.SortBySpanAscending())
	{
		auto lineIndex = tree.Text().GetLineIndex(diag.Span().Start());
		auto lineNumber = lineIndex + 1;
		auto& line = tree.Text().Lines()[lineIndex];
		auto character = diag.Span().Start() - line.Start() + 1;
		_out << '\n';

		MCF::SetConsoleColor(MCF::ConsoleColor::DarkRed);
		_out << "(" << lineNumber << ", " << character << ") ";
		_out << diag.Message() << '\n';
		MCF::ResetConsoleColor();

		auto prefixSpan = MCF::TextSpan::FromBounds(line.Start(), diag.Span().Start());
		auto suffixSpan = MCF::TextSpan::FromBounds(diag.Span().End(), line.End());

		auto prefix = tree.Text().ToString(prefixSpan);
		auto error = tree.Text().ToString(diag.Span());
		auto suffix = tree.Text().ToString(suffixSpan);
		_out << "    " << prefix;

		MCF::SetConsoleColor(MCF::ConsoleColor::DarkRed);
		_out << error;
		MCF::ResetConsoleColor();

		_out << suffix << '\n';
	}
	_out << '\n';
}

void IndentedTextWriter::WriteIndent()
{
	if (_indentPending)
	{
		for (size_t i = 0; i < _indentCount; ++i)
			_out << INDENT_UNIT;
		_indentPending = false;
	}
}

void IndentedTextWriter::Write(string_view text)
{
	WriteIndent();
	TextWriter::Write(text);
}

void IndentedTextWriter::WriteLine()
{
	WriteIndent();
	TextWriter::WriteLine();
	_indentPending = true;
}

}//MCF
