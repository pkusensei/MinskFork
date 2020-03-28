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

void TextWriter::WriteDiagnostics(DiagnosticBag& diagnostics)
{
	auto selector = [](const auto& a, const auto& b)
	{
		if (a.Location().FilePath() == b.Location().FilePath())
		{
			if (a.Location().Span().Start() == b.Location().Span().Start())
				return a.Location().Span().Length() < b.Location().Span().Length();
			else
				return a.Location().Span().Start() < b.Location().Span().Start();
		} else
		{
			return a.Location().FilePath() < b.Location().FilePath();
		}
	};

	for (const auto& diag : diagnostics.SortBy(selector))
	{
		auto text = diag.Location().Text();
		auto fileName = diag.Location().FilePath();
		auto startLine = diag.Location().StartLine() + 1;
		auto startCharacter = diag.Location().StartCharacter() + 1;
		auto endLine = diag.Location().EndLine() + 1;
		auto endCharacter = diag.Location().EndCharacter() + 1;

		auto span = diag.Location().Span();
		auto lineIndex = text.GetLineIndex(span.Start());
		auto line = text.Lines()[lineIndex];
		_out << '\n';

		MCF::SetConsoleColor(MCF::ConsoleColor::DarkRed);
		_out << fileName << "(" << startLine << "," << startCharacter << ','
			<< endLine << ',' << endCharacter << "): ";
		_out << diag.ToString() << '\n';
		MCF::ResetConsoleColor();

		auto prefixSpan = MCF::TextSpan::FromBounds(line.Start(), span.Start());
		auto suffixSpan = MCF::TextSpan::FromBounds(span.End(), line.End());

		auto prefix = text.ToString(prefixSpan);
		auto error = text.ToString(span);
		auto suffix = text.ToString(suffixSpan);
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
