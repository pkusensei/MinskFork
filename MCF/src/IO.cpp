#include "IO.h"

#include "ConsoleHelper.h"
#include "Diagnostic.h"
#include "Parsing.h"

namespace MCF {

void TextWriter::SetForeground(const ConsoleColor& color)
{
	if (IsConsole())
		SetConsoleColor(color);
}

void TextWriter::ResetColor()
{
	if (IsConsole())
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

void TextWriter::WriteDiagnostics(vector<const Diagnostic*> diagnostics)
{
	auto second = std::partition(diagnostics.begin(), diagnostics.end(),
		[](const auto& it) { return !it->HasLocation(); });

	for (auto it = diagnostics.cbegin(); it != second; ++it)
	{
		auto color = (*it)->IsWarning() ?
			ConsoleColor::DarkYellow : ConsoleColor::DarkRed;
		SetConsoleColor(color);
		_out << (*it)->ToString() << '\n';
		ResetConsoleColor();
	}

	auto selector = [](const auto& a, const auto& b)
	{
		if (a->Location().FilePath() == b->Location().FilePath())
		{
			if (a->Location().Span().Start() == b->Location().Span().Start())
				return a->Location().Span().Length() < b->Location().Span().Length();
			else
				return a->Location().Span().Start() < b->Location().Span().Start();
		} else
		{
			return a->Location().FilePath() < b->Location().FilePath();
		}
	};

	std::sort(second, diagnostics.end(), selector);

	for (; second != diagnostics.cend(); ++second)
	{
		auto text = (*second)->Location().Text();
		auto fileName = (*second)->Location().FilePath();
		auto startLine = (*second)->Location().StartLine() + 1;
		auto startCharacter = (*second)->Location().StartCharacter() + 1;
		auto endLine = (*second)->Location().EndLine() + 1;
		auto endCharacter = (*second)->Location().EndCharacter() + 1;

		auto span = (*second)->Location().Span();
		auto lineIndex = text.GetLineIndex(span.Start());
		auto line = text.Lines()[lineIndex];
		_out << '\n';

		auto color = (*second)->IsWarning() ?
			ConsoleColor::DarkYellow : ConsoleColor::DarkRed;
		SetConsoleColor(color);
		_out << fileName << "(" << startLine << "," << startCharacter << ','
			<< endLine << ',' << endCharacter << "): ";
		_out << (*second)->ToString() << '\n';
		ResetConsoleColor();

		auto prefixSpan = TextSpan::FromBounds(line.Start(), span.Start());
		auto suffixSpan = TextSpan::FromBounds(span.End(), line.End());

		auto prefix = text.ToString(prefixSpan);
		auto error = text.ToString(span);
		auto suffix = text.ToString(suffixSpan);
		_out << "    " << prefix;

		SetConsoleColor(ConsoleColor::DarkRed);
		_out << error;
		ResetConsoleColor();

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
