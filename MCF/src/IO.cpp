#include "IO.h"

#include "ConsoleHelper.h"
#include "SyntaxKind.h"

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
