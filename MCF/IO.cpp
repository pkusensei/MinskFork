#include "stdafx.h"
#include "IO.h"

#include "ConsoleHelper.h"
#include "SyntaxKind.h"

namespace MCF {

void TextWriter::SetForeground(const ConsoleColor & color)
{
	if (IsConsoleOutput())
		SetConsoleColor(color);
}

void TextWriter::ResetColor()
{
	if (IsConsoleOutput())
		ResetConsoleColor();
}

void TextWriter::Write(const string & text)
{
	_out << text;
}

void TextWriter::WriteKeyword(const SyntaxKind & kind)
{
	WriteKeyword(GetText(kind));
}

void TextWriter::WriteLine()
{
	_out << NEW_LINE;
}

void TextWriter::WriteKeyword(const string & text)
{
	SetForeground(ConsoleColor::Blue);
	Write(text);
	ResetColor();
}

void TextWriter::WriteIdentifier(const string & text)
{
	SetForeground(ConsoleColor::DarkYellow);
	Write(text);
	ResetColor();
}

void TextWriter::WriteNumber(const string & text)
{
	SetForeground(ConsoleColor::Cyan);
	Write(text);
	ResetColor();
}

void TextWriter::WriteString(const string & text)
{
	SetForeground(ConsoleColor::Magenta);
	Write(text);
	ResetColor();
}

void TextWriter::WriteSpace()
{
	Write(" ");
}

void TextWriter::WritePunctuation(const SyntaxKind & kind)
{
	WritePunctuation(GetText(kind));
}

void TextWriter::WritePunctuation(const string & text)
{
	SetForeground(ConsoleColor::DarkGray);
	Write(text);
	ResetColor();
}

void IndentedTextWriter::WriteIndent()
{
	if (_indentPending)
	{
		for (auto i = 0; i < _indentCount; ++i)
			_out << INDENT_UNIT;
		_indentPending = false;
	}
}

void IndentedTextWriter::Write(const string & text)
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
