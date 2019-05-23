#include "stdafx.h"
#include "IO.h"

#include "ConsoleHelper.h"

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

void TextWriter::WriteKeyword(const string & text)
{
	SetForeground(ConsoleColor::Blue);
	_out << text;
	ResetColor();
}

void TextWriter::WriteIdentifier(const string & text)
{
	SetForeground(ConsoleColor::DarkYellow);
	_out << text;
	ResetColor();
}

void TextWriter::WriteNumber(const string & text)
{
	SetForeground(ConsoleColor::Cyan);
	_out << text;
	ResetColor();
}

void TextWriter::WriteString(const string & text)
{
	SetForeground(ConsoleColor::Magenta);
	_out << text;
	ResetColor();
}

void TextWriter::WritePunctuation(const string & text)
{
	SetForeground(ConsoleColor::DarkGray);
	_out << text;
	ResetColor();
}

}//MCF
