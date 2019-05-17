#pragma once

#include "Symbols.h"
#include "SyntaxKind.h"

namespace MCF {

class DiagnosticBag;
class SourceText;
class SyntaxToken;

class Lexer final
{
private:
	const SourceText* _text;
	unique_ptr<DiagnosticBag> _diagnostics;

	size_t _position;
	size_t _start;
	SyntaxKind _kind;
	ValueType _value;

	char Peek(int offset) const;
	char Current() const { return Peek(0); }
	char Lookahead() const { return Peek(1); }
	constexpr void Next(size_t step = 1) noexcept { _position += step; }

	void ReadString();
	void ReadWhiteSpace();
	void ReadNumberToken();
	void ReadIdentifierOrKeyword();

public:
	explicit Lexer(const SourceText& text);

	SyntaxToken Lex();
	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }
};

}//MCF