#pragma once

#include "Symbols.h"
#include "SyntaxKind.h"

namespace MCF {

class DiagnosticBag;
class SourceText;
class SyntaxToken;
class SyntaxTree;

class Lexer final
{
private:
	const SyntaxTree& _tree;
	const SourceText& _text;
	DiagnosticBag& _diagnostics;

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
	explicit Lexer(const SyntaxTree& text);

	[[nodiscard]] SyntaxToken Lex();
	constexpr const DiagnosticBag& Diagnostics()const noexcept { return _diagnostics; }
};

}//MCF