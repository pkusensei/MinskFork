#pragma once
#include <vector>
#include <memory>

#include "common.h"

namespace MCF {

class DiagnosticBag;

enum class SyntaxKind
{
	// Tokens
	BadToken,
	EndOfFileToken,
	WhitespaceToken,
	NumberToken,
	PlusToken,
	MinusToken,
	StarToken,
	SlashToken,
	BangToken,
	EqualsToken,
	AmpersandAmpersandToken,
	PipePipeToken,
	EqualsEqualsToken,
	BangEqualsToken,
	OpenParenthesisToken,
	CloseParenthesisToken,
	IdentifierToken,

	// Keywords
	FalseKeyword,
	TrueKeyword,

	// Expressions
	LiteralExpression,
	NameExpression,
	UnaryExpression,
	BinaryExpression,
	ParenthesizedExpression,
	AssignmentExpression
};

class SyntaxNode
{
public:
	virtual SyntaxKind Kind() const = 0;
	virtual std::vector<SyntaxNode*> GetChildren() const = 0;
};

class SyntaxToken final :public SyntaxNode
{
private:
	const SyntaxKind _kind;
	const int _position;
	const std::string _text;
	const ValueType _value;

public:
	MCF_API SyntaxToken(SyntaxKind kind, int position, const std::string& text, ValueType value);
	//SyntaxToken(const SyntaxToken& other);
	MCF_API virtual ~SyntaxToken();

	// Inherited via SyntaxNode
	MCF_API virtual SyntaxKind Kind() const override { return _kind; }
	MCF_API virtual std::vector<SyntaxNode*> GetChildren() const override;

	int Position() const { return _position; }
	std::string Text() const { return _text; }
	ValueType Value() const { return _value; }
};

class Lexer final
{
private:
	const std::string _text;
	int _position;
	std::unique_ptr<DiagnosticBag> _diagnostics;

	char Peek(int offset) const;
	char Current() { return Peek(0); }
	char Lookahead() { return Peek(1); }
	void Next() { _position++; }
public:
	MCF_API Lexer(std::string text);
	MCF_API ~Lexer();

	static SyntaxKind GetKeywordKind(const std::string& text);

	MCF_API SyntaxToken Lex();
	MCF_API DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
};
}//MCF
