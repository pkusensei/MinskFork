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
	virtual ~SyntaxNode() = default;
	virtual SyntaxKind Kind() const = 0;
	virtual std::vector<const SyntaxNode*> GetChildren() const = 0;
};

class SyntaxToken final :public SyntaxNode
{
private:
	const SyntaxKind _kind;
	const size_t _position;
	const std::string _text;
	const ValueType _value;

public:
	SyntaxToken(SyntaxKind kind, size_t position, const std::string& text, ValueType value);
	SyntaxToken(const SyntaxToken& other);
	SyntaxToken(SyntaxToken&& other);
	virtual ~SyntaxToken() = default;

	// Inherited via SyntaxNode
	MCF_API virtual SyntaxKind Kind() const override { return _kind; }
	MCF_API virtual std::vector<const SyntaxNode*> GetChildren() const override;

	size_t Position() const { return _position; }
	std::string Text() const { return _text; }
	ValueType Value() const { return _value; }
	TextSpan Span()const { return TextSpan(_position, _text.length(), _position + _text.length()); }
};

class Lexer final
{
private:
	const std::string _text;
	size_t _position;
	std::unique_ptr<DiagnosticBag> _diagnostics;

	char Peek(int offset) const;
	char Current() { return Peek(0); }
	char Lookahead() { return Peek(1); }
	void Next() { _position++; }
public:
	Lexer(std::string text);
	~Lexer() = default;

	static SyntaxKind GetKeywordKind(const std::string& text);

	SyntaxToken Lex();
	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
};

class ExpressionSyntax :public SyntaxNode
{
public:
	virtual ~ExpressionSyntax() = default;
	// Inherited via SyntaxNode
	virtual SyntaxKind Kind() const override { return SyntaxKind::BadToken; }// HACK
	virtual std::vector<const SyntaxNode*> GetChildren() const override;
};

class AssignmentExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;
	SyntaxToken _equalsToken;
	std::unique_ptr<ExpressionSyntax> _expression;

public:
	AssignmentExpressionSyntax(SyntaxToken& identifier, SyntaxToken& equals,
							   std::unique_ptr<ExpressionSyntax>& expression);
	virtual ~AssignmentExpressionSyntax() = default;
	AssignmentExpressionSyntax(AssignmentExpressionSyntax&& other);

	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::AssignmentExpression; }
	virtual std::vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IdentifierToken() const { return _identifierToken; }
	SyntaxToken EqualsToken() const { return _equalsToken; }
	ExpressionSyntax* Expression() const { return _expression.get(); }
};

class UnaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	std::unique_ptr<ExpressionSyntax> _operand;
public:
	UnaryExpressionSyntax(SyntaxToken& operatorToken, std::unique_ptr<ExpressionSyntax>& operand);
	virtual ~UnaryExpressionSyntax() = default;
	UnaryExpressionSyntax(UnaryExpressionSyntax&& other);

	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::UnaryExpression; }
	virtual std::vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OperatorToken() const { return _operatorToken; }
	ExpressionSyntax* Operand() const { return _operand.get(); }
};

class BinaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	std::unique_ptr<ExpressionSyntax> _left;
	std::unique_ptr<ExpressionSyntax> _right;
public:
	BinaryExpressionSyntax(std::unique_ptr<ExpressionSyntax>& left, SyntaxToken& operatorToken, std::unique_ptr<ExpressionSyntax>& right);
	virtual ~BinaryExpressionSyntax() = default;
	BinaryExpressionSyntax(BinaryExpressionSyntax&& other);

	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::BinaryExpression; }
	virtual std::vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OperatorToken() const { return _operatorToken; }
	ExpressionSyntax* Left()const { return _left.get(); }
	ExpressionSyntax* right()const { return _right.get(); }
};

class ParenthesizedExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _openParenthesisToken;
	SyntaxToken _closeParenthesisToken;
	std::unique_ptr<ExpressionSyntax> _expression;
public:
	ParenthesizedExpressionSyntax(SyntaxToken& open, std::unique_ptr<ExpressionSyntax>& expression, SyntaxToken& close);
	virtual ~ParenthesizedExpressionSyntax() = default;
	ParenthesizedExpressionSyntax(ParenthesizedExpressionSyntax&& other);
	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::ParenthesizedExpression; }
	virtual std::vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OpenParenthesisToken()const { return _openParenthesisToken; }
	SyntaxToken CloseParenthesisToken() const { return _closeParenthesisToken; }
	ExpressionSyntax* Expression()const { return _expression.get(); }
};

class LiteralExpressionSyntax :public ExpressionSyntax
{
private:
	SyntaxToken _literalToken;
	ValueType _value;
public:
	LiteralExpressionSyntax(SyntaxToken& literalToken, ValueType& value);
	LiteralExpressionSyntax(SyntaxToken& literalToken);
	virtual ~LiteralExpressionSyntax() = default;
	LiteralExpressionSyntax(LiteralExpressionSyntax&& other);
	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::LiteralExpression; }
	virtual std::vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken LiteralToken()const { return _literalToken; }
	ValueType Value()const { return _value; }
};

class NameExpressionSyntax :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;
public:
	NameExpressionSyntax(SyntaxToken& identifier);
	virtual ~NameExpressionSyntax() = default;
	NameExpressionSyntax(NameExpressionSyntax&& other);
	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::NameExpression; }
	virtual std::vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IdentifierToken()const { return _identifierToken; }
};

}//MCF
