#pragma once

#include "common.h"


namespace MCF {

class DiagnosticBag;
class TextSpan;
class SourceText;

class MCF_API SyntaxNode
{
private:
	static void PrettyPrint(std::ostream& out, const SyntaxNode* node, std::string indent = "", bool isLast = true);
public:
	virtual ~SyntaxNode() = default;
	virtual SyntaxKind Kind() const = 0;
	virtual TextSpan Span()const;
	virtual const vector<const SyntaxNode*> GetChildren() const = 0;

	void WriteTo(std::ostream& out)const { PrettyPrint(out, this); }
	string ToString() const;
};

class SyntaxToken final :public SyntaxNode
{
private:
	const SyntaxKind _kind;
	const size_t _position;
	const string _text;
	const ValueType _value;

public:
	SyntaxToken(SyntaxKind kind, size_t position, const string& text, ValueType value);
	SyntaxToken(const SyntaxToken& other) = default;
	virtual ~SyntaxToken() = default;

	// Inherited via SyntaxNode
	virtual SyntaxKind Kind() const override { return _kind; }
	virtual TextSpan Span()const override;
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	size_t Position() const { return _position; }
	string Text() const { return _text; }
	ValueType Value() const { return _value; }
};

class Lexer final
{
private:
	const SourceText& _text;
	unique_ptr<DiagnosticBag> _diagnostics;

	size_t _position;
	size_t _start;
	SyntaxKind _kind;
	ValueType _value;

	char Peek(int offset) const;
	char Current() const { return Peek(0); }
	char Lookahead() const { return Peek(1); }
	void Next() { _position++; }

	void ReadWhiteSpace();
	void ReadNumberToken();
	void ReadIdentifierOrKeyword();
public:
	explicit Lexer(const SourceText& text);
	~Lexer() = default;

	SyntaxToken Lex();
	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
};

class ExpressionSyntax :public SyntaxNode
{
public:
	virtual ~ExpressionSyntax() = default;
	// Inherited via SyntaxNode
	virtual SyntaxKind Kind() const override { return SyntaxKind::BadToken; } // HACK
	virtual const vector<const SyntaxNode*> GetChildren() const override;
};

class AssignmentExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _expression;

public:
	AssignmentExpressionSyntax(const SyntaxToken& identifier, const SyntaxToken& equals,
							   unique_ptr<ExpressionSyntax>& expression);
	virtual ~AssignmentExpressionSyntax() = default;
	AssignmentExpressionSyntax(AssignmentExpressionSyntax&& other);

	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::AssignmentExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IdentifierToken() const { return _identifierToken; }
	SyntaxToken EqualsToken() const { return _equalsToken; }
	const ExpressionSyntax* Expression() const { return _expression.get(); }
};

class UnaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _operand;
public:
	UnaryExpressionSyntax(const SyntaxToken& operatorToken, unique_ptr<ExpressionSyntax>& operand);
	virtual ~UnaryExpressionSyntax() = default;
	UnaryExpressionSyntax(UnaryExpressionSyntax&& other);

	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::UnaryExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OperatorToken() const { return _operatorToken; }
	const ExpressionSyntax* Operand() const { return _operand.get(); }
};

class BinaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _left;
	unique_ptr<ExpressionSyntax> _right;
public:
	BinaryExpressionSyntax(unique_ptr<ExpressionSyntax>& left, const SyntaxToken& operatorToken, unique_ptr<ExpressionSyntax>& right);
	virtual ~BinaryExpressionSyntax() = default;
	BinaryExpressionSyntax(BinaryExpressionSyntax&& other);

	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::BinaryExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OperatorToken() const { return _operatorToken; }
	const ExpressionSyntax* Left()const { return _left.get(); }
	const ExpressionSyntax* Right()const { return _right.get(); }
};

class ParenthesizedExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _openParenthesisToken;
	SyntaxToken _closeParenthesisToken;
	unique_ptr<ExpressionSyntax> _expression;
public:
	ParenthesizedExpressionSyntax(const SyntaxToken& open, unique_ptr<ExpressionSyntax>& expression, const SyntaxToken& close);
	virtual ~ParenthesizedExpressionSyntax() = default;
	ParenthesizedExpressionSyntax(ParenthesizedExpressionSyntax&& other);
	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::ParenthesizedExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OpenParenthesisToken()const { return _openParenthesisToken; }
	SyntaxToken CloseParenthesisToken() const { return _closeParenthesisToken; }
	const ExpressionSyntax* Expression()const { return _expression.get(); }
};

class LiteralExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _literalToken;
	ValueType _value;
public:
	LiteralExpressionSyntax(const SyntaxToken& literalToken, const ValueType& value);
	explicit LiteralExpressionSyntax(const SyntaxToken& literalToken);
	virtual ~LiteralExpressionSyntax() = default;
	LiteralExpressionSyntax(LiteralExpressionSyntax&& other);
	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::LiteralExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken LiteralToken()const { return _literalToken; }
	ValueType Value()const { return _value; }
};

class NameExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;
public:
	explicit NameExpressionSyntax(const SyntaxToken& identifier);
	virtual ~NameExpressionSyntax() = default;
	NameExpressionSyntax(NameExpressionSyntax&& other);
	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::NameExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IdentifierToken()const { return _identifierToken; }
};

}//MCF
