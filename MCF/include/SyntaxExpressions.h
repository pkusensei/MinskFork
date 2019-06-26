#pragma once

#include "SyntaxToken.h"

namespace MCF {

class ExpressionSyntax :public SyntaxNode
{
};

class AssignmentExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _expression;

public:
	AssignmentExpressionSyntax(const SyntaxToken& identifier,
		const SyntaxToken& equals, unique_ptr<ExpressionSyntax>& expression)
		:_identifierToken(identifier), _equalsToken(equals),
		_expression(std::move(expression))
	{
	}
	AssignmentExpressionSyntax(AssignmentExpressionSyntax&&) = default;
	AssignmentExpressionSyntax& operator=(AssignmentExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::AssignmentExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& IdentifierToken()const noexcept { return _identifierToken; }
	const SyntaxToken& EqualsToken()const noexcept { return _equalsToken; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

class UnaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _operand;

public:
	UnaryExpressionSyntax(const SyntaxToken& operatorToken,
		unique_ptr<ExpressionSyntax>& operand)
		:_operatorToken(operatorToken), _operand(std::move(operand))
	{
	}
	UnaryExpressionSyntax(UnaryExpressionSyntax&&) = default;
	UnaryExpressionSyntax& operator=(UnaryExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::UnaryExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& OperatorToken()const noexcept { return _operatorToken; }
	const ExpressionSyntax* Operand()const noexcept { return _operand.get(); }
};

class BinaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _left;
	unique_ptr<ExpressionSyntax> _right;

public:
	BinaryExpressionSyntax(unique_ptr<ExpressionSyntax>& left,
		const SyntaxToken& operatorToken, unique_ptr<ExpressionSyntax>& right)
		:_operatorToken(operatorToken),
		_left(std::move(left)), _right(std::move(right))
	{
	}
	BinaryExpressionSyntax(BinaryExpressionSyntax&&) = default;
	BinaryExpressionSyntax& operator=(BinaryExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BinaryExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& OperatorToken()const noexcept { return _operatorToken; }
	const ExpressionSyntax* Left()const noexcept { return _left.get(); }
	const ExpressionSyntax* Right()const noexcept { return _right.get(); }
};

class ParenthesizedExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _openParenthesisToken;
	SyntaxToken _closeParenthesisToken;
	unique_ptr<ExpressionSyntax> _expression;

public:
	ParenthesizedExpressionSyntax(const SyntaxToken& open,
		unique_ptr<ExpressionSyntax>& expression, const SyntaxToken& close)
		:_openParenthesisToken(open), _closeParenthesisToken(close),
		_expression(std::move(expression))
	{
	}
	ParenthesizedExpressionSyntax(ParenthesizedExpressionSyntax&&) = default;
	ParenthesizedExpressionSyntax& operator=(ParenthesizedExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ParenthesizedExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& OpenParenthesisToken()const noexcept { return _openParenthesisToken; }
	const SyntaxToken& CloseParenthesisToken()const noexcept { return _closeParenthesisToken; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

class LiteralExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _literalToken;
	ValueType _value;

public:
	LiteralExpressionSyntax(const SyntaxToken& literalToken, const ValueType& value)
		:_literalToken(literalToken), _value(value)
	{
	}
	explicit LiteralExpressionSyntax(const SyntaxToken& literalToken)
		:_literalToken(literalToken), _value(literalToken.Value())
	{
	}
	LiteralExpressionSyntax(LiteralExpressionSyntax&&) = default;
	LiteralExpressionSyntax& operator=(LiteralExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::LiteralExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& LiteralToken()const noexcept { return _literalToken; }
	const ValueType& Value()const noexcept { return _value; }
};

class NameExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;

public:
	explicit NameExpressionSyntax(const SyntaxToken& identifier)
		:_identifierToken(identifier)
	{
	}
	NameExpressionSyntax(NameExpressionSyntax&&) = default;
	NameExpressionSyntax& operator=(NameExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::NameExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& IdentifierToken()const noexcept { return _identifierToken; }
};

class CallExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifier;
	SyntaxToken _openParenthesisToken;
	SeparatedSyntaxList<ExpressionSyntax> _arguments;
	SyntaxToken _closeParenthesisToken;

public:
	CallExpressionSyntax(const SyntaxToken& identifier, const SyntaxToken& open,
		SeparatedSyntaxList<ExpressionSyntax>& arguments,
		const SyntaxToken& close)
		: _identifier(identifier), _openParenthesisToken(open),
		_arguments(std::move(arguments)), _closeParenthesisToken(close)
	{
	}
	CallExpressionSyntax(CallExpressionSyntax&& other) = default;
	CallExpressionSyntax& operator=(CallExpressionSyntax&& other) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::CallExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& Identifier()const noexcept { return _identifier; }
	const SyntaxToken& OpenParenthesisToken()const noexcept { return _openParenthesisToken; }
	const SeparatedSyntaxList<ExpressionSyntax>* Arguments()const noexcept { return &_arguments; }
	const SyntaxToken& CloseParenthesisToken()const noexcept { return _closeParenthesisToken; }
};

class PostfixExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifier;
	SyntaxToken _op;
	unique_ptr<ExpressionSyntax> _expression;

public:
	PostfixExpressionSyntax(const SyntaxToken& identifier, const SyntaxToken& op,
		unique_ptr<ExpressionSyntax>& expression)
		:_identifier(identifier), _op(op), _expression(std::move(expression))
	{
	}
	PostfixExpressionSyntax(PostfixExpressionSyntax&&) = default;
	PostfixExpressionSyntax& operator=(PostfixExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::PostfixExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& IdentifierToken()const noexcept { return _identifier; }
	const SyntaxToken& Op()const noexcept { return _op; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

}//MCF
