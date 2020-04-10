#pragma once

#include "SyntaxToken.h"

namespace MCF {

class ExpressionSyntax :public SyntaxNode
{
protected:
	explicit ExpressionSyntax(const SyntaxTree& tree)
		:SyntaxNode(tree)
	{
	}
};

class AssignmentExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _expression;

public:
	AssignmentExpressionSyntax(const SyntaxTree& tree,
		SyntaxToken identifier,
		SyntaxToken equals, unique_ptr<ExpressionSyntax> expression)
		:ExpressionSyntax(tree),
		_identifierToken(std::move(identifier)),
		_equalsToken(std::move(equals)), _expression(std::move(expression))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::AssignmentExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& IdentifierToken()const noexcept { return _identifierToken; }
	constexpr const SyntaxToken& EqualsToken()const noexcept { return _equalsToken; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

class UnaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _operand;

public:
	UnaryExpressionSyntax(const SyntaxTree& tree,
		SyntaxToken operatorToken,
		unique_ptr<ExpressionSyntax> operand)
		:ExpressionSyntax(tree),
		_operatorToken(std::move(operatorToken)), _operand(std::move(operand))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::UnaryExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& OperatorToken()const noexcept { return _operatorToken; }
	const ExpressionSyntax* Operand()const noexcept { return _operand.get(); }
};

class BinaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _left;
	unique_ptr<ExpressionSyntax> _right;

public:
	BinaryExpressionSyntax(const SyntaxTree& tree,
		unique_ptr<ExpressionSyntax> left,
		SyntaxToken operatorToken,
		unique_ptr<ExpressionSyntax> right)
		:ExpressionSyntax(tree),
		_operatorToken(std::move(operatorToken)),
		_left(std::move(left)), _right(std::move(right))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BinaryExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& OperatorToken()const noexcept { return _operatorToken; }
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
	ParenthesizedExpressionSyntax(const SyntaxTree& tree,
		SyntaxToken open,
		unique_ptr<ExpressionSyntax> expression,
		SyntaxToken close)
		:ExpressionSyntax(tree),
		_openParenthesisToken(std::move(open)),
		_closeParenthesisToken(std::move(close)),
		_expression(std::move(expression))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ParenthesizedExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& OpenParenthesisToken()const noexcept { return _openParenthesisToken; }
	constexpr const SyntaxToken& CloseParenthesisToken()const noexcept { return _closeParenthesisToken; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

class LiteralExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _literalToken;
	ValueType _value;

public:
	LiteralExpressionSyntax(const SyntaxTree& tree,
		SyntaxToken literalToken, ValueType value)
		:ExpressionSyntax(tree),
		_literalToken(std::move(literalToken)), _value(std::move(value))
	{
	}
	LiteralExpressionSyntax(const SyntaxTree& tree, SyntaxToken literalToken)
		:LiteralExpressionSyntax(tree, literalToken, literalToken.Value())
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::LiteralExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& LiteralToken()const noexcept { return _literalToken; }
	constexpr const ValueType& Value()const noexcept { return _value; }
};

class NameExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;

public:
	NameExpressionSyntax(const SyntaxTree& tree, SyntaxToken identifier)
		:ExpressionSyntax(tree),
		_identifierToken(std::move(identifier))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::NameExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& IdentifierToken()const noexcept { return _identifierToken; }
};

class CallExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifier;
	SyntaxToken _openParenthesisToken;
	SeparatedSyntaxList<ExpressionSyntax> _arguments;
	SyntaxToken _closeParenthesisToken;

public:
	CallExpressionSyntax(const SyntaxTree& tree,
		SyntaxToken identifier,
		SyntaxToken open,
		SeparatedSyntaxList<ExpressionSyntax> arguments,
		SyntaxToken close)
		:ExpressionSyntax(tree),
		_identifier(std::move(identifier)),
		_openParenthesisToken(std::move(open)),
		_arguments(std::move(arguments)),
		_closeParenthesisToken(std::move(close))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::CallExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& Identifier()const noexcept { return _identifier; }
	constexpr const SyntaxToken& OpenParenthesisToken()const noexcept { return _openParenthesisToken; }
	constexpr const SeparatedSyntaxList<ExpressionSyntax>& Arguments()const noexcept { return _arguments; }
	constexpr const SyntaxToken& CloseParenthesisToken()const noexcept { return _closeParenthesisToken; }
};

class PostfixExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifier;
	SyntaxToken _op;
	unique_ptr<ExpressionSyntax> _expression;

public:
	PostfixExpressionSyntax(const SyntaxTree& tree, SyntaxToken identifier, SyntaxToken op,
		unique_ptr<ExpressionSyntax> expression)
		:ExpressionSyntax(tree),
		_identifier(std::move(identifier)), _op(std::move(op)),
		_expression(std::move(expression))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::PostfixExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& IdentifierToken()const noexcept { return _identifier; }
	constexpr const SyntaxToken& Op()const noexcept { return _op; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

}//MCF
