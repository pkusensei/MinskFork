#pragma once

#include "SyntaxToken.h"

namespace MCF {

struct ExpressionSyntax :public SyntaxNode
{
protected:
	explicit ExpressionSyntax(const SyntaxTree& tree)noexcept
		:SyntaxNode(tree)
	{
	}
};

struct AssignmentExpressionSyntax final :public ExpressionSyntax
{
	SyntaxToken IdentifierToken;
	SyntaxToken AssignmentToken;
	unique_ptr<ExpressionSyntax> Expression;

public:
	AssignmentExpressionSyntax(const SyntaxTree& tree,
							   SyntaxToken identifier,
							   SyntaxToken assignment,
							   unique_ptr<ExpressionSyntax> expression)noexcept
		:ExpressionSyntax(tree),
		IdentifierToken(std::move(identifier)),
		AssignmentToken(std::move(assignment)),
		Expression(std::move(expression))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::AssignmentExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct UnaryExpressionSyntax final :public ExpressionSyntax
{
	SyntaxToken OperatorToken;
	unique_ptr<ExpressionSyntax> Operand;

public:
	UnaryExpressionSyntax(const SyntaxTree& tree,
						  SyntaxToken operatorToken,
						  unique_ptr<ExpressionSyntax> operand)noexcept
		:ExpressionSyntax(tree),
		OperatorToken(std::move(operatorToken)), Operand(std::move(operand))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::UnaryExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct BinaryExpressionSyntax final :public ExpressionSyntax
{
	SyntaxToken OperatorToken;
	unique_ptr<ExpressionSyntax> Left;
	unique_ptr<ExpressionSyntax> Right;

public:
	BinaryExpressionSyntax(const SyntaxTree& tree,
						   unique_ptr<ExpressionSyntax> left,
						   SyntaxToken operatorToken,
						   unique_ptr<ExpressionSyntax> right)noexcept
		:ExpressionSyntax(tree),
		OperatorToken(std::move(operatorToken)),
		Left(std::move(left)), Right(std::move(right))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BinaryExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct ParenthesizedExpressionSyntax final :public ExpressionSyntax
{
	SyntaxToken OpenParenthesisToken;
	SyntaxToken CloseParenthesisToken;
	unique_ptr<ExpressionSyntax> Expression;

public:
	ParenthesizedExpressionSyntax(const SyntaxTree& tree,
								  SyntaxToken open,
								  unique_ptr<ExpressionSyntax> expression,
								  SyntaxToken close)noexcept
		:ExpressionSyntax(tree),
		OpenParenthesisToken(std::move(open)),
		CloseParenthesisToken(std::move(close)),
		Expression(std::move(expression))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ParenthesizedExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct LiteralExpressionSyntax final :public ExpressionSyntax
{
	SyntaxToken LiteralToken;
	ValueType Value;

public:
	LiteralExpressionSyntax(const SyntaxTree& tree,
							SyntaxToken literalToken, ValueType value)noexcept
		:ExpressionSyntax(tree),
		LiteralToken(std::move(literalToken)), Value(std::move(value))
	{
	}
	LiteralExpressionSyntax(const SyntaxTree& tree, SyntaxToken literalToken)
		:LiteralExpressionSyntax(tree, literalToken, literalToken.Value)
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::LiteralExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct NameExpressionSyntax final :public ExpressionSyntax
{
	SyntaxToken IdentifierToken;

public:
	NameExpressionSyntax(const SyntaxTree& tree, SyntaxToken identifier)noexcept
		:ExpressionSyntax(tree),
		IdentifierToken(std::move(identifier))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::NameExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct CallExpressionSyntax final :public ExpressionSyntax
{
	SyntaxToken Identifier;
	SyntaxToken OpenParenthesisToken;
	SyntaxToken CloseParenthesisToken;
	SeparatedSyntaxList<ExpressionSyntax> Arguments;

public:
	CallExpressionSyntax(const SyntaxTree& tree,
						 SyntaxToken identifier,
						 SyntaxToken open,
						 SeparatedSyntaxList<ExpressionSyntax> arguments,
						 SyntaxToken close)noexcept
		:ExpressionSyntax(tree),
		Identifier(std::move(identifier)),
		OpenParenthesisToken(std::move(open)),
		CloseParenthesisToken(std::move(close)),
		Arguments(std::move(arguments))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::CallExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct PostfixExpressionSyntax final :public ExpressionSyntax
{
	SyntaxToken Identifier;
	SyntaxToken Op;
	unique_ptr<ExpressionSyntax> Expression;

public:
	PostfixExpressionSyntax(const SyntaxTree& tree, SyntaxToken identifier, SyntaxToken op,
							unique_ptr<ExpressionSyntax> expression)noexcept
		:ExpressionSyntax(tree),
		Identifier(std::move(identifier)), Op(std::move(op)),
		Expression(std::move(expression))
	{
	}

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::PostfixExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

}//MCF
