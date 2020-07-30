#pragma once

#include <optional>

#include "SyntaxExpressions.h"

namespace MCF {

struct StatementSyntax :public SyntaxNode
{
protected:
	explicit StatementSyntax(const SyntaxTree& tree)noexcept
		:SyntaxNode(tree)
	{
	}
};

struct BlockStatementSyntax final : public StatementSyntax
{
	SyntaxToken OpenBraceToken;
	SyntaxToken CloseBraceToken;
	vector<unique_ptr<StatementSyntax>> Statements;

public:
	BlockStatementSyntax(const SyntaxTree& tree, SyntaxToken open,
						 vector<unique_ptr<StatementSyntax>> statements,
						 SyntaxToken close)noexcept
		:StatementSyntax(tree),
		OpenBraceToken(std::move(open)), CloseBraceToken(std::move(close)),
		Statements(std::move(statements))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BlockStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;
};

struct TypeClauseSyntax final :public SyntaxNode
{
	SyntaxToken ColonToken;
	SyntaxToken Identifier;

public:
	TypeClauseSyntax(const SyntaxTree& tree,
					 SyntaxToken colon, SyntaxToken identifier)noexcept
		:SyntaxNode(tree),
		ColonToken(std::move(colon)), Identifier(std::move(identifier))
	{
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::TypeClause; };
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct VariableDeclarationSyntax final : public StatementSyntax
{
	std::optional<TypeClauseSyntax> TypeClause;
	SyntaxToken Keyword;
	SyntaxToken Identifier;
	SyntaxToken EqualsToken;
	unique_ptr<ExpressionSyntax> Initializer;

public:
	VariableDeclarationSyntax(const SyntaxTree& tree,
							  SyntaxToken keyword, SyntaxToken identifier,
							  std::optional<TypeClauseSyntax> typeClause,
							  SyntaxToken equals,
							  unique_ptr<ExpressionSyntax> initializer)noexcept
		:StatementSyntax(tree),
		TypeClause(std::move(typeClause)),
		Keyword(std::move(keyword)), Identifier(std::move(identifier)),
		EqualsToken(std::move(equals)), Initializer(std::move(initializer))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::VariableDeclaration; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct ElseClauseSyntax final :public SyntaxNode
{
	SyntaxToken ElseKeyword;
	unique_ptr<StatementSyntax> ElseStatement;

public:
	ElseClauseSyntax(const SyntaxTree& tree, SyntaxToken elseKeyword,
					 unique_ptr<StatementSyntax> elseStatement)noexcept
		:SyntaxNode(tree),
		ElseKeyword(std::move(elseKeyword)), ElseStatement(std::move(elseStatement))
	{
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ElseClause; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct IfStatementSyntax final :public StatementSyntax
{
	SyntaxToken IfKeyword;
	unique_ptr<ExpressionSyntax> Condition;
	unique_ptr<StatementSyntax> ThenStatement;
	unique_ptr<ElseClauseSyntax> ElseClause;

public:
	IfStatementSyntax(const SyntaxTree& tree,
					  SyntaxToken ifKeyword,
					  unique_ptr<ExpressionSyntax> condition,
					  unique_ptr<StatementSyntax> thenStatement,
					  unique_ptr<ElseClauseSyntax> elseClause)noexcept
		:StatementSyntax(tree),
		IfKeyword(std::move(ifKeyword)), Condition(std::move(condition)),
		ThenStatement(std::move(thenStatement)), ElseClause(std::move(elseClause))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::IfStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct WhileStatementSyntax final :public StatementSyntax
{
	SyntaxToken WhileKeyword;
	unique_ptr<ExpressionSyntax> Condition;
	unique_ptr<StatementSyntax> Body;

public:
	WhileStatementSyntax(const SyntaxTree& tree,
						 SyntaxToken whileKeyword,
						 unique_ptr<ExpressionSyntax> condition,
						 unique_ptr<StatementSyntax> body)noexcept
		:StatementSyntax(tree),
		WhileKeyword(std::move(whileKeyword)),
		Condition(std::move(condition)), Body(std::move(body))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::WhileStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct DoWhileStatementSyntax final :public StatementSyntax
{
	SyntaxToken DoKeyword;
	SyntaxToken WhileKeyword;
	unique_ptr<StatementSyntax> Body;
	unique_ptr<ExpressionSyntax> Condition;

public:
	DoWhileStatementSyntax(const SyntaxTree& tree, SyntaxToken doKeyword,
						   unique_ptr<StatementSyntax> body, SyntaxToken whileKeyword,
						   unique_ptr<ExpressionSyntax> condition)noexcept
		:StatementSyntax(tree),
		DoKeyword(std::move(doKeyword)), WhileKeyword(std::move(whileKeyword)),
		Body(std::move(body)), Condition(std::move(condition))
	{
	}

	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const noexcept override { return SyntaxKind::DoWhileStatement; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

};

struct ForStatementSyntax final :public StatementSyntax
{
	SyntaxToken Keyword;
	SyntaxToken Identifier;
	SyntaxToken EqualsToken;
	SyntaxToken ToKeyword;
	unique_ptr<ExpressionSyntax> LowerBound;
	unique_ptr<ExpressionSyntax> UpperBound;
	unique_ptr<StatementSyntax> Body;

public:
	ForStatementSyntax(const SyntaxTree& tree,
					   SyntaxToken keyword, SyntaxToken identifier,
					   SyntaxToken equals, unique_ptr<ExpressionSyntax> lowerBound,
					   SyntaxToken toKeyword, unique_ptr<ExpressionSyntax> upperBound,
					   unique_ptr<StatementSyntax> body)noexcept
		:StatementSyntax(tree),
		Keyword(std::move(keyword)), Identifier(std::move(identifier)),
		EqualsToken(std::move(equals)), ToKeyword(std::move(toKeyword)),
		LowerBound(std::move(lowerBound)),
		UpperBound(std::move(upperBound)), Body(std::move(body))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ForStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct BreakStatementSyntax final :public StatementSyntax
{
	SyntaxToken Keyword;

public:
	BreakStatementSyntax(const SyntaxTree& tree, SyntaxToken keyword)noexcept
		:StatementSyntax(tree), Keyword(std::move(keyword))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BreakStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct ContinueStatementSyntax final :public StatementSyntax
{
	SyntaxToken Keyword;

public:
	ContinueStatementSyntax(const SyntaxTree& tree, SyntaxToken keyword)noexcept
		:StatementSyntax(tree), Keyword(std::move(keyword))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ContinueStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct ReturnStatementSyntax final :public StatementSyntax
{
	SyntaxToken Keyword;
	unique_ptr<ExpressionSyntax> Expression;

public:
	ReturnStatementSyntax(const SyntaxTree& tree, SyntaxToken keyword,
						  unique_ptr<ExpressionSyntax> expression)noexcept
		:StatementSyntax(tree),
		Keyword(std::move(keyword)), Expression(std::move(expression))
	{
	}
	ReturnStatementSyntax(const SyntaxTree& tree, SyntaxToken keyword)noexcept
		:ReturnStatementSyntax(tree, std::move(keyword), nullptr)
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ReturnStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct ExpressionStatementSyntax final : public StatementSyntax
{
	unique_ptr<ExpressionSyntax> Expression;

public:
	ExpressionStatementSyntax(const SyntaxTree& tree,
							  unique_ptr<ExpressionSyntax>& expression)noexcept
		:StatementSyntax(tree),
		Expression(std::move(expression))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ExpressionStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

}//MCF
