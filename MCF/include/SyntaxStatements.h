#pragma once

#include <optional>

#include "SyntaxExpressions.h"

namespace MCF {

class StatementSyntax :public SyntaxNode
{
protected:
	explicit StatementSyntax(const SyntaxTree& tree)noexcept
		:SyntaxNode(tree)
	{
	}
};

class BlockStatementSyntax final : public StatementSyntax
{
private:
	SyntaxToken _openBraceToken;
	SyntaxToken _closeBraceToken;
	vector<unique_ptr<StatementSyntax>> _statements;

public:
	BlockStatementSyntax(const SyntaxTree& tree, SyntaxToken open,
						 vector<unique_ptr<StatementSyntax>> statements,
						 SyntaxToken close)noexcept
		:StatementSyntax(tree),
		_openBraceToken(std::move(open)), _closeBraceToken(std::move(close)),
		_statements(std::move(statements))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BlockStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& OpenBraceToken()const noexcept { return _openBraceToken; }
	constexpr const SyntaxToken& CloseBraceToken()const noexcept { return _closeBraceToken; }
	const vector<const StatementSyntax*> Statements()const;
};

class TypeClauseSyntax final :public SyntaxNode
{
private:
	SyntaxToken _colonToken;
	SyntaxToken _identifier;

public:
	TypeClauseSyntax(const SyntaxTree& tree,
					 SyntaxToken colon, SyntaxToken identifier)noexcept
		:SyntaxNode(tree),
		_colonToken(std::move(colon)), _identifier(std::move(identifier))
	{
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::TypeClause; };
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& ColonToken()const noexcept { return _colonToken; }
	constexpr const SyntaxToken& Identifier()const noexcept { return _identifier; }
};

class VariableDeclarationSyntax final : public StatementSyntax
{
private:
	std::optional<TypeClauseSyntax> _typeClause;
	SyntaxToken _keyword;
	SyntaxToken _identifier;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _initializer;

public:
	VariableDeclarationSyntax(const SyntaxTree& tree,
							  SyntaxToken keyword, SyntaxToken identifier,
							  std::optional<TypeClauseSyntax> typeClause,
							  SyntaxToken equals,
							  unique_ptr<ExpressionSyntax> initializer)noexcept
		:StatementSyntax(tree),
		_typeClause(std::move(typeClause)),
		_keyword(std::move(keyword)), _identifier(std::move(identifier)),
		_equalsToken(std::move(equals)), _initializer(std::move(initializer))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::VariableDeclaration; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& Keyword()const noexcept { return _keyword; }
	constexpr const SyntaxToken& Identifier()const { return _identifier; }
	constexpr const std::optional<TypeClauseSyntax>& TypeClause()const noexcept { return _typeClause; }
	constexpr const SyntaxToken& EqualsToken()const noexcept { return _equalsToken; }
	const ExpressionSyntax* Initializer()const noexcept { return _initializer.get(); }
};

class ElseClauseSyntax final :public SyntaxNode
{
private:
	SyntaxToken _elseKeyword;
	unique_ptr<StatementSyntax> _elseStatement;

public:
	ElseClauseSyntax(const SyntaxTree& tree, SyntaxToken elseKeyword,
					 unique_ptr<StatementSyntax> elseStatement)noexcept
		:SyntaxNode(tree),
		_elseKeyword(std::move(elseKeyword)), _elseStatement(std::move(elseStatement))
	{
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ElseClause; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& ElseKeyword()const noexcept { return _elseKeyword; }
	const StatementSyntax* ElseStatement()const noexcept { return _elseStatement.get(); }
};

class IfStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _ifKeyword;
	unique_ptr<ExpressionSyntax> _condition;
	unique_ptr<StatementSyntax> _thenStatement;
	unique_ptr<ElseClauseSyntax> _elseClause;

public:
	IfStatementSyntax(const SyntaxTree& tree,
					  SyntaxToken ifKeyword,
					  unique_ptr<ExpressionSyntax> condition,
					  unique_ptr<StatementSyntax> thenStatement,
					  unique_ptr<ElseClauseSyntax> elseClause)noexcept
		:StatementSyntax(tree),
		_ifKeyword(std::move(ifKeyword)), _condition(std::move(condition)),
		_thenStatement(std::move(thenStatement)), _elseClause(std::move(elseClause))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::IfStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& IfKeyword()const noexcept { return _ifKeyword; }
	const ExpressionSyntax* Condition()const noexcept { return _condition.get(); }
	const StatementSyntax* ThenStatement()const noexcept { return _thenStatement.get(); }
	const ElseClauseSyntax* ElseClause()const noexcept { return _elseClause.get(); }
};

class WhileStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _whileKeyword;
	unique_ptr<ExpressionSyntax> _condition;
	unique_ptr<StatementSyntax> _body;

public:
	WhileStatementSyntax(const SyntaxTree& tree,
						 SyntaxToken whileKeyword,
						 unique_ptr<ExpressionSyntax> condition,
						 unique_ptr<StatementSyntax> body)noexcept
		:StatementSyntax(tree),
		_whileKeyword(std::move(whileKeyword)),
		_condition(std::move(condition)), _body(std::move(body))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::WhileStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& WhileKeyword()const noexcept { return _whileKeyword; }
	const ExpressionSyntax* Condition()const noexcept { return _condition.get(); }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
};

class DoWhileStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _doKeyword;
	SyntaxToken _whileKeyword;
	unique_ptr<StatementSyntax> _body;
	unique_ptr<ExpressionSyntax> _condition;

public:
	DoWhileStatementSyntax(const SyntaxTree& tree, SyntaxToken doKeyword,
						   unique_ptr<StatementSyntax> body, SyntaxToken whileKeyword,
						   unique_ptr<ExpressionSyntax> condition)noexcept
		:StatementSyntax(tree),
		_doKeyword(std::move(doKeyword)), _whileKeyword(std::move(whileKeyword)),
		_body(std::move(body)), _condition(std::move(condition))
	{
	}

	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const noexcept override { return SyntaxKind::DoWhileStatement; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& DoKeyword()const noexcept { return _doKeyword; }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
	constexpr const SyntaxToken& WhileKeyword()const noexcept { return _whileKeyword; }
	const ExpressionSyntax* Condition()const noexcept { return _condition.get(); }
};

class ForStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _keyword;
	SyntaxToken _identifier;
	SyntaxToken _equalsToken;
	SyntaxToken _toKeyword;
	unique_ptr<ExpressionSyntax> _lowerBound;
	unique_ptr<ExpressionSyntax> _upperBound;
	unique_ptr<StatementSyntax> _body;

public:
	ForStatementSyntax(const SyntaxTree& tree,
					   SyntaxToken keyword, SyntaxToken identifier,
					   SyntaxToken equals, unique_ptr<ExpressionSyntax> lowerBound,
					   SyntaxToken toKeyword, unique_ptr<ExpressionSyntax> upperBound,
					   unique_ptr<StatementSyntax> body)noexcept
		:StatementSyntax(tree),
		_keyword(std::move(keyword)), _identifier(std::move(identifier)),
		_equalsToken(std::move(equals)), _toKeyword(std::move(toKeyword)),
		_lowerBound(std::move(lowerBound)),
		_upperBound(std::move(upperBound)), _body(std::move(body))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ForStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& Keyword() const noexcept { return _keyword; }
	constexpr const SyntaxToken& Identifier() const noexcept { return _identifier; }
	constexpr const SyntaxToken& EqualsToken()const noexcept { return _equalsToken; }
	const ExpressionSyntax* LowerBound()const noexcept { return _lowerBound.get(); }
	constexpr const SyntaxToken& ToKeyword()const noexcept { return _toKeyword; }
	const ExpressionSyntax* UpperBound()const noexcept { return _upperBound.get(); }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
};

class BreakStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _keyword;

public:
	BreakStatementSyntax(const SyntaxTree& tree, SyntaxToken keyword)noexcept
		:StatementSyntax(tree), _keyword(std::move(keyword))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BreakStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& Keyword()const noexcept { return _keyword; }
};

class ContinueStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _keyword;

public:
	ContinueStatementSyntax(const SyntaxTree& tree, SyntaxToken keyword)noexcept
		:StatementSyntax(tree), _keyword(std::move(keyword))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ContinueStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& Keyword()const noexcept { return _keyword; }
};

class ReturnStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _keyword;
	unique_ptr<ExpressionSyntax> _expression;

public:
	ReturnStatementSyntax(const SyntaxTree& tree, SyntaxToken keyword,
						  unique_ptr<ExpressionSyntax> expression)noexcept
		:StatementSyntax(tree),
		_keyword(std::move(keyword)), _expression(std::move(expression))
	{
	}
	ReturnStatementSyntax(const SyntaxTree& tree, SyntaxToken keyword)noexcept
		:ReturnStatementSyntax(tree, std::move(keyword), nullptr)
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ReturnStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& Keyword()const noexcept { return _keyword; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

class ExpressionStatementSyntax final : public StatementSyntax
{
private:
	unique_ptr<ExpressionSyntax> _expression;

public:
	ExpressionStatementSyntax(const SyntaxTree& tree,
							  unique_ptr<ExpressionSyntax>& expression)noexcept
		:StatementSyntax(tree),
		_expression(std::move(expression))
	{
	}

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ExpressionStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

}//MCF
