#pragma once

#include <optional>

#include "SyntaxExpressions.h"

namespace MCF {

class StatementSyntax :public SyntaxNode
{
};

class BlockStatementSyntax final : public StatementSyntax
{
private:
	SyntaxToken _openBraceToken;
	SyntaxToken _closeBraceToken;
	vector<unique_ptr<StatementSyntax>> _statements;

public:
	BlockStatementSyntax(const SyntaxToken& open,
		vector<unique_ptr<StatementSyntax>>& statements, const SyntaxToken& close)
		:_openBraceToken(open), _closeBraceToken(close),
		_statements(std::move(statements))
	{
	}
	BlockStatementSyntax(BlockStatementSyntax&&) = default;
	BlockStatementSyntax& operator=(BlockStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BlockStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& OpenBraceToken()const noexcept { return _openBraceToken; }
	const SyntaxToken& CloseBraceToken()const noexcept { return _closeBraceToken; }
	const vector<const StatementSyntax*> Statements()const;
};

class TypeClauseSyntax final :public SyntaxNode
{
private:
	SyntaxToken _colonToken;
	SyntaxToken _identifier;

public:
	TypeClauseSyntax(const SyntaxToken& colon, const SyntaxToken& identifier)
		:_colonToken(colon), _identifier(identifier)
	{
	}
	TypeClauseSyntax(const TypeClauseSyntax&) = default;
	TypeClauseSyntax& operator=(const TypeClauseSyntax&) = default;

	// Inherited via SyntaxNode
	SyntaxKind Kind() const override { return SyntaxKind::TypeClause; };
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& ColonToken()const noexcept { return _colonToken; }
	const SyntaxToken& Identifier()const noexcept { return _identifier; }
};

class VariableDeclarationSyntax final : public StatementSyntax
{
private:
	SyntaxToken _keyword;
	SyntaxToken _identifier;
	std::optional<TypeClauseSyntax> _typeClause;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _initializer;

public:
	VariableDeclarationSyntax(const SyntaxToken& keyword, const SyntaxToken& identifier,
		const std::optional<TypeClauseSyntax>& typeClause,
		const SyntaxToken& equals,
		unique_ptr<ExpressionSyntax>& initializer)
		: _keyword(keyword), _identifier(identifier), _typeClause(typeClause),
		_equalsToken(equals), _initializer(std::move(initializer))
	{
	}

	VariableDeclarationSyntax(VariableDeclarationSyntax&&) = default;
	VariableDeclarationSyntax& operator=(VariableDeclarationSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::VariableDeclaration; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& Keyword()const noexcept { return _keyword; }
	const SyntaxToken& Identifier()const { return _identifier; }
	const std::optional<TypeClauseSyntax>& TypeClause()const noexcept { return _typeClause; }
	const SyntaxToken& EqualsToken()const noexcept { return _equalsToken; }
	const ExpressionSyntax* Initializer()const noexcept { return _initializer.get(); }
};

class ElseClauseSyntax final :public SyntaxNode
{
private:
	SyntaxToken _elseKeyword;
	unique_ptr<StatementSyntax> _elseStatement;

public:
	ElseClauseSyntax(const SyntaxToken& elseKeyword,
		unique_ptr<StatementSyntax>& elseStatement)
		:_elseKeyword(elseKeyword), _elseStatement(std::move(elseStatement))
	{
	}
	ElseClauseSyntax(ElseClauseSyntax&&) = default;
	ElseClauseSyntax& operator=(ElseClauseSyntax&&) = default;

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ElseClause; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& ElseKeyword()const noexcept { return _elseKeyword; }
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
	IfStatementSyntax(const SyntaxToken& ifKeyword,
		unique_ptr<ExpressionSyntax>& condition,
		unique_ptr<StatementSyntax>& thenStatement,
		unique_ptr<ElseClauseSyntax>& elseClause)
		:_ifKeyword(ifKeyword), _condition(std::move(condition)),
		_thenStatement(std::move(thenStatement)), _elseClause(std::move(elseClause))
	{
	}
	IfStatementSyntax(IfStatementSyntax&&) = default;
	IfStatementSyntax& operator=(IfStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::IfStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& IfKeyword()const noexcept { return _ifKeyword; }
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
	WhileStatementSyntax(const SyntaxToken& whileKeyword,
		unique_ptr<ExpressionSyntax>& condition, unique_ptr<StatementSyntax>& body)
		:_whileKeyword(whileKeyword), _condition(std::move(condition)),
		_body(std::move(body))
	{
	}
	WhileStatementSyntax(WhileStatementSyntax&&) = default;
	WhileStatementSyntax& operator=(WhileStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::WhileStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& WhileKeyword()const noexcept { return _whileKeyword; }
	const ExpressionSyntax* Condition()const noexcept { return _condition.get(); }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
};

class DoWhileStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _doKeyword;
	unique_ptr<StatementSyntax> _body;
	SyntaxToken _whileKeyword;
	unique_ptr<ExpressionSyntax> _condition;

public:
	DoWhileStatementSyntax(const SyntaxToken& doKeyword,
		unique_ptr<StatementSyntax>& body, const SyntaxToken& whileKeyword,
		unique_ptr<ExpressionSyntax>& condition)
		:_doKeyword(doKeyword), _body(std::move(body)),
		_whileKeyword(whileKeyword), _condition(std::move(condition))
	{
	}
	DoWhileStatementSyntax(DoWhileStatementSyntax&&) = default;
	DoWhileStatementSyntax& operator=(DoWhileStatementSyntax&&) = default;


	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::DoWhileStatement; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& DoKeyword()const noexcept { return _doKeyword; }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
	const SyntaxToken& WhileKeyword()const noexcept { return _whileKeyword; }
	const ExpressionSyntax* Condition()const noexcept { return _condition.get(); }
};

class ForStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _keyword;
	SyntaxToken _identifier;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _lowerBound;
	SyntaxToken _toKeyword;
	unique_ptr<ExpressionSyntax> _upperBound;
	unique_ptr<StatementSyntax> _body;

public:
	ForStatementSyntax(const SyntaxToken& keyword, const SyntaxToken& identifier,
		const SyntaxToken& equals, unique_ptr<ExpressionSyntax>& lowerBound,
		const SyntaxToken& toKeyword, unique_ptr<ExpressionSyntax>& upperBound,
		unique_ptr<StatementSyntax>& body)
		:_keyword(keyword), _identifier(identifier), _equalsToken(equals),
		_lowerBound(std::move(lowerBound)), _toKeyword(toKeyword),
		_upperBound(std::move(upperBound)), _body(std::move(body))
	{
	}
	ForStatementSyntax(ForStatementSyntax&&) = default;
	ForStatementSyntax& operator=(ForStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ForStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& Keyword() const noexcept { return _keyword; }
	const SyntaxToken& Identifier() const noexcept { return _identifier; }
	const SyntaxToken& EqualsToken()const noexcept { return _equalsToken; }
	const ExpressionSyntax* LowerBound()const noexcept { return _lowerBound.get(); }
	const SyntaxToken& ToKeyword()const noexcept { return _toKeyword; }
	const ExpressionSyntax* UpperBound()const noexcept { return _upperBound.get(); }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
};

class BreakStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _keyword;

public:
	explicit BreakStatementSyntax(const SyntaxToken& keyword)
		:_keyword(keyword)
	{
	}
	BreakStatementSyntax(BreakStatementSyntax&&) = default;
	BreakStatementSyntax& operator=(BreakStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BreakStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& Keyword()const noexcept { return _keyword; }
};

class ContinueStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _keyword;

public:
	explicit ContinueStatementSyntax(const SyntaxToken& keyword)
		:_keyword(keyword)
	{
	}
	ContinueStatementSyntax(ContinueStatementSyntax&&) = default;
	ContinueStatementSyntax& operator=(ContinueStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ContinueStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& Keyword()const noexcept { return _keyword; }
};

class ReturnStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _keyword;
	unique_ptr<ExpressionSyntax> _expression;

public:
	explicit ReturnStatementSyntax(const SyntaxToken& retKeyword,
		std::nullptr_t n = nullptr)
		:_keyword(retKeyword), _expression(nullptr)
	{
	}
	ReturnStatementSyntax(const SyntaxToken& retKeyword,
		unique_ptr<ExpressionSyntax>& expression)
		: _keyword(retKeyword), _expression(std::move(expression))
	{
	}
	ReturnStatementSyntax(ReturnStatementSyntax&&) = default;
	ReturnStatementSyntax& operator=(ReturnStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ReturnStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& Keyword()const noexcept { return _keyword; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

class ExpressionStatementSyntax final : public StatementSyntax
{
private:
	unique_ptr<ExpressionSyntax> _expression;

public:
	explicit ExpressionStatementSyntax(unique_ptr<ExpressionSyntax>& expression)
		:_expression(std::move(expression))
	{
	}
	ExpressionStatementSyntax(ExpressionStatementSyntax&&) = default;
	ExpressionStatementSyntax& operator=(ExpressionStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ExpressionStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

}//MCF
