#pragma once

#include "BoundLabel.h"
#include "BoundNode.h"
#include "Symbols.h"

namespace MCF {

struct BoundExpression;

struct BoundStatement :public BoundNode
{
protected:
	explicit BoundStatement(const SyntaxNode& syntax)noexcept
		:BoundNode(syntax)
	{
	}
};

struct BoundBlockStatement final : public BoundStatement
{
	vector<shared_ptr<BoundStatement>> Statements;

public:
	explicit BoundBlockStatement(const SyntaxNode& syntax,
								 vector<shared_ptr<BoundStatement>> statements)noexcept
		:BoundStatement(syntax),
		Statements(std::move(statements))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BlockStatement; }

};

struct BoundNopStatement final :public BoundStatement
{
	explicit BoundNopStatement(const SyntaxNode& syntax)noexcept
		:BoundStatement(syntax)
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::NopStatement; }
};

struct BoundVariableDeclaration final :public BoundStatement
{
	shared_ptr<VariableSymbol> Variable;
	shared_ptr<BoundExpression> Initializer;

public:
	explicit BoundVariableDeclaration(const SyntaxNode& syntax,
									  shared_ptr<VariableSymbol> variable,
									  shared_ptr<BoundExpression> initializer)noexcept
		:BoundStatement(syntax),
		Variable(std::move(variable)), Initializer(std::move(initializer))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableDeclaration; }

};

struct BoundIfStatement final :public BoundStatement
{
	shared_ptr<BoundExpression> Condition;
	shared_ptr<BoundStatement> ThenStatement;
	shared_ptr<BoundStatement> ElseStatement;

public:
	explicit BoundIfStatement(const SyntaxNode& syntax,
							  shared_ptr<BoundExpression> condition,
							  shared_ptr<BoundStatement> thenStatement,
							  shared_ptr<BoundStatement> elseStatement)noexcept
		:BoundStatement(syntax),
		Condition(std::move(condition)), ThenStatement(std::move(thenStatement)),
		ElseStatement(std::move(elseStatement))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::IfStatement; }

};

struct BoundLoopStatement :public BoundStatement
{
	BoundLabel BreakLabel;
	BoundLabel ContinueLabel;

protected:
	explicit BoundLoopStatement(const SyntaxNode& syntax,
								BoundLabel breakLabel,
								BoundLabel continueLabel)noexcept
		:BoundStatement(syntax),
		BreakLabel(std::move(breakLabel)), ContinueLabel(std::move(continueLabel))
	{
	}

};

struct BoundWhileStatement final :public BoundLoopStatement
{
	shared_ptr<BoundExpression> Condition;
	shared_ptr<BoundStatement> Body;

public:
	explicit BoundWhileStatement(const SyntaxNode& syntax,
								 shared_ptr<BoundExpression> condition,
								 shared_ptr<BoundStatement> body,
								 BoundLabel breakLabel, BoundLabel continueLabel)
		: BoundLoopStatement(syntax, std::move(breakLabel), std::move(continueLabel)),
		Condition(std::move(condition)), Body(std::move(body))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::WhileStatement; }

};

struct BoundDoWhileStatement final :public BoundLoopStatement
{
	shared_ptr<BoundStatement> Body;
	shared_ptr<BoundExpression> Condition;

public:
	explicit BoundDoWhileStatement(const SyntaxNode& syntax,
								   shared_ptr<BoundStatement> body,
								   shared_ptr<BoundExpression> condition,
								   BoundLabel breakLabel,
								   BoundLabel continueLabel)noexcept
		: BoundLoopStatement(syntax, std::move(breakLabel), std::move(continueLabel)),
		Body(std::move(body)), Condition(std::move(condition))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::DoWhileStatement; }

};

struct BoundForStatement final :public BoundLoopStatement
{
	shared_ptr<VariableSymbol> Variable;
	shared_ptr<BoundExpression> LowerBound;
	shared_ptr<BoundExpression> UpperBound;
	shared_ptr<BoundStatement> Body;

public:
	explicit BoundForStatement(const SyntaxNode& syntax,
							   shared_ptr<VariableSymbol> variable,
							   shared_ptr<BoundExpression> lowerBound,
							   shared_ptr<BoundExpression> upperBound,
							   shared_ptr<BoundStatement> body,
							   BoundLabel breakLabel,
							   BoundLabel continueLabel)noexcept
		: BoundLoopStatement(syntax, std::move(breakLabel), std::move(continueLabel)),
		Variable(std::move(variable)), LowerBound(std::move(lowerBound)),
		UpperBound(std::move(upperBound)), Body(std::move(body))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ForStatement; }

};

struct BoundLabelStatement final :public BoundStatement
{
	BoundLabel Label;

public:
	explicit BoundLabelStatement(const SyntaxNode& syntax, BoundLabel label)noexcept
		:BoundStatement(syntax),
		Label(std::move(label))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LabelStatement; }

};

struct BoundGotoStatement final :public BoundStatement
{
	BoundLabel Label;

public:
	explicit BoundGotoStatement(const SyntaxNode& syntax, BoundLabel label)noexcept
		:BoundStatement(syntax),
		Label(std::move(label))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::GotoStatement; }

};

struct BoundConditionalGotoStatement final :public BoundStatement
{
	BoundLabel Label;
	shared_ptr<BoundExpression> Condition;
	bool JumpIfTrue;

public:
	explicit BoundConditionalGotoStatement(const SyntaxNode& syntax,
										   BoundLabel label,
										   shared_ptr<BoundExpression> condition,
										   bool jumpIfTrue = true)noexcept
		:BoundStatement(syntax),
		Label(std::move(label)), Condition(std::move(condition)),
		JumpIfTrue(std::move(jumpIfTrue))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConditionalGotoStatement; }

};

struct BoundReturnStatement final :public BoundStatement
{
	shared_ptr<BoundExpression> Expression;

public:
	explicit BoundReturnStatement(const SyntaxNode& syntax,
								  shared_ptr<BoundExpression> expression)
		:BoundStatement(syntax),
		Expression(std::move(expression))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ReturnStatement; }

};

struct BoundExpressionStatement final : public BoundStatement
{
	shared_ptr<BoundExpression> Expression;

public:
	explicit BoundExpressionStatement(const SyntaxNode& syntax,
									  shared_ptr<BoundExpression> expression)
		:BoundStatement(syntax),
		Expression(std::move(expression))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ExpressionStatement; }

};

}//MCF
