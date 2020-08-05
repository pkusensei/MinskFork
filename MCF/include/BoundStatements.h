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
		:BoundNode{ syntax }
	{
	}

public:
	virtual unique_ptr<BoundStatement> Clone()const = 0;
	virtual bool Equals(const BoundStatement& other)const noexcept
	{
		return std::addressof(Syntax()) == std::addressof(other.Syntax()) && Kind() == other.Kind();
	}

	bool operator==(const BoundStatement& other)const noexcept { return Equals(other); }
	bool operator!=(const BoundStatement& other)const noexcept { return !Equals(other); }
};

struct BoundBlockStatement final : public BoundStatement
{
	vector<unique_ptr<BoundStatement>> Statements;

public:
	explicit BoundBlockStatement(const SyntaxNode& syntax,
								 vector<unique_ptr<BoundStatement>> statements)noexcept
		:BoundStatement(syntax),
		Statements(std::move(statements))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BlockStatement; }

	unique_ptr<BoundStatement> Clone()const override;
	bool Equals(const BoundStatement& other)const noexcept override;
};

struct BoundNopStatement final :public BoundStatement
{
	explicit BoundNopStatement(const SyntaxNode& syntax)noexcept
		:BoundStatement(syntax)
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::NopStatement; }

	unique_ptr<BoundStatement> Clone()const override
	{
		return UniqueClone<BoundNopStatement, BoundStatement>(*this);
	}
};

struct BoundVariableDeclaration final :public BoundStatement
{
	unique_ptr<VariableSymbol> Variable;
	unique_ptr<BoundExpression> Initializer;

public:
	explicit BoundVariableDeclaration(const SyntaxNode& syntax,
									  unique_ptr<VariableSymbol> variable,
									  unique_ptr<BoundExpression> initializer)noexcept
		:BoundStatement(syntax),
		Variable(std::move(variable)), Initializer(std::move(initializer))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableDeclaration; }

	unique_ptr<BoundStatement> Clone()const override;
	bool Equals(const BoundStatement& other)const noexcept override;
};

struct BoundIfStatement final :public BoundStatement
{
	unique_ptr<BoundExpression> Condition;
	unique_ptr<BoundStatement> ThenStatement;
	unique_ptr<BoundStatement> ElseStatement;

public:
	explicit BoundIfStatement(const SyntaxNode& syntax,
							  unique_ptr<BoundExpression> condition,
							  unique_ptr<BoundStatement> thenStatement,
							  unique_ptr<BoundStatement> elseStatement)noexcept
		:BoundStatement(syntax),
		Condition(std::move(condition)), ThenStatement(std::move(thenStatement)),
		ElseStatement(std::move(elseStatement))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::IfStatement; }

	unique_ptr<BoundStatement> Clone()const override;
	bool Equals(const BoundStatement& other)const noexcept override;
};

struct BoundLoopStatement :public BoundStatement
{
	BoundLabel BreakLabel;
	BoundLabel ContinueLabel;

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
	unique_ptr<BoundExpression> Condition;
	unique_ptr<BoundStatement> Body;

public:
	explicit BoundWhileStatement(const SyntaxNode& syntax,
								 unique_ptr<BoundExpression> condition,
								 unique_ptr<BoundStatement> body,
								 BoundLabel breakLabel, BoundLabel continueLabel)
		: BoundLoopStatement(syntax, std::move(breakLabel), std::move(continueLabel)),
		Condition(std::move(condition)), Body(std::move(body))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::WhileStatement; }

	unique_ptr<BoundStatement> Clone()const override;
	bool Equals(const BoundStatement& other)const noexcept override;
};

struct BoundDoWhileStatement final :public BoundLoopStatement
{
	unique_ptr<BoundStatement> Body;
	unique_ptr<BoundExpression> Condition;

public:
	explicit BoundDoWhileStatement(const SyntaxNode& syntax,
								   unique_ptr<BoundStatement> body,
								   unique_ptr<BoundExpression> condition,
								   BoundLabel breakLabel,
								   BoundLabel continueLabel)noexcept
		: BoundLoopStatement(syntax, std::move(breakLabel), std::move(continueLabel)),
		Body(std::move(body)), Condition(std::move(condition))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::DoWhileStatement; }

	unique_ptr<BoundStatement> Clone()const override;

	bool Equals(const BoundStatement& other)const noexcept override;
};

struct BoundForStatement final :public BoundLoopStatement
{
	unique_ptr<VariableSymbol> Variable;
	unique_ptr<BoundExpression> LowerBound;
	unique_ptr<BoundExpression> UpperBound;
	unique_ptr<BoundStatement> Body;

public:
	explicit BoundForStatement(const SyntaxNode& syntax,
							   unique_ptr<VariableSymbol> variable,
							   unique_ptr<BoundExpression> lowerBound,
							   unique_ptr<BoundExpression> upperBound,
							   unique_ptr<BoundStatement> body,
							   BoundLabel breakLabel,
							   BoundLabel continueLabel)noexcept
		: BoundLoopStatement(syntax, std::move(breakLabel), std::move(continueLabel)),
		Variable(std::move(variable)), LowerBound(std::move(lowerBound)),
		UpperBound(std::move(upperBound)), Body(std::move(body))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ForStatement; }

	unique_ptr<BoundStatement> Clone()const override;

	bool Equals(const BoundStatement& other)const noexcept override;
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

	unique_ptr<BoundStatement> Clone()const override
	{
		return UniqueClone<BoundLabelStatement, BoundStatement>(*this);
	}
	bool Equals(const BoundStatement& other)const noexcept override;
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

	unique_ptr<BoundStatement> Clone()const override
	{
		return UniqueClone<BoundGotoStatement, BoundStatement>(*this);
	}

	bool Equals(const BoundStatement& other)const noexcept override;
};

struct BoundConditionalGotoStatement final :public BoundStatement
{
	BoundLabel Label;
	unique_ptr<BoundExpression> Condition;
	bool JumpIfTrue;

public:
	explicit BoundConditionalGotoStatement(const SyntaxNode& syntax,
										   BoundLabel label,
										   unique_ptr<BoundExpression> condition,
										   bool jumpIfTrue = true)noexcept
		:BoundStatement(syntax),
		Label(std::move(label)), Condition(std::move(condition)),
		JumpIfTrue(jumpIfTrue)
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConditionalGotoStatement; }

	unique_ptr<BoundStatement> Clone()const override;

	bool Equals(const BoundStatement& other)const noexcept override;
};

struct BoundReturnStatement final :public BoundStatement
{
	unique_ptr<BoundExpression> Expression;

public:
	explicit BoundReturnStatement(const SyntaxNode& syntax,
								  unique_ptr<BoundExpression> expression)
		:BoundStatement(syntax),
		Expression(std::move(expression))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ReturnStatement; }

	unique_ptr<BoundStatement> Clone()const override;

	bool Equals(const BoundStatement& other)const noexcept override;
};

struct BoundExpressionStatement final : public BoundStatement
{
	unique_ptr<BoundExpression> Expression;

public:
	explicit BoundExpressionStatement(const SyntaxNode& syntax,
									  unique_ptr<BoundExpression> expression)
		:BoundStatement(syntax),
		Expression(std::move(expression))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ExpressionStatement; }

	unique_ptr<BoundStatement> Clone()const override;

	bool Equals(const BoundStatement& other)const noexcept override;
};

}//MCF
