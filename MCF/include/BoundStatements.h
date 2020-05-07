#pragma once

#include "BoundLabel.h"
#include "BoundNode.h"
#include "Symbols.h"

namespace MCF {

class BoundExpression;

class BoundStatement :public BoundNode
{
};

class BoundBlockStatement final : public BoundStatement
{
private:
	vector<shared_ptr<BoundStatement>> _statements;

public:
	explicit BoundBlockStatement(vector<shared_ptr<BoundStatement>> statements)
		:_statements(std::move(statements))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BlockStatement; }

	constexpr const vector<shared_ptr<BoundStatement>>& Statements()const noexcept { return _statements; }
};

class BoundNopStatement final :public BoundStatement
{
public:
	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::NopStatement; }

};

class BoundVariableDeclaration final :public BoundStatement
{
private:
	shared_ptr<VariableSymbol> _variable;
	shared_ptr<BoundExpression> _initializer;

public:
	BoundVariableDeclaration(shared_ptr<VariableSymbol> variable,
		shared_ptr<BoundExpression> initializer)
		:_variable(std::move(variable)), _initializer(std::move(initializer))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableDeclaration; }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
	constexpr const shared_ptr<BoundExpression>& Initializer()const noexcept { return _initializer; }
};

class BoundIfStatement final :public BoundStatement
{
private:
	shared_ptr<BoundExpression> _condition;
	shared_ptr<BoundStatement> _thenStatement;
	shared_ptr<BoundStatement> _elseStatement;

public:
	BoundIfStatement(shared_ptr<BoundExpression> condition,
		shared_ptr<BoundStatement> thenStatement,
		shared_ptr<BoundStatement> elseStatement)
		:_condition(std::move(condition)), _thenStatement(std::move(thenStatement)),
		_elseStatement(std::move(elseStatement))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::IfStatement; }

	constexpr const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
	constexpr const shared_ptr<BoundStatement>& ThenStatement()const noexcept { return _thenStatement; }
	constexpr const shared_ptr<BoundStatement>& ElseStatement()const noexcept { return _elseStatement; }
};

class BoundLoopStatement :public BoundStatement
{
private:
	BoundLabel _breakLabel;
	BoundLabel _continueLabel;

protected:
	BoundLoopStatement(const BoundLabel& breakLabel, const BoundLabel& continueLabel)
		:_breakLabel(std::move(breakLabel)), _continueLabel(std::move(continueLabel))
	{
	}

public:
	constexpr const BoundLabel& BreakLabel()const noexcept { return _breakLabel; }
	constexpr const BoundLabel& ContinueLabel()const noexcept { return _continueLabel; }
};

class BoundWhileStatement final :public BoundLoopStatement
{
private:
	shared_ptr<BoundExpression> _condition;
	shared_ptr<BoundStatement> _body;

public:
	BoundWhileStatement(shared_ptr<BoundExpression> condition,
		shared_ptr<BoundStatement> body,
		BoundLabel breakLabel, BoundLabel continueLabel)
		: BoundLoopStatement(std::move(breakLabel), std::move(continueLabel)),
		_condition(std::move(condition)), _body(std::move(body))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::WhileStatement; }

	constexpr const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
	constexpr const shared_ptr<BoundStatement>& Body()const noexcept { return _body; }
};

class BoundDoWhileStatement final :public BoundLoopStatement
{
private:
	shared_ptr<BoundStatement> _body;
	shared_ptr<BoundExpression> _condition;

public:
	BoundDoWhileStatement(shared_ptr<BoundStatement> body,
		shared_ptr<BoundExpression> condition,
		BoundLabel breakLabel, BoundLabel continueLabel)
		: BoundLoopStatement(std::move(breakLabel), std::move(continueLabel)),
		_body(std::move(body)), _condition(std::move(condition))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::DoWhileStatement; }

	constexpr const shared_ptr<BoundStatement>& Body()const noexcept { return _body; }
	constexpr const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
};

class BoundForStatement final :public BoundLoopStatement
{
private:
	shared_ptr<VariableSymbol> _variable;
	shared_ptr<BoundExpression> _lowerBound;
	shared_ptr<BoundExpression> _upperBound;
	shared_ptr<BoundStatement> _body;

public:
	BoundForStatement(shared_ptr<VariableSymbol> variable,
		shared_ptr<BoundExpression> lowerBound,
		shared_ptr<BoundExpression> upperBound,
		shared_ptr<BoundStatement> body,
		BoundLabel breakLabel, BoundLabel continueLabel)
		: BoundLoopStatement(std::move(breakLabel), std::move(continueLabel)),
		_variable(std::move(variable)), _lowerBound(std::move(lowerBound)),
		_upperBound(std::move(upperBound)), _body(std::move(body))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ForStatement; }

	constexpr const shared_ptr<VariableSymbol>& Variable()const { return _variable; }
	constexpr const shared_ptr<BoundExpression>& LowerBound()const noexcept { return _lowerBound; }
	constexpr const shared_ptr<BoundExpression>& UpperBound()const noexcept { return _upperBound; }
	constexpr const shared_ptr<BoundStatement>& Body()const noexcept { return _body; }
};

class BoundLabelStatement final :public BoundStatement
{
private:
	BoundLabel _label;

public:
	explicit BoundLabelStatement(BoundLabel label)
		:_label(std::move(label))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LabelStatement; }

	constexpr const BoundLabel& Label()const noexcept { return _label; }
};

class BoundGotoStatement final :public BoundStatement
{
private:
	BoundLabel _label;

public:
	explicit BoundGotoStatement(BoundLabel label)
		:_label(std::move(label))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::GotoStatement; }

	constexpr const BoundLabel& Label()const noexcept { return _label; }
};

class BoundConditionalGotoStatement final :public BoundStatement
{
private:
	BoundLabel _label;
	shared_ptr<BoundExpression> _condition;
	bool _jumpIfTrue;

public:
	BoundConditionalGotoStatement(BoundLabel label,
		shared_ptr<BoundExpression> condition, bool jumpIfTrue = true)
		:_label(std::move(label)), _condition(std::move(condition)),
		_jumpIfTrue(std::move(jumpIfTrue))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConditionalGotoStatement; }

	constexpr const BoundLabel& Label()const noexcept { return _label; }
	constexpr const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
	constexpr bool JumpIfTrue()const noexcept { return _jumpIfTrue; }
};

class BoundReturnStatement final :public BoundStatement
{
private:
	shared_ptr<BoundExpression> _expression;

public:
	explicit BoundReturnStatement(shared_ptr<BoundExpression> expression)
		:_expression(std::move(expression))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ReturnStatement; }

	const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }

};

class BoundExpressionStatement final : public BoundStatement
{
private:
	shared_ptr<BoundExpression> _expression;

public:
	explicit BoundExpressionStatement(shared_ptr<BoundExpression> expression)
		:_expression(std::move(expression))
	{
	}

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ExpressionStatement; }

	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

}//MCF
