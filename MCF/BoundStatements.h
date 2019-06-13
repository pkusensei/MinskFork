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
	explicit BoundBlockStatement(const vector<shared_ptr<BoundStatement>>& statements)
		:_statements(statements)
	{
	}
	BoundBlockStatement(BoundBlockStatement&&) = default;
	BoundBlockStatement& operator=(BoundBlockStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BlockStatement; }

	const vector<shared_ptr<BoundStatement>>& Statements()const noexcept { return _statements; }
};

class BoundVariableDeclaration final :public BoundStatement
{
private:
	shared_ptr<VariableSymbol> _variable;
	shared_ptr<BoundExpression> _initializer;

public:
	BoundVariableDeclaration(const shared_ptr<VariableSymbol>& variable,
		const shared_ptr<BoundExpression>& initializer)
		:_variable(variable), _initializer(initializer)
	{
	}
	BoundVariableDeclaration(BoundVariableDeclaration&&) = default;
	BoundVariableDeclaration& operator=(BoundVariableDeclaration&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableDeclaration; }

	const shared_ptr<VariableSymbol>& Variable()const { return _variable; }
	const shared_ptr<BoundExpression>& Initializer()const noexcept { return _initializer; }
};

class BoundIfStatement final :public BoundStatement
{
private:
	shared_ptr<BoundExpression> _condition;
	shared_ptr<BoundStatement> _thenStatement;
	shared_ptr<BoundStatement> _elseStatement;

public:
	BoundIfStatement(const shared_ptr<BoundExpression>& condition,
		const shared_ptr<BoundStatement>& thenStatement,
		const shared_ptr<BoundStatement>& elseStatement)
		:_condition(condition), _thenStatement(thenStatement),
		_elseStatement(elseStatement)
	{
	}
	BoundIfStatement(BoundIfStatement&&) = default;
	BoundIfStatement& operator=(BoundIfStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::IfStatement; }

	const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
	const shared_ptr<BoundStatement>& ThenStatement()const noexcept { return _thenStatement; }
	const shared_ptr<BoundStatement>& ElseStatement()const noexcept { return _elseStatement; }
};

class BoundLoopStatement :public BoundStatement
{
private:
	BoundLabel _breakLabel;
	BoundLabel _continueLabel;

protected:
	BoundLoopStatement(const BoundLabel& breakLabel, const BoundLabel& continueLabel)
		:_breakLabel(breakLabel), _continueLabel(continueLabel)
	{
	}

public:
	const BoundLabel& BreakLabel()const { return _breakLabel; }
	const BoundLabel& ContinueLabel()const { return _continueLabel; }
};

class BoundWhileStatement final :public BoundLoopStatement
{
private:
	shared_ptr<BoundExpression> _condition;
	shared_ptr<BoundStatement> _body;

public:
	BoundWhileStatement(const shared_ptr<BoundExpression>& condition,
		const shared_ptr<BoundStatement>& body,
		const BoundLabel& breakLabel, const BoundLabel& continueLabel)
		: BoundLoopStatement(breakLabel, continueLabel),
		_condition(condition), _body(body)
	{
	}
	BoundWhileStatement(BoundWhileStatement&&) = default;
	BoundWhileStatement& operator=(BoundWhileStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::WhileStatement; }

	const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
	const shared_ptr<BoundStatement>& Body()const noexcept { return _body; }
};

class BoundDoWhileStatement final :public BoundLoopStatement
{
private:
	shared_ptr<BoundStatement> _body;
	shared_ptr<BoundExpression> _condition;

public:
	BoundDoWhileStatement(const shared_ptr<BoundStatement>& body,
		const shared_ptr<BoundExpression>& condition,
		const BoundLabel& breakLabel, const BoundLabel& continueLabel)
		: BoundLoopStatement(breakLabel, continueLabel),
		_body(body), _condition(condition)
	{
	}
	BoundDoWhileStatement(BoundDoWhileStatement&&) = default;
	BoundDoWhileStatement& operator=(BoundDoWhileStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::DoWhileStatement; }

	const shared_ptr<BoundStatement>& Body()const noexcept { return _body; }
	const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
};

class BoundForStatement final :public BoundLoopStatement
{
private:
	shared_ptr<VariableSymbol> _variable;
	shared_ptr<BoundExpression> _lowerBound;
	shared_ptr<BoundExpression> _upperBound;
	shared_ptr<BoundStatement> _body;

public:
	BoundForStatement(const shared_ptr<VariableSymbol>& variable,
		const shared_ptr<BoundExpression>& lowerBound,
		const shared_ptr<BoundExpression>& upperBound,
		const shared_ptr<BoundStatement>& body,
		const BoundLabel& breakLabel, const BoundLabel& continueLabel)
		: BoundLoopStatement(breakLabel, continueLabel),
		_variable(variable), _lowerBound(lowerBound), _upperBound(upperBound),
		_body(body)
	{
	}

	BoundForStatement(BoundForStatement&&) = default;
	BoundForStatement& operator=(BoundForStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ForStatement; }

	const shared_ptr<VariableSymbol>& Variable()const { return _variable; }
	const shared_ptr<BoundExpression>& LowerBound()const noexcept { return _lowerBound; }
	const shared_ptr<BoundExpression>& UpperBound()const noexcept { return _upperBound; }
	const shared_ptr<BoundStatement>& Body()const noexcept { return _body; }
};

class BoundLabelStatement final :public BoundStatement
{
private:
	BoundLabel _label;

public:
	explicit BoundLabelStatement(const BoundLabel& label)
		:_label(label)
	{
	}
	BoundLabelStatement(BoundLabelStatement&&) = default;
	BoundLabelStatement& operator=(BoundLabelStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LabelStatement; }

	const BoundLabel& Label()const { return _label; }
};

class BoundGotoStatement final :public BoundStatement
{
private:
	BoundLabel _label;

public:
	explicit BoundGotoStatement(const BoundLabel& label)
		:_label(label)
	{
	}
	BoundGotoStatement(BoundGotoStatement&&) = default;
	BoundGotoStatement& operator=(BoundGotoStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::GotoStatement; }

	const BoundLabel& Label()const { return _label; }
};

class BoundConditionalGotoStatement final :public BoundStatement
{
private:
	BoundLabel _label;
	shared_ptr<BoundExpression> _condition;
	bool _jumpIfTrue;

public:
	BoundConditionalGotoStatement(const BoundLabel& label,
		const shared_ptr<BoundExpression>& condition, bool jumpIfTrue = true)
		:_label(label), _condition(condition), _jumpIfTrue(jumpIfTrue)
	{
	}
	BoundConditionalGotoStatement(BoundConditionalGotoStatement&&) = default;
	BoundConditionalGotoStatement& operator=(BoundConditionalGotoStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConditionalGotoStatement; }

	const BoundLabel& Label()const { return _label; }
	const shared_ptr<BoundExpression>& Condition()const { return _condition; }
	bool JumpIfTrue()const noexcept { return _jumpIfTrue; }
};

class BoundReturnStatement final :public BoundStatement
{
private:
	shared_ptr<BoundExpression> _expression;

public:
	explicit BoundReturnStatement(const shared_ptr<BoundExpression>& expression)
		:_expression(expression)
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
	explicit BoundExpressionStatement(const shared_ptr<BoundExpression>& expression)
		:_expression(expression)
	{
	}
	BoundExpressionStatement(BoundExpressionStatement&&) = default;
	BoundExpressionStatement& operator=(BoundExpressionStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ExpressionStatement; }

	const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

}//MCF
