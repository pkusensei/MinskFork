#include "stdafx.h"
#include "BoundStatements.h"

#include "BoundExpressions.h"
#include "ReflectionHelper.h"

namespace MCF {

BoundBlockStatement::BoundBlockStatement(const vector<shared_ptr<BoundStatement>>& statements)
	: _statements(statements)
{
}

BoundVariableDeclaration::BoundVariableDeclaration(const shared_ptr<VariableSymbol> & variable,
	const shared_ptr<BoundExpression>& initializer)
	: _variable(variable), _initializer(initializer)
{
}

BoundIfStatement::BoundIfStatement(const shared_ptr<BoundExpression>& condition,
	const shared_ptr<BoundStatement>& thenStatement,
	const shared_ptr<BoundStatement>& elseStatement)
	: _condition(condition), _thenStatement(thenStatement), _elseStatement(elseStatement)
{
}

BoundWhileStatement::BoundWhileStatement(const shared_ptr<BoundExpression>& condition,
	const shared_ptr<BoundStatement>& body,
	const BoundLabel& breakLabel, const BoundLabel& continueLabel)
	: BoundLoopStatement(breakLabel, continueLabel),
	_condition(condition), _body(body)
{
}

BoundDoWhileStatement::BoundDoWhileStatement(const shared_ptr<BoundStatement>& body,
	const shared_ptr<BoundExpression>& condition,
	const BoundLabel& breakLabel, const BoundLabel& continueLabel)
	: BoundLoopStatement(breakLabel, continueLabel),
	_body(body), _condition(condition)
{
}

BoundForStatement::BoundForStatement(const shared_ptr<VariableSymbol>& variable,
	const shared_ptr<BoundExpression>& lowerBound,
	const shared_ptr<BoundExpression>& upperBound,
	const shared_ptr<BoundStatement>& body,
	const BoundLabel& breakLabel, const BoundLabel& continueLabel)
	: BoundLoopStatement(breakLabel, continueLabel),
	_variable(variable), _lowerBound(lowerBound), _upperBound(upperBound), _body(body)
{
}

BoundLabelStatement::BoundLabelStatement(const BoundLabel & label)
	: _label(label)
{
}

BoundGotoStatement::BoundGotoStatement(const BoundLabel & label)
	: _label(label)
{
}

BoundConditionalGotoStatement::BoundConditionalGotoStatement(const BoundLabel & label,
	const shared_ptr<BoundExpression>& condition,
	bool jumpIfTrue)
	: _label(label), _condition(condition), _jumpIfTrue(jumpIfTrue)
{
}

BoundReturnStatement::BoundReturnStatement(const shared_ptr<BoundExpression>& expression)
	: _expression(expression)
{
}

BoundExpressionStatement::BoundExpressionStatement(const shared_ptr<BoundExpression>& expression)
	: _expression(expression)
{
}

}//MCF
