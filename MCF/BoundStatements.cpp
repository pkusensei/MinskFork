#include "stdafx.h"
#include "BoundStatements.h"

#include "BoundExpressions.h"
#include "ReflectionHelper.h"

namespace MCF {

const vector<std::pair<string, string>> BoundStatement::GetProperties() const
{
	return vector<std::pair<string, string>>();
}

const vector<const BoundNode*> BoundStatement::GetChildren() const
{
	return vector<const BoundNode*>();
}

BoundBlockStatement::BoundBlockStatement(const vector<shared_ptr<BoundStatement>>& statements)
	: _statements(statements)
{
}

const vector<const BoundNode*> BoundBlockStatement::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode, BoundStatement>(_statements.begin(), _statements.end());
}

BoundVariableDeclaration::BoundVariableDeclaration(const shared_ptr<VariableSymbol> & variable,
	const shared_ptr<BoundExpression>& initializer)
	:_variable(variable), _initializer(initializer)
{
}

const vector<std::pair<string, string>> BoundVariableDeclaration::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable()->ToString())
	};
}

const vector<const BoundNode*> BoundVariableDeclaration::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_initializer);
}

BoundIfStatement::BoundIfStatement(const shared_ptr<BoundExpression>& condition,
	const shared_ptr<BoundStatement>& thenStatement,
	const shared_ptr<BoundStatement>& elseStatement)
	: _condition(condition), _thenStatement(thenStatement), _elseStatement(elseStatement)
{
}

const vector<const BoundNode*> BoundIfStatement::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_condition, _thenStatement, _elseStatement);
}

BoundWhileStatement::BoundWhileStatement(const shared_ptr<BoundExpression>& condition,
	const shared_ptr<BoundStatement>& body,
	const BoundLabel& breakLabel, const BoundLabel& continueLabel)
	:BoundLoopStatement(breakLabel, continueLabel),
	_condition(condition), _body(body)
{
}

const vector<const BoundNode*> BoundWhileStatement::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_condition, _body);
}

BoundDoWhileStatement::BoundDoWhileStatement(const shared_ptr<BoundStatement>& body,
	const shared_ptr<BoundExpression>& condition,
	const BoundLabel& breakLabel, const BoundLabel& continueLabel)
	:BoundLoopStatement(breakLabel, continueLabel),
	_body(body), _condition(condition)
{
}

const vector<const BoundNode*> BoundDoWhileStatement::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_body, _condition);
}

BoundForStatement::BoundForStatement(const shared_ptr<VariableSymbol>& variable,
	const shared_ptr<BoundExpression>& lowerBound,
	const shared_ptr<BoundExpression>& upperBound,
	const shared_ptr<BoundStatement>& body,
	const BoundLabel& breakLabel, const BoundLabel& continueLabel)
	:BoundLoopStatement(breakLabel, continueLabel),
	_variable(variable), _lowerBound(lowerBound), _upperBound(upperBound), _body(body)
{
}

const vector<std::pair<string, string>> BoundForStatement::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable()->ToString())
	};
}

const vector<const BoundNode*> BoundForStatement::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_lowerBound, _upperBound, _body);
}

BoundLabelStatement::BoundLabelStatement(const BoundLabel & label)
	:_label(label)
{
}

const vector<std::pair<string, string>> BoundLabelStatement::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Label", Label().ToString())
	};
}

BoundGotoStatement::BoundGotoStatement(const BoundLabel & label)
	:_label(label)
{
}

const vector<std::pair<string, string>> BoundGotoStatement::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Label", Label().ToString())
	};
}

BoundConditionalGotoStatement::BoundConditionalGotoStatement(const BoundLabel & label,
	const shared_ptr<BoundExpression>& condition,
	bool jumpIfTrue)
	:_label(label), _condition(condition), _jumpIfTrue(jumpIfTrue)
{
}

const vector<std::pair<string, string>> BoundConditionalGotoStatement::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Label", Label().ToString()),
			std::pair<string, string>("JumpIfFalse", std::to_string(JumpIfTrue()))
	};
}

const vector<const BoundNode*> BoundConditionalGotoStatement::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_condition);
}

BoundExpressionStatement::BoundExpressionStatement(const shared_ptr<BoundExpression>& expression)
	: _expression(expression)
{
}

const vector<const BoundNode*> BoundExpressionStatement::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_expression);
}

}//MCF
