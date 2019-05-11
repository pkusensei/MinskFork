#include "stdafx.h"
#include "BoundStatements.h"

#include "BoundExpressions.h"

namespace MCF {

const vector<std::pair<string, string>> BoundStatement::GetProperties() const
{
	return vector<std::pair<string, string>>();
}

const vector<const BoundNode*> BoundStatement::GetChildren() const
{
	return vector<const BoundNode*>();
}

BoundBlockStatement::BoundBlockStatement(const vector<unique_ptr<BoundStatement>>& statements)
	: _statements(std::move(std::remove_const_t<vector<unique_ptr<BoundStatement>>&>(statements)))
{
}

const vector<const BoundNode*> BoundBlockStatement::GetChildren() const
{
	auto result = vector<const BoundNode*>();
	for (const auto& it : _statements)
		result.emplace_back(it.get());
	return result;
}

const vector<BoundStatement*> BoundBlockStatement::Statements() const
{
	auto result = vector<BoundStatement*>();
	for (const auto& it : _statements)
		result.emplace_back(it.get());
	return result;
}


BoundVariableDeclaration::BoundVariableDeclaration(const VariableSymbol & variable, const unique_ptr<BoundExpression>& initializer)
	:_variable(variable),
	_initializer(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(initializer)))
{
}

const vector<std::pair<string, string>> BoundVariableDeclaration::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable().ToString())
	};
}

const vector<const BoundNode*> BoundVariableDeclaration::GetChildren() const
{
	return vector<const BoundNode*>{
		_initializer.get()
	};
}

BoundIfStatement::BoundIfStatement(const unique_ptr<BoundExpression>& condition, const unique_ptr<BoundStatement>& thenStatement,
								   const unique_ptr<BoundStatement>& elseStatement)
	: _condition(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(condition))),
	_thenStatement(std::move(std::remove_const_t<unique_ptr<BoundStatement>&>(thenStatement))),
	_elseStatement(std::move(std::remove_const_t<unique_ptr<BoundStatement>&>(elseStatement)))
{
}

const vector<const BoundNode*> BoundIfStatement::GetChildren() const
{
	return vector<const BoundNode*>{
		_condition.get(),
			_thenStatement.get(),
			_elseStatement.get()
	};
}

BoundWhileStatement::BoundWhileStatement(const unique_ptr<BoundExpression>& condition,
										 const unique_ptr<BoundStatement>& body)
	:_condition(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(condition))),
	_body(std::move(std::remove_const_t<unique_ptr<BoundStatement>&>(body)))
{
}

const vector<const BoundNode*> BoundWhileStatement::GetChildren() const
{
	return vector<const BoundNode*>{
		_condition.get(),
			_body.get()
	};
}

BoundForStatement::BoundForStatement(const VariableSymbol & variable, const unique_ptr<BoundExpression>& lowerBound,
									 const unique_ptr<BoundExpression>& upperBound, const unique_ptr<BoundStatement>& body)
	: _variable(variable),
	_lowerBound(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(lowerBound))),
	_upperBound(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(upperBound))),
	_body(std::move(std::remove_const_t<unique_ptr<BoundStatement>&>(body)))
{
}

const vector<std::pair<string, string>> BoundForStatement::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable().ToString())
	};
}

const vector<const BoundNode*> BoundForStatement::GetChildren() const
{
	return vector<const BoundNode*>{
		_lowerBound.get(),
			_upperBound.get(),
			_body.get()
	};
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
															 const unique_ptr<BoundExpression>& condition,
															 bool jumpIfTrue)
	:_label(label),
	_condition((std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(condition)))),
	_jumpIfTrue(jumpIfTrue)
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
	return vector<const BoundNode*>{
		_condition.get()
	};
}

BoundExpressionStatement::BoundExpressionStatement(const unique_ptr<BoundExpression>& expression)
	: _expression((std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(expression))))
{
}

const vector<const BoundNode*> BoundExpressionStatement::GetChildren() const
{
	return vector<const BoundNode*>{
		_expression.get()
	};
}

}//MCF
