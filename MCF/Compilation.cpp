#include "stdafx.h"
#include "Compilation.h"

#include "Diagnostic.h"
#include "Binding.h"
#include "Parser.h"
#include "Syntax.h"

namespace MCF {

EvaluationResult::EvaluationResult(DiagnosticBag* diagnostics, const ValueType & value)
	:_diagnostics(diagnostics), _value(value)
{
}

EvaluationResult::EvaluationResult(EvaluationResult && other)
	: _diagnostics(other._diagnostics), _value(other._value)
{
	other._diagnostics = nullptr;
	other._value = ValueType();
}

Evaluator::Evaluator(const BoundStatement* root, std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables)
	:_root(root), _variables(&variables)
{
}

ValueType Evaluator::Evaluate()
{
	EvaluateStatement(_root);
	return _lastValue;
}

void Evaluator::EvaluateStatement(const BoundStatement * node)
{
	switch (node->Kind())
	{
		case BoundNodeKind::BlockStatement:
			EvaluateBlockStatement(node);
			break;
		case BoundNodeKind::VariableDeclaration:
			EvaluateVariableDeclaration(node);
			break;
		case BoundNodeKind::ExpressionStatement:
			EvaluateExpressionStatement(node);
			break;
		default:
			throw std::invalid_argument("unexpected node.");
	}

}

void Evaluator::EvaluateBlockStatement(const BoundStatement * node)
{
	auto p = dynamic_cast<const BoundBlockStatement*>(node);
	if (p == nullptr) return;

	for (const auto& it : p->Statements())
		EvaluateStatement(it);
}

void Evaluator::EvaluateVariableDeclaration(const BoundStatement * node)
{
	auto p = dynamic_cast<const BoundVariableDeclaration*>(node);
	if (p == nullptr) return;

	auto value = EvaluateExpression(p->Initializer());
	(*_variables)[p->Variable()] = value;
	_lastValue = value;
}

void Evaluator::EvaluateExpressionStatement(const BoundStatement * node)
{
	auto p = dynamic_cast<const BoundExpressionStatement*>(node);
	if (p == nullptr) return;
	_lastValue = EvaluateExpression(p->Expression());
}

ValueType Evaluator::EvaluateExpression(const BoundExpression * node)const
{
	switch (node->Kind())
	{
		case BoundNodeKind::LiteralExpression:
			return EvaluateLiteralExpression(node);
		case BoundNodeKind::VariableExpression:
			return EvaluateVariableExpression(node);
		case BoundNodeKind::AssignmentExpression:
			return EvaluateAssignmentExpression(node);
		case BoundNodeKind::UnaryExpression:
			return EvaluateUnaryExpression(node);
		case BoundNodeKind::BinaryExpression:
			return EvaluateBinaryExpression(node);
		default:
			throw std::invalid_argument("Invalid expression; evaluation failed.");
	}
}

ValueType Evaluator::EvaluateLiteralExpression(const BoundExpression * node)const
{
	auto p = dynamic_cast<const BoundLiteralExpression*>(node);
	return p != nullptr ? p->Value() : ValueType();
}

ValueType Evaluator::EvaluateVariableExpression(const BoundExpression * node)const
{
	auto p = dynamic_cast<const BoundVariableExpression*>(node);
	return p != nullptr ? (*_variables)[p->Variable()] : ValueType();
}

ValueType Evaluator::EvaluateAssignmentExpression(const BoundExpression * node)const
{
	auto p = dynamic_cast<const BoundAssignmentExpression*>(node);
	if (p == nullptr) return ValueType();
	auto value = EvaluateExpression(p->Expression());
	_variables->insert_or_assign(p->Variable(), value);
	return value;
}

ValueType Evaluator::EvaluateUnaryExpression(const BoundExpression * node)const
{
	auto p = dynamic_cast<const BoundUnaryExpression*>(node);
	if (p == nullptr) return ValueType();
	auto operand = EvaluateExpression(p->Operand());
	switch (p->Op()->Kind())
	{
		case BoundUnaryOperatorKind::Identity:
			return operand.GetValue<long>();
		case BoundUnaryOperatorKind::Negation:
			return -operand.GetValue<long>();
		case BoundUnaryOperatorKind::LogicalNegation:
			return !operand.GetValue<bool>();
		default:
			throw std::invalid_argument("Invalid unary operator.");
	}
}

ValueType Evaluator::EvaluateBinaryExpression(const BoundExpression * node)const
{
	auto p = dynamic_cast<const BoundBinaryExpression*>(node);
	if (p == nullptr) return ValueType();

	auto left = EvaluateExpression(p->Left());
	auto right = EvaluateExpression(p->Right());
	switch (p->Op()->Kind())
	{
		case BoundBinaryOperatorKind::Addition:
			return left.GetValue<long>() + right.GetValue<long>();
		case BoundBinaryOperatorKind::Subtraction:
			return left.GetValue<long>() - right.GetValue<long>();
		case BoundBinaryOperatorKind::Multiplication:
			return left.GetValue<long>() * right.GetValue<long>();
		case BoundBinaryOperatorKind::Division:
			return left.GetValue<long>() / right.GetValue<long>();
		case BoundBinaryOperatorKind::LogicalAnd:
			return left.GetValue<bool>() && right.GetValue<bool>();
		case BoundBinaryOperatorKind::LogicalOr:
			return left.GetValue<bool>() || right.GetValue<bool>();
		case BoundBinaryOperatorKind::Equals:
			return left == right;
		case BoundBinaryOperatorKind::NotEquals:
			return left != right;
		default:
			throw std::invalid_argument("Invalid binary operator");
	}
}

Compilation::Compilation()
	: Compilation(nullptr)
{
}

Compilation::Compilation(const Compilation* previous, const SyntaxTree& tree)
	: _previous(std::remove_const_t<Compilation*>(previous)),
	_syntaxTree(std::remove_const_t<SyntaxTree*>(&tree)),
	_globalScope(nullptr)
{
}

Compilation::Compilation(const Compilation* previous, const unique_ptr<SyntaxTree>& tree)
	: _previous(std::remove_const_t<Compilation*>(previous)),
	_syntaxTree(std::move(std::remove_const_t<unique_ptr<SyntaxTree>&>(tree))),
	_globalScope(nullptr)
{
}


Compilation::Compilation(const SyntaxTree & tree)
	:Compilation(nullptr, tree)
{
}

Compilation::Compilation(const unique_ptr<SyntaxTree>& tree)
	: Compilation(nullptr, tree)
{
}

Compilation::~Compilation() = default;

Compilation::Compilation(Compilation&& other)
	:_previous(std::move(other._previous)), _syntaxTree(std::move(other._syntaxTree)),
	_globalScope(std::move(other._globalScope))
{
}

Compilation& Compilation::operator=(Compilation&& other)
{
	_previous.swap(other._previous);
	_syntaxTree.swap(other._syntaxTree);
	_globalScope.swap(other._globalScope);
	return *this;
}

std::weak_ptr<BoundGlobalScope> Compilation::GlobalScope()
{
	while (_globalScope == nullptr)
	{
		std::shared_ptr<BoundGlobalScope> tmp{nullptr};
		if (_previous == nullptr)
			tmp = Binder::BindGlobalScope({}, _syntaxTree->Root());
		else
			tmp = Binder::BindGlobalScope(_previous->GlobalScope(), _syntaxTree->Root());

		std::unique_lock<std::mutex> lock(_mtx, std::defer_lock);
		if (lock.try_lock())
			_globalScope.swap(tmp);
	}
	return _globalScope;
}

Compilation Compilation::ContinueWith(const SyntaxTree & tree)
{
	return Compilation(this, tree);
}

Compilation Compilation::ContinueWith(const unique_ptr<SyntaxTree>& tree)
{
	return Compilation(this, tree);
}

EvaluationResult Compilation::Evaluate(std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables)
{
	_syntaxTree->Diagnostics()->AddRange(*(GlobalScope().lock()->Diagnostics()));
	auto diagnostics = _syntaxTree->Diagnostics();

	if (diagnostics->size() > 0)
		return EvaluationResult(diagnostics, ValueType());

	Evaluator evaluator(GlobalScope().lock()->Statement(), variables);
	auto value = evaluator.Evaluate();
	return EvaluationResult(diagnostics, value);
}

}