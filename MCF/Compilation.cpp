#include "stdafx.h"
#include "Compilation.h"

#include "Diagnostic.h"
#include "Binder.h"
#include "Parser.h"
#include "SyntaxNode.h"

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

Evaluator::Evaluator(const unique_ptr<BoundExpression>& root, std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables)
	:_root(root.get()), _variables(&variables)
{
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
			throw std::exception();
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
			throw std::exception();
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
			throw std::exception();
	}
}

Compilation::Compilation(const SyntaxTree & tree)
	:_syntax(&tree)
{
}

Compilation::Compilation(const unique_ptr<SyntaxTree>& tree)
	:_syntax(tree.get())
{
}

EvaluationResult Compilation::Evaluate(std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables)
{
	Binder binder(variables);
	auto boundExpression = binder.BindExpression(_syntax->Root());
	_syntax->Diagnostics()->AddRange(*binder.Diagnostics());
	auto diagnostics = _syntax->Diagnostics();
	
	if (diagnostics->size() > 0)
		return EvaluationResult(diagnostics, ValueType());

	Evaluator evaluator(boundExpression, variables);
	auto value = evaluator.Evaluate();
	return EvaluationResult(nullptr, value);
}

}