#include "stdafx.h"
#include "Compilation.h"

#include "Diagnostic.h"
#include "Binding.h"
#include "Syntax.h"

namespace MCF {

EvaluationResult::EvaluationResult(const DiagnosticBag* diagnostics, const ValueType & value)
	:_diagnostics(diagnostics), _value(value)
{
}

Evaluator::Evaluator(const BoundStatement* root, const std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables)
	: _root(root),
	_variables(std::remove_const_t<std::unordered_map<VariableSymbol, ValueType, VariableHash>*>(&variables))
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
		case BoundNodeKind::IfStatement:
			EvaluateIfStatement(node);
			break;
		case BoundNodeKind::WhileStatement:
			EvaluateWhileStatement(node);
			break;
		case BoundNodeKind::ForStatement:
			EvaluateForStatement(node);
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

void Evaluator::EvaluateIfStatement(const BoundStatement * node)
{
	auto p = dynamic_cast<const BoundIfStatement*>(node);
	if (p == nullptr) return;

	auto condition = EvaluateExpression(p->Condition()).GetValue<bool>();
	if (condition)
		EvaluateStatement(p->ThenStatement());
	else if (p->ElseStatement() != nullptr)
		EvaluateStatement(p->ElseStatement());
}

void Evaluator::EvaluateWhileStatement(const BoundStatement * node)
{
	auto p = dynamic_cast<const BoundWhileStatement*>(node);
	if (p == nullptr) return;

	while (EvaluateExpression(p->Condition()).GetValue<bool>())
		EvaluateStatement(p->Body());
}

void Evaluator::EvaluateForStatement(const BoundStatement * node)
{
	auto p = dynamic_cast<const BoundForStatement*>(node);
	if (p == nullptr) return;

	auto lowerBound = EvaluateExpression(p->LowerBound()).GetValue<IntegerType>();
	auto upperBound = EvaluateExpression(p->UpperBound()).GetValue<IntegerType>();

	for (auto i = lowerBound; i <= upperBound; ++i) // NOTE inclusive for loop
	{
		(*_variables)[p->Variable()] = i;
		EvaluateStatement(p->Body());
	}
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
			return operand.GetValue<IntegerType>();
		case BoundUnaryOperatorKind::Negation:
			return -operand.GetValue<IntegerType>();
		case BoundUnaryOperatorKind::LogicalNegation:
			return !operand.GetValue<bool>();
		case BoundUnaryOperatorKind::OnesComplement:
			return ~operand.GetValue<IntegerType>();
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
			return left.GetValue<IntegerType>() + right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::Subtraction:
			return left.GetValue<IntegerType>() - right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::Multiplication:
			return left.GetValue<IntegerType>() * right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::Division:
			return left.GetValue<IntegerType>() / right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::BitwiseAnd:
			if (p->Type() == type_index(typeid(IntegerType)))
				return left.GetValue<IntegerType>() & right.GetValue<IntegerType>();
			else return left.GetValue<bool>() & right.GetValue<bool>();
		case BoundBinaryOperatorKind::BitwiseOr:
			if (p->Type() == type_index(typeid(IntegerType)))
				return left.GetValue<IntegerType>() | right.GetValue<IntegerType>();
			else return left.GetValue<bool>() | right.GetValue<bool>();
		case BoundBinaryOperatorKind::BitwiseXor:
			if (p->Type() == type_index(typeid(IntegerType)))
				return left.GetValue<IntegerType>() ^ right.GetValue<IntegerType>();
			else return left.GetValue<bool>() ^ right.GetValue<bool>();
		case BoundBinaryOperatorKind::LogicalAnd:
			return left.GetValue<bool>() && right.GetValue<bool>();
		case BoundBinaryOperatorKind::LogicalOr:
			return left.GetValue<bool>() || right.GetValue<bool>();
		case BoundBinaryOperatorKind::Equals:
			return left == right;
		case BoundBinaryOperatorKind::NotEquals:
			return left != right;
		case BoundBinaryOperatorKind::Less:
			return left.GetValue<IntegerType>() < right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::LessOrEquals:
			return left.GetValue<IntegerType>() <= right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::Greater:
			return left.GetValue<IntegerType>() > right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::GreaterOrEquals:
			return left.GetValue<IntegerType>() >= right.GetValue<IntegerType>();

		default:
			throw std::invalid_argument("Invalid binary operator");
	}
}

Compilation::Compilation()
	: Compilation(nullptr)
{
}

Compilation::Compilation(const unique_ptr<Compilation>& previous, const SyntaxTree& tree)
	: _previous(std::move(std::remove_const_t<unique_ptr<Compilation>&>(previous))),
	_syntaxTree(&tree),
	_globalScope(nullptr)
{
}

Compilation::Compilation(const unique_ptr<Compilation>& previous, const unique_ptr<SyntaxTree>& tree)
	: _previous(std::move(std::remove_const_t<unique_ptr<Compilation>&>(previous))),
	_syntaxTree(tree.get()),
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

const BoundGlobalScope* Compilation::GlobalScope()
{
	while (_globalScope == nullptr)
	{
		unique_ptr<BoundGlobalScope> tmp{nullptr};
		if (_previous == nullptr)
			tmp = Binder::BindGlobalScope({}, _syntaxTree->Root());
		else
			tmp = Binder::BindGlobalScope(_previous->GlobalScope(), _syntaxTree->Root());

		std::unique_lock<std::mutex> lock(_mtx, std::defer_lock);
		if (lock.try_lock() && _globalScope == nullptr)
			_globalScope.swap(tmp);
	}
	return _globalScope.get();
}

unique_ptr<Compilation> Compilation::ContinueWith(const unique_ptr<Compilation>& previous, const SyntaxTree & tree)
{
	return std::make_unique<Compilation>(previous, tree);
}

unique_ptr<Compilation> Compilation::ContinueWith(const unique_ptr<Compilation>& previous, const unique_ptr<SyntaxTree>& tree)
{
	return std::make_unique<Compilation>(previous, tree);
}

EvaluationResult Compilation::Evaluate(std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables)
{
	_syntaxTree->Diagnostics()->AddRange(*(GlobalScope()->Diagnostics()));
	auto diagnostics = _syntaxTree->Diagnostics();

	if (diagnostics->size() > 0)
		return EvaluationResult(diagnostics, ValueType());

	Evaluator evaluator(GlobalScope()->Statement(), variables);
	auto value = evaluator.Evaluate();
	return EvaluationResult(diagnostics, value);
}

void Compilation::EmitTree(std::ostream & out)
{
	auto statement = GlobalScope()->Statement();
	statement->WriteTo(out);
}

}//MCF