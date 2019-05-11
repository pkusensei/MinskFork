#include "stdafx.h"
#include "Compilation.h"

#include <iostream>
#include <random>

#include "Diagnostic.h"
#include "Binding.h"
#include "Syntax.h"

namespace MCF {

EvaluationResult::EvaluationResult(const DiagnosticBag* diagnostics, const ValueType & value)
	:_diagnostics(diagnostics), _value(value)
{
}

Evaluator::Evaluator(const BoundBlockStatement* root, const std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables)
	: _root(root),
	_variables(std::remove_const_t<std::unordered_map<VariableSymbol, ValueType, VariableHash>*>(&variables))
{
}

ValueType Evaluator::Evaluate()
{
	auto labelToIndex = std::unordered_map<BoundLabel, size_t, LabelHash>();
	auto statements = _root->Statements();
	for (size_t i = 0; i < statements.size(); ++i)
	{
		auto p = dynamic_cast<BoundLabelStatement*>(statements.at(i));
		if (p)
			labelToIndex.emplace(p->Label(), i + 1);
	}

	size_t index = 0;
	while (index < statements.size())
	{
		auto s = statements.at(index);
		switch (s->Kind())
		{
			case BoundNodeKind::VariableDeclaration:
			{
				auto p = dynamic_cast<BoundVariableDeclaration*>(s);
				if (p)
				{
					EvaluateVariableDeclaration(p);
					++index;
					break;
				}
			}
			case BoundNodeKind::ExpressionStatement:
			{
				auto p = dynamic_cast<BoundExpressionStatement*>(s);
				if (p)
				{
					EvaluateExpressionStatement(p);
					++index;
					break;
				}
			}
			case BoundNodeKind::GotoStatement:
			{
				auto gs = dynamic_cast<BoundGotoStatement*>(s);
				if (gs)
				{
					index = labelToIndex.at(gs->Label());
					break;
				}
			}
			case BoundNodeKind::ConditionalGotoStatement:
			{
				auto cgs = dynamic_cast<BoundConditionalGotoStatement*>(s);
				if (cgs)
				{
					auto condition = EvaluateExpression(cgs->Condition()).GetValue<bool>();
					if (condition == cgs->JumpIfTrue())
						index = labelToIndex.at(cgs->Label());
					else ++index;
					break;
				}
			}
			case BoundNodeKind::LabelStatement:
				++index;
				break;
			default:
				throw std::invalid_argument("Unexpected statement " + GetEnumText(s->Kind()));
		}
	}
	return _lastValue;
}

void Evaluator::EvaluateVariableDeclaration(const BoundVariableDeclaration * node)
{
	auto value = EvaluateExpression(node->Initializer());
	_variables->emplace(node->Variable(), value);
	_lastValue = value;
}

void Evaluator::EvaluateExpressionStatement(const BoundExpressionStatement * node)
{
	_lastValue = EvaluateExpression(node->Expression());
}

ValueType Evaluator::EvaluateExpression(const BoundExpression * node)const
{
	switch (node->Kind())
	{
		case BoundNodeKind::LiteralExpression:
		{
			auto p = dynamic_cast<const BoundLiteralExpression*>(node);
			if (p) return EvaluateLiteralExpression(p);
			else break;
		}
		case BoundNodeKind::VariableExpression:
		{
			auto p = dynamic_cast<const BoundVariableExpression*>(node);
			if (p) return EvaluateVariableExpression(p);
			else break;
		}
		case BoundNodeKind::AssignmentExpression:
		{
			auto p = dynamic_cast<const BoundAssignmentExpression*>(node);
			if (p) return EvaluateAssignmentExpression(p);
			else break;
		}
		case BoundNodeKind::UnaryExpression:
		{
			auto p = dynamic_cast<const BoundUnaryExpression*>(node);
			if (p) return EvaluateUnaryExpression(p);
			else break;
		}
		case BoundNodeKind::BinaryExpression:
		{
			auto p = dynamic_cast<const BoundBinaryExpression*>(node);
			if (p) return EvaluateBinaryExpression(p);
			else break;
		}
		case BoundNodeKind::CallExpression:
		{
			auto p = dynamic_cast<const BoundCallExpression*>(node);
			if (p) return EvaluateCallExpression(p);
			else break;
		}
		case BoundNodeKind::PostfixExpression:
		{
			auto p = dynamic_cast<const BoundPostfixExpression*>(node);
			if (p) return EvaluatePostfixExpression(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Invalid expression " + GetEnumText(node->Kind()));
}

ValueType Evaluator::EvaluateLiteralExpression(const BoundLiteralExpression * node)const
{
	return node->Value();
}

ValueType Evaluator::EvaluateVariableExpression(const BoundVariableExpression * node)const
{
	return _variables->at(node->Variable());
}

ValueType Evaluator::EvaluateAssignmentExpression(const BoundAssignmentExpression * node)const
{
	auto value = EvaluateExpression(node->Expression());
	_variables->insert_or_assign(node->Variable(), value);
	return value;
}

ValueType Evaluator::EvaluateUnaryExpression(const BoundUnaryExpression * node)const
{
	auto operand = EvaluateExpression(node->Operand());
	switch (node->Op()->Kind())
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
			throw std::invalid_argument("Invalid unary operator " + GetEnumText(node->Op()->Kind()));
	}
}

ValueType Evaluator::EvaluateBinaryExpression(const BoundBinaryExpression * node)const
{
	auto left = EvaluateExpression(node->Left());
	auto right = EvaluateExpression(node->Right());
	switch (node->Op()->Kind())
	{
		case BoundBinaryOperatorKind::Addition:
			if (node->Type() == TypeSymbol::GetType(TypeEnum::Int))
				return left.GetValue<IntegerType>() + right.GetValue<IntegerType>();
			else return left.GetValue<string>() + right.GetValue<string>();
		case BoundBinaryOperatorKind::Subtraction:
			return left.GetValue<IntegerType>() - right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::Multiplication:
			return left.GetValue<IntegerType>() * right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::Division:
			return left.GetValue<IntegerType>() / right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::Modulus:
			return left.GetValue<IntegerType>() % right.GetValue<IntegerType>();
		case BoundBinaryOperatorKind::BitwiseAnd:
			if (node->Type() == TypeSymbol::GetType(TypeEnum::Int))
				return left.GetValue<IntegerType>() & right.GetValue<IntegerType>();
			else return left.GetValue<bool>() & right.GetValue<bool>();
		case BoundBinaryOperatorKind::BitwiseOr:
			if (node->Type() == TypeSymbol::GetType(TypeEnum::Int))
				return left.GetValue<IntegerType>() | right.GetValue<IntegerType>();
			else return left.GetValue<bool>() | right.GetValue<bool>();
		case BoundBinaryOperatorKind::BitwiseXor:
			if (node->Type() == TypeSymbol::GetType(TypeEnum::Int))
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
			throw std::invalid_argument("Invalid binary operator " + GetEnumText(node->Op()->Kind()));
	}
}

ValueType Evaluator::EvaluateCallExpression(const BoundCallExpression * node) const
{
	if (node->Function() == GetBuiltinFunction(BuiltinFuncEnum::Input))
	{
		auto f = []() {
			auto result = string();
			std::getline(std::cin, result);
			return result;
		};
		return f();
	} else if (node->Function() == GetBuiltinFunction(BuiltinFuncEnum::Print))
	{
		auto message = EvaluateExpression(node->Arguments()[0]);
		std::cout << message.GetValue<string>();
		return NullValue;
	} else if (node->Function() == GetBuiltinFunction(BuiltinFuncEnum::Rnd))
	{
		auto max = EvaluateExpression(node->Arguments()[0]).GetValue<IntegerType>();
		auto f = [max]() {
			auto rd = std::random_device();
			auto mt = std::mt19937(rd());
			auto dist = std::uniform_int_distribution<IntegerType>(0, max);

			return dist(mt);
		};
		return f();
	} else
	{
		throw std::invalid_argument("Unexpected function " + node->Function().ToString());
	}
}

ValueType Evaluator::EvaluatePostfixExpression(const BoundPostfixExpression * node) const
{
	auto value = EvaluateExpression(node->Expression());
	auto result = value.GetValue<IntegerType>();
	switch (node->OperatorKind())
	{
		case BoundPostfixOperatorEnum::Increment:
			_variables->insert_or_assign(node->Variable(), ++result);
			return result;
		case BoundPostfixOperatorEnum::Decrement:
			_variables->insert_or_assign(node->Variable(), --result);
			return result;
		default:
			throw std::invalid_argument("Unexpected postfix operator " + GetEnumText(node->OperatorKind()));
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
	return make_unique<Compilation>(previous, tree);
}

unique_ptr<Compilation> Compilation::ContinueWith(const unique_ptr<Compilation>& previous, const unique_ptr<SyntaxTree>& tree)
{
	return make_unique<Compilation>(previous, tree);
}

EvaluationResult Compilation::Evaluate(std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables)
{
	_syntaxTree->Diagnostics()->AddRange(*(GlobalScope()->Diagnostics()));
	auto diagnostics = _syntaxTree->Diagnostics();

	if (!diagnostics->empty())
		return EvaluationResult(diagnostics, NullValue);

	auto statement = GetStatement();
	Evaluator evaluator(statement.get(), variables);
	auto value = evaluator.Evaluate();
	return EvaluationResult(diagnostics, value);
}

unique_ptr<BoundBlockStatement> Compilation::GetStatement()
{
	auto result = GlobalScope()->Statement();
	return Lowerer::Lower(result);
}

void Compilation::EmitTree(std::ostream & out)
{
	auto statement = GetStatement();
	statement->WriteTo(out);
}

}//MCF