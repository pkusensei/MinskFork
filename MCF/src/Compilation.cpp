#include "Compilation.h"

#include <fstream>
#include <iostream>
#include <random>

#include "Binding.h"
#include "BoundLabel.h"
#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "ControlFlowGraph.h"
#include "Diagnostic.h"
#include "helpers.h"
#include "Parsing.h"

namespace MCF {

Evaluator::Evaluator(unique_ptr<BoundProgram> program, VarMap& variables)
	: _program(std::move(program)), _globals(variables)
{
	_locals.emplace(VarMap());
}

ValueType Evaluator::Evaluate()
{
	return EvaluateStatement(_program->Statement());
}

ValueType Evaluator::EvaluateStatement(const BoundBlockStatement* body)
{
	auto labelToIndex = std::unordered_map<BoundLabel, size_t, LabelHash>();
	auto statements = body->Statements();
	for (size_t i = 0; i < statements.size(); ++i)
	{
		auto p = dynamic_cast<const BoundLabelStatement*>(statements.at(i).get());
		if (p)
			labelToIndex.emplace(p->Label(), i + 1);
	}

	size_t index = 0;
	while (index < statements.size())
	{
		auto s = statements.at(index).get();
		switch (s->Kind())
		{
			case BoundNodeKind::VariableDeclaration:
			{
				auto p = dynamic_cast<const BoundVariableDeclaration*>(s);
				if (p)
					EvaluateVariableDeclaration(p);
				++index;
				break;
			}
			case BoundNodeKind::ExpressionStatement:
			{
				auto p = dynamic_cast<const BoundExpressionStatement*>(s);
				if (p)
					EvaluateExpressionStatement(p);
				++index;
				break;
			}
			case BoundNodeKind::GotoStatement:
			{
				auto gs = dynamic_cast<const BoundGotoStatement*>(s);
				if (gs)
					index = labelToIndex.at(gs->Label());
				break;
			}
			case BoundNodeKind::ConditionalGotoStatement:
			{
				auto cgs = dynamic_cast<const BoundConditionalGotoStatement*>(s);
				if (cgs)
				{
					auto condition =
						EvaluateExpression(cgs->Condition().get()).GetValue<bool>();
					if (condition == cgs->JumpIfTrue())
						index = labelToIndex.at(cgs->Label());
					else ++index;
				}
				break;
			}
			case BoundNodeKind::LabelStatement:
				++index;
				break;
			case BoundNodeKind::ReturnStatement:
			{
				auto rs = dynamic_cast<const BoundReturnStatement*>(s);
				_lastValue = rs->Expression() == nullptr ?
					NullValue : EvaluateExpression(rs->Expression().get());
				return _lastValue;
			}
			default:
				throw std::invalid_argument(BuildStringFrom("Unexpected statement ", nameof(s->Kind())));
		}
	}
	return _lastValue;
}

void Evaluator::EvaluateVariableDeclaration(const BoundVariableDeclaration* node)
{
	auto value = EvaluateExpression(node->Initializer().get());
	_lastValue = value;
	Assign(node->Variable(), value);
}

void Evaluator::EvaluateExpressionStatement(const BoundExpressionStatement* node)
{
	_lastValue = EvaluateExpression(node->Expression().get());
}

ValueType Evaluator::EvaluateExpression(const BoundExpression* node)
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
		case BoundNodeKind::ConversionExpression:
		{
			auto p = dynamic_cast<const BoundConversionExpression*>(node);
			if (p) return EvaluateConversionExpression(p);
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
	throw std::invalid_argument(BuildStringFrom("Invalid expression ", nameof(node->Kind())));
}

ValueType Evaluator::EvaluateLiteralExpression(const BoundLiteralExpression* node)const
{
	return node->Value();
}

ValueType Evaluator::EvaluateVariableExpression(const BoundVariableExpression* node)
{
	if (node->Variable()->Kind() == SymbolKind::GlobalVariable)
		return _globals.at(node->Variable());
	else
	{
		auto& locals = _locals.top();
		return locals.at(node->Variable());
	}
}

ValueType Evaluator::EvaluateAssignmentExpression(const BoundAssignmentExpression* node)
{
	auto value = EvaluateExpression(node->Expression().get());
	Assign(node->Variable(), value);
	return value;
}

ValueType Evaluator::EvaluateUnaryExpression(const BoundUnaryExpression* node)
{
	auto operand = EvaluateExpression(node->Operand().get());
	switch (node->Op().Kind())
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
			throw std::invalid_argument(BuildStringFrom("Invalid unary operator "
				, nameof(node->Op().Kind())));
	}
}

ValueType Evaluator::EvaluateBinaryExpression(const BoundBinaryExpression* node)
{
	auto left = EvaluateExpression(node->Left().get());
	auto right = EvaluateExpression(node->Right().get());
	switch (node->Op().Kind())
	{
		case BoundBinaryOperatorKind::Addition:
			if (node->Type().get() == TypeSymbol::Get(TypeEnum::Int))
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
			if (node->Type().get() == TypeSymbol::Get(TypeEnum::Int))
				return left.GetValue<IntegerType>() & right.GetValue<IntegerType>();
			else return left.GetValue<bool>() & right.GetValue<bool>();
		case BoundBinaryOperatorKind::BitwiseOr:
			if (node->Type().get() == TypeSymbol::Get(TypeEnum::Int))
				return left.GetValue<IntegerType>() | right.GetValue<IntegerType>();
			else return left.GetValue<bool>() | right.GetValue<bool>();
		case BoundBinaryOperatorKind::BitwiseXor:
			if (node->Type().get() == TypeSymbol::Get(TypeEnum::Int))
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
			throw std::invalid_argument(BuildStringFrom("Invalid binary operator "
				, nameof(node->Op().Kind())));
	}
}

ValueType Evaluator::EvaluateCallExpression(const BoundCallExpression* node)
{
	if (*(node->Function()) == GetBuiltinFunction(BuiltinFuncEnum::Input))
	{
		auto f = []()
		{
			auto result = string();
			std::getline(std::cin, result);
			return result;
		};
		return f();
	} else if (*(node->Function()) == GetBuiltinFunction(BuiltinFuncEnum::Print))
	{
		auto message = EvaluateExpression(node->Arguments()[0].get());
		std::cout << message.GetValue<string>() << NEW_LINE;
		return NullValue;
	} else if (*(node->Function()) == GetBuiltinFunction(BuiltinFuncEnum::Rnd))
	{
		auto max =
			EvaluateExpression(node->Arguments()[0].get()).GetValue<IntegerType>();
		auto f = [max]()
		{
			static auto rd = std::random_device();
			auto mt = std::mt19937(rd());
			auto dist = std::uniform_int_distribution<IntegerType>(0, max);

			return dist(mt);
		};
		return f();
	} else
	{
		auto locals = VarMap();
		for (size_t i = 0; i < node->Arguments().size(); ++i)
		{
			auto param = node->Function()->Parameters()[i];
			auto value = EvaluateExpression(node->Arguments()[i].get());
			locals.emplace(make_shared<ParameterSymbol>(param), std::move(value));
		}
		_locals.push(std::move(locals));
		auto statement = _program->Functions().at(node->Function().get()).get();
		auto result = EvaluateStatement(statement);

		_locals.pop();
		return result;
	}
}

ValueType Evaluator::EvaluateConversionExpression(const BoundConversionExpression* node)
{
	auto value = EvaluateExpression(node->Expression().get());
	if (node->Type().get() == TypeSymbol::Get(TypeEnum::Bool))
		return value.ToBoolean();
	else if (node->Type().get() == TypeSymbol::Get(TypeEnum::Int))
		return value.ToInteger();
	else if (node->Type().get() == TypeSymbol::Get(TypeEnum::String))
		return value.ToString();
	else
		throw std::invalid_argument("Unexpected type " + node->Type().get().ToString());
}

ValueType Evaluator::EvaluatePostfixExpression(const BoundPostfixExpression* node)
{
	auto value = EvaluateExpression(node->Expression().get());
	auto result = value.GetValue<IntegerType>();
	switch (node->OperatorKind())
	{
		case BoundPostfixOperatorEnum::Increment:
			Assign(node->Variable(), ++result);
			return result;
		case BoundPostfixOperatorEnum::Decrement:
			Assign(node->Variable(), --result);
			return result;
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected postfix operator ", nameof(node->OperatorKind())));
	}
}

void Evaluator::Assign(const shared_ptr<VariableSymbol>& variable, const ValueType& value)
{
	if (variable->Kind() == SymbolKind::GlobalVariable)
		_globals.insert_or_assign(variable, value);
	else
	{
		auto& locals = _locals.top();
		locals.insert_or_assign(variable, value);
	}
}

Compilation::Compilation(unique_ptr<Compilation> previous,
	vector<unique_ptr<SyntaxTree>> trees)
	:_previous(std::move(previous)), _syntaxTrees(),
	_globalScope(nullptr), _diagnostics(make_unique<DiagnosticBag>())
{
	auto vec = vector<unique_ptr<SyntaxTree>>();
	while (!trees.empty())
	{
		auto tree = std::move(trees.back());
		trees.pop_back();
		auto flat = SyntaxTree::Flatten(std::move(tree));
		vec.insert(vec.end(), std::make_move_iterator(flat.begin()),
			std::make_move_iterator(flat.end()));
	}
	_syntaxTrees = std::move(vec);
}

Compilation::Compilation(vector<unique_ptr<SyntaxTree>> trees)
	: Compilation(nullptr, std::move(trees))
{
}

Compilation::Compilation(unique_ptr<Compilation> previous, unique_ptr<SyntaxTree> tree)
	: Compilation(std::move(previous), std::vector<unique_ptr<SyntaxTree>>{})
{
	auto trees = SyntaxTree::Flatten(std::move(tree));
	_syntaxTrees = std::move(trees);
}

Compilation::Compilation(unique_ptr<SyntaxTree> tree)
	:Compilation(nullptr, std::move(tree))
{
}

Compilation::~Compilation() = default;

Compilation::Compilation(Compilation&& other) noexcept
	:_previous(std::move(other._previous)), _syntaxTrees(std::move(other._syntaxTrees)),
	_globalScope(std::move(other._globalScope)), _diagnostics(std::move(other._diagnostics)),
	_mtx()
{
}

const vector<const SyntaxTree*> Compilation::SynTrees()const noexcept
{
	auto result = vector<const SyntaxTree*>();
	std::for_each(_syntaxTrees.cbegin(), _syntaxTrees.cend(),
		[&result](const auto& tree)mutable { result.push_back(tree.get()); });
	return result;
}

const BoundGlobalScope* Compilation::GlobalScope()
{
	while (_globalScope == nullptr)
	{
		unique_ptr<BoundGlobalScope> tmp{ nullptr };
		if (_previous == nullptr)
			tmp = Binder::BindGlobalScope(nullptr, SynTrees());
		else
			tmp = Binder::BindGlobalScope(_previous->GlobalScope(), SynTrees());

		std::unique_lock<std::mutex> lock(_mtx, std::defer_lock);
		if (lock.try_lock() && _globalScope == nullptr)
			_globalScope.swap(tmp);
	}
	return _globalScope.get();
}

unique_ptr<Compilation> Compilation::ContinueWith(unique_ptr<Compilation> previous,
	unique_ptr<SyntaxTree> tree)
{
	auto vec = vector<unique_ptr<SyntaxTree>>();
	vec.push_back(std::move(tree));
	return make_unique<Compilation>(std::move(previous), std::move(vec));
}

EvaluationResult Compilation::Evaluate(VarMap& variables)
{
	auto createCfgFile = [](const BoundProgram& p)
	{
		auto file = std::ofstream();
		file.open("cfg.dot", std::ios_base::out | std::ios_base::trunc);
		if (file.is_open())
		{
			auto cfgStatement =
				p.Statement()->Statements().empty() && !p.Functions().empty() ?
				(--p.Functions().end())->second.get()
				: p.Statement();
			auto cfg = ControlFlowGraph::Create(cfgStatement);
			cfg.WriteTo(file);
		}
	};

	std::for_each(_syntaxTrees.cbegin(), _syntaxTrees.cend(),
		[this](const auto& tree) { _diagnostics->AddRange(tree->Diagnostics()); });

	_diagnostics->AddRange(GlobalScope()->Diagnostics());

	if (!_diagnostics->empty())
		return EvaluationResult(*_diagnostics, NullValue);

	auto program = Binder::BindProgram(GlobalScope());
	createCfgFile(*program);
	if (!program->Diagnostics().empty())
	{
		_diagnostics->AddRange(program->Diagnostics());
		return EvaluationResult(*_diagnostics, NullValue);
	}

	Evaluator evaluator(std::move(program), variables);
	auto value = evaluator.Evaluate();
	return EvaluationResult(*_diagnostics, value);
}

void Compilation::EmitTree(std::ostream& out)
{
	auto program = Binder::BindProgram(GlobalScope());
	if (!program->Statement()->Statements().empty())
	{
		program->Statement()->WriteTo(out);
	} else
	{
		for (const auto& [fs, body] : program->Functions())
		{
			auto& funcs = GlobalScope()->Functions();
			if (std::find_if(funcs.begin(), funcs.end(),
				[fs = fs](const auto& it) { return it.get() == fs; }) == funcs.end())
			{
				continue;
			}
			fs->WriteTo(out);
			body->WriteTo(out);
		}
	}
}

}//MCF