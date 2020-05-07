#include "Compilation.h"

#include <fstream>
#include <iostream>
#include <random>
#include <stack>
#include <unordered_set>

#include "Binding.h"
#include "BoundLabel.h"
#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "ControlFlowGraph.h"
#include "Diagnostic.h"
#include "Emitter.h"
#include "helpers.h"
#include "Parsing.h"

namespace MCF {

class Evaluator final
{
	using FuncMap = std::unordered_map<const FunctionSymbol*,
		const BoundBlockStatement*, SymbolHash, SymbolEqual>;

private:
	unique_ptr<BoundProgram> _program;
	VarMap& _globals;
	FuncMap _functions;
	std::stack<VarMap> _locals;
	ValueType _lastValue;

	ValueType EvaluateStatement(const BoundBlockStatement* body);
	void EvaluateVariableDeclaration(const BoundVariableDeclaration* node);
	void EvaluateExpressionStatement(const BoundExpressionStatement* node);

	ValueType EvaluateExpression(const BoundExpression* node);
	ValueType EvaluateLiteralExpression(const BoundLiteralExpression* node)const;
	ValueType EvaluateVariableExpression(const BoundVariableExpression* node);
	ValueType EvaluateAssignmentExpression(const BoundAssignmentExpression* node);
	ValueType EvaluateUnaryExpression(const BoundUnaryExpression* node);
	ValueType EvaluateBinaryExpression(const BoundBinaryExpression* node);
	ValueType EvaluateCallExpression(const BoundCallExpression* node);
	ValueType EvaluateConversionExpression(const BoundConversionExpression* node);
	ValueType EvaluatePostfixExpression(const BoundPostfixExpression* node);

	void Assign(const VariableSymbol* variable, const ValueType& value);

public:
	Evaluator(unique_ptr<BoundProgram> program, VarMap& variables);
	ValueType Evaluate();
};

Evaluator::Evaluator(unique_ptr<BoundProgram> program, VarMap& variables)
	: _program(std::move(program)), _globals(variables)
{
	_locals.emplace(VarMap());

	const auto* current = _program.get();
	while (current != nullptr)
	{
		for (const auto& [key, value] : current->Functions())
			_functions.emplace(key, value.get());
		current = current->Previous();
	}
}

ValueType Evaluator::Evaluate()
{
	auto function = _program->MainFunc() ? _program->MainFunc() : _program->ScriptFunc();
	if (function == nullptr)
		return NULL_VALUE;
	auto body = _functions.at(function);
	return EvaluateStatement(body);
}

ValueType Evaluator::EvaluateStatement(const BoundBlockStatement* body)
{
	auto labelToIndex = std::unordered_map<BoundLabel, size_t, LabelHash>();
	auto statements = body->Statements();
	for (size_t i = 0; i < statements.size(); ++i)
	{
		auto ptr = statements.at(i).get();
		if (ptr->Kind() == BoundNodeKind::LabelStatement)
		{
			auto p = static_cast<const BoundLabelStatement*>(ptr);
			labelToIndex.emplace(p->Label(), i + 1);
		}
	}

	size_t index = 0;
	while (index < statements.size())
	{
		auto s = statements.at(index).get();
		switch (s->Kind())
		{
			case BoundNodeKind::VariableDeclaration:
			{
				auto p = static_cast<const BoundVariableDeclaration*>(s);
				EvaluateVariableDeclaration(p);
				++index;
				break;
			}
			case BoundNodeKind::ExpressionStatement:
			{
				auto p = static_cast<const BoundExpressionStatement*>(s);
				EvaluateExpressionStatement(p);
				++index;
				break;
			}
			case BoundNodeKind::GotoStatement:
			{
				auto gs = static_cast<const BoundGotoStatement*>(s);
				index = labelToIndex.at(gs->Label());
				break;
			}
			case BoundNodeKind::ConditionalGotoStatement:
			{
				auto cgs = static_cast<const BoundConditionalGotoStatement*>(s);
				auto condition =
					EvaluateExpression(cgs->Condition().get()).GetValue<bool>();
				if (condition == cgs->JumpIfTrue())
					index = labelToIndex.at(cgs->Label());
				else ++index;
				break;
			}
			case BoundNodeKind::LabelStatement:
				++index;
				break;
			case BoundNodeKind::ReturnStatement:
			{
				auto rs = static_cast<const BoundReturnStatement*>(s);
				_lastValue = rs->Expression() == nullptr ?
					NULL_VALUE : EvaluateExpression(rs->Expression().get());
				return _lastValue;
			}
			default:
				throw std::invalid_argument(BuildStringFrom("Unexpected statement: ", nameof(s->Kind())));
		}
	}
	return _lastValue;
}

void Evaluator::EvaluateVariableDeclaration(const BoundVariableDeclaration* node)
{
	auto value = EvaluateExpression(node->Initializer().get());
	_lastValue = value;
	Assign(node->Variable().get(), value);
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
			auto p = static_cast<const BoundLiteralExpression*>(node);
			return EvaluateLiteralExpression(p);
		}
		case BoundNodeKind::VariableExpression:
		{
			auto p = static_cast<const BoundVariableExpression*>(node);
			return EvaluateVariableExpression(p);
		}
		case BoundNodeKind::AssignmentExpression:
		{
			auto p = static_cast<const BoundAssignmentExpression*>(node);
			return EvaluateAssignmentExpression(p);
		}
		case BoundNodeKind::UnaryExpression:
		{
			auto p = static_cast<const BoundUnaryExpression*>(node);
			return EvaluateUnaryExpression(p);
		}
		case BoundNodeKind::BinaryExpression:
		{
			auto p = static_cast<const BoundBinaryExpression*>(node);
			return EvaluateBinaryExpression(p);
		}
		case BoundNodeKind::CallExpression:
		{
			auto p = static_cast<const BoundCallExpression*>(node);
			return EvaluateCallExpression(p);
		}
		case BoundNodeKind::ConversionExpression:
		{
			auto p = static_cast<const BoundConversionExpression*>(node);
			return EvaluateConversionExpression(p);
		}
		case BoundNodeKind::PostfixExpression:
		{
			auto p = static_cast<const BoundPostfixExpression*>(node);
			return EvaluatePostfixExpression(p);
		}
		default:
			break;
	}
	throw std::invalid_argument(BuildStringFrom("Invalid expression: ", nameof(node->Kind())));
}

ValueType Evaluator::EvaluateLiteralExpression(const BoundLiteralExpression* node)const
{
	return node->Value();
}

ValueType Evaluator::EvaluateVariableExpression(const BoundVariableExpression* node)
{
	if (node->Variable()->Kind() == SymbolKind::GlobalVariable)
		return _globals.at(node->Variable().get());
	else
	{
		auto& locals = _locals.top();
		return locals.at(node->Variable().get());
	}
}

ValueType Evaluator::EvaluateAssignmentExpression(const BoundAssignmentExpression* node)
{
	auto value = EvaluateExpression(node->Expression().get());
	Assign(node->Variable().get(), value);
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
			if (node->Type() == TYPE_INT)
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
			if (node->Type() == TYPE_INT)
				return left.GetValue<IntegerType>() & right.GetValue<IntegerType>();
			else return left.GetValue<bool>() & right.GetValue<bool>();
		case BoundBinaryOperatorKind::BitwiseOr:
			if (node->Type() == TYPE_INT)
				return left.GetValue<IntegerType>() | right.GetValue<IntegerType>();
			else return left.GetValue<bool>() | right.GetValue<bool>();
		case BoundBinaryOperatorKind::BitwiseXor:
			if (node->Type() == TYPE_INT)
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
	if (*(node->Function()) == BUILTIN_INPUT)
	{
		auto f = []()
		{
			auto result = string();
			std::getline(std::cin, result);
			return result;
		};
		return f();
	} else if (*(node->Function()) == BUILTIN_PRINT)
	{
		auto message = EvaluateExpression(node->Arguments()[0].get());
		std::cout << message.ToString() << NEW_LINE;
		return NULL_VALUE;
	} else if (*(node->Function()) == BUILTIN_RND)
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
			auto& param = node->Function()->Parameters()[i];
			auto value = EvaluateExpression(node->Arguments()[i].get());
			locals.emplace(&param, std::move(value));
		}
		_locals.push(std::move(locals));
		auto statement = _functions.at(node->Function().get());
		auto result = EvaluateStatement(statement);

		_locals.pop();
		return result;
	}
}

ValueType Evaluator::EvaluateConversionExpression(const BoundConversionExpression* node)
{
	auto value = EvaluateExpression(node->Expression().get());
	if (node->Type() == TYPE_ANY)
		return value;
	else if (node->Type() == TYPE_BOOL)
		return value.ToBoolean();
	else if (node->Type() == TYPE_INT)
		return value.ToInteger();
	else if (node->Type() == TYPE_STRING)
		return value.ToString();
	else
		throw std::invalid_argument("Unexpected type: " + node->Type().ToString());
}

ValueType Evaluator::EvaluatePostfixExpression(const BoundPostfixExpression* node)
{
	auto value = EvaluateExpression(node->Expression().get());
	auto result = value.GetValue<IntegerType>();
	switch (node->OperatorKind())
	{
		case BoundPostfixOperatorEnum::Increment:
			Assign(node->Variable().get(), ++result);
			return result;
		case BoundPostfixOperatorEnum::Decrement:
			Assign(node->Variable().get(), --result);
			return result;
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected postfix operator: ", nameof(node->OperatorKind())));
	}
}

void Evaluator::Assign(const VariableSymbol* variable, const ValueType& value)
{
	if (variable->Kind() == SymbolKind::GlobalVariable)
		_globals.insert_or_assign(variable, value);
	else
	{
		auto& locals = _locals.top();
		locals.insert_or_assign(variable, value);
	}
}

Compilation::Compilation(bool isScript,
	unique_ptr<Compilation> previous,
	vector<unique_ptr<SyntaxTree>> trees)
	:_isScript(isScript), _previous(std::move(previous)), _syntaxTrees(),
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

Compilation::~Compilation() = default;

Compilation::Compilation(Compilation&& other) noexcept
	:_previous(std::move(other._previous)), _syntaxTrees(std::move(other._syntaxTrees)),
	_globalScope(std::move(other._globalScope)), _diagnostics(std::move(other._diagnostics)),
	_mtx()
{
}

unique_ptr<Compilation> Compilation::Create(unique_ptr<SyntaxTree> tree)
{
	return unique_ptr<Compilation>(new Compilation(false, nullptr, SyntaxTree::Flatten(std::move(tree))));
}

unique_ptr<Compilation> Compilation::CreateScript(unique_ptr<Compilation> previous,
	unique_ptr<SyntaxTree> tree)
{
	auto vec = vector<unique_ptr<SyntaxTree>>();
	vec.push_back(std::move(tree));
	return unique_ptr<Compilation>(new Compilation(true, std::move(previous), std::move(vec)));
}

const vector<const SyntaxTree*> Compilation::SyntaxTrees()const noexcept
{
	auto result = vector<const SyntaxTree*>();
	std::transform(_syntaxTrees.cbegin(), _syntaxTrees.cend(), std::back_inserter(result),
		[](const auto& tree) { return tree.get(); });
	return result;
}

const BoundGlobalScope* Compilation::GlobalScope()
{
	while (_globalScope == nullptr)
	{
		unique_ptr<BoundGlobalScope> tmp{ nullptr };
		if (_previous == nullptr)
			tmp = BindGlobalScope(IsScript(), nullptr, SyntaxTrees());
		else
			tmp = BindGlobalScope(IsScript(), _previous->GlobalScope(), SyntaxTrees());

		std::unique_lock<std::mutex> lock(_mtx, std::defer_lock);
		if (lock.try_lock() && _globalScope == nullptr)
			_globalScope.swap(tmp);
	}
	return _globalScope.get();
}

const vector<shared_ptr<FunctionSymbol>>& Compilation::Functions()
{
	return GlobalScope()->Functions();
}

const vector<shared_ptr<VariableSymbol>>& Compilation::Variables()
{
	return GlobalScope()->Variables();
}

const vector<const Symbol*> Compilation::GetSymbols()
{
	auto submission = this;
	auto result = vector<const Symbol*>();
	auto seenNames = std::unordered_set<string_view>();
	while (submission != nullptr)
	{
		for (const auto& func : submission->Functions())
		{
			auto [_, success] = seenNames.insert(func->Name());
			if (success)
				result.push_back(func.get());
		}
		for (const auto& var : submission->Variables())
		{
			auto [_, success] = seenNames.insert(var->Name());
			if (success)
				result.push_back(var.get());
		}
		for (const auto& builtin : GetAllBuiltinFunctions())
		{
			auto [_, success] = seenNames.insert(builtin.Name());
			if (success)
				result.push_back(&builtin);
		}
		submission = submission->Previous();
	}
	return result;
}

unique_ptr<BoundProgram> Compilation::GetProgram()
{
	auto previous = Previous() ? Previous()->GetProgram() : nullptr;
	return BindProgram(IsScript(), std::move(previous), GlobalScope());
}

EvaluationResult Compilation::Evaluate(VarMap& variables)
{
	//auto createCfgFile = [](const BoundProgram& p)
	//{
	//	auto file = std::ofstream();
	//	file.open("cfg.dot", std::ios_base::out | std::ios_base::trunc);
	//	if (file.is_open())
	//	{
	//		auto cfgStatement =
	//			p.Statement()->Statements().empty() && !p.Functions().empty() ?
	//			(--p.Functions().end())->second.get()
	//			: p.Statement();
	//		auto cfg = ControlFlowGraph::Create(cfgStatement);
	//		cfg.WriteTo(file);
	//	}
	//};

	std::for_each(_syntaxTrees.cbegin(), _syntaxTrees.cend(),
		[this](const auto& tree) { _diagnostics->AddRange(tree->Diagnostics()); });

	_diagnostics->AddRange(GlobalScope()->Diagnostics());

	if (!_diagnostics->empty())
		return EvaluationResult(*_diagnostics, NULL_VALUE);

	auto program = GetProgram();
	//createCfgFile(*program);
	if (!program->Diagnostics().empty())
	{
		_diagnostics->AddRange(program->Diagnostics());
		return EvaluationResult(*_diagnostics, NULL_VALUE);
	}

	Evaluator evaluator(std::move(program), variables);
	auto value = evaluator.Evaluate();
	return EvaluationResult(*_diagnostics, value);
}

void Compilation::EmitTree(std::ostream& out)
{
	if (GlobalScope()->MainFunc() != nullptr)
		EmitTree(GlobalScope()->MainFunc(), out);
	else if (GlobalScope()->ScriptFunc() != nullptr)
		EmitTree(GlobalScope()->ScriptFunc(), out);
}

void Compilation::EmitTree(const FunctionSymbol* symbol, std::ostream& out)
{
	auto program = GetProgram();
	try
	{
		auto& body = program->Functions().at(symbol);
		symbol->WriteTo(out);
		out << '\n';
		body->WriteTo(out);
	} catch (const std::out_of_range&)
	{
		return;
	}
}

DiagnosticBag Compilation::Emit(const string& moduleName, const fs::path& outPath)
{
	auto p = GetProgram();
	return MCF::Emit(*p, moduleName, outPath);
}

}//MCF