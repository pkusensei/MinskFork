#include "stdafx.h"
#include "Binding.h"

#include <iostream>
#include <sstream>
#include <stack>

#include "helpers.h"
#include "Diagnostic.h"
#include "Syntax.h"
#include "SourceText.h"

namespace MCF {

size_t LabelHash::operator()(const BoundLabel & label) const noexcept
{
	return std::hash<string>{}(label.Name());
}

string GetEnumText(const BoundNodeKind & kind)
{
	switch (kind)
	{
		case BoundNodeKind::BlockStatement:
			return "BlockStatement";
		case BoundNodeKind::VariableDeclaration:
			return "VariableDeclaration";
		case BoundNodeKind::IfStatement:
			return "IfStatement";
		case BoundNodeKind::WhileStatement:
			return "WhileStatement";
		case BoundNodeKind::ForStatement:
			return "ForStatement";
		case BoundNodeKind::LabelStatement:
			return "LabelStatement";
		case BoundNodeKind::GotoStatement:
			return "GotoStatement";
		case BoundNodeKind::ConditionalGotoStatement:
			return "ConditionalGotoStatement";
		case BoundNodeKind::ExpressionStatement:
			return "ExpressionStatement";

		case BoundNodeKind::ErrorExpression:
			return "ErrorExpression";
		case BoundNodeKind::LiteralExpression:
			return "LiteralExpression";
		case BoundNodeKind::VariableExpression:
			return "VariableExpression";
		case BoundNodeKind::AssignmentExpression:
			return "AssignmentExpression";
		case BoundNodeKind::UnaryExpression:
			return "UnaryExpression";
		case BoundNodeKind::BinaryExpression:
			return "BinaryExpression";
		case BoundNodeKind::CallExpression:
			return "CallExpression";
		case BoundNodeKind::ConversionExpression:
			return "ConversionExpression";
		case BoundNodeKind::PostfixExpression:
			return "PostfixExpression";

		default:
			return string();
	}
}

string GetEnumText(const BoundUnaryOperatorKind & kind)
{
	switch (kind)
	{
		case BoundUnaryOperatorKind::Identity:
			return "Identity";
		case BoundUnaryOperatorKind::Negation:
			return "Negation";
		case BoundUnaryOperatorKind::LogicalNegation:
			return "LogicalNegation";
		case BoundUnaryOperatorKind::OnesComplement:
			return "OnesComplement";

		default:
			return string();
	}
}

string GetEnumText(const BoundBinaryOperatorKind & kind)
{
	switch (kind)
	{
		case BoundBinaryOperatorKind::Addition:
			return "Addition";
		case BoundBinaryOperatorKind::Subtraction:
			return "Subtraction";
		case BoundBinaryOperatorKind::Multiplication:
			return "Multiplication";
		case BoundBinaryOperatorKind::Division:
			return "Division";
		case BoundBinaryOperatorKind::Modulus:
			return "Modulus";
		case BoundBinaryOperatorKind::LogicalAnd:
			return "LogicalAnd";
		case BoundBinaryOperatorKind::LogicalOr:
			return "LogicalOr";
		case BoundBinaryOperatorKind::BitwiseAnd:
			return "BitwiseAnd";
		case BoundBinaryOperatorKind::BitwiseOr:
			return "BitwiseOr";
		case BoundBinaryOperatorKind::BitwiseXor:
			return "BitwiseXor";
		case BoundBinaryOperatorKind::Equals:
			return "Equals";
		case BoundBinaryOperatorKind::NotEquals:
			return "NotEquals";
		case BoundBinaryOperatorKind::Less:
			return "Less";
		case BoundBinaryOperatorKind::LessOrEquals:
			return "LessOrEquals";
		case BoundBinaryOperatorKind::Greater:
			return "Greater";
		case BoundBinaryOperatorKind::GreaterOrEquals:
			return "GreaterOrEquals";

		default:
			return string();
	}
}

string GetEnumText(const BoundPostfixOperatorEnum & kind)
{
	switch (kind)
	{
		case BoundPostfixOperatorEnum::Increment:
			return "Increment";
		case BoundPostfixOperatorEnum::Decrement:
			return "Decrement";
		default:
			return string();
	}
}

ConsoleColor BoundNode::GetColor(const BoundNode * node)
{
	auto e = dynamic_cast<const BoundExpression*>(node);
	auto s = dynamic_cast<const BoundStatement*>(node);
	if (e != nullptr)
		return ConsoleColor::Blue;
	else if (s != nullptr)
		return ConsoleColor::Cyan;
	else return ConsoleColor::Yellow;
}

string BoundNode::GetText(const BoundNode * node)
{
	auto b = dynamic_cast<const BoundBinaryExpression*>(node);
	auto u = dynamic_cast<const BoundUnaryExpression*>(node);
	if (b != nullptr)
		return GetEnumText(b->Op()->Kind()) + "Expression";
	else if (u != nullptr)
		return GetEnumText(u->Op()->Kind()) + "Expression";
	else return GetEnumText(node->Kind());
}

void BoundNode::PrettyPrint(std::ostream & out, const BoundNode * node, string indent, bool isLast)
{
	auto isToConsole = out.rdbuf() == std::cout.rdbuf();
	string marker = isLast ? "+--" : "---";//"└──" : "├──";

	if (isToConsole)
		SetConsoleColor(ConsoleColor::Grey);
	out << indent << marker;

	if (isToConsole)
		SetConsoleColor(GetColor(node));
	out << GetText(node);

	auto isFirstProperty = true;
	auto properties = node->GetProperties();
	if (!properties.empty())
	{
		for (const auto p : properties)
		{
			if (isFirstProperty)
				isFirstProperty = false;
			else
			{
				if (isToConsole)
					SetConsoleColor(ConsoleColor::Grey);
				out << ",";
			}
			out << ' ';

			if (isToConsole)
				SetConsoleColor(ConsoleColor::Yellow);
			out << p.first;

			if (isToConsole)
				SetConsoleColor(ConsoleColor::Grey);
			out << " = ";

			if (isToConsole)
				SetConsoleColor(ConsoleColor::DarkYellow);
			out << p.second;
		}
	}

	if (isToConsole)
		ResetConsoleColor();

	out << "\n";
	indent += isLast ? "   " : "|  ";
	auto children = node->GetChildren();
	if (!children.empty())
	{
		auto lastChild = children.back();
		for (const auto& child : children)
			PrettyPrint(out, child, indent, lastChild == child);
	}
}

string BoundNode::ToString() const
{
	std::stringstream ss;
	WriteTo(ss);
	return ss.str();
}

#pragma region Expression

const vector<const BoundNode*> BoundExpression::GetChildren() const
{
	return vector<const BoundNode*>();
}

const vector<std::pair<string, string>> BoundErrorExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Type", Type().Name())
	};
}

BoundUnaryOperator::BoundUnaryOperator(const enum SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
									   const TypeSymbol& operandType, const TypeSymbol& resultType)
	:_syntaxKind(synKind), _kind(kind), _operandType(operandType), _resultType(resultType)
{
}

BoundUnaryOperator::BoundUnaryOperator(const enum SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
									   const TypeSymbol& operandType)
	: BoundUnaryOperator(synKind, kind, operandType, operandType)
{
}

BoundUnaryOperator::BoundUnaryOperator()
	: BoundUnaryOperator(SyntaxKind::BadToken, BoundUnaryOperatorKind::Identity, TypeSymbol::GetType(TypeEnum::Error))
{
	_isUseful = false;
}

const vector<BoundUnaryOperator>& BoundUnaryOperator::Operators()
{
	static const auto operators = vector<BoundUnaryOperator>{
	BoundUnaryOperator(SyntaxKind::BangToken, BoundUnaryOperatorKind::LogicalNegation, TypeSymbol::GetType(TypeEnum::Bool)),
	BoundUnaryOperator(SyntaxKind::PlusToken, BoundUnaryOperatorKind::Identity, TypeSymbol::GetType(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::MinusToken, BoundUnaryOperatorKind::Negation, TypeSymbol::GetType(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::TildeToken, BoundUnaryOperatorKind::OnesComplement, TypeSymbol::GetType(TypeEnum::Int))
	};

	return operators;
}

BoundUnaryOperator BoundUnaryOperator::Bind(const enum SyntaxKind& synKind, const TypeSymbol& type)
{
	for (const auto& op : Operators())
	{
		if (op.SyntaxKind() == synKind && op.OperandType() == type)
			return op;
	}
	return BoundUnaryOperator();
}

BoundUnaryExpression::BoundUnaryExpression(const BoundUnaryOperator & op, const unique_ptr<BoundExpression>& operand)
	:_op(op),
	_operand(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(operand)))
{
}

const vector<const BoundNode*> BoundUnaryExpression::GetChildren() const
{
	return vector<const BoundNode*>{
		_operand.get()
	};
}

const vector<std::pair<string, string>> BoundUnaryExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Type", Type().Name())
	};
}

BoundBinaryOperator::BoundBinaryOperator(const enum SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
										 const TypeSymbol& left, const TypeSymbol& right, const TypeSymbol& result)
	: _syntaxKind(synKind), _kind(kind), _leftType(left), _rightType(right), _resultType(result)
{
}

BoundBinaryOperator::BoundBinaryOperator(const enum SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
										 const TypeSymbol& operandType, const TypeSymbol& resultType)
	: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
{
}

BoundBinaryOperator::BoundBinaryOperator(const enum SyntaxKind& synKind, const BoundBinaryOperatorKind& kind, const TypeSymbol& type)
	: BoundBinaryOperator(synKind, kind, type, type, type)
{
}

BoundBinaryOperator::BoundBinaryOperator()
	: BoundBinaryOperator(SyntaxKind::BadToken, BoundBinaryOperatorKind::Addition, TypeSymbol::GetType(TypeEnum::Error))
{
	_isUseful = false;
}

const vector<BoundBinaryOperator>& BoundBinaryOperator::Operators()
{
	static const auto operators = vector<BoundBinaryOperator>{
		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition, TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::MinusToken, BoundBinaryOperatorKind::Subtraction, TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::StarToken, BoundBinaryOperatorKind::Multiplication, TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::SlashToken, BoundBinaryOperatorKind::Division, TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::PercentToken, BoundBinaryOperatorKind::Modulus, TypeSymbol::GetType(TypeEnum::Int)),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd, TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr, TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor, TypeSymbol::GetType(TypeEnum::Int)),

		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::LessToken, BoundBinaryOperatorKind::Less, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::LessOrEqualsToken, BoundBinaryOperatorKind::LessOrEquals, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::GreaterToken, BoundBinaryOperatorKind::Greater, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::GreaterOrEqualsToken, BoundBinaryOperatorKind::GreaterOrEquals, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd, TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::AmpersandAmpersandToken, BoundBinaryOperatorKind::LogicalAnd, TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr, TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::PipePipeToken, BoundBinaryOperatorKind::LogicalOr, TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor, TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals, TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals, TypeSymbol::GetType(TypeEnum::Bool)),

		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition, TypeSymbol::GetType(TypeEnum::String))
	};

	return operators;
}

BoundBinaryOperator BoundBinaryOperator::Bind(const enum SyntaxKind& synKind, const TypeSymbol& leftType, const TypeSymbol& rightType)
{
	for (const auto& op : Operators())
	{
		if (op.SyntaxKind() == synKind && op.LeftType() == leftType && op.RightType() == rightType)
			return op;
	}
	return BoundBinaryOperator();
}

BoundBinaryExpression::BoundBinaryExpression(const unique_ptr<BoundExpression>& left, const BoundBinaryOperator & op, const unique_ptr<BoundExpression>& right)
	:_left(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(left))),
	_right(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(right))),
	_op(op)
{
}

const vector<const BoundNode*> BoundBinaryExpression::GetChildren() const
{
	return vector<const BoundNode*>{
		_left.get(),
			_right.get()
	};
}

const vector<std::pair<string, string>> BoundBinaryExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Type", Type().Name())
	};
}

BoundAssignmentExpression::BoundAssignmentExpression(const VariableSymbol & variable, const unique_ptr<BoundExpression>& expression)
	:_variable(variable),
	_expression(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(expression)))
{
}

const vector<const BoundNode*> BoundAssignmentExpression::GetChildren() const
{
	return vector<const BoundNode*>{
		_expression.get()
	};
}

const vector<std::pair<string, string>> BoundAssignmentExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable().ToString()),
			std::pair<string, string>("Type", Type().Name())
	};
}

BoundLiteralExpression::BoundLiteralExpression(const ValueType & value)
	: _value(value)
{
}

const vector<std::pair<string, string>> BoundLiteralExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Value", Value().ToString()),
			std::pair<string, string>("Type", Type().Name())
	};
}

BoundVariableExpression::BoundVariableExpression(const VariableSymbol & variable)
	: _variable(variable)
{
}

const vector<std::pair<string, string>> BoundVariableExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable().ToString()),
			std::pair<string, string>("Type", Type().Name())
	};
}

BoundPostfixExpression::BoundPostfixExpression(const VariableSymbol & variable, const BoundPostfixOperatorEnum& kind, const unique_ptr<BoundExpression>& expression)
	:_variable(variable), _kind(kind),
	_expression(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(expression)))
{
}

const vector<const BoundNode*> BoundPostfixExpression::GetChildren() const
{
	return vector<const BoundNode*>{_expression.get()};
}

const vector<std::pair<string, string>> BoundPostfixExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable().ToString()),
			std::pair<string, string>("Type", Type().Name()),
			std::pair<string, string>("OperatorKind", GetEnumText(OperatorKind()))
	};
}

#pragma endregion

#pragma region Statement

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

BoundWhileStatement::BoundWhileStatement(const unique_ptr<BoundExpression>& condition, const unique_ptr<BoundStatement>& body)
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

BoundConditionalGotoStatement::BoundConditionalGotoStatement(const BoundLabel & label, const unique_ptr<BoundExpression>& condition, bool jumpIfTrue)
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

#pragma endregion

BoundScope::BoundScope(const unique_ptr<BoundScope>& parent)
	: _parent(std::move((std::remove_const_t<unique_ptr<BoundScope>&>(parent))))
{
}

bool BoundScope::TryDeclare(const VariableSymbol & variable)
{
	if (_variables.find(variable.Name()) == _variables.end() && !variable.Name().empty())
	{
		_variables.emplace(variable.Name(), variable);
		return true;
	}
	return false;
}

bool BoundScope::TryLookup(const string & name, VariableSymbol & variable)const
{
	if (_variables.find(name) != _variables.end())
	{
		variable = _variables.at(name);
		return true;
	}
	if (_parent == nullptr || name.empty())
		return false;
	return _parent->TryLookup(name, variable);
}

const vector<VariableSymbol> BoundScope::GetDeclaredVariables() const
{
	auto result = vector<VariableSymbol>();
	for (const auto& it : _variables)
		result.emplace_back(it.second);
	return result;
}

void BoundScope::ResetToParent(unique_ptr<BoundScope>& current)
{
	if (current->Parent() == nullptr) return;
	current.swap(current->_parent);
}


BoundGlobalScope::BoundGlobalScope(const BoundGlobalScope* previous, const unique_ptr<DiagnosticBag>& diagnostics,
								   const vector<VariableSymbol>& variables, const unique_ptr<BoundStatement>& statement)
	:_previous(previous),
	_diagnostics(std::move(std::remove_const_t<unique_ptr<DiagnosticBag>&>(diagnostics))),
	_variables(variables),
	_statement(std::move(std::remove_const_t<unique_ptr<BoundStatement>&>(statement)))
{
}

Binder::Binder(const unique_ptr<BoundScope>& parent)
	: _diagnostics(make_unique<DiagnosticBag>()),
	_scope(make_unique<BoundScope>(parent))
{
}

unique_ptr<BoundStatement> Binder::BindStatement(const StatementSyntax * syntax)
{
	switch (syntax->Kind())
	{
		case SyntaxKind::BlockStatement:
		{
			auto p = dynamic_cast<const BlockStatementSyntax*>(syntax);
			if (p) return BindBlockStatement(p);
			else break;
		}
		case SyntaxKind::VariableDeclaration:
		{
			auto p = dynamic_cast<const VariableDeclarationSyntax*>(syntax);
			if (p) return BindVariableDeclaration(p);
			else break;
		}
		case SyntaxKind::IfStatement:
		{
			auto p = dynamic_cast<const IfStatementSyntax*>(syntax);
			if (p) return BindIfStatement(p);
			else break;
		}
		case SyntaxKind::WhileStatement:
		{
			auto p = dynamic_cast<const WhileStatementSyntax*>(syntax);
			if (p) return BindWhileStatement(p);
			else break;
		}
		case SyntaxKind::ForStatement:
		{
			auto p = dynamic_cast<const ForStatementSyntax*>(syntax);
			if (p) return BindForStatement(p);
			else break;
		}
		case SyntaxKind::ExpressionStatement:
		{
			auto p = dynamic_cast<const ExpressionStatementSyntax*>(syntax);
			if (p) return BindExpressionStatement(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Unexpected syntax " + GetSyntaxKindName(syntax->Kind()));
}

unique_ptr<BoundStatement> Binder::BindBlockStatement(const BlockStatementSyntax * syntax)
{
	auto result = vector<unique_ptr<BoundStatement>>();
	_scope = make_unique<BoundScope>(_scope);
	auto statements = syntax->Statements();
	for (const auto& it : statements)
		result.emplace_back(BindStatement(it));
	BoundScope::ResetToParent(_scope);
	return make_unique<BoundBlockStatement>(result);
}

unique_ptr<BoundStatement> Binder::BindVariableDeclaration(const VariableDeclarationSyntax * syntax)
{
	auto readOnly = syntax->Keyword().Kind() == SyntaxKind::LetKeyword;
	auto init = BindExpression(syntax->Initializer());
	auto variable = BindVariable(syntax->Identifier(), readOnly, init->Type());

	return make_unique<BoundVariableDeclaration>(variable, init);
}

unique_ptr<BoundStatement> Binder::BindIfStatement(const IfStatementSyntax * syntax)
{
	auto condition = BindExpression(syntax->Condition(), TypeSymbol::GetType(TypeEnum::Bool));
	auto thenStatement = BindStatement(syntax->ThenStatement());
	auto elseStatement = syntax->ElseClause() == nullptr ? nullptr
		: BindStatement(syntax->ElseClause()->ElseStatement());
	return make_unique<BoundIfStatement>(condition, thenStatement, elseStatement);
}

unique_ptr<BoundStatement> Binder::BindWhileStatement(const WhileStatementSyntax * syntax)
{
	auto condition = BindExpression(syntax->Condition(), TypeSymbol::GetType(TypeEnum::Bool));
	auto body = BindStatement(syntax->Body());
	return make_unique<BoundWhileStatement>(condition, body);
}

unique_ptr<BoundStatement> Binder::BindForStatement(const ForStatementSyntax * syntax)
{
	auto lowerBound = BindExpression(syntax->LowerBound(), TypeSymbol::GetType(TypeEnum::Int));
	auto upperBound = BindExpression(syntax->UpperBound(), TypeSymbol::GetType(TypeEnum::Int));

	_scope = make_unique<BoundScope>(_scope);

	auto variable = BindVariable(syntax->Identifier(), true, TypeSymbol::GetType(TypeEnum::Int));
	auto body = BindStatement(syntax->Body());

	BoundScope::ResetToParent(_scope);
	return make_unique<BoundForStatement>(variable, lowerBound, upperBound, body);
}

unique_ptr<BoundStatement> Binder::BindExpressionStatement(const ExpressionStatementSyntax * syntax)
{
	auto expression = BindExpression(syntax->Expression());
	return make_unique<BoundExpressionStatement>(expression);
}

unique_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax * syntax, const TypeSymbol & targetType)
{
	auto result = BindExpression(syntax);
	if (targetType != TypeSymbol::GetType(TypeEnum::Error) && result->Type() != TypeSymbol::GetType(TypeEnum::Error)
		&& result->Type() != targetType)
		_diagnostics->ReportCannotConvert(syntax->Span(), result->Type(), targetType);
	return result;
}

unique_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax * syntax)
{
	switch (syntax->Kind())
	{
		case SyntaxKind::ParenthesizedExpression:
		{
			auto p = dynamic_cast<const ParenthesizedExpressionSyntax*>(syntax);
			if (p) return BindParenthesizedExpression(p);
			else break;
		}
		case SyntaxKind::LiteralExpression:
		{
			auto p = dynamic_cast<const LiteralExpressionSyntax*>(syntax);
			if (p) return BindLiteralExpression(p);
			else break;
		}
		case SyntaxKind::NameExpression:
		{
			auto p = dynamic_cast<const NameExpressionSyntax*>(syntax);
			if (p) return BindNameExpression(p);
			else break;
		}
		case SyntaxKind::AssignmentExpression:
		{
			auto p = dynamic_cast<const AssignmentExpressionSyntax*>(syntax);
			if (p) return BindAssignmentExpression(p);
			else break;
		}
		case SyntaxKind::UnaryExpression:
		{
			auto p = dynamic_cast<const UnaryExpressionSyntax*>(syntax);
			if (p) return BindUnaryExpression(p);
			else break;
		}
		case SyntaxKind::BinaryExpression:
		{
			auto p = dynamic_cast<const BinaryExpressionSyntax*>(syntax);
			if (p) return BindBinaryExpression(p);
			else break;
		}
		case SyntaxKind::PostfixExpression:
		{
			auto p = dynamic_cast<const PostfixExpressionSyntax*>(syntax);
			if (p) return BindPostfixExpression(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Invalid expression " + GetSyntaxKindName(syntax->Kind()));

}

unique_ptr<BoundExpression> Binder::BindParenthesizedExpression(const ParenthesizedExpressionSyntax * syntax)
{
	return BindExpression(syntax->Expression());
}

unique_ptr<BoundExpression> Binder::BindLiteralExpression(const LiteralExpressionSyntax * syntax)
{
	return make_unique<BoundLiteralExpression>(syntax->Value());
}

unique_ptr<BoundExpression> Binder::BindNameExpression(const NameExpressionSyntax * syntax)
{
	auto name = syntax->IdentifierToken().Text();
	if (syntax->IdentifierToken().IsMissing()) // NOTE this token was injected by Parser::MatchToken
		return make_unique<BoundErrorExpression>();

	VariableSymbol variable;
	if (!_scope->TryLookup(name, variable))
	{
		_diagnostics->ReportUndefinedName(syntax->IdentifierToken().Span(), name);
		return make_unique<BoundErrorExpression>();
	}
	return make_unique<BoundVariableExpression>(variable);
}

unique_ptr<BoundExpression> Binder::BindAssignmentExpression(const AssignmentExpressionSyntax * syntax)
{
	auto name = syntax->IdentifierToken().Text();
	auto boundExpression = BindExpression(syntax->Expression());

	VariableSymbol variable;

	if (!_scope->TryLookup(name, variable))
	{
		_diagnostics->ReportUndefinedName(syntax->IdentifierToken().Span(), name);
		return boundExpression;
	}
	if (variable.IsReadOnly())
		_diagnostics->ReportCannotAssign(syntax->EqualsToken().Span(), name);
	if (boundExpression->Type() != variable.Type())
	{
		_diagnostics->ReportCannotConvert(syntax->Expression()->Span(), boundExpression->Type(), variable.Type());
		return boundExpression;
	}
	return make_unique<BoundAssignmentExpression>(variable, boundExpression);
}

unique_ptr<BoundExpression> Binder::BindUnaryExpression(const UnaryExpressionSyntax * syntax)
{
	auto boundOperand = BindExpression(syntax->Operand());
	if (boundOperand->Type() == TypeSymbol::GetType(TypeEnum::Error))
		return make_unique<BoundErrorExpression>();

	auto boundOperator = BoundUnaryOperator::Bind(syntax->OperatorToken().Kind(), boundOperand->Type());
	if (boundOperator.IsUseful())
	{
		return make_unique<BoundUnaryExpression>(boundOperator, boundOperand);
	} else
	{
		_diagnostics->ReportUndefinedUnaryOperator(syntax->OperatorToken().Span(), syntax->OperatorToken().Text(), boundOperand->Type());
		return make_unique<BoundErrorExpression>();
	}
}

unique_ptr<BoundExpression> Binder::BindBinaryExpression(const BinaryExpressionSyntax * syntax)
{
	auto boundLeft = BindExpression(syntax->Left());
	auto boundRight = BindExpression(syntax->Right());
	if (boundLeft->Type() == TypeSymbol::GetType(TypeEnum::Error) || boundRight->Type() == TypeSymbol::GetType(TypeEnum::Error))
		return make_unique<BoundErrorExpression>();

	auto boundOperator = BoundBinaryOperator::Bind(syntax->OperatorToken().Kind(), boundLeft->Type(), boundRight->Type());
	if (boundOperator.IsUseful())
	{
		return make_unique<BoundBinaryExpression>(boundLeft, boundOperator, boundRight);
	} else
	{
		_diagnostics->ReportUndefinedBinaryOperator(syntax->OperatorToken().Span(), syntax->OperatorToken().Text(),
													boundLeft->Type(), boundRight->Type());
		return make_unique<BoundErrorExpression>();
	}
}

unique_ptr<BoundExpression> Binder::BindPostfixExpression(const PostfixExpressionSyntax * syntax)
{
	auto name = syntax->IdentifierToken().Text();
	auto boundExpression = BindExpression(syntax->Expression());

	VariableSymbol variable;

	if (!_scope->TryLookup(name, variable))
	{
		_diagnostics->ReportUndefinedName(syntax->IdentifierToken().Span(), name);
		return make_unique<BoundErrorExpression>();
	}
	if (variable.IsReadOnly())
	{
		_diagnostics->ReportCannotAssign(syntax->Op().Span(), name);
		return make_unique<BoundErrorExpression>();
	}
	if (boundExpression->Type() != variable.Type())
	{
		_diagnostics->ReportCannotConvert(syntax->Expression()->Span(), boundExpression->Type(), variable.Type());
		return make_unique<BoundErrorExpression>();
	}
	if (variable.Type() != TypeSymbol::GetType(TypeEnum::Int))
	{
		_diagnostics->ReportVariableNotSupportPostfixOperator(syntax->Expression()->Span(), syntax->Op().Text(), variable.Type());
		return make_unique<BoundErrorExpression>();
	}
	switch (syntax->Op().Kind())
	{
		case SyntaxKind::PlusPlusToken:
			return make_unique<BoundPostfixExpression>(variable, BoundPostfixOperatorEnum::Increment, boundExpression);
		case SyntaxKind::MinusMinusToken:
			return make_unique<BoundPostfixExpression>(variable, BoundPostfixOperatorEnum::Decrement, boundExpression);
		default:
			throw std::invalid_argument("Unexpected operator token " + GetSyntaxKindName(syntax->Op().Kind()));
	}
}

VariableSymbol Binder::BindVariable(const SyntaxToken & identifier, bool isReadOnly, const TypeSymbol & type)
{
	auto name = identifier.Text().empty() ? "?" : identifier.Text();
	auto declare = !identifier.IsMissing();
	auto variable = VariableSymbol(name, isReadOnly, type);

	if (declare && !_scope->TryDeclare(variable))
		_diagnostics->ReportVariableAlreadyDeclared(identifier.Span(), name);
	return variable;
}

unique_ptr<BoundScope> Binder::CreateParentScope(const BoundGlobalScope* previous)
{
	auto stack = std::stack<const BoundGlobalScope*>();
	while (previous != nullptr)
	{
		stack.emplace(previous);
		previous = previous->Previous();
	}
	unique_ptr<BoundScope> parent{nullptr};
	while (!stack.empty())
	{
		auto current = stack.top();
		auto scope = make_unique<BoundScope>(parent);
		for (const auto& it : current->Variables())
			scope->TryDeclare(it);
		parent.swap(scope);
		stack.pop();
	}
	return parent;
}

unique_ptr<BoundGlobalScope> Binder::BindGlobalScope(const BoundGlobalScope* previous, const CompilationUnitSyntax* syntax)
{
	auto parentScope = CreateParentScope(previous);
	Binder binder(parentScope);
	auto expression = binder.BindStatement(syntax->Statement());
	auto variables = binder._scope->GetDeclaredVariables();
	auto diagnostics = binder.Diagnostics();
	if (previous != nullptr)
		diagnostics->AddRangeFront(*previous->Diagnostics());
	return make_unique<BoundGlobalScope>(previous, binder._diagnostics, variables, expression);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteStatement(const BoundStatement * node)
{
	switch (node->Kind())
	{
		case BoundNodeKind::BlockStatement:
		{
			auto p = dynamic_cast<const BoundBlockStatement*>(node);
			if (p) return RewriteBlockStatement(p);
			else break;
		}
		case BoundNodeKind::VariableDeclaration:
		{
			auto p = dynamic_cast<const BoundVariableDeclaration*>(node);
			if (p) return RewriteVariableDeclaration(p);
			else break;
		}
		case BoundNodeKind::IfStatement:
		{
			auto p = dynamic_cast<const BoundIfStatement*>(node);
			if (p) return RewriteIfStatement(p);
			else break;
		}
		case BoundNodeKind::WhileStatement:
		{
			auto p = dynamic_cast<const BoundWhileStatement*>(node);
			if (p) return RewriteWhileStatement(p);
			else break;
		}
		case BoundNodeKind::ForStatement:
		{
			auto p = dynamic_cast<const BoundForStatement*>(node);
			if (p) return RewriteForStatement(p);
			else break;
		}
		case BoundNodeKind::LabelStatement:
		{
			auto p = dynamic_cast<const BoundLabelStatement*>(node);
			if (p) return RewriteLabelStatement(p);
			else break;
		}
		case BoundNodeKind::GotoStatement:
		{
			auto p = dynamic_cast<const BoundGotoStatement*>(node);
			if (p) return RewriteGotoStatement(p);
			else break;
		}
		case BoundNodeKind::ConditionalGotoStatement:
		{
			auto p = dynamic_cast<const BoundConditionalGotoStatement*>(node);
			if (p) return RewriteConditionalGotoStatement(p);
			else break;
		}
		case BoundNodeKind::ExpressionStatement:
		{
			auto p = dynamic_cast<const BoundExpressionStatement*>(node);
			if (p) return RewriteExpressionStatement(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Unexpected node: " + GetEnumText(node->Kind()));
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteBlockStatement(const BoundBlockStatement * node)
{
	auto result = vector<unique_ptr<BoundStatement>>();
	auto statements = node->Statements();
	for (const auto& it : statements)
		result.emplace_back(RewriteStatement(it));
	return make_unique<BoundBlockStatement>(result);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteVariableDeclaration(const BoundVariableDeclaration * node)
{
	auto initializer = RewriteExpression(node->Initializer());
	return make_unique<BoundVariableDeclaration>(node->Variable(), initializer);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteIfStatement(const BoundIfStatement * node)
{
	auto condition = RewriteExpression(node->Condition());
	auto thenStatement = RewriteStatement(node->ThenStatement());
	auto elseStatement = node->ElseStatement() == nullptr ? nullptr : RewriteStatement(node->ElseStatement());
	return make_unique<BoundIfStatement>(condition, thenStatement, elseStatement);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteWhileStatement(const BoundWhileStatement * node)
{
	auto condition = RewriteExpression(node->Condition());
	auto body = RewriteStatement(node->Body());
	return make_unique<BoundWhileStatement>(condition, body);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteForStatement(const BoundForStatement * node)
{
	auto lowerBound = RewriteExpression(node->LowerBound());
	auto upperBound = RewriteExpression(node->UpperBound());
	auto body = RewriteStatement(node->Body());
	return make_unique<BoundForStatement>(node->Variable(), lowerBound, upperBound, body);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteLabelStatement(const BoundLabelStatement * node)
{
	return make_unique<BoundLabelStatement>(node->Label());
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteGotoStatement(const BoundGotoStatement * node)
{
	return make_unique<BoundGotoStatement>(node->Label());
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteConditionalGotoStatement(const BoundConditionalGotoStatement * node)
{
	auto condition = RewriteExpression(node->Condition());
	return make_unique<BoundConditionalGotoStatement>(node->Label(), condition, node->JumpIfTrue());
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteExpressionStatement(const BoundExpressionStatement * node)
{
	auto expression = RewriteExpression(node->Expression());
	return make_unique<BoundExpressionStatement>(expression);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteExpression(const BoundExpression * node)
{
	switch (node->Kind())
	{
		case BoundNodeKind::ErrorExpression:
		{
			auto p = dynamic_cast<const BoundErrorExpression*>(node);
			if (p) return RewriteErrorExpression(p);
			else break;
		}
		case BoundNodeKind::LiteralExpression:
		{
			auto p = dynamic_cast<const BoundLiteralExpression*>(node);
			if (p) return RewriteLiteralExpression(p);
			else break;
		}
		case BoundNodeKind::VariableExpression:
		{
			auto p = dynamic_cast<const BoundVariableExpression*>(node);
			if (p) return RewriteVariableExpression(p);
			else break;
		}
		case BoundNodeKind::AssignmentExpression:
		{
			auto p = dynamic_cast<const BoundAssignmentExpression*>(node);
			if (p) return RewriteAssignmentExpression(p);
			else break;
		}
		case BoundNodeKind::UnaryExpression:
		{
			auto p = dynamic_cast<const BoundUnaryExpression*>(node);
			if (p) return RewriteUnaryExpression(p);
			else break;
		}
		case BoundNodeKind::BinaryExpression:
		{
			auto p = dynamic_cast<const BoundBinaryExpression*>(node);
			if (p) return RewriteBinaryExpression(p);
			else break;
		}
		case BoundNodeKind::PostfixExpression:
		{
			auto p = dynamic_cast<const BoundPostfixExpression*>(node);
			if (p) return RewritePostfixExpression(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Unexpected node: " + GetEnumText(node->Kind()));
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteErrorExpression(const BoundErrorExpression * node)
{
	return make_unique<BoundErrorExpression>();
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteLiteralExpression(const BoundLiteralExpression * node)
{
	return make_unique<BoundLiteralExpression>(node->Value());
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteVariableExpression(const BoundVariableExpression * node)
{
	return make_unique<BoundVariableExpression>(node->Variable());
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteAssignmentExpression(const BoundAssignmentExpression * node)
{
	auto expression = RewriteExpression(node->Expression());
	return make_unique<BoundAssignmentExpression>(node->Variable(), expression);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteUnaryExpression(const BoundUnaryExpression * node)
{
	auto operand = RewriteExpression(node->Operand());
	return make_unique<BoundUnaryExpression>(*(node->Op()), operand);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteBinaryExpression(const BoundBinaryExpression * node)
{
	auto left = RewriteExpression(node->Left());
	auto right = RewriteExpression(node->Right());
	return make_unique<BoundBinaryExpression>(left, *(node->Op()), right);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewritePostfixExpression(const BoundPostfixExpression * node)
{
	auto expression = RewriteExpression(node->Expression());
	return make_unique<BoundPostfixExpression>(node->Variable(), node->OperatorKind(), expression);
}

BoundLabel Lowerer::GenerateLabel()
{
	++_labelCount;
	string name("Label" + std::to_string(_labelCount));
	return BoundLabel(name);
}

unique_ptr<BoundBlockStatement> Lowerer::Lower(const BoundStatement * statement)
{
	auto lowerer = Lowerer();
	auto result = lowerer.RewriteStatement(statement);
	return lowerer.Flatten(result);
}

unique_ptr<BoundBlockStatement> Lowerer::Flatten(unique_ptr<BoundStatement>& statement)
{
	auto result = vector<unique_ptr<BoundStatement>>();
	auto stack = std::stack<unique_ptr<BoundStatement>>();
	stack.emplace(std::move(statement));

	while (!stack.empty())
	{
		auto current = std::move(stack.top());
		stack.pop();

		if (auto p = dynamic_cast<BoundBlockStatement*>(current.get()))
		{
			auto statements = p->Statements();
			for (auto it = statements.rbegin(); it != statements.rend(); ++it)
				stack.emplace(RewriteStatement(*it));
		} else
		{
			result.emplace_back(std::move(current));
		}
	}
	return make_unique<BoundBlockStatement>(result);
}

unique_ptr<BoundStatement> Lowerer::RewriteIfStatement(const BoundIfStatement * node)
{
	if (node->ElseStatement() == nullptr)
	{
		auto endLabel = GenerateLabel();
		auto endLabelStatement = make_unique<BoundLabelStatement>(endLabel);
		// HACK construct new BoundExpression
		auto condition = RewriteExpression(node->Condition());
		auto thenStatement = RewriteStatement(node->ThenStatement());
		auto gotoFalse = make_unique<BoundConditionalGotoStatement>(endLabel, condition, false);

		auto statements = vector<unique_ptr<BoundStatement>>();
		statements.emplace_back(std::move(gotoFalse));
		statements.emplace_back(std::move(thenStatement));
		statements.emplace_back(std::move(endLabelStatement));

		auto result = make_unique<BoundBlockStatement>(statements);
		return RewriteStatement(result.get());
	} else
	{
		auto elseLabel = GenerateLabel();
		auto endLabel = GenerateLabel();
		auto elseLabelStatement = make_unique<BoundLabelStatement>(elseLabel);
		auto endLabelStatement = make_unique<BoundLabelStatement>(endLabel);
		auto gotoEndStatement = make_unique<BoundGotoStatement>(endLabel);

		auto condition = RewriteExpression(node->Condition());
		auto thenStatement = RewriteStatement(node->ThenStatement());
		auto elseStatement = RewriteStatement(node->ElseStatement());
		auto gotoFalse = make_unique<BoundConditionalGotoStatement>(elseLabel, condition, false);

		auto statements = vector<unique_ptr<BoundStatement>>();
		statements.emplace_back(std::move(gotoFalse));
		statements.emplace_back(std::move(thenStatement));
		statements.emplace_back(std::move(gotoEndStatement));
		statements.emplace_back(std::move(elseLabelStatement));
		statements.emplace_back(std::move(elseStatement));
		statements.emplace_back(std::move(endLabelStatement));

		auto result = make_unique<BoundBlockStatement>(statements);
		return RewriteStatement(result.get());
	}
}

unique_ptr<BoundStatement> Lowerer::RewriteWhileStatement(const BoundWhileStatement * node)
{
	auto continueLabel = GenerateLabel();
	auto checkLabel = GenerateLabel();
	auto endLabel = GenerateLabel();
	auto continueLabelStatement = make_unique<BoundLabelStatement>(continueLabel);
	auto checkLabelStatement = make_unique<BoundLabelStatement>(checkLabel);
	auto gotoCheck = make_unique<BoundGotoStatement>(checkLabel);
	auto endLabelStatement = make_unique<BoundLabelStatement>(endLabel);

	auto condition = RewriteExpression(node->Condition());
	auto body = RewriteStatement(node->Body());
	auto gotoTrue = make_unique<BoundConditionalGotoStatement>(continueLabel, condition);

	auto statements = vector<unique_ptr<BoundStatement>>();
	statements.emplace_back(std::move(gotoCheck));
	statements.emplace_back(std::move(continueLabelStatement));
	statements.emplace_back(std::move(body));
	statements.emplace_back(std::move(checkLabelStatement));
	statements.emplace_back(std::move(gotoTrue));
	statements.emplace_back(std::move(endLabelStatement));

	auto result = make_unique<BoundBlockStatement>(statements);
	return RewriteStatement(result.get());
}

unique_ptr<BoundStatement> Lowerer::RewriteForStatement(const BoundForStatement * node)
{
	auto lowerBound = RewriteExpression(node->LowerBound());
	auto upperBound = RewriteExpression(node->UpperBound());
	auto body = RewriteStatement(node->Body());

	auto variableDeclaration = make_unique<BoundVariableDeclaration>(node->Variable(), lowerBound);
	auto variableExpression = make_unique<BoundVariableExpression>(node->Variable());
	auto upperBoundSymbol = VariableSymbol("upperBound", true, TypeSymbol::GetType(TypeEnum::Int));
	auto upperBoundDeclaration = make_unique<BoundVariableDeclaration>(upperBoundSymbol, upperBound);
	auto condition = make_unique<BoundBinaryExpression>(
		RewriteExpression(variableExpression.get()),
		BoundBinaryOperator::Bind(SyntaxKind::LessOrEqualsToken, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Int)),
		make_unique<BoundVariableExpression>(upperBoundSymbol)
		);

	auto increment = make_unique<BoundExpressionStatement>(
		make_unique<BoundAssignmentExpression>(
			node->Variable(),
			make_unique<BoundBinaryExpression>(
				RewriteExpression(variableExpression.get()),
				BoundBinaryOperator::Bind(SyntaxKind::PlusToken, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Int)),
				make_unique<BoundLiteralExpression>(1)
				)
			)
		);

	auto statements = vector<unique_ptr<BoundStatement>>();
	statements.emplace_back(std::move(body));
	statements.emplace_back(std::move(increment));
	auto whileStatement = make_unique<BoundWhileStatement>(
		RewriteExpression(condition.get()),
		make_unique<BoundBlockStatement>(statements) // whileBody
		);

	statements.clear();
	statements.emplace_back(std::move(variableDeclaration));
	statements.emplace_back(std::move(upperBoundDeclaration));
	statements.emplace_back(std::move(whileStatement));
	auto result = make_unique<BoundBlockStatement>(statements);
	return RewriteStatement(result.get());
}

}//MCF