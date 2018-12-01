#include "stdafx.h"
#include "Binding.h"

#include <sstream>
#include <stack>

#include "Diagnostic.h"
#include "Syntax.h"
#include "SourceText.h"

namespace MCF {

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

		case BoundNodeKind::VoidExpression:
			return "VoidExpression";
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
	string marker = isLast ? "+--" : "---";//"©¸©¤©¤" : "©À©¤©¤";
	out << indent << marker << GetText(node);

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
				out << ",";
			}
			out << ' ' << p.first << " = " << p.second;
		}
	}
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

const vector<std::pair<string, string>> BoundExpression::GetProperties() const
{
	return vector<std::pair<string, string>>();
}

BoundUnaryOperator::BoundUnaryOperator(const enum SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
									   const type_index& operandType, const type_index& resultType)
	:_syntaxKind(synKind), _kind(kind), _operandType(operandType), _resultType(resultType)
{
}

BoundUnaryOperator::BoundUnaryOperator(const enum SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
									   const type_index& operandType)
	: BoundUnaryOperator(synKind, kind, operandType, operandType)
{
}

BoundUnaryOperator::BoundUnaryOperator()
	: BoundUnaryOperator(SyntaxKind::BadToken, BoundUnaryOperatorKind::Identity, typeid(std::monostate))
{
	_isUseful = false;
}

BoundUnaryOperator BoundUnaryOperator::_operators[] = {
	BoundUnaryOperator(SyntaxKind::BangToken, BoundUnaryOperatorKind::LogicalNegation, typeid(bool)),
	BoundUnaryOperator(SyntaxKind::PlusToken, BoundUnaryOperatorKind::Identity, typeid(IntegerType)),
	BoundUnaryOperator(SyntaxKind::MinusToken, BoundUnaryOperatorKind::Negation, typeid(IntegerType)),
	BoundUnaryOperator(SyntaxKind::TildeToken, BoundUnaryOperatorKind::OnesComplement, typeid(IntegerType))
};

BoundUnaryOperator BoundUnaryOperator::Bind(const enum SyntaxKind& synKind, const type_index& type)
{
	for (const auto& op : _operators)
	{
		if (op.SyntaxKind() == synKind && op.OperandType() == type)
			return op;
	}
	return BoundUnaryOperator();
}

BoundUnaryExpression::BoundUnaryExpression(const BoundUnaryOperator & op, const unique_ptr<BoundExpression>& operand)
	:_op(std::make_unique<BoundUnaryOperator>(op)),
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
		std::pair<string, string>("Type", ValueType::GetTypeName(Type()))
	};
}

BoundBinaryOperator::BoundBinaryOperator(const enum SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
										 const type_index& left, const type_index& right, const type_index& result)
	: _syntaxKind(synKind), _kind(kind), _leftType(left), _rightType(right), _resultType(result)
{
}

BoundBinaryOperator::BoundBinaryOperator(const enum SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
										 const type_index& operandType, const type_index& resultType)
	: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
{
}

BoundBinaryOperator::BoundBinaryOperator(const enum SyntaxKind& synKind, const BoundBinaryOperatorKind& kind, const type_index& type)
	: BoundBinaryOperator(synKind, kind, type, type, type)
{
}

BoundBinaryOperator::BoundBinaryOperator()
	: BoundBinaryOperator(SyntaxKind::BadToken, BoundBinaryOperatorKind::Addition, typeid(std::monostate))
{
	_isUseful = false;
}

BoundBinaryOperator BoundBinaryOperator::_operators[] = {
	BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition, typeid(IntegerType)),
	BoundBinaryOperator(SyntaxKind::MinusToken, BoundBinaryOperatorKind::Subtraction, typeid(IntegerType)),
	BoundBinaryOperator(SyntaxKind::StarToken, BoundBinaryOperatorKind::Multiplication, typeid(IntegerType)),
	BoundBinaryOperator(SyntaxKind::SlashToken, BoundBinaryOperatorKind::Division, typeid(IntegerType)),

	BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd, typeid(IntegerType)),
	BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr, typeid(IntegerType)),
	BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor, typeid(IntegerType)),

	BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals, typeid(IntegerType), typeid(bool)),
	BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals, typeid(IntegerType), typeid(bool)),
	BoundBinaryOperator(SyntaxKind::LessToken, BoundBinaryOperatorKind::Less, typeid(IntegerType), typeid(bool)),
	BoundBinaryOperator(SyntaxKind::LessOrEqualsToken, BoundBinaryOperatorKind::LessOrEquals, typeid(IntegerType), typeid(bool)),
	BoundBinaryOperator(SyntaxKind::GreaterToken, BoundBinaryOperatorKind::Greater, typeid(IntegerType), typeid(bool)),
	BoundBinaryOperator(SyntaxKind::GreaterOrEqualsToken, BoundBinaryOperatorKind::GreaterOrEquals, typeid(IntegerType), typeid(bool)),

	BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd, typeid(bool)),
	BoundBinaryOperator(SyntaxKind::AmpersandAmpersandToken, BoundBinaryOperatorKind::LogicalAnd, typeid(bool)),
	BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr, typeid(bool)),
	BoundBinaryOperator(SyntaxKind::PipePipeToken, BoundBinaryOperatorKind::LogicalOr, typeid(bool)),
	BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor, typeid(bool)),
	BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals, typeid(bool)),
	BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals, typeid(bool))
};

BoundBinaryOperator BoundBinaryOperator::Bind(const enum SyntaxKind& synKind, const type_index& leftType, const type_index& rightType)
{
	for (const auto& op : _operators)
	{
		if (op.SyntaxKind() == synKind && op.LeftType() == leftType && op.RightType() == rightType)
			return op;
	}
	return BoundBinaryOperator();
}

BoundBinaryExpression::BoundBinaryExpression(const unique_ptr<BoundExpression>& left, const BoundBinaryOperator & op, const unique_ptr<BoundExpression>& right)
	:_left(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(left))),
	_right(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(right))),
	_op(std::make_unique<BoundBinaryOperator>(op))
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
		std::pair<string, string>("Type", ValueType::GetTypeName(Type()))
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
			std::pair<string, string>("Type", ValueType::GetTypeName(Type()))
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
			std::pair<string, string>("Type", ValueType::GetTypeName(Type()))
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
			std::pair<string, string>("Type", ValueType::GetTypeName(Type()))
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

BoundLabelStatement::BoundLabelStatement(const LabelSymbol & label)
	:_label(label)
{
}

const vector<std::pair<string, string>> BoundLabelStatement::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Label", Label().ToString())
	};
}

BoundGotoStatement::BoundGotoStatement(const LabelSymbol & label)
	:_label(label)
{
}

const vector<std::pair<string, string>> BoundGotoStatement::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Label", Label().ToString())
	};
}

BoundConditionalGotoStatement::BoundConditionalGotoStatement(const LabelSymbol & label, const unique_ptr<BoundExpression>& condition, bool jumpIfFalse)
	:_label(label),
	_condition((std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(condition)))),
	_jumpIfFalse(jumpIfFalse)
{
}

const vector<std::pair<string, string>> BoundConditionalGotoStatement::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Label", Label().ToString()),
			std::pair<string, string>("JumpIfFalse", std::to_string(JumpIfFalse()))
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
	: _diagnostics(std::make_unique<DiagnosticBag>()),
	_scope(std::make_unique<BoundScope>(parent))
{
}

unique_ptr<BoundStatement> Binder::BindStatement(const StatementSyntax * syntax)
{
	switch (syntax->Kind())
	{
		case SyntaxKind::BlockStatement:
			return BindBlockStatement(syntax);
		case SyntaxKind::VariableDeclaration:
			return BindVariableDeclaration(syntax);
		case SyntaxKind::IfStatement:
			return BindIfStatement(syntax);
		case SyntaxKind::WhileStatement:
			return BindWhileStatement(syntax);
		case SyntaxKind::ForStatement:
			return BindForStatement(syntax);
		case SyntaxKind::ExpressionStatement:
			return BindExpressionStatement(syntax);
		default:
			throw std::invalid_argument("Unexpected syntax " + GetSyntaxKindName(syntax->Kind()));
	}
}

unique_ptr<BoundStatement> Binder::BindBlockStatement(const StatementSyntax * syntax)
{
	auto p = dynamic_cast<const BlockStatementSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto result = vector<unique_ptr<BoundStatement>>();
	_scope = std::make_unique<BoundScope>(_scope);
	auto statements = p->Statements();
	for (const auto& it : statements)
		result.emplace_back(BindStatement(it));
	BoundScope::ResetToParent(_scope);
	return std::make_unique<BoundBlockStatement>(result);
}

unique_ptr<BoundStatement> Binder::BindVariableDeclaration(const StatementSyntax * syntax)
{
	auto p = dynamic_cast<const VariableDeclarationSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto name = p->Identifier().Text();
	auto readOnly = p->Keyword().Kind() == SyntaxKind::LetKeyword;
	auto init = BindExpression(p->Initializer());
	auto variable = VariableSymbol(name, readOnly, init->Type());

	if (!_scope->TryDeclare(variable))
		_diagnostics->ReportVariableAlreadyDeclared(p->Identifier().Span(), name);

	return std::make_unique<BoundVariableDeclaration>(variable, init);
}

unique_ptr<BoundStatement> Binder::BindIfStatement(const StatementSyntax * syntax)
{
	auto p = dynamic_cast<const IfStatementSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto condition = BindExpression(p->Condition(), typeid(bool));
	auto thenStatement = BindStatement(p->ThenStatement());
	auto elseStatement = p->ElseClause() == nullptr ? nullptr
		: BindStatement(p->ElseClause()->ElseStatement());
	return std::make_unique<BoundIfStatement>(condition, thenStatement, elseStatement);
}

unique_ptr<BoundStatement> Binder::BindWhileStatement(const StatementSyntax * syntax)
{
	auto p = dynamic_cast<const WhileStatementSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto condition = BindExpression(p->Condition(), typeid(bool));
	auto body = BindStatement(p->Body());
	return std::make_unique<BoundWhileStatement>(condition, body);
}

unique_ptr<BoundStatement> Binder::BindForStatement(const StatementSyntax * syntax)
{
	auto p = dynamic_cast<const ForStatementSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto lowerBound = BindExpression(p->LowerBound(), typeid(IntegerType));
	auto upperBound = BindExpression(p->UpperBound(), typeid(IntegerType));

	_scope = std::make_unique<BoundScope>(_scope);

	auto name = p->Identifier().Text();
	VariableSymbol variable(name, true, typeid(IntegerType));
	if (!_scope->TryDeclare(variable))
		_diagnostics->ReportVariableAlreadyDeclared(p->Identifier().Span(), name);

	auto body = BindStatement(p->Body());
	BoundScope::ResetToParent(_scope);
	return std::make_unique<BoundForStatement>(variable, lowerBound, upperBound, body);
}

unique_ptr<BoundStatement> Binder::BindExpressionStatement(const StatementSyntax * syntax)
{
	auto p = dynamic_cast<const ExpressionStatementSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto expression = BindExpression(p->Expression());
	return std::make_unique<BoundExpressionStatement>(expression);
}

unique_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax * syntax, const type_index & targetType)
{
	auto result = BindExpression(syntax);
	if (result->Type() != targetType)
		_diagnostics->ReportCannotConvert(syntax->Span(), result->Type(), targetType);
	return result;
}

unique_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax * syntax)
{
	switch (syntax->Kind())
	{
		case SyntaxKind::ParenthesizedExpression:
			return BindParenthesizedExpression(syntax);
		case SyntaxKind::LiteralExpression:
			return BindLiteralExpression(syntax);
		case SyntaxKind::NameExpression:
			return BindNameExpression(syntax);
		case SyntaxKind::AssignmentExpression:
			return BindAssignmentExpression(syntax);
		case SyntaxKind::UnaryExpression:
			return BindUnaryExpression(syntax);
		case SyntaxKind::BinaryExpression:
			return BindBinaryExpression(syntax);
		default:
			throw std::invalid_argument("Invalid expression " + GetSyntaxKindName(syntax->Kind()));
	}
}

unique_ptr<BoundExpression> Binder::BindParenthesizedExpression(const ExpressionSyntax * syntax)
{
	auto p = dynamic_cast<const ParenthesizedExpressionSyntax*>(syntax);
	return p != nullptr ? BindExpression(p->Expression()) : nullptr;
}

unique_ptr<BoundExpression> Binder::BindLiteralExpression(const ExpressionSyntax * syntax)
{
	auto p = dynamic_cast<const LiteralExpressionSyntax*>(syntax);
	return p != nullptr ? std::make_unique<BoundLiteralExpression>(p->Value()) : nullptr;
}

unique_ptr<BoundExpression> Binder::BindNameExpression(const ExpressionSyntax * syntax)
{
	auto p = dynamic_cast<const NameExpressionSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto name = p->IdentifierToken().Text();
	if (name.empty()) // NOTE this token was injected by Parser::MatchToken
		return std::make_unique<BoundLiteralExpression>(0);

	VariableSymbol variable;
	if (!_scope->TryLookup(name, variable))
	{
		_diagnostics->ReportUndefinedName(p->IdentifierToken().Span(), name);
		return std::make_unique<BoundLiteralExpression>(0);
	}
	return std::make_unique<BoundVariableExpression>(variable);
}

unique_ptr<BoundExpression> Binder::BindAssignmentExpression(const ExpressionSyntax * syntax)
{
	auto p = dynamic_cast<const AssignmentExpressionSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto name = p->IdentifierToken().Text();
	auto boundExpression = BindExpression(p->Expression());

	VariableSymbol variable;

	if (!_scope->TryLookup(name, variable))
	{
		_diagnostics->ReportUndefinedName(p->IdentifierToken().Span(), name);
		return boundExpression;
	}
	if (variable.IsReadOnly())
		_diagnostics->ReportCannotAssign(p->EqualsToken().Span(), name);
	if (boundExpression->Type() != variable.Type())
	{
		_diagnostics->ReportCannotConvert(p->Expression()->Span(), boundExpression->Type(), variable.Type());
		return boundExpression;
	}
	return std::make_unique<BoundAssignmentExpression>(variable, boundExpression);
}

unique_ptr<BoundExpression> Binder::BindUnaryExpression(const ExpressionSyntax * syntax)
{
	auto p = dynamic_cast<const UnaryExpressionSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto boundOperand = BindExpression(p->Operand());
	auto boundOperator = BoundUnaryOperator::Bind(p->OperatorToken().Kind(), boundOperand->Type());
	if (boundOperator.IsUseful())
	{
		return std::make_unique<BoundUnaryExpression>(boundOperator, boundOperand);
	} else
	{
		_diagnostics->ReportUndefinedUnaryOperator(p->OperatorToken().Span(), p->OperatorToken().Text(), boundOperand->Type());
		return boundOperand;
	}
}

unique_ptr<BoundExpression> Binder::BindBinaryExpression(const ExpressionSyntax * syntax)
{
	auto p = dynamic_cast<const BinaryExpressionSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto boundLeft = BindExpression(p->Left());
	auto boundRight = BindExpression(p->Right());
	auto boundOperator = BoundBinaryOperator::Bind(p->OperatorToken().Kind(), boundLeft->Type(), boundRight->Type());
	if (boundOperator.IsUseful())
	{
		return std::make_unique<BoundBinaryExpression>(boundLeft, boundOperator, boundRight);
	} else
	{
		_diagnostics->ReportUndefinedBinaryOperator(p->OperatorToken().Span(), p->OperatorToken().Text(),
													boundLeft->Type(), boundRight->Type());
		return boundLeft;
	}
}

unique_ptr<BoundScope>  Binder::CreateParentScope(const BoundGlobalScope* previous)
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
		auto scope = std::make_unique<BoundScope>(parent);
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
	return std::make_unique<BoundGlobalScope>(previous, binder._diagnostics, variables, expression);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteStatement(const BoundStatement * node) const
{
	switch (node->Kind())
	{
		case BoundNodeKind::BlockStatement:
			return RewriteBlockStatement(node);
		case BoundNodeKind::VariableDeclaration:
			return RewriteVariableDeclaration(node);
		case BoundNodeKind::IfStatement:
			return RewriteIfStatement(node);
		case BoundNodeKind::WhileStatement:
			return RewriteWhileStatement(node);
		case BoundNodeKind::ForStatement:
			return RewriteForStatement(node);
		case BoundNodeKind::LabelStatement:
			return RewriteLabelStatement(node);
		case BoundNodeKind::GotoStatement:
			return RewriteGotoStatement(node);
		case BoundNodeKind::ConditionalGotoStatement:
			return RewriteConditionalGotoStatement(node);
		case BoundNodeKind::ExpressionStatement:
			return RewriteExpressionStatement(node);
		default:
			throw std::invalid_argument("Unexpected node: " + GetEnumText(node->Kind()));
	}
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteBlockStatement(const BoundStatement * node) const
{
	auto p = dynamic_cast<const BoundBlockStatement*>(node);
	if (p == nullptr)return nullptr;

	auto result = vector<unique_ptr<BoundStatement>>();
	auto statements = p->Statements();
	for (const auto& it : statements)
		result.emplace_back(RewriteStatement(it));
	return std::make_unique<BoundBlockStatement>(result);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteVariableDeclaration(const BoundStatement * node) const
{
	auto p = dynamic_cast<const BoundVariableDeclaration*>(node);
	if (p == nullptr)return nullptr;

	auto initializer = RewriteExpression(p->Initializer());
	return std::make_unique<BoundVariableDeclaration>(p->Variable(), initializer);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteIfStatement(const BoundStatement * node) const
{
	auto p = dynamic_cast<const BoundIfStatement*>(node);
	if (p == nullptr)return nullptr;

	auto condition = RewriteExpression(p->Condition());
	auto thenStatement = RewriteStatement(p->ThenStatement());
	auto elseStatement = p->ElseStatement() == nullptr ? nullptr : RewriteStatement(p->ElseStatement());
	return std::make_unique<BoundIfStatement>(condition, thenStatement, elseStatement);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteWhileStatement(const BoundStatement * node) const
{
	auto p = dynamic_cast<const BoundWhileStatement*>(node);
	if (p == nullptr)return nullptr;

	auto condition = RewriteExpression(p->Condition());
	auto body = RewriteStatement(p->Body());
	return std::make_unique<BoundWhileStatement>(condition, body);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteForStatement(const BoundStatement * node) const
{
	auto p = dynamic_cast<const BoundForStatement*>(node);
	if (p == nullptr)return nullptr;

	auto lowerBound = RewriteExpression(p->LowerBound());
	auto upperBound = RewriteExpression(p->UpperBound());
	auto body = RewriteStatement(p->Body());
	return std::make_unique<BoundForStatement>(p->Variable(), lowerBound, upperBound, body);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteLabelStatement(const BoundStatement * node) const
{
	auto p = dynamic_cast<const BoundLabelStatement*>(node);
	if (p == nullptr)return nullptr;

	return std::make_unique<BoundLabelStatement>(p->Label());
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteGotoStatement(const BoundStatement * node) const
{
	auto p = dynamic_cast<const BoundGotoStatement*>(node);
	if (p == nullptr)return nullptr;

	return std::make_unique<BoundGotoStatement>(p->Label());
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteConditionalGotoStatement(const BoundStatement * node) const
{
	auto p = dynamic_cast<const BoundConditionalGotoStatement*>(node);
	if (p == nullptr)return nullptr;

	auto condition = RewriteExpression(p->Condition());
	return std::make_unique<BoundConditionalGotoStatement>(p->Label(), condition, p->JumpIfFalse());
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteExpressionStatement(const BoundStatement * node) const
{
	auto p = dynamic_cast<const BoundExpressionStatement*>(node);
	if (p == nullptr)return nullptr;

	auto expression = RewriteExpression(p->Expression());
	return std::make_unique<BoundExpressionStatement>(expression);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteExpression(const BoundExpression * node) const
{
	switch (node->Kind())
	{
		case BoundNodeKind::LiteralExpression:
			return RewriteLiteralExpression(node);
		case BoundNodeKind::VariableExpression:
			return RewriteVariableExpression(node);
		case BoundNodeKind::AssignmentExpression:
			return RewriteAssignmentExpression(node);
		case BoundNodeKind::UnaryExpression:
			return RewriteUnaryExpression(node);
		case BoundNodeKind::BinaryExpression:
			return RewriteBinaryExpression(node);
		default:
			throw std::invalid_argument("Unexpected node: " + GetEnumText(node->Kind()));
	}
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteLiteralExpression(const BoundExpression * node) const
{
	auto p = dynamic_cast<const BoundLiteralExpression*>(node);
	if (p == nullptr)return nullptr;

	return std::make_unique<BoundLiteralExpression>(p->Value());
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteVariableExpression(const BoundExpression * node) const
{
	auto p = dynamic_cast<const BoundVariableExpression*>(node);
	if (p == nullptr)return nullptr;

	return std::make_unique<BoundVariableExpression>(p->Variable());
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteAssignmentExpression(const BoundExpression * node) const
{
	auto p = dynamic_cast<const BoundAssignmentExpression*>(node);
	if (p == nullptr)return nullptr;

	auto expression = RewriteExpression(p->Expression());
	return std::make_unique<BoundAssignmentExpression>(p->Variable(), expression);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteUnaryExpression(const BoundExpression * node) const
{
	auto p = dynamic_cast<const BoundUnaryExpression*>(node);
	if (p == nullptr)return nullptr;

	auto operand = RewriteExpression(p->Operand());
	return std::make_unique<BoundUnaryExpression>(*(p->Op()), operand);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteBinaryExpression(const BoundExpression * node) const
{
	auto p = dynamic_cast<const BoundBinaryExpression*>(node);
	if (p == nullptr)return nullptr;

	auto left = RewriteExpression(p->Left());
	auto right = RewriteExpression(p->Right());
	return std::make_unique<BoundBinaryExpression>(left, *(p->Op()), right);
}

}//MCF