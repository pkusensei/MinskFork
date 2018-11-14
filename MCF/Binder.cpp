#include "stdafx.h"
#include "Binder.h"

#include <stack>

#include "Diagnostic.h"
#include "SyntaxNode.h"
#include "SourceText.h"

namespace MCF {

#pragma region Unary
BoundUnaryOperator::BoundUnaryOperator(enum SyntaxKind synKind, BoundUnaryOperatorKind kind,
									   const type_index& operandType, const type_index& resultType)
	:_syntaxKind(synKind), _kind(kind), _operandType(operandType), _resultType(resultType)
{
}

BoundUnaryOperator::BoundUnaryOperator(enum SyntaxKind synKind, BoundUnaryOperatorKind kind,
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
	BoundUnaryOperator(SyntaxKind::PlusToken, BoundUnaryOperatorKind::Identity, typeid(long)),
	BoundUnaryOperator(SyntaxKind::MinusToken, BoundUnaryOperatorKind::Negation, typeid(long))
};

BoundUnaryOperator BoundUnaryOperator::Bind(enum SyntaxKind synKind, const type_index& type)
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

BoundUnaryExpression::BoundUnaryExpression(BoundUnaryExpression && other)
{
	_op.swap(other._op);
	_operand.swap(other._operand);
}
#pragma endregion

#pragma region Binary
BoundBinaryOperator::BoundBinaryOperator(enum SyntaxKind synKind, BoundBinaryOperatorKind kind,
										 const type_index& left, const type_index& right, const type_index& result)
	:_syntaxKind(synKind), _kind(kind), _leftType(left), _rightType(right), _resultType(result)
{
}

BoundBinaryOperator::BoundBinaryOperator(enum SyntaxKind synKind, BoundBinaryOperatorKind kind,
										 const type_index& operandType, const type_index& resultType)
	: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
{
}

BoundBinaryOperator::BoundBinaryOperator(enum SyntaxKind synKind, BoundBinaryOperatorKind kind, const type_index& type)
	: BoundBinaryOperator(synKind, kind, type, type, type)
{
}

BoundBinaryOperator::BoundBinaryOperator()
	: BoundBinaryOperator(SyntaxKind::BadToken, BoundBinaryOperatorKind::Addition, typeid(std::monostate))
{
	_isUseful = false;
}

BoundBinaryOperator BoundBinaryOperator::_operators[] = {
	BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition, typeid(long)),
	BoundBinaryOperator(SyntaxKind::MinusToken, BoundBinaryOperatorKind::Subtraction, typeid(long)),
	BoundBinaryOperator(SyntaxKind::StarToken, BoundBinaryOperatorKind::Multiplication, typeid(long)),
	BoundBinaryOperator(SyntaxKind::SlashToken, BoundBinaryOperatorKind::Division, typeid(long)),

	BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals, typeid(long), typeid(bool)),
	BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals, typeid(long), typeid(bool)),

	BoundBinaryOperator(SyntaxKind::AmpersandAmpersandToken, BoundBinaryOperatorKind::LogicalAnd, typeid(bool)),
	BoundBinaryOperator(SyntaxKind::PipePipeToken, BoundBinaryOperatorKind::LogicalOr, typeid(bool)),
	BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals, typeid(bool)),
	BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals, typeid(bool))
};

BoundBinaryOperator BoundBinaryOperator::Bind(enum SyntaxKind synKind, type_index leftType, type_index rightType)
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

BoundBinaryExpression::BoundBinaryExpression(BoundBinaryExpression && other)
{
	_left.swap(other._left);
	_right.swap(other._right);
	_op.swap(other._op);
}
#pragma endregion

BoundAssignmentExpression::BoundAssignmentExpression(const VariableSymbol & variable, const unique_ptr<BoundExpression>& expression)
	:_variable(variable),
	_expression(std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(expression)))
{
}

BoundAssignmentExpression::BoundAssignmentExpression(BoundAssignmentExpression && other)
	: _variable(other._variable)

{
	_expression.swap(other._expression);
	other._variable = VariableSymbol();
}

BoundLiteralExpression::BoundLiteralExpression(const ValueType & value)
	:_value(value)
{
}

BoundLiteralExpression::BoundLiteralExpression(BoundLiteralExpression && other)
	: _value(std::move(other._value))
{
}

BoundVariableExpression::BoundVariableExpression(const VariableSymbol & variable)
	: _variable(variable)
{
}

BoundVariableExpression::BoundVariableExpression(BoundVariableExpression && other)
	: _variable(other._variable)
{
	other._variable = {};
}

BoundBlockStatement::BoundBlockStatement(const vector<unique_ptr<BoundStatement>>& statements)
	: _statements(std::move(std::remove_const_t<vector<unique_ptr<BoundStatement>>&>(statements)))
{
}

BoundBlockStatement::BoundBlockStatement(BoundBlockStatement && other)
	: _statements(std::move(other._statements))
{
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
	_initializer((std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(initializer))))
{
}

BoundVariableDeclaration::BoundVariableDeclaration(BoundVariableDeclaration && other)
	: _variable(std::move(other._variable)), _initializer(std::move(other._initializer))
{
}

BoundExpressionStatement::BoundExpressionStatement(const unique_ptr<BoundExpression>& expression)
	: _expression((std::move(std::remove_const_t<unique_ptr<BoundExpression>&>(expression))))
{
}

BoundExpressionStatement::BoundExpressionStatement(BoundExpressionStatement && other)
	: _expression(std::move(other._expression))
{
}

BoundScope::BoundScope(const std::weak_ptr<BoundScope>& parent)
	: _parent(nullptr)
{
	if (!parent.expired())
		_parent = parent.lock();
}

BoundScope::BoundScope(const std::shared_ptr<BoundScope>& parent)
	:_parent(parent)
{
}

bool BoundScope::TryDeclare(const VariableSymbol & variable)
{
	if (_variables.find(variable.Name()) == _variables.end())
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
	if (_parent == nullptr)
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

BoundGlobalScope::BoundGlobalScope(const std::shared_ptr<BoundGlobalScope>& previous, const unique_ptr<DiagnosticBag>& diagnostics,
								   const vector<VariableSymbol>& variables, const unique_ptr<BoundStatement>& statement)
	:_previous(previous), 
	_diagnostics(std::move(std::remove_const_t<unique_ptr<DiagnosticBag>&>(diagnostics))),
	_variables(variables),
	_statement(std::move(std::remove_const_t<unique_ptr<BoundStatement>&>(statement)))
{
}

BoundGlobalScope::BoundGlobalScope(const std::weak_ptr<BoundGlobalScope>& previous, unique_ptr<DiagnosticBag>& diagnostics,
								   const vector<VariableSymbol>& variables, const unique_ptr<BoundStatement>& statement)
	:_previous(nullptr), 
	_diagnostics(std::move(std::remove_const_t<unique_ptr<DiagnosticBag>&>(diagnostics))),
	_variables(variables),
	_statement(std::move(std::remove_const_t<unique_ptr<BoundStatement>&>(statement)))
{
	if (!previous.expired())
		_previous = previous.lock();
}

Binder::Binder(const std::shared_ptr<BoundScope>& parent)
	:_diagnostics(std::make_unique<DiagnosticBag>()),
	_scope(std::make_shared<BoundScope>(parent))
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
		case SyntaxKind::ExpressionStatement:
			return BindExpressionStatement(syntax);
		default:
			throw std::invalid_argument("Unexpected syntax.");
	}
}

unique_ptr<BoundStatement> Binder::BindBlockStatement(const StatementSyntax * syntax)
{
	auto p = dynamic_cast<const BlockStatementSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto statements = vector<unique_ptr<BoundStatement>>();
	//auto tmp = std::make_shared<BoundScope>(_scope);
	_scope = std::make_shared<BoundScope>(_scope);
	for (const auto& it : p->Statements())
		statements.emplace_back(BindStatement(it));
	_scope = _scope->Parent().lock();
	return std::make_unique<BoundBlockStatement>(statements);
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

unique_ptr<BoundStatement> Binder::BindExpressionStatement(const StatementSyntax * syntax)
{
	auto p = dynamic_cast<const ExpressionStatementSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto expression = BindExpression(p->Expression());
	return std::make_unique<BoundExpressionStatement>(expression);
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
			throw std::invalid_argument("Invalid expression; binding failed.");
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
	VariableSymbol tmp;
	VariableSymbol& variable = tmp;
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
	VariableSymbol tmp;
	VariableSymbol& variable = tmp;

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

std::shared_ptr<BoundScope> Binder::CreateParentScope(const std::shared_ptr<BoundGlobalScope>& previous)
{
	auto stack = std::stack<std::weak_ptr<BoundGlobalScope>>();
	auto current = std::remove_const_t<std::shared_ptr<BoundGlobalScope>&>(previous);
	while (current != nullptr)
	{
		stack.emplace(current);
		current = current->Previous().lock();
	}
	std::shared_ptr<BoundScope> parent{nullptr};
	while (!stack.empty())
	{
		current = stack.top().lock();
		auto scope = std::make_shared<BoundScope>(parent);
		for (const auto& it : previous->Variables())
			scope->TryDeclare(it);
		parent.swap(scope);
		stack.pop();
	}
	return parent;
}


std::shared_ptr<BoundGlobalScope> Binder::BindGlobalScope(const std::weak_ptr<BoundGlobalScope>& previous, const CompilationUnitSyntax * syntax)
{
	std::shared_ptr<BoundScope> parentScope{nullptr};
	if (previous.expired())
		parentScope = CreateParentScope(nullptr);
	else
		parentScope = CreateParentScope(previous.lock());
	Binder binder(parentScope);
	auto expression = binder.BindStatement(syntax->Statement());
	auto variables = binder._scope->GetDeclaredVariables();
	auto diagnostics = binder.Diagnostics();
	if (!previous.expired())
		diagnostics->AddRangeFront(*previous.lock()->Diagnostics());
	return std::make_shared<BoundGlobalScope>(previous, binder._diagnostics, variables, expression);
}



}//MCF