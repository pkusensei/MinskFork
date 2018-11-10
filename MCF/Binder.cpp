#include "stdafx.h"
#include "Binder.h"

#include <stack>

#include "Diagnostic.h"
#include "SyntaxNode.h"
#include "SourceText.h"

namespace MCF {

#pragma region Unary
BoundUnaryOperator::BoundUnaryOperator(enum SyntaxKind synKind, BoundUnaryOperatorKind kind,
									   type_index operandType, type_index resultType)
	:_syntaxKind(synKind), _kind(kind), _operandType(operandType), _resultType(resultType)
{
}

BoundUnaryOperator::BoundUnaryOperator(enum SyntaxKind synKind, BoundUnaryOperatorKind kind,
									   type_index operandType)
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

BoundUnaryOperator BoundUnaryOperator::Bind(enum SyntaxKind synKind, type_index type)
{
	for (const auto& op : _operators)
	{
		if (op.SyntaxKind() == synKind && op.OperandType() == type)
			return op;
	}
	return BoundUnaryOperator();
}

BoundUnaryExpression::BoundUnaryExpression(const BoundUnaryOperator & op, unique_ptr<BoundExpression>& operand)
	:_op(std::make_unique<BoundUnaryOperator>(op))
{
	_operand.swap(operand);
}

BoundUnaryExpression::BoundUnaryExpression(BoundUnaryExpression && other)
{
	_op.swap(other._op);
	_operand.swap(other._operand);
}
#pragma endregion

#pragma region Binary
BoundBinaryOperator::BoundBinaryOperator(enum SyntaxKind synKind, BoundBinaryOperatorKind kind,
										 type_index left, type_index right, type_index result)
	:_syntaxKind(synKind), _kind(kind), _leftType(left), _rightType(right), _resultType(result)
{
}

BoundBinaryOperator::BoundBinaryOperator(enum SyntaxKind synKind, BoundBinaryOperatorKind kind,
										 type_index operandType, type_index resultType)
	: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
{
}

BoundBinaryOperator::BoundBinaryOperator(enum SyntaxKind synKind, BoundBinaryOperatorKind kind, type_index type)
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

BoundBinaryExpression::BoundBinaryExpression(unique_ptr<BoundExpression>& left, const BoundBinaryOperator & op, unique_ptr<BoundExpression>& right)
	:_op(std::make_unique<BoundBinaryOperator>(op))
{
	_left.swap(left);
	_right.swap(right);
}

BoundBinaryExpression::BoundBinaryExpression(BoundBinaryExpression && other)
{
	_left.swap(other._left);
	_right.swap(other._right);
	_op.swap(other._op);
}
#pragma endregion

BoundAssignmentExpression::BoundAssignmentExpression(const VariableSymbol & variable, unique_ptr<BoundExpression>& expression)
	:_variable(variable)
{
	_expression.swap(expression);
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

BoundBlockStatement::BoundBlockStatement(vector<unique_ptr<BoundStatement>>& statements)
	: _statements(std::move(statements))
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


BoundVariableDeclaration::BoundVariableDeclaration(const VariableSymbol & variable, unique_ptr<BoundExpression>& initializer)
	:_variable(variable), _initializer(std::move(initializer))
{
}

BoundVariableDeclaration::BoundVariableDeclaration(BoundVariableDeclaration && other)
	: _variable(std::move(other._variable)), _initializer(std::move(other._initializer))
{
}

BoundExpressionStatement::BoundExpressionStatement(unique_ptr<BoundExpression>& expression)
	: _expression(std::move(expression))
{
}

BoundExpressionStatement::BoundExpressionStatement(BoundExpressionStatement && other)
	: _expression(std::move(other._expression))
{
}

BoundScope::BoundScope(const BoundScope* parent)
	: _variables({}), _parent(parent)
{
}

BoundScope::BoundScope(const unique_ptr<BoundScope>& parent)
	:BoundScope(parent.get())
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


BoundGlobalScope::BoundGlobalScope(const BoundGlobalScope* previous, unique_ptr<DiagnosticBag>& diagnostics,
								   const vector<VariableSymbol>& variables, unique_ptr<BoundStatement>& statement)
	:_previous(previous), _diagnostics(std::move(diagnostics)),
	_variables(variables), _statement(std::move(statement))
{
}

Binder::Binder(unique_ptr<BoundScope>& parent)
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
	auto tmp = std::make_unique<BoundScope>(_scope);
	_scope.swap(tmp);
	for (const auto& it : p->Statements())
			statements.emplace_back(BindStatement(it));
	//_scope.reset(std::remove_cv_t<BoundScope*>(_scope->Parent()));
	_scope.swap(tmp);
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
		return std::make_unique<BoundLiteralExpression>(static_cast<long>(0));
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

unique_ptr<BoundScope> Binder::CreateParentScope(const BoundGlobalScope * previous)
{
	auto stack = std::stack<const BoundGlobalScope*>();
	while (previous != nullptr)
	{
		stack.emplace(previous);
		previous = previous->Previous();
	}
	unique_ptr<BoundScope> parent{nullptr};
	//auto parent = std::make_unique<BoundScope>();
	while (!stack.empty())
	{
		previous = stack.top();
		auto scope = std::make_unique<BoundScope>(parent);
		for (const auto& it : previous->Variables())
			scope->TryDeclare(it);
		parent.swap(scope);
		stack.pop();
	}
	return parent;
}


unique_ptr<BoundGlobalScope> Binder::BindGlobalScope(const BoundGlobalScope * previous, const CompilationUnitSyntax * syntax)
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

}//MCF