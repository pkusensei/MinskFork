#include "stdafx.h"
#include "Binder.h"

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
	for(const auto& op:_operators)
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
	:_value(std::move(other._value))
{
}

BoundVariableExpression::BoundVariableExpression(const VariableSymbol & variable)
	:_variable(variable)
{
}

BoundVariableExpression::BoundVariableExpression(BoundVariableExpression && other)
	:_variable(other._variable)
{
	other._variable = {};
}

Binder::Binder(std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables)
	:_diagnostics(std::make_unique<DiagnosticBag>()), _variables(&variables)
{
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
			throw std::exception(); // TODO
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
	for (const auto& it : *_variables)	
		if (it.first.Name() == name)
			return std::make_unique<BoundVariableExpression>(it.first);
	_diagnostics->ReportUndefinedName(p->IdentifierToken().Span(), name);
	return std::make_unique<BoundLiteralExpression>(static_cast<long>(0));
}

unique_ptr<BoundExpression> Binder::BindAssignmentExpression(const ExpressionSyntax * syntax)
{
	auto p = dynamic_cast<const AssignmentExpressionSyntax*>(syntax);
	if (p == nullptr) return nullptr;

	auto name = p->IdentifierToken().Text();
	auto boundExpression = BindExpression(p->Expression());

	VariableSymbol existingVariable;
	bool found{false};
	for (const auto& it : *_variables)
		if (it.first.Name() == name)
		{
			existingVariable = it.first;
			found = true;
		}
	if (found)
		_variables->erase(existingVariable);

	VariableSymbol variable(name, boundExpression->Type());
	_variables->emplace(variable, ValueType());
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

}//MCF