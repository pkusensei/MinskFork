#include "stdafx.h"
#include "Binder.h"

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
	return BoundUnaryOperator(); //HACK
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

}//MCF