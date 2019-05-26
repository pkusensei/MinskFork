#include "stdafx.h"
#include "BoundExpressions.h"

#include "ReflectionHelper.h"
#include "SyntaxKind.h"

namespace MCF {

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

BoundUnaryOperator::BoundUnaryOperator(const enum SyntaxKind& synKind,
									   const BoundUnaryOperatorKind& kind,
									   const TypeSymbol& operandType,
									   const TypeSymbol& resultType)
	:_syntaxKind(synKind), _kind(kind),
	_operandType(operandType), _resultType(resultType)
{
}

BoundUnaryOperator::BoundUnaryOperator(const enum SyntaxKind& synKind,
									   const BoundUnaryOperatorKind& kind,
									   const TypeSymbol& operandType)
	: BoundUnaryOperator(synKind, kind, operandType, operandType)
{
}

BoundUnaryOperator::BoundUnaryOperator()
	: BoundUnaryOperator(SyntaxKind::BadToken, BoundUnaryOperatorKind::Identity,
						 GetTypeSymbol(TypeEnum::Error))
{
	_isUseful = false;
}

const vector<BoundUnaryOperator>& BoundUnaryOperator::Operators()
{
	static const auto operators = vector<BoundUnaryOperator>{
	BoundUnaryOperator(SyntaxKind::BangToken, BoundUnaryOperatorKind::LogicalNegation,
						GetTypeSymbol(TypeEnum::Bool)),
	BoundUnaryOperator(SyntaxKind::PlusToken, BoundUnaryOperatorKind::Identity,
						GetTypeSymbol(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::MinusToken, BoundUnaryOperatorKind::Negation,
						GetTypeSymbol(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::TildeToken, BoundUnaryOperatorKind::OnesComplement,
						GetTypeSymbol(TypeEnum::Int))
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

BoundUnaryExpression::BoundUnaryExpression(const BoundUnaryOperator & op,
										   const shared_ptr<BoundExpression>& operand)
	:_op(op), _operand(operand)
{
}

BoundBinaryOperator::BoundBinaryOperator(const enum SyntaxKind& synKind,
										 const BoundBinaryOperatorKind& kind,
										 const TypeSymbol& left, const TypeSymbol& right,
										 const TypeSymbol& result)
	: _syntaxKind(synKind), _kind(kind),
	_leftType(left), _rightType(right), _resultType(result)
{
}

BoundBinaryOperator::BoundBinaryOperator(const enum SyntaxKind& synKind,
										 const BoundBinaryOperatorKind& kind,
										 const TypeSymbol& operandType,
										 const TypeSymbol& resultType)
	: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
{
}

BoundBinaryOperator::BoundBinaryOperator(const enum SyntaxKind& synKind,
										 const BoundBinaryOperatorKind& kind,
										 const TypeSymbol& type)
	: BoundBinaryOperator(synKind, kind, type, type, type)
{
}

BoundBinaryOperator::BoundBinaryOperator()
	: BoundBinaryOperator(SyntaxKind::BadToken, BoundBinaryOperatorKind::Addition,
						  GetTypeSymbol(TypeEnum::Error))
{
	_isUseful = false;
}

const vector<BoundBinaryOperator>& BoundBinaryOperator::Operators()
{
	static const auto operators = vector<BoundBinaryOperator>{
		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition,
							GetTypeSymbol(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::MinusToken, BoundBinaryOperatorKind::Subtraction,
							GetTypeSymbol(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::StarToken, BoundBinaryOperatorKind::Multiplication,
							GetTypeSymbol(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::SlashToken, BoundBinaryOperatorKind::Division,
							GetTypeSymbol(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::PercentToken, BoundBinaryOperatorKind::Modulus,
							GetTypeSymbol(TypeEnum::Int)),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd,
							GetTypeSymbol(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr,
							GetTypeSymbol(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor,
							GetTypeSymbol(TypeEnum::Int)),

		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							GetTypeSymbol(TypeEnum::Int), GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							GetTypeSymbol(TypeEnum::Int), GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::LessToken, BoundBinaryOperatorKind::Less,
							GetTypeSymbol(TypeEnum::Int), GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::LessOrEqualsToken, BoundBinaryOperatorKind::LessOrEquals,
							GetTypeSymbol(TypeEnum::Int), GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::GreaterToken, BoundBinaryOperatorKind::Greater,
							GetTypeSymbol(TypeEnum::Int), GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::GreaterOrEqualsToken, BoundBinaryOperatorKind::GreaterOrEquals,
							GetTypeSymbol(TypeEnum::Int), GetTypeSymbol(TypeEnum::Bool)),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd,
							GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::AmpersandAmpersandToken, BoundBinaryOperatorKind::LogicalAnd,
							GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr,
							GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::PipePipeToken, BoundBinaryOperatorKind::LogicalOr,
							GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor,
							GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							GetTypeSymbol(TypeEnum::Bool)),

		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition,
							GetTypeSymbol(TypeEnum::String)),
		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							GetTypeSymbol(TypeEnum::String), GetTypeSymbol(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							GetTypeSymbol(TypeEnum::String), GetTypeSymbol(TypeEnum::Bool)),
	};

	return operators;
}

BoundBinaryOperator BoundBinaryOperator::Bind(const enum SyntaxKind& synKind,
											  const TypeSymbol& leftType,
											  const TypeSymbol& rightType)
{
	for (const auto& op : Operators())
	{
		if (op.SyntaxKind() == synKind
			&& op.LeftType() == leftType
			&& op.RightType() == rightType)
			return op;
	}
	return BoundBinaryOperator();
}

BoundBinaryExpression::BoundBinaryExpression(const shared_ptr<BoundExpression>& left,
											 const BoundBinaryOperator & op,
											 const shared_ptr<BoundExpression>& right)
	:_left(left), _right(right), _op(op)
{
}

BoundAssignmentExpression::BoundAssignmentExpression(const shared_ptr<VariableSymbol>& variable,
													 const shared_ptr<BoundExpression>& expression)
	:_variable(variable), _expression(expression)
{
}

BoundLiteralExpression::BoundLiteralExpression(const ValueType & value)
	: _value(value)
{
}

BoundVariableExpression::BoundVariableExpression(const shared_ptr<VariableSymbol>& variable)
	: _variable(variable)
{
}

BoundCallExpression::BoundCallExpression(const shared_ptr<FunctionSymbol>& function,
										 const vector<shared_ptr<BoundExpression>>& arguments)
	:_function(function), _arguments(arguments)
{
}

BoundConversionExpression::BoundConversionExpression(const TypeSymbol& type,
													 const shared_ptr<BoundExpression>& expression)
	:_type(type), _expression(expression)
{
}

BoundPostfixExpression::BoundPostfixExpression(const shared_ptr<VariableSymbol>& variable,
											   const BoundPostfixOperatorEnum& kind,
											   const shared_ptr<BoundExpression>& expression)
	:_variable(variable), _kind(kind), _expression(expression)
{
}

}//MCF
