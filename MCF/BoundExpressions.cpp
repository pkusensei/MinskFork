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
						 TypeSymbol::GetType(TypeEnum::Error))
{
	_isUseful = false;
}

const vector<BoundUnaryOperator>& BoundUnaryOperator::Operators()
{
	static const auto operators = vector<BoundUnaryOperator>{
	BoundUnaryOperator(SyntaxKind::BangToken, BoundUnaryOperatorKind::LogicalNegation,
						TypeSymbol::GetType(TypeEnum::Bool)),
	BoundUnaryOperator(SyntaxKind::PlusToken, BoundUnaryOperatorKind::Identity,
						TypeSymbol::GetType(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::MinusToken, BoundUnaryOperatorKind::Negation,
						TypeSymbol::GetType(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::TildeToken, BoundUnaryOperatorKind::OnesComplement,
						TypeSymbol::GetType(TypeEnum::Int))
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

const vector<const BoundNode*> BoundUnaryExpression::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_operand);
}

const vector<std::pair<string, string>> BoundUnaryExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Type", Type().Name())
	};
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
						  TypeSymbol::GetType(TypeEnum::Error))
{
	_isUseful = false;
}

const vector<BoundBinaryOperator>& BoundBinaryOperator::Operators()
{
	static const auto operators = vector<BoundBinaryOperator>{
		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition,
							TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::MinusToken, BoundBinaryOperatorKind::Subtraction,
							TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::StarToken, BoundBinaryOperatorKind::Multiplication,
							TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::SlashToken, BoundBinaryOperatorKind::Division,
							TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::PercentToken, BoundBinaryOperatorKind::Modulus,
							TypeSymbol::GetType(TypeEnum::Int)),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd,
							TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr,
							TypeSymbol::GetType(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor,
							TypeSymbol::GetType(TypeEnum::Int)),

		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::LessToken, BoundBinaryOperatorKind::Less,
							TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::LessOrEqualsToken, BoundBinaryOperatorKind::LessOrEquals,
							TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::GreaterToken, BoundBinaryOperatorKind::Greater,
							TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::GreaterOrEqualsToken, BoundBinaryOperatorKind::GreaterOrEquals,
							TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Bool)),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd,
							TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::AmpersandAmpersandToken, BoundBinaryOperatorKind::LogicalAnd,
							TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr,
							TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::PipePipeToken, BoundBinaryOperatorKind::LogicalOr,
							TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor,
							TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							TypeSymbol::GetType(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							TypeSymbol::GetType(TypeEnum::Bool)),

		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition,
							TypeSymbol::GetType(TypeEnum::String))
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

const vector<const BoundNode*> BoundBinaryExpression::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_left, _right);
}

const vector<std::pair<string, string>> BoundBinaryExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Type", Type().Name())
	};
}

BoundAssignmentExpression::BoundAssignmentExpression(const shared_ptr<VariableSymbol>& variable,
													 const shared_ptr<BoundExpression>& expression)
	:_variable(variable), _expression(expression)
{
}

const vector<const BoundNode*> BoundAssignmentExpression::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_expression);
}

const vector<std::pair<string, string>> BoundAssignmentExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable()->ToString()),
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

BoundVariableExpression::BoundVariableExpression(const shared_ptr<VariableSymbol>& variable)
	: _variable(variable)
{
}

const vector<std::pair<string, string>> BoundVariableExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable()->ToString()),
			std::pair<string, string>("Type", Type().Name())
	};
}

BoundCallExpression::BoundCallExpression(const shared_ptr<FunctionSymbol>& function,
										 const vector<shared_ptr<BoundExpression>>& arguments)
	:_function(function), _arguments(arguments)
{
}

const vector<const BoundNode*> BoundCallExpression::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode, BoundExpression>(_arguments.begin(),
														  _arguments.end());
}

const vector<std::pair<string, string>> BoundCallExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Type", Type().Name())
	};
}

BoundConversionExpression::BoundConversionExpression(const TypeSymbol& type,
													 const shared_ptr<BoundExpression>& expression)
	:_type(type), _expression(expression)
{
}

const vector<const BoundNode*> BoundConversionExpression::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_expression);
}

const vector<std::pair<string, string>> BoundConversionExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Type", Type().ToString())
	};
}

BoundPostfixExpression::BoundPostfixExpression(const shared_ptr<VariableSymbol>& variable,
											   const BoundPostfixOperatorEnum& kind,
											   const shared_ptr<BoundExpression>& expression)
	:_variable(variable), _kind(kind), _expression(expression)
{
}

const vector<const BoundNode*> BoundPostfixExpression::GetChildren() const
{
	return MakeVecOfRaw<const BoundNode>(_expression);
}

const vector<std::pair<string, string>> BoundPostfixExpression::GetProperties() const
{
	return vector<std::pair<string, string>>{
		std::pair<string, string>("Variable", Variable()->ToString()),
			std::pair<string, string>("Type", Type().Name()),
			std::pair<string, string>("OperatorKind", GetEnumText(OperatorKind()))
	};
}

}//MCF
