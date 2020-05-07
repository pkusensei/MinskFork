#include "BoundExpressions.h"

#include "helpers.h"

namespace MCF {

string_view nameof(BoundUnaryOperatorKind kind)
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
			return string_view();
	}
}

string_view nameof(BoundBinaryOperatorKind kind)
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
			return string_view();
	}
}

string_view nameof(BoundPostfixOperatorEnum kind)
{
	switch (kind)
	{
		case BoundPostfixOperatorEnum::Increment:
			return "Increment";
		case BoundPostfixOperatorEnum::Decrement:
			return "Decrement";
		default:
			return string_view();
	}
}

BoundUnaryOperator BoundUnaryOperator::Bind(SyntaxKind synKind, const TypeSymbol& type)
{
	for (const auto& op : operators)
	{
		if (op.SynKind() == synKind && op.OperandType() == type)
			return op;
	}
	return BoundUnaryOperator();
}

const std::array<BoundUnaryOperator, 4> BoundUnaryOperator::operators = {
	BoundUnaryOperator(SyntaxKind::BangToken, BoundUnaryOperatorKind::LogicalNegation,
						TYPE_BOOL),
	BoundUnaryOperator(SyntaxKind::PlusToken, BoundUnaryOperatorKind::Identity,
						TYPE_INT),
	BoundUnaryOperator(SyntaxKind::MinusToken, BoundUnaryOperatorKind::Negation,
						TYPE_INT),
	BoundUnaryOperator(SyntaxKind::TildeToken, BoundUnaryOperatorKind::OnesComplement,
						TYPE_INT)
};

BoundBinaryOperator BoundBinaryOperator::Bind(SyntaxKind synKind,
	const TypeSymbol& leftType, const TypeSymbol& rightType)
{
	for (const auto& op : operators)
	{
		if (op.SynKind() == synKind && op.LeftType() == leftType
			&& op.RightType() == rightType)
			return op;
	}
	return BoundBinaryOperator();
}

const std::array<BoundBinaryOperator, 26> BoundBinaryOperator::operators = {
		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition,
							TYPE_INT),
		BoundBinaryOperator(SyntaxKind::MinusToken, BoundBinaryOperatorKind::Subtraction,
							TYPE_INT),
		BoundBinaryOperator(SyntaxKind::StarToken, BoundBinaryOperatorKind::Multiplication,
							TYPE_INT),
		BoundBinaryOperator(SyntaxKind::SlashToken, BoundBinaryOperatorKind::Division,
							TYPE_INT),
		BoundBinaryOperator(SyntaxKind::PercentToken, BoundBinaryOperatorKind::Modulus,
							TYPE_INT),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd,
							TYPE_INT),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr,
							TYPE_INT),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor,
							TYPE_INT),

		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							TYPE_INT, TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							TYPE_INT, TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::LessToken, BoundBinaryOperatorKind::Less,
							TYPE_INT, TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::LessOrEqualsToken, BoundBinaryOperatorKind::LessOrEquals,
							TYPE_INT, TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::GreaterToken, BoundBinaryOperatorKind::Greater,
							TYPE_INT, TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::GreaterOrEqualsToken, BoundBinaryOperatorKind::GreaterOrEquals,
							TYPE_INT, TYPE_BOOL),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd,
							TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::AmpersandAmpersandToken, BoundBinaryOperatorKind::LogicalAnd,
							TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr,
							TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::PipePipeToken, BoundBinaryOperatorKind::LogicalOr,
							TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor,
							TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							TYPE_BOOL),

		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition,
							TYPE_STRING),
		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							TYPE_STRING, TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							TYPE_STRING, TYPE_BOOL),

		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							TYPE_ANY, TYPE_BOOL),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							TYPE_ANY, TYPE_BOOL),
};

BoundConstant ComputeConstant(const BoundUnaryOperator& op, const BoundExpression& operand)
{
	if (operand.ConstantValue() != NULL_VALUE)
	{
		switch (op.Kind())
		{
			case BoundUnaryOperatorKind::Identity:
				return BoundConstant(operand.ConstantValue().GetValue<IntegerType>());
			case BoundUnaryOperatorKind::Negation:
				return BoundConstant(-operand.ConstantValue().GetValue<IntegerType>());
			case BoundUnaryOperatorKind::LogicalNegation:
				return BoundConstant(!operand.ConstantValue().GetValue<bool>());
			case BoundUnaryOperatorKind::OnesComplement:
				return BoundConstant(~operand.ConstantValue().GetValue<IntegerType>());
			default:
				throw std::invalid_argument(BuildStringFrom("Unexpected unary operator '",
					nameof(op.Kind()), "'."));
		}
	}

	return NULL_VALUE;
}

BoundConstant ComputeConstant(const BoundExpression& left, const BoundBinaryOperator& op,
	const BoundExpression& right)
{
	auto& lc = left.ConstantValue();
	auto& rc = right.ConstantValue();

	if (op.Kind() == BoundBinaryOperatorKind::LogicalAnd)
	{
		if ((lc != NULL_VALUE && !lc.GetValue<bool>()) ||
			(rc != NULL_VALUE && !rc.GetValue<bool>()))
		{
			return BoundConstant(false);
		}
	}

	if (op.Kind() == BoundBinaryOperatorKind::LogicalOr)
	{
		if ((lc != NULL_VALUE && lc.GetValue<bool>()) ||
			(rc != NULL_VALUE && rc.GetValue<bool>()))
		{
			return  BoundConstant(true);
		}
	}

	if (lc == NULL_VALUE || rc == NULL_VALUE)
		return NULL_VALUE;

	switch (op.Kind())
	{
		case BoundBinaryOperatorKind::Addition:
			if (left.Type() == TYPE_INT)
				return  BoundConstant(lc.GetValue<IntegerType>() + rc.GetValue<IntegerType>());
			else
				return  BoundConstant(lc.GetValue<string>() + rc.GetValue<string>());
		case BoundBinaryOperatorKind::Subtraction:
			return  BoundConstant(lc.GetValue<IntegerType>() - rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::Multiplication:
			return  BoundConstant(lc.GetValue<IntegerType>() * rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::Division:
			return  BoundConstant(lc.GetValue<IntegerType>() / rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::BitwiseAnd:
			if (left.Type() == TYPE_INT)
				return  BoundConstant(lc.GetValue<IntegerType>() & rc.GetValue<IntegerType>());
			else
				return  BoundConstant(lc.GetValue<bool>() & rc.GetValue<bool>());
		case BoundBinaryOperatorKind::BitwiseOr:
			if (left.Type() == TYPE_INT)
				return  BoundConstant(lc.GetValue<IntegerType>() | rc.GetValue<IntegerType>());
			else
				return  BoundConstant(lc.GetValue<bool>() | rc.GetValue<bool>());
		case BoundBinaryOperatorKind::BitwiseXor:
			if (left.Type() == TYPE_INT)
				return  BoundConstant(lc.GetValue<IntegerType>() ^ rc.GetValue<IntegerType>());
			else
				return  BoundConstant(lc.GetValue<bool>() ^ rc.GetValue<bool>());
		case BoundBinaryOperatorKind::LogicalAnd:
			return  BoundConstant(lc.GetValue<bool>() && rc.GetValue<bool>());
		case BoundBinaryOperatorKind::LogicalOr:
			return  BoundConstant(lc.GetValue<bool>() || rc.GetValue<bool>());
		case BoundBinaryOperatorKind::Equals:
			return  BoundConstant(lc == rc);
		case BoundBinaryOperatorKind::NotEquals:
			return  BoundConstant(lc != rc);
		case BoundBinaryOperatorKind::Less:
			return  BoundConstant(lc.GetValue<IntegerType>() < rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::LessOrEquals:
			return  BoundConstant(lc.GetValue<IntegerType>() <= rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::Greater:
			return  BoundConstant(lc.GetValue<IntegerType>() > rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::GreaterOrEquals:
			return  BoundConstant(lc.GetValue<IntegerType>() >= rc.GetValue<IntegerType>());
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected binary operator '",
				nameof(op.Kind()), "'."));
	}
}

} //MCF