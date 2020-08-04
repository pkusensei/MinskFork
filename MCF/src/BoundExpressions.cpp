#include "BoundExpressions.h"

#include "StringHelper.h"

namespace MCF {

string_view nameof(BoundUnaryOperatorKind kind)noexcept
{
#define NAME(kind) \
case BoundUnaryOperatorKind::kind: return #kind;

	switch (kind)
	{
		NAME(Identity);
		NAME(Negation);
		NAME(LogicalNegation);
		NAME(OnesComplement);

		default:
			return string_view();
	}

#undef NAME
}

string_view nameof(BoundBinaryOperatorKind kind)noexcept
{
#define NAME(kind) \
case BoundBinaryOperatorKind::kind: return #kind;

	switch (kind)
	{
		NAME(Addition);
		NAME(Subtraction);
		NAME(Multiplication);
		NAME(Division);
		NAME(Modulus);
		NAME(LogicalAnd);
		NAME(LogicalOr);
		NAME(BitwiseAnd);
		NAME(BitwiseOr);
		NAME(BitwiseXor);
		NAME(Equals);
		NAME(NotEquals);
		NAME(Less);
		NAME(LessOrEquals);
		NAME(Greater);
		NAME(GreaterOrEquals);

		default:
			return string_view();
	}

#undef NAME
}

string_view nameof(BoundPostfixOperatorEnum kind)noexcept
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

BoundUnaryOperator BoundUnaryOperator::Bind(SyntaxKind synKind, const TypeSymbol& type)noexcept
{
	for (const auto& op : operators)
	{
		if (op.SynKind == synKind && op.OperandType == type)
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
											  const TypeSymbol& leftType, const TypeSymbol& rightType)noexcept
{
	for (const auto& op : operators)
	{
		if (op.SynKind == synKind && op.LeftType == leftType
			&& op.RightType == rightType)
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

BoundConstant Fold(const BoundUnaryOperator& op, const BoundExpression& operand)
{
	if (operand.ConstantValue() != NULL_VALUE)
	{
		switch (op.Kind)
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
															nameof(op.Kind), "'."));
		}
	}

	return NULL_VALUE;
}

BoundConstant Fold(const BoundExpression& left, const BoundBinaryOperator& op,
				   const BoundExpression& right)
{
	auto& lc = left.ConstantValue();
	auto& rc = right.ConstantValue();

	if (op.Kind == BoundBinaryOperatorKind::LogicalAnd)
	{
		if ((lc != NULL_VALUE && !lc.GetValue<bool>()) ||
			(rc != NULL_VALUE && !rc.GetValue<bool>()))
		{
			return BoundConstant(false);
		}
	}

	if (op.Kind == BoundBinaryOperatorKind::LogicalOr)
	{
		if ((lc != NULL_VALUE && lc.GetValue<bool>()) ||
			(rc != NULL_VALUE && rc.GetValue<bool>()))
		{
			return BoundConstant(true);
		}
	}

	if (lc == NULL_VALUE || rc == NULL_VALUE)
		return NULL_VALUE;

	switch (op.Kind)
	{
		case BoundBinaryOperatorKind::Addition:
			if (left.Type() == TYPE_INT)
				return BoundConstant(lc.GetValue<IntegerType>() + rc.GetValue<IntegerType>());
			else
				return BoundConstant(lc.GetValue<string>() + rc.GetValue<string>());
		case BoundBinaryOperatorKind::Subtraction:
			return BoundConstant(lc.GetValue<IntegerType>() - rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::Multiplication:
			return BoundConstant(lc.GetValue<IntegerType>() * rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::Division:
			return BoundConstant(lc.GetValue<IntegerType>() / rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::Modulus:
			return BoundConstant(lc.GetValue<IntegerType>() % rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::BitwiseAnd:
			if (left.Type() == TYPE_INT)
				return BoundConstant(lc.GetValue<IntegerType>() & rc.GetValue<IntegerType>());
			else
				return BoundConstant(lc.GetValue<bool>() & rc.GetValue<bool>());
		case BoundBinaryOperatorKind::BitwiseOr:
			if (left.Type() == TYPE_INT)
				return BoundConstant(lc.GetValue<IntegerType>() | rc.GetValue<IntegerType>());
			else
				return BoundConstant(lc.GetValue<bool>() | rc.GetValue<bool>());
		case BoundBinaryOperatorKind::BitwiseXor:
			if (left.Type() == TYPE_INT)
				return BoundConstant(lc.GetValue<IntegerType>() ^ rc.GetValue<IntegerType>());
			else
				return BoundConstant(lc.GetValue<bool>() ^ rc.GetValue<bool>());
		case BoundBinaryOperatorKind::LogicalAnd:
			return BoundConstant(lc.GetValue<bool>() && rc.GetValue<bool>());
		case BoundBinaryOperatorKind::LogicalOr:
			return BoundConstant(lc.GetValue<bool>() || rc.GetValue<bool>());
		case BoundBinaryOperatorKind::Equals:
			return BoundConstant(lc == rc);
		case BoundBinaryOperatorKind::NotEquals:
			return BoundConstant(lc != rc);
		case BoundBinaryOperatorKind::Less:
			return BoundConstant(lc.GetValue<IntegerType>() < rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::LessOrEquals:
			return BoundConstant(lc.GetValue<IntegerType>() <= rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::Greater:
			return BoundConstant(lc.GetValue<IntegerType>() > rc.GetValue<IntegerType>());
		case BoundBinaryOperatorKind::GreaterOrEquals:
			return BoundConstant(lc.GetValue<IntegerType>() >= rc.GetValue<IntegerType>());
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected binary operator '",
														nameof(op.Kind), "'."));
	}
}

} //MCF