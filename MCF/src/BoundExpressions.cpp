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

bool BoundExpression::Equals(const BoundExpression& other)const noexcept
{
	if (std::addressof(Syntax()) == std::addressof(other.Syntax()))
		return Kind() == other.Kind() && Type() == other.Type()
			&& ConstantValue() == other.ConstantValue();
	return false;
}

bool BoundUnaryExpression::Equals(const BoundExpression& other)const noexcept
{
	if (BoundExpression::Equals(other))
	{
		auto& u = static_cast<const BoundUnaryExpression&>(other);
		return Op == u.Op && *Operand == *u.Operand;
	}
	return false;
}

bool BoundBinaryExpression::Equals(const BoundExpression& other)const noexcept
{
	if (BoundExpression::Equals(other))
	{
		auto& b = static_cast<const BoundBinaryExpression&>(other);
		return Op == b.Op && *Left == *b.Left && *Right == *b.Right;
	}
	return false;
}

bool BoundAssignmentExpression::Equals(const BoundExpression& other)const noexcept
{
	if (BoundExpression::Equals(other))
	{
		auto& a = static_cast<const BoundAssignmentExpression&>(other);
		return *Variable == *a.Variable && *Expression == *a.Expression;
	}
	return false;
}

bool BoundCompoundAssignmentExpression::Equals(const BoundExpression& other)const noexcept
{
	if (BoundExpression::Equals(other))
	{
		auto& c = static_cast<const BoundCompoundAssignmentExpression&>(other);
		return Op == c.Op && *Variable == *c.Variable && *Expression == *c.Expression;
	}
	return false;
}

bool BoundVariableExpression::Equals(const BoundExpression& other)const noexcept
{
	if (BoundExpression::Equals(other))
	{
		auto& v = static_cast<const BoundVariableExpression&>(other);
		return *Variable == *v.Variable;
	}
	return false;
}

unique_ptr<BoundExpression> BoundCallExpression::Clone() const
{
	auto args = vector<unique_ptr<BoundExpression>>{};
	for (const auto& arg : Arguments)
		args.push_back(arg->Clone());
	return make_unique<BoundCallExpression>(Syntax(),
											Function->UniqueCloneAs<FunctionSymbol>(),
											std::move(args));
}

bool BoundCallExpression::Equals(const BoundExpression& other)const noexcept
{
	if (BoundExpression::Equals(other))
	{
		auto& f = static_cast<const BoundCallExpression&>(other);
		if (*Function == *f.Function && Arguments.size() == f.Arguments.size())
		{
			return std::equal(Arguments.cbegin(), Arguments.cend(),
							  f.Arguments.cbegin(),
							  [](const auto& it1, const auto& it2)
							  {
								  return *it1 == *it2;
							  });
		}
	}
	return false;
}

bool BoundConversionExpression::Equals(const BoundExpression& other)const noexcept
{
	if (BoundExpression::Equals(other))
	{
		auto& c = static_cast<const BoundConversionExpression&>(other);
		return *Expression == *c.Expression;
	}
	return false;
}

bool BoundPostfixExpression::Equals(const BoundExpression& other)const noexcept
{
	if (BoundExpression::Equals(other))
	{
		auto& p = static_cast<const BoundPostfixExpression&>(other);
		return OperatorKind == p.OperatorKind && *Variable == *p.Variable
			&& *Expression == *p.Expression;
	}
	return false;
}

} //MCF