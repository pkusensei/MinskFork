#pragma once

#include <stdexcept>

#include "BoundNode.h"
#include "Symbols.h"
#include "SyntaxKind.h"

namespace MCF {

enum class BoundUnaryOperatorKind
{
	Identity,
	Negation,
	LogicalNegation,
	OnesComplement
};

enum class BoundBinaryOperatorKind
{
	Addition,
	Subtraction,
	Multiplication,
	Division,
	Modulus,
	LogicalAnd,
	LogicalOr,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	Equals,
	NotEquals,
	Less,
	LessOrEquals,
	Greater,
	GreaterOrEquals
};

enum class BoundPostfixOperatorEnum
{
	Increment,
	Decrement,
};

string_view nameof(BoundUnaryOperatorKind kind)noexcept;
string_view nameof(BoundBinaryOperatorKind kind)noexcept;
string_view nameof(BoundPostfixOperatorEnum kind)noexcept;

struct BoundUnaryOperator final
{
	TypeSymbol OperandType;
	TypeSymbol Type; // result type
	SyntaxKind SynKind;
	BoundUnaryOperatorKind Kind;
	bool IsUseful = true;

private:
	explicit BoundUnaryOperator(SyntaxKind synKind, BoundUnaryOperatorKind kind,
								const TypeSymbol& operandType, const TypeSymbol& resultType)noexcept
		: OperandType(operandType), Type(resultType),
		SynKind(synKind), Kind(kind)
	{
	}

	explicit BoundUnaryOperator(SyntaxKind synKind,
								BoundUnaryOperatorKind kind, const TypeSymbol& operandType)noexcept
		: BoundUnaryOperator(synKind, kind, operandType, operandType)
	{
	}

	explicit BoundUnaryOperator()noexcept
		: BoundUnaryOperator(SyntaxKind::BadToken, BoundUnaryOperatorKind::Identity,
							 TYPE_ERROR)
	{
		IsUseful = false;
	}

	static const std::array<BoundUnaryOperator, 4> operators;

public:
	bool operator==(const BoundUnaryOperator& other)const noexcept
	{
		return OperandType == other.OperandType && Type == other.Type
			&& SynKind == other.SynKind && Kind == other.Kind;
	}
	bool operator!=(const BoundUnaryOperator& other)const noexcept
	{
		return !(*this == other);
	}

	static BoundUnaryOperator Bind(SyntaxKind synKind, const TypeSymbol& type)noexcept;
};

struct BoundBinaryOperator final
{
	TypeSymbol LeftType;
	TypeSymbol RightType;
	TypeSymbol Type; // result type
	SyntaxKind SynKind;
	BoundBinaryOperatorKind Kind;
	bool IsUseful = true;

private:
	explicit BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
								 const TypeSymbol& left, const TypeSymbol& right,
								 const TypeSymbol& result)noexcept
		:LeftType(left), RightType(right), Type(result),
		SynKind(synKind), Kind(kind)
	{
	}

	explicit BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
								 const TypeSymbol& operandType,
								 const TypeSymbol& resultType)noexcept
		: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
	{
	}

	explicit BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
								 const TypeSymbol& type)noexcept
		: BoundBinaryOperator(synKind, kind, type, type, type)
	{
	}

	explicit BoundBinaryOperator()noexcept
		: BoundBinaryOperator(SyntaxKind::BadToken,
							  BoundBinaryOperatorKind::Addition,
							  TYPE_ERROR)
	{
		IsUseful = false;
	}

	static const std::array<BoundBinaryOperator, 26> operators;

public:
	bool operator==(const BoundBinaryOperator& other)const noexcept
	{
		return LeftType == other.LeftType && RightType == other.RightType
			&& Type == other.Type
			&& SynKind == other.SynKind && Kind == other.Kind;
	}
	bool operator!=(const BoundBinaryOperator& other)const noexcept
	{
		return !(*this == other);
	}

	static BoundBinaryOperator Bind(SyntaxKind synKind,
									const TypeSymbol& leftType, const TypeSymbol& rightType)noexcept;

};

struct BoundExpression :public BoundNode
{
protected:
	explicit BoundExpression(const SyntaxNode& syntax)noexcept
		:BoundNode{ syntax }
	{
	}

public:
	virtual const TypeSymbol& Type() const noexcept = 0;
	virtual const BoundConstant& ConstantValue()const noexcept { return NULL_VALUE; }
	virtual bool Equals(const BoundExpression& other)const noexcept;
	virtual unique_ptr<BoundExpression> Clone() const = 0;

	bool operator==(const BoundExpression& other)const noexcept { return Equals(other); }
	bool operator!=(const BoundExpression& other)const noexcept { return !Equals(other); }
};

struct BoundErrorExpression final :public BoundExpression
{
public:
	explicit BoundErrorExpression(const SyntaxNode& syntax)noexcept
		:BoundExpression{ syntax }
	{
	}

	// Inherited via BoundExpression
	const TypeSymbol& Type() const noexcept override { return TYPE_ERROR; }
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ErrorExpression; }

	unique_ptr<BoundExpression> Clone() const override
	{
		return UniqueClone<BoundErrorExpression, BoundExpression>(*this);
	}

};

[[nodiscard]] BoundConstant Fold(const BoundUnaryOperator& op, const BoundExpression& operand);
[[nodiscard]] BoundConstant Fold(const BoundExpression& left, const BoundBinaryOperator& op,
								 const BoundExpression& right);

struct BoundUnaryExpression final : public BoundExpression
{
	BoundUnaryOperator Op;
	BoundConstant Constant;
	unique_ptr<BoundExpression> Operand;

public:
	explicit BoundUnaryExpression(const SyntaxNode& syntax,
								  const BoundUnaryOperator& op,
								  unique_ptr<BoundExpression> operand)noexcept
		:BoundExpression{ syntax },
		Op(op), Constant(Fold(op, *operand)),
		Operand(std::move(operand))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::UnaryExpression; }
	const TypeSymbol& Type() const noexcept override { return Op.Type; }
	const BoundConstant& ConstantValue()const noexcept override { return Constant; }

public:
	unique_ptr<BoundExpression> Clone() const override
	{
		return make_unique<BoundUnaryExpression>(Syntax(), Op, Operand->Clone());
	}
	bool Equals(const BoundExpression& other)const noexcept override;
};

struct BoundBinaryExpression final : public BoundExpression
{
	BoundBinaryOperator Op;
	BoundConstant Constant;
	unique_ptr<BoundExpression> Left;
	unique_ptr<BoundExpression> Right;

public:
	explicit BoundBinaryExpression(const SyntaxNode& syntax,
								   unique_ptr<BoundExpression> left,
								   BoundBinaryOperator op,
								   unique_ptr<BoundExpression> right)noexcept
		:BoundExpression{ syntax },
		Op(std::move(op)), Constant(Fold(*left, op, *right)),
		Left(std::move(left)), Right(std::move(right))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BinaryExpression; }
	const TypeSymbol& Type() const noexcept override { return Op.Type; }
	const BoundConstant& ConstantValue()const noexcept override { return Constant; }

	unique_ptr<BoundExpression> Clone() const override
	{
		return make_unique<BoundBinaryExpression>(Syntax(), Left->Clone(), Op, Right->Clone());
	}
	bool Equals(const BoundExpression& other)const noexcept override;

};

struct BoundAssignmentExpression final : public BoundExpression
{
	unique_ptr<VariableSymbol> Variable;
	unique_ptr<BoundExpression> Expression;

public:
	explicit BoundAssignmentExpression(const SyntaxNode& syntax,
									   unique_ptr<VariableSymbol> variable,
									   unique_ptr<BoundExpression> expression)noexcept
		:BoundExpression{ syntax },
		Variable(std::move(variable)), Expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::AssignmentExpression; }
	const TypeSymbol& Type() const noexcept override { return Expression->Type(); }

	unique_ptr<BoundExpression> Clone() const override
	{
		return make_unique<BoundAssignmentExpression>(Syntax(),
													  Variable->UniqueCloneAs<VariableSymbol>(),
													  Expression->Clone());
	}
	bool Equals(const BoundExpression& other)const noexcept override;

};

struct BoundCompoundAssignmentExpression final :public BoundExpression
{
	BoundBinaryOperator Op;
	unique_ptr<VariableSymbol> Variable;
	unique_ptr<BoundExpression> Expression;

public:
	explicit BoundCompoundAssignmentExpression(const SyntaxNode& syntax,
											   unique_ptr<VariableSymbol> variable,
											   BoundBinaryOperator op,
											   unique_ptr<BoundExpression> expression)noexcept
		:BoundExpression{ syntax },
		Op(std::move(op)),
		Variable(std::move(variable)), Expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::CompoundAssignmentExpression; }
	const TypeSymbol& Type() const noexcept override { return Expression->Type(); }

	unique_ptr<BoundExpression> Clone() const override
	{
		return make_unique<BoundCompoundAssignmentExpression>(Syntax(),
															  Variable->UniqueCloneAs<VariableSymbol>(),
															  Op,
															  Expression->Clone());
	}
	bool Equals(const BoundExpression& other)const noexcept override;

};

struct BoundLiteralExpression final : public BoundExpression
{
	BoundConstant Constant;
	TypeSymbol LiteralType;

public:
	explicit BoundLiteralExpression(const SyntaxNode& syntax, ValueType value)
		:BoundExpression{ syntax },
		Constant(std::move(value)), LiteralType(Constant.Type())
	{
		if (!Constant.HasValue())
			throw std::invalid_argument("Unexpected literal: " + Constant.ToString());
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LiteralExpression; }
	const TypeSymbol& Type() const noexcept override { return LiteralType; }
	const BoundConstant& ConstantValue()const noexcept override { return Constant; }

	constexpr const ValueType& Value()const noexcept { return Constant; }

	unique_ptr<BoundExpression> Clone() const override
	{
		return UniqueClone<BoundLiteralExpression, BoundExpression>(*this);
	}

};

struct BoundVariableExpression final : public BoundExpression
{
	unique_ptr<VariableSymbol> Variable;

public:
	explicit BoundVariableExpression(const SyntaxNode& syntax,
									 unique_ptr<VariableSymbol> variable)noexcept
		:BoundExpression{ syntax },
		Variable(std::move(variable))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableExpression; }
	const TypeSymbol& Type() const noexcept override { return Variable->Type; }
	const BoundConstant& ConstantValue()const noexcept override { return Variable->Constant; }

	unique_ptr<BoundExpression> Clone() const override
	{
		return make_unique<BoundVariableExpression>(Syntax(), Variable->UniqueCloneAs<VariableSymbol>());
	}
	bool Equals(const BoundExpression& other)const noexcept override;

};

struct BoundCallExpression final :public BoundExpression
{
	vector<unique_ptr<BoundExpression>> Arguments;
	unique_ptr<FunctionSymbol> Function;

public:
	explicit BoundCallExpression(const SyntaxNode& syntax,
								 unique_ptr<FunctionSymbol> function,
								 vector<unique_ptr<BoundExpression>> arguments)noexcept
		:BoundExpression{ syntax },
		Arguments(std::move(arguments)), Function(std::move(function))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::CallExpression; }
	const TypeSymbol& Type() const noexcept override { return Function->Type; }

	unique_ptr<BoundExpression> Clone() const override;
	bool Equals(const BoundExpression& other)const noexcept override;

};

struct BoundConversionExpression final :public BoundExpression
{
	TypeSymbol ResultType;
	unique_ptr<BoundExpression> Expression;

public:
	explicit BoundConversionExpression(const SyntaxNode& syntax,
									   const TypeSymbol& type,
									   unique_ptr<BoundExpression> expression)noexcept
		:BoundExpression{ syntax },
		ResultType(type), Expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConversionExpression; }
	const TypeSymbol& Type() const noexcept override { return ResultType; }

	unique_ptr<BoundExpression> Clone() const override
	{
		return make_unique<BoundConversionExpression>(Syntax(),
													  ResultType,
													  Expression->Clone());
	}
	bool Equals(const BoundExpression& other)const noexcept override;

};

struct BoundPostfixExpression final :public BoundExpression
{
	unique_ptr<VariableSymbol> Variable;
	unique_ptr<BoundExpression> Expression;
	BoundPostfixOperatorEnum OperatorKind;

public:
	explicit BoundPostfixExpression(const SyntaxNode& syntax,
									unique_ptr<VariableSymbol> variable,
									BoundPostfixOperatorEnum kind,
									unique_ptr<BoundExpression> expression)noexcept
		:BoundExpression{ syntax },
		Variable(std::move(variable)), Expression(std::move(expression)), OperatorKind(kind)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::PostfixExpression; }
	const TypeSymbol& Type() const noexcept override { return Variable->Type; }

	unique_ptr<BoundExpression> Clone() const override
	{
		return make_unique<BoundPostfixExpression>(Syntax(),
												   Variable->UniqueCloneAs<VariableSymbol>(),
												   OperatorKind,
												   Expression->Clone());
	}
	bool Equals(const BoundExpression& other)const noexcept override;

};

}//MCF
