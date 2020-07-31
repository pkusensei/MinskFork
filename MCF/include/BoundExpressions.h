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

string_view nameof(BoundUnaryOperatorKind kind);
string_view nameof(BoundBinaryOperatorKind kind);
string_view nameof(BoundPostfixOperatorEnum kind);

struct BoundExpression :public BoundNode
{
protected:
	explicit BoundExpression(const SyntaxNode& syntax)noexcept
		:BoundNode(syntax)
	{
	}

public:
	virtual const TypeSymbol& Type() const noexcept = 0;
	virtual const BoundConstant& ConstantValue()const noexcept { return NULL_VALUE; }
};

struct BoundErrorExpression final :public BoundExpression
{
public:
	explicit BoundErrorExpression(const SyntaxNode& syntax)noexcept
		:BoundExpression(syntax)
	{
	}

	// Inherited via BoundExpression
	const TypeSymbol& Type() const noexcept override { return TYPE_ERROR; }
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ErrorExpression; }
};

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

	static BoundUnaryOperator Bind(SyntaxKind synKind, const TypeSymbol& type);
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

	static BoundBinaryOperator Bind(SyntaxKind synKind,
									const TypeSymbol& leftType, const TypeSymbol& rightType);

};

[[nodiscard]] BoundConstant Fold(const BoundUnaryOperator& op, const BoundExpression& operand);
[[nodiscard]] BoundConstant Fold(const BoundExpression& left, const BoundBinaryOperator& op,
								 const BoundExpression& right);

struct BoundUnaryExpression final : public BoundExpression
{
	BoundUnaryOperator Op;
	BoundConstant Constant;
	shared_ptr<BoundExpression> Operand;

public:
	explicit BoundUnaryExpression(const SyntaxNode& syntax,
								  const BoundUnaryOperator& op,
								  shared_ptr<BoundExpression> operand)noexcept
		:BoundExpression(syntax),
		Op(op), Constant(Fold(op, *operand)),
		Operand(std::move(operand))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::UnaryExpression; }
	const TypeSymbol& Type() const noexcept override { return Op.Type; }
	const BoundConstant& ConstantValue()const noexcept override { return Constant; }

};

struct BoundBinaryExpression final : public BoundExpression
{
	BoundBinaryOperator Op;
	BoundConstant Constant;
	shared_ptr<BoundExpression> Left;
	shared_ptr<BoundExpression> Right;

public:
	explicit BoundBinaryExpression(const SyntaxNode& syntax,
								   shared_ptr<BoundExpression> left,
								   BoundBinaryOperator op,
								   shared_ptr<BoundExpression> right)noexcept
		:BoundExpression(syntax),
		Op(std::move(op)), Constant(Fold(*left, op, *right)),
		Left(std::move(left)), Right(std::move(right))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BinaryExpression; }
	const TypeSymbol& Type() const noexcept override { return Op.Type; }
	const BoundConstant& ConstantValue()const noexcept override { return Constant; }

};

struct BoundAssignmentExpression final : public BoundExpression
{
	shared_ptr<VariableSymbol> Variable;
	shared_ptr<BoundExpression> Expression;

public:
	explicit BoundAssignmentExpression(const SyntaxNode& syntax,
									   shared_ptr<VariableSymbol> variable,
									   shared_ptr<BoundExpression> expression)noexcept
		:BoundExpression(syntax),
		Variable(std::move(variable)), Expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::AssignmentExpression; }
	const TypeSymbol& Type() const noexcept override { return Expression->Type(); }

};

struct BoundCompoundAssignmentExpression final :public BoundExpression
{
	BoundBinaryOperator Op;
	shared_ptr<VariableSymbol> Variable;
	shared_ptr<BoundExpression> Expression;

public:
	explicit BoundCompoundAssignmentExpression(const SyntaxNode& syntax,
											   shared_ptr<VariableSymbol> variable,
											   BoundBinaryOperator op,
											   shared_ptr<BoundExpression> expression)noexcept
		:BoundExpression(syntax),
		Op(std::move(op)),
		Variable(std::move(variable)), Expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::CompoundAssignmentExpression; }
	const TypeSymbol& Type() const noexcept override { return Expression->Type(); }

};

struct BoundLiteralExpression final : public BoundExpression
{
	BoundConstant Constant;
	TypeSymbol LiteralType;

public:
	explicit BoundLiteralExpression(const SyntaxNode& syntax, ValueType value)
		:BoundExpression(syntax),
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
};

struct BoundVariableExpression final : public BoundExpression
{
	shared_ptr<VariableSymbol> Variable;

public:
	explicit BoundVariableExpression(const SyntaxNode& syntax,
									 shared_ptr<VariableSymbol> variable)noexcept
		:BoundExpression(syntax),
		Variable(std::move(variable))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableExpression; }
	const TypeSymbol& Type() const noexcept override { return Variable->Type; }
	const BoundConstant& ConstantValue()const noexcept override { return Variable->Constant; }

};

struct BoundCallExpression final :public BoundExpression
{
	vector<shared_ptr<BoundExpression>> Arguments;
	shared_ptr<FunctionSymbol> Function;

public:
	explicit BoundCallExpression(const SyntaxNode& syntax,
								 shared_ptr<FunctionSymbol> function,
								 vector<shared_ptr<BoundExpression>> arguments)noexcept
		:BoundExpression(syntax),
		Arguments(std::move(arguments)), Function(std::move(function))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::CallExpression; }
	const TypeSymbol& Type() const noexcept override { return Function->Type; }

};

struct BoundConversionExpression final :public BoundExpression
{
	TypeSymbol ResultType;
	shared_ptr<BoundExpression> Expression;

public:
	explicit BoundConversionExpression(const SyntaxNode& syntax,
									   const TypeSymbol& type,
									   shared_ptr<BoundExpression> expression)noexcept
		:BoundExpression(syntax),
		ResultType(type), Expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConversionExpression; }
	const TypeSymbol& Type() const noexcept override { return ResultType; }

};

struct BoundPostfixExpression final :public BoundExpression
{
	shared_ptr<VariableSymbol> Variable;
	shared_ptr<BoundExpression> Expression;
	BoundPostfixOperatorEnum OperatorKind;

public:
	explicit BoundPostfixExpression(const SyntaxNode& syntax,
									shared_ptr<VariableSymbol> variable,
									BoundPostfixOperatorEnum kind,
									shared_ptr<BoundExpression> expression)noexcept
		:BoundExpression(syntax),
		Variable(std::move(variable)), Expression(std::move(expression)), OperatorKind(kind)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::PostfixExpression; }
	const TypeSymbol& Type() const noexcept override { return Variable->Type; }

};

}//MCF
