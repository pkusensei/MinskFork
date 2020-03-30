#pragma once

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

inline string_view nameof(BoundUnaryOperatorKind kind)
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

inline string_view nameof(BoundBinaryOperatorKind kind)
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

enum class BoundPostfixOperatorEnum
{
	Increment,
	Decrement,
};

inline string_view nameof(BoundPostfixOperatorEnum kind)
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

class BoundExpression :public BoundNode
{
public:
	virtual ConstTypeRef Type() const = 0;
};

class BoundErrorExpression final :public BoundExpression
{
public:
	// Inherited via BoundExpression
	ConstTypeRef Type() const override { return TypeSymbol::Get(TypeEnum::Error); }
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ErrorExpression; }
};

class BoundUnaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundUnaryOperatorKind _kind;
	ConstTypeRef _operandType;
	ConstTypeRef _resultType;
	bool _isUseful = true;

	BoundUnaryOperator(SyntaxKind synKind, BoundUnaryOperatorKind kind,
		ConstTypeRef operandType, ConstTypeRef resultType)
		:_syntaxKind(synKind), _kind(kind),
		_operandType(operandType), _resultType(resultType)
	{
	}

	BoundUnaryOperator(SyntaxKind synKind,
		BoundUnaryOperatorKind kind, ConstTypeRef operandType)
		: BoundUnaryOperator(synKind, kind, operandType, operandType)
	{
	}

	BoundUnaryOperator()
		: BoundUnaryOperator(SyntaxKind::BadToken, BoundUnaryOperatorKind::Identity,
			TypeSymbol::Get(TypeEnum::Error))
	{
		_isUseful = false;
	}

	static const std::array<BoundUnaryOperator, 4> operators;

public:
	constexpr SyntaxKind SynKind()const noexcept { return _syntaxKind; }
	constexpr BoundUnaryOperatorKind Kind()const noexcept { return _kind; }
	ConstTypeRef OperandType()const { return _operandType; }
	ConstTypeRef Type()const { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundUnaryOperator Bind(SyntaxKind synKind,
		ConstTypeRef type)
	{
		for (const auto& op : operators)
		{
			if (op.SynKind() == synKind && op.OperandType().get() == type.get())
				return op;
		}
		return BoundUnaryOperator();
	}
};

inline const std::array<BoundUnaryOperator, 4> BoundUnaryOperator::operators = {
	BoundUnaryOperator(SyntaxKind::BangToken, BoundUnaryOperatorKind::LogicalNegation,
						TypeSymbol::Get(TypeEnum::Bool)),
	BoundUnaryOperator(SyntaxKind::PlusToken, BoundUnaryOperatorKind::Identity,
						TypeSymbol::Get(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::MinusToken, BoundUnaryOperatorKind::Negation,
						TypeSymbol::Get(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::TildeToken, BoundUnaryOperatorKind::OnesComplement,
						TypeSymbol::Get(TypeEnum::Int))
};

class BoundUnaryExpression final : public BoundExpression
{
private:
	BoundUnaryOperator _op;
	shared_ptr<BoundExpression> _operand;

public:
	BoundUnaryExpression(const BoundUnaryOperator& op,
		shared_ptr<BoundExpression> operand)
		:_op(op), _operand(std::move(operand))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::UnaryExpression; }
	ConstTypeRef Type() const  override { return _op.Type(); }

	constexpr const BoundUnaryOperator& Op()const noexcept { return _op; }
	constexpr const shared_ptr<BoundExpression>& Operand()const noexcept { return _operand; }
};

class BoundBinaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundBinaryOperatorKind _kind;
	ConstTypeRef _leftType;
	ConstTypeRef _rightType;
	ConstTypeRef _resultType;
	bool _isUseful = true;

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
		ConstTypeRef left, ConstTypeRef right, ConstTypeRef result)
		:_syntaxKind(synKind), _kind(kind), _leftType(left), _rightType(right),
		_resultType(result)
	{
	}

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
		ConstTypeRef operandType, ConstTypeRef resultType)
		: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
	{
	}

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
		ConstTypeRef type)
		: BoundBinaryOperator(synKind, kind, type, type, type)
	{
	}

	BoundBinaryOperator()
		: BoundBinaryOperator(SyntaxKind::BadToken, BoundBinaryOperatorKind::Addition,
			TypeSymbol::Get(TypeEnum::Error))
	{
		_isUseful = false;
	}

	static const std::array<BoundBinaryOperator, 24> operators;

public:
	constexpr SyntaxKind SynKind()const noexcept { return _syntaxKind; }
	constexpr BoundBinaryOperatorKind Kind()const noexcept { return _kind; }
	ConstTypeRef LeftType()const noexcept { return _leftType; }
	ConstTypeRef RightType()const noexcept { return _rightType; }
	ConstTypeRef Type()const noexcept { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundBinaryOperator Bind(SyntaxKind synKind,
		ConstTypeRef leftType, ConstTypeRef rightType)
	{
		for (const auto& op : operators)
		{
			if (op.SynKind() == synKind && op.LeftType().get() == leftType.get()
				&& op.RightType().get() == rightType.get())
				return op;
		}
		return BoundBinaryOperator();
	}

};

inline const std::array<BoundBinaryOperator, 24> BoundBinaryOperator::operators = {
		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition,
							TypeSymbol::Get(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::MinusToken, BoundBinaryOperatorKind::Subtraction,
							TypeSymbol::Get(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::StarToken, BoundBinaryOperatorKind::Multiplication,
							TypeSymbol::Get(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::SlashToken, BoundBinaryOperatorKind::Division,
							TypeSymbol::Get(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::PercentToken, BoundBinaryOperatorKind::Modulus,
							TypeSymbol::Get(TypeEnum::Int)),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd,
							TypeSymbol::Get(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr,
							TypeSymbol::Get(TypeEnum::Int)),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor,
							TypeSymbol::Get(TypeEnum::Int)),

		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							TypeSymbol::Get(TypeEnum::Int), TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							TypeSymbol::Get(TypeEnum::Int), TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::LessToken, BoundBinaryOperatorKind::Less,
							TypeSymbol::Get(TypeEnum::Int), TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::LessOrEqualsToken, BoundBinaryOperatorKind::LessOrEquals,
							TypeSymbol::Get(TypeEnum::Int), TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::GreaterToken, BoundBinaryOperatorKind::Greater,
							TypeSymbol::Get(TypeEnum::Int), TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::GreaterOrEqualsToken, BoundBinaryOperatorKind::GreaterOrEquals,
							TypeSymbol::Get(TypeEnum::Int), TypeSymbol::Get(TypeEnum::Bool)),

		BoundBinaryOperator(SyntaxKind::AmpersandToken, BoundBinaryOperatorKind::BitwiseAnd,
							TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::AmpersandAmpersandToken, BoundBinaryOperatorKind::LogicalAnd,
							TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::PipeToken, BoundBinaryOperatorKind::BitwiseOr,
							TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::PipePipeToken, BoundBinaryOperatorKind::LogicalOr,
							TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::HatToken, BoundBinaryOperatorKind::BitwiseXor,
							TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							TypeSymbol::Get(TypeEnum::Bool)),

		BoundBinaryOperator(SyntaxKind::PlusToken, BoundBinaryOperatorKind::Addition,
							TypeSymbol::Get(TypeEnum::String)),
		BoundBinaryOperator(SyntaxKind::EqualsEqualsToken, BoundBinaryOperatorKind::Equals,
							TypeSymbol::Get(TypeEnum::String), TypeSymbol::Get(TypeEnum::Bool)),
		BoundBinaryOperator(SyntaxKind::BangEqualsToken, BoundBinaryOperatorKind::NotEquals,
							TypeSymbol::Get(TypeEnum::String), TypeSymbol::Get(TypeEnum::Bool)),
};

class BoundBinaryExpression final : public BoundExpression
{
private:
	shared_ptr<BoundExpression> _left;
	shared_ptr<BoundExpression> _right;
	BoundBinaryOperator _op;

public:
	BoundBinaryExpression(shared_ptr<BoundExpression> left,
		BoundBinaryOperator op, shared_ptr<BoundExpression> right)
		:_left(std::move(left)), _right(std::move(right)), _op(std::move(op))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BinaryExpression; }
	ConstTypeRef Type() const  override { return _op.Type(); }

	constexpr const shared_ptr<BoundExpression>& Left()const noexcept { return _left; }
	constexpr const shared_ptr<BoundExpression>& Right()const noexcept { return _right; }
	constexpr const BoundBinaryOperator& Op()const noexcept { return _op; }
};

class BoundAssignmentExpression final : public BoundExpression
{
private:
	shared_ptr<VariableSymbol> _variable;
	shared_ptr<BoundExpression> _expression;

public:
	BoundAssignmentExpression(shared_ptr<VariableSymbol> variable,
		shared_ptr<BoundExpression> expression)
		:_variable(std::move(variable)), _expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::AssignmentExpression; }
	ConstTypeRef Type() const  override { return _expression->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

class BoundLiteralExpression final : public BoundExpression
{
private:
	ValueType _value;

public:
	explicit BoundLiteralExpression(ValueType value)
		: _value(std::move(value))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LiteralExpression; }
	ConstTypeRef Type() const  override { return _value.Type(); }

	constexpr const ValueType& Value()const noexcept { return _value; }
};

class BoundVariableExpression final : public BoundExpression
{
private:
	shared_ptr<VariableSymbol> _variable;

public:
	explicit BoundVariableExpression(shared_ptr<VariableSymbol> variable)
		: _variable(std::move(variable))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableExpression; }
	ConstTypeRef Type() const override { return _variable->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
};

class BoundCallExpression final :public BoundExpression
{
private:
	shared_ptr<FunctionSymbol> _function;
	vector<shared_ptr<BoundExpression>> _arguments;

public:
	BoundCallExpression(shared_ptr<FunctionSymbol> function,
		vector<shared_ptr<BoundExpression>> arguments)
		:_function(std::move(function)), _arguments(std::move(arguments))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::CallExpression; }
	ConstTypeRef Type() const override { return _function->Type(); }

	constexpr const shared_ptr<FunctionSymbol>& Function()const noexcept { return _function; }
	constexpr const vector<shared_ptr<BoundExpression>>& Arguments()const noexcept { return _arguments; }
};

class BoundConversionExpression final :public BoundExpression
{
private:
	ConstTypeRef _type;
	shared_ptr<BoundExpression> _expression;

public:
	BoundConversionExpression(ConstTypeRef type,
		shared_ptr<BoundExpression> expression)
		:_type(type), _expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConversionExpression; }
	ConstTypeRef Type() const override { return _type; }

	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

class BoundPostfixExpression final :public BoundExpression
{
private:
	shared_ptr<VariableSymbol> _variable;
	BoundPostfixOperatorEnum _kind;
	shared_ptr<BoundExpression> _expression;

public:
	BoundPostfixExpression(shared_ptr<VariableSymbol> variable,
		BoundPostfixOperatorEnum kind,
		shared_ptr<BoundExpression> expression)
		:_variable(std::move(variable)), _kind(kind), _expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::PostfixExpression; }
	ConstTypeRef Type() const override { return _variable->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
	constexpr BoundPostfixOperatorEnum OperatorKind()const noexcept { return _kind; }
	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

}//MCF
