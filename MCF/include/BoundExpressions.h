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

inline string_view GetEnumText(BoundUnaryOperatorKind kind)
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

inline string_view GetEnumText(BoundBinaryOperatorKind kind)
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

inline string_view GetEnumText(BoundPostfixOperatorEnum kind)
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
	virtual const TypeSymbol& Type() const noexcept = 0;
};

class BoundErrorExpression final :public BoundExpression
{
public:
	// Inherited via BoundExpression
	const TypeSymbol& Type() const noexcept override { return GetTypeSymbol(TypeEnum::Error); }
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ErrorExpression; }
};

class BoundUnaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundUnaryOperatorKind _kind;
	TypeSymbol _operandType;
	TypeSymbol _resultType;
	bool _isUseful = true;

	BoundUnaryOperator(const enum SyntaxKind& synKind,
		const BoundUnaryOperatorKind& kind,
		const TypeSymbol& operandType, const TypeSymbol& resultType)
		:_syntaxKind(synKind), _kind(kind),
		_operandType(operandType), _resultType(resultType)
	{
	}

	BoundUnaryOperator(const enum SyntaxKind& synKind,
		const BoundUnaryOperatorKind& kind, const TypeSymbol& operandType)
		: BoundUnaryOperator(synKind, kind, operandType, operandType)
	{
	}

	BoundUnaryOperator()
		: BoundUnaryOperator(SyntaxKind::BadToken, BoundUnaryOperatorKind::Identity,
			GetTypeSymbol(TypeEnum::Error))
	{
		_isUseful = false;
	}

	static const vector<BoundUnaryOperator> operators;

public:
	constexpr SyntaxKind SyntaxKind()const noexcept { return _syntaxKind; }
	constexpr BoundUnaryOperatorKind Kind()const noexcept { return _kind; }
	constexpr const TypeSymbol& OperandType()const { return _operandType; }
	constexpr const TypeSymbol& Type()const { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundUnaryOperator Bind(const enum SyntaxKind& synKind,
		const TypeSymbol& type)
	{
		for (const auto& op : operators)
		{
			if (op.SyntaxKind() == synKind && op.OperandType() == type)
				return op;
		}
		return BoundUnaryOperator();
	}
};

inline const vector<BoundUnaryOperator> BoundUnaryOperator::operators = {
	BoundUnaryOperator(SyntaxKind::BangToken, BoundUnaryOperatorKind::LogicalNegation,
						GetTypeSymbol(TypeEnum::Bool)),
	BoundUnaryOperator(SyntaxKind::PlusToken, BoundUnaryOperatorKind::Identity,
						GetTypeSymbol(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::MinusToken, BoundUnaryOperatorKind::Negation,
						GetTypeSymbol(TypeEnum::Int)),
	BoundUnaryOperator(SyntaxKind::TildeToken, BoundUnaryOperatorKind::OnesComplement,
						GetTypeSymbol(TypeEnum::Int))
};

class BoundUnaryExpression final : public BoundExpression
{
private:
	BoundUnaryOperator _op;
	shared_ptr<BoundExpression> _operand;

public:
	BoundUnaryExpression(const BoundUnaryOperator& op,
		const shared_ptr<BoundExpression>& operand)
		:_op(op), _operand(operand)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::UnaryExpression; }
	const TypeSymbol& Type() const noexcept override { return _op.Type(); }

	constexpr const BoundUnaryOperator& Op()const noexcept { return _op; }
	constexpr const shared_ptr<BoundExpression>& Operand()const noexcept { return _operand; }
};

class BoundBinaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundBinaryOperatorKind _kind;
	TypeSymbol _leftType;
	TypeSymbol _rightType;
	TypeSymbol _resultType;
	bool _isUseful = true;

	BoundBinaryOperator(const enum SyntaxKind& synKind,
		const BoundBinaryOperatorKind& kind,
		const TypeSymbol& left, const TypeSymbol& right, const TypeSymbol& result)
		:_syntaxKind(synKind), _kind(kind), _leftType(left), _rightType(right),
		_resultType(result)
	{
	}

	BoundBinaryOperator(const enum SyntaxKind& synKind,
		const BoundBinaryOperatorKind& kind,
		const TypeSymbol& operandType, const TypeSymbol& resultType)
		: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
	{
	}

	BoundBinaryOperator(const enum SyntaxKind& synKind,
		const BoundBinaryOperatorKind& kind, const TypeSymbol& type)
		: BoundBinaryOperator(synKind, kind, type, type, type)
	{
	}

	BoundBinaryOperator()
		: BoundBinaryOperator(SyntaxKind::BadToken, BoundBinaryOperatorKind::Addition,
			GetTypeSymbol(TypeEnum::Error))
	{
		_isUseful = false;
	}

	static const vector<BoundBinaryOperator> operators;

public:
	constexpr SyntaxKind SyntaxKind()const noexcept { return _syntaxKind; }
	constexpr BoundBinaryOperatorKind Kind()const noexcept { return _kind; }
	constexpr const TypeSymbol& LeftType()const noexcept { return _leftType; }
	constexpr const TypeSymbol& RightType()const noexcept { return _rightType; }
	constexpr const TypeSymbol& Type()const noexcept { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundBinaryOperator Bind(const enum SyntaxKind& synKind,
		const TypeSymbol& leftType, const TypeSymbol& rightType)
	{
		for (const auto& op : operators)
		{
			if (op.SyntaxKind() == synKind && op.LeftType() == leftType
				&& op.RightType() == rightType)
				return op;
		}
		return BoundBinaryOperator();
	}

};

inline const vector<BoundBinaryOperator> BoundBinaryOperator::operators = {
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

class BoundBinaryExpression final : public BoundExpression
{
private:
	shared_ptr<BoundExpression> _left;
	shared_ptr<BoundExpression> _right;
	BoundBinaryOperator _op;

public:
	BoundBinaryExpression(const shared_ptr<BoundExpression>& left,
		const BoundBinaryOperator& op, const shared_ptr<BoundExpression>& right)
		:_left(left), _right(right), _op(op)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BinaryExpression; }
	const TypeSymbol& Type() const noexcept override { return _op.Type(); }

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
	BoundAssignmentExpression(const shared_ptr<VariableSymbol>& variable,
		const shared_ptr<BoundExpression>& expression)
		:_variable(variable), _expression(expression)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::AssignmentExpression; }
	const TypeSymbol& Type() const noexcept override { return _expression->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

class BoundLiteralExpression final : public BoundExpression
{
private:
	ValueType _value;

public:
	explicit BoundLiteralExpression(const ValueType& value)
		: _value(value)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LiteralExpression; }
	const TypeSymbol& Type() const noexcept override { return _value.Type(); }

	constexpr const ValueType& Value()const noexcept { return _value; }
};

class BoundVariableExpression final : public BoundExpression
{
private:
	shared_ptr<VariableSymbol> _variable;

public:
	explicit BoundVariableExpression(const shared_ptr<VariableSymbol>& variable)
		: _variable(variable)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableExpression; }
	const TypeSymbol& Type() const noexcept override { return _variable->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
};

class BoundCallExpression final :public BoundExpression
{
private:
	shared_ptr<FunctionSymbol> _function;
	vector<shared_ptr<BoundExpression>> _arguments;

public:
	BoundCallExpression(const shared_ptr<FunctionSymbol>& function,
		const vector<shared_ptr<BoundExpression>>& arguments)
		:_function(function), _arguments(arguments)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::CallExpression; }
	const TypeSymbol& Type() const noexcept override { return _function->Type(); }

	constexpr const shared_ptr<FunctionSymbol>& Function()const noexcept { return _function; }
	constexpr const vector<shared_ptr<BoundExpression>>& Arguments()const noexcept { return _arguments; }
};

class BoundConversionExpression final :public BoundExpression
{
private:
	TypeSymbol _type;
	shared_ptr<BoundExpression> _expression;

public:
	BoundConversionExpression(const TypeSymbol& type,
		const shared_ptr<BoundExpression>& expression)
		:_type(type), _expression(expression)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConversionExpression; }
	const TypeSymbol& Type() const noexcept override { return _type; }

	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

class BoundPostfixExpression final :public BoundExpression
{
private:
	shared_ptr<VariableSymbol> _variable;
	BoundPostfixOperatorEnum _kind;
	shared_ptr<BoundExpression> _expression;

public:
	BoundPostfixExpression(const shared_ptr<VariableSymbol>& variable,
		const BoundPostfixOperatorEnum& kind,
		const shared_ptr<BoundExpression>& expression)
		:_variable(variable), _kind(kind), _expression(expression)
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::PostfixExpression; }
	const TypeSymbol& Type() const noexcept override { return _variable->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
	constexpr BoundPostfixOperatorEnum OperatorKind()const noexcept { return _kind; }
	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

}//MCF
