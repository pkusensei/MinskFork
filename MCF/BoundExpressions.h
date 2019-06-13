#pragma once

#include "BoundNode.h"
#include "Symbols.h"

namespace MCF {

enum class SyntaxKind;

enum class BoundUnaryOperatorKind
{
	Identity,
	Negation,
	LogicalNegation,
	OnesComplement
};

string GetEnumText(const BoundUnaryOperatorKind& kind);

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

string GetEnumText(const BoundBinaryOperatorKind& kind);

enum class BoundPostfixOperatorEnum
{
	Increment,
	Decrement,
};

string GetEnumText(const BoundPostfixOperatorEnum& kind);

class BoundExpression :public BoundNode
{
public:
	virtual TypeSymbol Type() const = 0;
};

class BoundErrorExpression final :public BoundExpression
{
public:
	// Inherited via BoundExpression
	TypeSymbol Type()const override { return GetTypeSymbol(TypeEnum::Error); }
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

	BoundUnaryOperator(const SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
		const TypeSymbol& operandType, const TypeSymbol& resultType);
	BoundUnaryOperator(const SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
		const TypeSymbol& operandType);
	BoundUnaryOperator();

	static const vector<BoundUnaryOperator>& Operators();

public:
	constexpr SyntaxKind SyntaxKind()const noexcept { return _syntaxKind; }
	constexpr BoundUnaryOperatorKind Kind()const noexcept { return _kind; }
	const TypeSymbol& OperandType()const { return _operandType; }
	const TypeSymbol& Type()const { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundUnaryOperator Bind(const enum SyntaxKind& synKind,
		const TypeSymbol& type);
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
	BoundUnaryExpression(BoundUnaryExpression&&) = default;
	BoundUnaryExpression& operator=(BoundUnaryExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::UnaryExpression; }
	TypeSymbol Type() const override { return _op.Type(); }

	const BoundUnaryOperator& Op()const noexcept { return _op; }
	const shared_ptr<BoundExpression>& Operand()const noexcept { return _operand; }
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

	BoundBinaryOperator(const SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
		const TypeSymbol& left, const TypeSymbol& right, const TypeSymbol& result);
	BoundBinaryOperator(const SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
		const TypeSymbol& operandType, const TypeSymbol& resultType);
	BoundBinaryOperator(const SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
		const TypeSymbol& type);
	BoundBinaryOperator();

	static const vector<BoundBinaryOperator>& Operators();

public:
	constexpr SyntaxKind SyntaxKind()const noexcept { return _syntaxKind; }
	constexpr BoundBinaryOperatorKind Kind()const noexcept { return _kind; }
	const TypeSymbol& LeftType()const { return _leftType; }
	const TypeSymbol& RightType()const { return _rightType; }
	const TypeSymbol& Type()const { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundBinaryOperator Bind(const enum SyntaxKind& synKind,
		const TypeSymbol& leftType, const TypeSymbol& rightType);
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
	BoundBinaryExpression(BoundBinaryExpression&&) = default;
	BoundBinaryExpression& operator=(BoundBinaryExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BinaryExpression; }
	TypeSymbol Type() const override { return _op.Type(); }

	const shared_ptr<BoundExpression> Left()const noexcept { return _left; }
	const shared_ptr<BoundExpression> Right()const noexcept { return _right; }
	const BoundBinaryOperator& Op()const noexcept { return _op; }
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
	BoundAssignmentExpression(BoundAssignmentExpression&&) = default;
	BoundAssignmentExpression& operator=(BoundAssignmentExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::AssignmentExpression; }
	TypeSymbol Type() const override { return _expression->Type(); }

	const shared_ptr<VariableSymbol>& Variable()const { return _variable; }
	const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
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
	BoundLiteralExpression(BoundLiteralExpression&&) = default;
	BoundLiteralExpression& operator=(BoundLiteralExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LiteralExpression; }
	TypeSymbol Type() const override { return _value.Type(); }

	const ValueType& Value()const { return _value; }
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
	BoundVariableExpression(BoundVariableExpression&&) = default;
	BoundVariableExpression& operator=(BoundVariableExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableExpression; }
	TypeSymbol Type() const override { return _variable->Type(); }

	const shared_ptr<VariableSymbol>& Variable()const { return _variable; }
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
	BoundCallExpression(BoundCallExpression&&) = default;
	BoundCallExpression& operator=(BoundCallExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::CallExpression; }
	TypeSymbol Type() const override { return _function->Type(); }

	const shared_ptr<FunctionSymbol>& Function()const { return _function; }
	const vector<shared_ptr<BoundExpression>>& Arguments()const noexcept { return _arguments; }
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
	BoundConversionExpression(BoundConversionExpression&&) = default;
	BoundConversionExpression& operator=(BoundConversionExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConversionExpression; }
	TypeSymbol Type() const override { return _type; }

	const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
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
	BoundPostfixExpression(BoundPostfixExpression&&) = default;
	BoundPostfixExpression& operator=(BoundPostfixExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::PostfixExpression; }
	TypeSymbol Type() const override { return _variable->Type(); }

	const shared_ptr<VariableSymbol>& Variable()const { return _variable; }
	BoundPostfixOperatorEnum OperatorKind()const { return _kind; }
	const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

}//MCF
