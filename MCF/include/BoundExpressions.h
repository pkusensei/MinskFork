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

class BoundExpression :public BoundNode
{
public:
	virtual const TypeSymbol& Type() const = 0;
	virtual const BoundConstant& ConstantValue()const noexcept { return NULL_VALUE; }
};

class BoundErrorExpression final :public BoundExpression
{
public:
	// Inherited via BoundExpression
	const TypeSymbol& Type() const override { return TYPE_ERROR; }
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

	BoundUnaryOperator(SyntaxKind synKind, BoundUnaryOperatorKind kind,
		const TypeSymbol& operandType, const TypeSymbol& resultType)
		:_syntaxKind(synKind), _kind(kind),
		_operandType(operandType), _resultType(resultType)
	{
	}

	BoundUnaryOperator(SyntaxKind synKind,
		BoundUnaryOperatorKind kind, const TypeSymbol& operandType)
		: BoundUnaryOperator(synKind, kind, operandType, operandType)
	{
	}

	BoundUnaryOperator()
		: BoundUnaryOperator(SyntaxKind::BadTokenTrivia, BoundUnaryOperatorKind::Identity,
			TYPE_ERROR)
	{
		_isUseful = false;
	}

	static const std::array<BoundUnaryOperator, 4> operators;

public:
	constexpr SyntaxKind SynKind()const noexcept { return _syntaxKind; }
	constexpr BoundUnaryOperatorKind Kind()const noexcept { return _kind; }
	constexpr const TypeSymbol& OperandType()const { return _operandType; }
	constexpr const TypeSymbol& Type()const { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundUnaryOperator Bind(SyntaxKind synKind, const TypeSymbol& type);
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

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
		const TypeSymbol& left, const TypeSymbol& right, const TypeSymbol& result)
		:_syntaxKind(synKind), _kind(kind), _leftType(left), _rightType(right),
		_resultType(result)
	{
	}

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
		const TypeSymbol& operandType, const TypeSymbol& resultType)
		: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
	{
	}

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
		const TypeSymbol& type)
		: BoundBinaryOperator(synKind, kind, type, type, type)
	{
	}

	BoundBinaryOperator()
		: BoundBinaryOperator(SyntaxKind::BadTokenTrivia, BoundBinaryOperatorKind::Addition,
			TYPE_ERROR)
	{
		_isUseful = false;
	}

	static const std::array<BoundBinaryOperator, 26> operators;

public:
	constexpr SyntaxKind SynKind()const noexcept { return _syntaxKind; }
	constexpr BoundBinaryOperatorKind Kind()const noexcept { return _kind; }
	constexpr const TypeSymbol& LeftType()const noexcept { return _leftType; }
	constexpr const TypeSymbol& RightType()const noexcept { return _rightType; }
	constexpr const TypeSymbol& Type()const noexcept { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundBinaryOperator Bind(SyntaxKind synKind,
		const TypeSymbol& leftType, const TypeSymbol& rightType);

};

BoundConstant ComputeConstant(const BoundUnaryOperator& op, const BoundExpression& operand);
BoundConstant ComputeConstant(const BoundExpression& left, const BoundBinaryOperator& op,
	const BoundExpression& right);

class BoundUnaryExpression final : public BoundExpression
{
private:
	BoundConstant _constant;
	BoundUnaryOperator _op;
	shared_ptr<BoundExpression> _operand;

public:
	BoundUnaryExpression(const BoundUnaryOperator& op,
		shared_ptr<BoundExpression> operand)
		:_constant(ComputeConstant(op, *operand)),
		_op(op), _operand(std::move(operand))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::UnaryExpression; }
	const TypeSymbol& Type() const  override { return _op.Type(); }
	const BoundConstant& ConstantValue()const noexcept override { return _constant; }

	constexpr const BoundUnaryOperator& Op()const noexcept { return _op; }
	constexpr const shared_ptr<BoundExpression>& Operand()const noexcept { return _operand; }
};

class BoundBinaryExpression final : public BoundExpression
{
private:
	BoundConstant _constant;
	shared_ptr<BoundExpression> _left;
	shared_ptr<BoundExpression> _right;
	BoundBinaryOperator _op;

public:
	BoundBinaryExpression(shared_ptr<BoundExpression> left,
		BoundBinaryOperator op, shared_ptr<BoundExpression> right)
		:_constant(ComputeConstant(*left, op, *right)),
		_left(std::move(left)), _right(std::move(right)), _op(std::move(op))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BinaryExpression; }
	const TypeSymbol& Type() const override { return _op.Type(); }
	const BoundConstant& ConstantValue()const noexcept override { return _constant; }

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
	const TypeSymbol& Type() const  override { return _expression->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

class BoundLiteralExpression final : public BoundExpression
{
private:
	TypeSymbol _type;
	BoundConstant _constant;

public:
	explicit BoundLiteralExpression(ValueType value)
		:_type(value.Type()), _constant(std::move(value))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LiteralExpression; }
	const TypeSymbol& Type() const override { return _type; }
	const BoundConstant& ConstantValue()const noexcept override { return _constant; }

	constexpr const ValueType& Value()const noexcept { return _constant; }
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
	const TypeSymbol& Type() const override { return _variable->Type(); }
	const BoundConstant& ConstantValue()const noexcept override { return _variable->Constant(); }

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
	const TypeSymbol& Type() const override { return _function->Type(); }

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
		shared_ptr<BoundExpression> expression)
		:_type(type), _expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConversionExpression; }
	const TypeSymbol& Type() const override { return _type; }

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
	const TypeSymbol& Type() const override { return _variable->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
	constexpr BoundPostfixOperatorEnum OperatorKind()const noexcept { return _kind; }
	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

}//MCF
