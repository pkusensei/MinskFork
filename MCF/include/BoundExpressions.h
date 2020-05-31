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

class BoundExpression :public BoundNode
{
protected:
	explicit BoundExpression(const SyntaxNode* syntax)noexcept
		:BoundNode(syntax)
	{
	}

public:
	virtual const TypeSymbol& Type() const = 0;
	virtual const BoundConstant& ConstantValue()const noexcept { return NULL_VALUE; }
};

class BoundErrorExpression final :public BoundExpression
{
public:
	explicit BoundErrorExpression(const SyntaxNode* syntax)noexcept
		:BoundExpression(syntax)
	{
	}

	// Inherited via BoundExpression
	const TypeSymbol& Type() const override { return TYPE_ERROR; }
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ErrorExpression; }
};

class BoundUnaryOperator final
{
private:
	TypeSymbol _operandType;
	TypeSymbol _resultType;
	SyntaxKind _syntaxKind;
	BoundUnaryOperatorKind _kind;
	bool _isUseful = true;

	BoundUnaryOperator(SyntaxKind synKind, BoundUnaryOperatorKind kind,
					   const TypeSymbol& operandType, const TypeSymbol& resultType)noexcept
		: _operandType(operandType), _resultType(resultType),
		_syntaxKind(synKind), _kind(kind)
	{
	}

	BoundUnaryOperator(SyntaxKind synKind,
					   BoundUnaryOperatorKind kind, const TypeSymbol& operandType)noexcept
		: BoundUnaryOperator(synKind, kind, operandType, operandType)
	{
	}

	BoundUnaryOperator()noexcept
		: BoundUnaryOperator(SyntaxKind::BadToken, BoundUnaryOperatorKind::Identity,
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
	TypeSymbol _leftType;
	TypeSymbol _rightType;
	TypeSymbol _resultType;
	SyntaxKind _syntaxKind;
	BoundBinaryOperatorKind _kind;
	bool _isUseful = true;

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
						const TypeSymbol& left, const TypeSymbol& right, const TypeSymbol& result)noexcept
		:_leftType(left), _rightType(right), _resultType(result),
		_syntaxKind(synKind), _kind(kind)
	{
	}

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
						const TypeSymbol& operandType, const TypeSymbol& resultType)noexcept
		: BoundBinaryOperator(synKind, kind, operandType, operandType, resultType)
	{
	}

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
						const TypeSymbol& type)noexcept
		: BoundBinaryOperator(synKind, kind, type, type, type)
	{
	}

	BoundBinaryOperator()noexcept
		: BoundBinaryOperator(SyntaxKind::BadToken, BoundBinaryOperatorKind::Addition,
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

[[nodiscard]] BoundConstant Fold(const BoundUnaryOperator& op, const BoundExpression& operand);
[[nodiscard]] BoundConstant Fold(const BoundExpression& left, const BoundBinaryOperator& op,
								 const BoundExpression& right);

class BoundUnaryExpression final : public BoundExpression
{
private:
	BoundUnaryOperator _op;
	BoundConstant _constant;
	shared_ptr<BoundExpression> _operand;

public:
	explicit BoundUnaryExpression(const SyntaxNode* syntax,
								  const BoundUnaryOperator& op,
								  shared_ptr<BoundExpression> operand)noexcept
		:BoundExpression(syntax),
		_op(op), _constant(Fold(op, *operand)),
		_operand(std::move(operand))
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
	BoundBinaryOperator _op;
	BoundConstant _constant;
	shared_ptr<BoundExpression> _left;
	shared_ptr<BoundExpression> _right;

public:
	explicit BoundBinaryExpression(const SyntaxNode* syntax,
								   shared_ptr<BoundExpression> left,
								   BoundBinaryOperator op,
								   shared_ptr<BoundExpression> right)noexcept
		:BoundExpression(syntax),
		_op(std::move(op)), _constant(Fold(*left, op, *right)),
		_left(std::move(left)), _right(std::move(right))
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
	explicit BoundAssignmentExpression(const SyntaxNode* syntax,
									   shared_ptr<VariableSymbol> variable,
									   shared_ptr<BoundExpression> expression)noexcept
		:BoundExpression(syntax),
		_variable(std::move(variable)), _expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::AssignmentExpression; }
	const TypeSymbol& Type() const  override { return _expression->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

class BoundCompoundAssignmentExpression final :public BoundExpression
{
private:
	BoundBinaryOperator _op;
	shared_ptr<VariableSymbol> _variable;
	shared_ptr<BoundExpression> _expression;

public:
	explicit BoundCompoundAssignmentExpression(const SyntaxNode* syntax,
											   shared_ptr<VariableSymbol> variable,
											   BoundBinaryOperator op,
											   shared_ptr<BoundExpression> expression)noexcept
		:BoundExpression(syntax),
		_op(std::move(op)),
		_variable(std::move(variable)), _expression(std::move(expression))
	{
	}

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::CompoundAssignmentExpression; }
	const TypeSymbol& Type() const  override { return _expression->Type(); }

	constexpr const shared_ptr<VariableSymbol>& Variable()const noexcept { return _variable; }
	constexpr const BoundBinaryOperator& Op()const noexcept { return _op; }
	constexpr const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

class BoundLiteralExpression final : public BoundExpression
{
private:
	BoundConstant _constant;
	TypeSymbol _type;

public:
	explicit BoundLiteralExpression(const SyntaxNode* syntax, ValueType value)
		:BoundExpression(syntax),
		_constant(std::move(value)), _type(_constant.Type())
	{
		if (!value.HasValue())
			throw std::invalid_argument("Unexpected literal: " + value.ToString());
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
	explicit BoundVariableExpression(const SyntaxNode* syntax,
									 shared_ptr<VariableSymbol> variable)noexcept
		:BoundExpression(syntax),
		_variable(std::move(variable))
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
	vector<shared_ptr<BoundExpression>> _arguments;
	shared_ptr<FunctionSymbol> _function;

public:
	explicit BoundCallExpression(const SyntaxNode* syntax,
								 shared_ptr<FunctionSymbol> function,
								 vector<shared_ptr<BoundExpression>> arguments)noexcept
		:BoundExpression(syntax),
		_arguments(std::move(arguments)), _function(std::move(function))
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
	explicit BoundConversionExpression(const SyntaxNode* syntax,
									   const TypeSymbol& type,
									   shared_ptr<BoundExpression> expression)noexcept
		:BoundExpression(syntax),
		_type(type), _expression(std::move(expression))
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
	shared_ptr<BoundExpression> _expression;
	BoundPostfixOperatorEnum _kind;

public:
	explicit BoundPostfixExpression(const SyntaxNode* syntax,
									shared_ptr<VariableSymbol> variable,
									BoundPostfixOperatorEnum kind,
									shared_ptr<BoundExpression> expression)noexcept
		:BoundExpression(syntax),
		_variable(std::move(variable)), _expression(std::move(expression)), _kind(kind)
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
