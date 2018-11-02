#pragma once

#include <typeindex>
#include "common.h"

namespace MCF {

enum class BoundNodeKind
{
	LiteralExpression,
	VariableExpression,
	AssignmentExpression,
	UnaryExpression,
	BinaryExpression,

	EmptyExpression //HACK
};

enum class BoundUnaryOperatorKind
{
	Identity,
	Negation,
	LogicalNegation
};

enum class BoundBinaryOperatorKind
{
	Addition,
	Subtraction,
	Multiplication,
	Division,
	LogicalAnd,
	LogicalOr,
	Equals,
	NotEquals
};

class BoundNode
{
public:
	virtual ~BoundNode() = default;
	virtual BoundNodeKind Kind() const = 0;
};

class BoundExpression :public BoundNode
{
	// HACK
public:
	virtual ~BoundExpression() = default;
	// Inherited via BoundNode
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::EmptyExpression; }
	virtual const type_index Type() const { return typeid(std::monostate); }
};

class BoundUnaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundUnaryOperatorKind _kind;
	type_index _operandType;
	type_index _resultType;

	BoundUnaryOperator(SyntaxKind synKind, BoundUnaryOperatorKind kind,
					   type_index operandType, type_index resultType);
	BoundUnaryOperator(SyntaxKind synKind, BoundUnaryOperatorKind kind,
					   type_index operandType);
	BoundUnaryOperator();
	static BoundUnaryOperator _operators[3];
public:
	BoundUnaryOperator(const BoundUnaryOperator&) = default;

	SyntaxKind SyntaxKind()const { return _syntaxKind; }
	BoundUnaryOperatorKind Kind()const { return _kind; }
	type_index OperandType()const { return _operandType; }
	type_index Type()const { return _resultType; }

	static BoundUnaryOperator Bind(enum SyntaxKind synKind, type_index type);
};

class BoundUnaryExpression final : public BoundExpression
{
private:
	unique_ptr<BoundUnaryOperator> _op;
	unique_ptr<BoundExpression> _operand;
public:
	BoundUnaryExpression(const BoundUnaryOperator& op, unique_ptr<BoundExpression>& operand);
	virtual ~BoundUnaryExpression() = default;
	BoundUnaryExpression(BoundUnaryExpression&& other);

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::UnaryExpression; }
	virtual const type_index Type() const override { return _op->Type(); }

	BoundUnaryOperator* Op()const { return _op.get(); }
	BoundExpression* Operand()const { return _operand.get(); }
};

class BoundBinaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundBinaryOperatorKind _kind;
	type_index _leftType;
	type_index _rightType;
	type_index _resultType;

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
						type_index left, type_index right, type_index result);
	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
						type_index operandType, type_index resultType);
	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind, type_index type);
	BoundBinaryOperator();

	static BoundBinaryOperator _operators[10];
public:
	BoundBinaryOperator(const BoundBinaryOperator&) = default;

	SyntaxKind SyntaxKind()const { return _syntaxKind; }
	BoundBinaryOperatorKind Kind()const { return _kind; }
	type_index LeftType()const { return _leftType; }
	type_index RightType()const { return _rightType; }
	type_index Type()const { return _resultType; }

	static BoundBinaryOperator Bind(enum SyntaxKind synKind, type_index leftType, type_index rightType);
};

class BoundBinaryExpression final : public BoundExpression
{
private:
	unique_ptr<BoundExpression> _left;
	unique_ptr<BoundExpression> _right;
	unique_ptr<BoundBinaryOperator> _op;

public:
	BoundBinaryExpression(unique_ptr<BoundExpression>& left, const BoundBinaryOperator& op, unique_ptr<BoundExpression>& right);
	virtual ~BoundBinaryExpression() = default;
	BoundBinaryExpression(BoundBinaryExpression&& other);
	
	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::BinaryExpression; }
	virtual const type_index Type() const override { return typeid(_op->Type()); }

	BoundExpression* Left()const { return _left.get(); }
	BoundExpression* Right()const { return _right.get(); }
	BoundBinaryOperator* Op()const { return _op.get(); }
};

class BoundAssignmentExpression final : public BoundExpression
{
public:
	virtual ~BoundAssignmentExpression() = default;
	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::AssignmentExpression; }
	virtual const type_index Type() const override { return typeid(std::monostate); }
};


//class Binder
//{
//public:
//	Binder();
//	~Binder();
//};

}//MCF

