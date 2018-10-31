#pragma once

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
	virtual size_t TypeHash() const { return NULL; }
};

//class Binder
//{
//public:
//	Binder();
//	~Binder();
//};

}//MCF

