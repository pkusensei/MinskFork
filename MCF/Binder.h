#pragma once

namespace MCF {

enum class BoundNodeKind
{
	LiteralExpression,
	VariableExpression,
	AssignmentExpression,
	UnaryExpression,
	BinaryExpression
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

//class Binder
//{
//public:
//	Binder();
//	~Binder();
//};

}//MCF

