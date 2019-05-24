#pragma once

#include "common.h"

namespace MCF {

enum class BoundNodeKind
{
	// Statements
	BlockStatement,
	VariableDeclaration,
	IfStatement,
	WhileStatement,
	DoWhileStatement,
	ForStatement,
	LabelStatement,
	GotoStatement,
	ConditionalGotoStatement,
	ExpressionStatement,

	// Expressions
	ErrorExpression,
	LiteralExpression,
	VariableExpression,
	AssignmentExpression,
	UnaryExpression,
	BinaryExpression,
	CallExpression,
	ConversionExpression,
	PostfixExpression,

};

string GetEnumText(const BoundNodeKind& kind);

class BoundNode
{
public:
	virtual ~BoundNode() = default;
	virtual BoundNodeKind Kind() const = 0;

	void WriteTo(std::ostream& out)const;
	string ToString() const;
};

}//MCF