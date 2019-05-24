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
	virtual const vector<const BoundNode*> GetChildren() const = 0;
	// HACK will be ugly and dirty
	// pair: name of property + its value
	virtual const vector<std::pair<string, string>> GetProperties() const = 0;

	void WriteTo(std::ostream& out)const;
	string ToString() const;
};

}//MCF