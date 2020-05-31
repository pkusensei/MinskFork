#pragma once

#include "common.h"

namespace MCF {

class SyntaxNode;

enum class BoundNodeKind
{
	// Statements
	BlockStatement,
	NopStatement,
	VariableDeclaration,
	IfStatement,
	WhileStatement,
	DoWhileStatement,
	ForStatement,
	LabelStatement,
	GotoStatement,
	ConditionalGotoStatement,
	ReturnStatement,
	ExpressionStatement,

	// Expressions
	ErrorExpression,
	LiteralExpression,
	VariableExpression,
	AssignmentExpression,
	CompoundAssignmentExpression,
	UnaryExpression,
	BinaryExpression,
	CallExpression,
	ConversionExpression,
	PostfixExpression,

};

string_view nameof(BoundNodeKind kind);

class BoundNode
{
private:
	const SyntaxNode* _syntax;

protected:
	BoundNode(const SyntaxNode* syntax)noexcept
		:_syntax(syntax)
	{
	}

public:
	virtual ~BoundNode() = default;
	virtual BoundNodeKind Kind() const noexcept = 0;

	constexpr const SyntaxNode* Syntax()const noexcept { return _syntax; }
	void WriteTo(std::ostream& out)const;
	string ToString() const;
};

}//MCF