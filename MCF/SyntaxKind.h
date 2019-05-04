#pragma once

#include "common.h"

namespace MCF {

enum class SyntaxKind
{
	// Tokens
	BadToken,
	EndOfFileToken,
	WhitespaceToken,
	NumberToken,
	StringToken,
	PlusToken,
	MinusToken,
	StarToken,
	SlashToken,
	PercentToken,
	BangToken,
	PlusPlusToken,
	MinusMinusToken,
	EqualsToken,
	TildeToken,
	HatToken,
	AmpersandToken,
	AmpersandAmpersandToken,
	PipeToken,
	PipePipeToken,
	EqualsEqualsToken,
	BangEqualsToken,
	LessToken,
	LessOrEqualsToken,
	GreaterToken,
	GreaterOrEqualsToken,
	OpenParenthesisToken,
	CloseParenthesisToken,
	OpenBraceToken,
	CloseBraceToken,
	IdentifierToken,

	// Keywords
	ElseKeyword,
	FalseKeyword,
	ForKeyword,
	IfKeyword,
	LetKeyword,
	ToKeyword,
	TrueKeyword,
	VarKeyword,
	WhileKeyword,

	// Nodes
	CompilationUnit,
	ElseClause,

	// Statements
	BlockStatement,
	VariableDeclaration,
	IfStatement,
	WhileStatement,
	ForStatement,
	ExpressionStatement,

	// Expressions
	LiteralExpression,
	NameExpression,
	UnaryExpression,
	BinaryExpression,
	ParenthesizedExpression,
	PostfixExpression,
	AssignmentExpression,
};

MCF_API string GetSyntaxKindName(const SyntaxKind& kind);
extern "C" MCF_API const vector<SyntaxKind> AllSyntaxKinds; // NOTE global constant

}//MCF