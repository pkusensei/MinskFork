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
	ColonToken,
	CommaToken,
	IdentifierToken,

	// Keywords
	BreakKeyword,
	ContinueKeyword,
	ElseKeyword,
	FalseKeyword,
	ForKeyword,
	FunctionKeyword,
	IfKeyword,
	LetKeyword,
	ReturnKeyword,
	ToKeyword,
	TrueKeyword,
	VarKeyword,
	WhileKeyword,
	DoKeyword,
	UsingKeyworld,

	// Nodes
	CompilationUnit,
	FunctionDeclaration,
	GlobalStatement,
	Parameter,
	TypeClause,
	ElseClause,
	UsingDirective,

	// Statements
	BlockStatement,
	VariableDeclaration,
	IfStatement,
	WhileStatement,
	DoWhileStatement,
	ForStatement,
	BreakStatement,
	ContinueStatement,
	ReturnStatement,
	ExpressionStatement,

	// Expressions
	LiteralExpression,
	NameExpression,
	UnaryExpression,
	BinaryExpression,
	ParenthesizedExpression,
	AssignmentExpression,
	CallExpression,
	PostfixExpression,
};

MCF_API string_view GetSyntaxKindName(SyntaxKind kind);
extern "C" MCF_API const vector<SyntaxKind> AllSyntaxKinds; // NOTE global constant

SyntaxKind GetKeywordKind(string_view text) noexcept;
MCF_API string_view GetText(SyntaxKind kind);
MCF_API int GetUnaryOperatorPrecedence(SyntaxKind kind)noexcept;
MCF_API int GetBinaryOperatorPrecedence(SyntaxKind kind)noexcept;
MCF_API const vector<SyntaxKind>& GetUnaryOperatorKinds();
MCF_API const vector<SyntaxKind>& GetBinaryOperatorKinds();

}//MCF