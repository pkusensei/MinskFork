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
	ElseKeyword,
	FalseKeyword,
	ForKeyword, 
	FunctionKeyword,
	IfKeyword,
	LetKeyword,
	ToKeyword,
	TrueKeyword,
	VarKeyword,
	WhileKeyword,
	DoKeyword,

	// Nodes
	CompilationUnit,
	FunctionDeclaration,
	GlobalStatement,
	Parameter,
	TypeClause,
	ElseClause,

	// Statements
	BlockStatement,
	VariableDeclaration,
	IfStatement,
	WhileStatement,
	DoWhileStatement,
	ForStatement,
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

MCF_API string GetSyntaxKindName(const SyntaxKind& kind);
extern "C" MCF_API const vector<SyntaxKind> AllSyntaxKinds; // NOTE global constant

SyntaxKind GetKeywordKind(const string& text) noexcept;
MCF_API string GetText(const SyntaxKind& kind);
MCF_API int GetUnaryOperatorPrecedence(const SyntaxKind& kind)noexcept;
MCF_API int GetBinaryOperatorPrecedence(const SyntaxKind& kind)noexcept;
MCF_API vector<SyntaxKind> GetUnaryOperatorKinds();
MCF_API vector<SyntaxKind> GetBinaryOperatorKinds();

}//MCF