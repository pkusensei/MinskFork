#pragma once

#include "common.h"

namespace MCF {

enum class SyntaxKind
{
	BadToken = 0,

	// Trivia
	SkippedTextTrivia,
	LineBreakTrivia,
	WhitespaceTrivia,
	SingleLineCommentTrivia,
	MultiLineCommentTrivia,

	// Tokens
	EndOfFileToken,
	NumberToken,
	StringToken,
	PlusToken,
	PlusEqualsToken,
	MinusToken,
	MinusEqualsToken,
	StarToken,
	StarEqualsToken,
	SlashToken,
	SlashEqualsToken,
	PercentToken, //
	PercentEqualsToken,
	BangToken,
	PlusPlusToken, //
	MinusMinusToken, //
	EqualsToken,
	TildeToken,
	HatToken,
	HatEqualsToken,
	AmpersandToken,
	AmpersandEqualsToken,
	AmpersandAmpersandToken,
	PipeToken,
	PipeEqualsToken,
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
	UsingKeyword, //

	// Nodes
	CompilationUnit,
	FunctionDeclaration,
	GlobalStatement,
	Parameter,
	TypeClause,
	ElseClause,
	UsingDirective, //

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
	CompoundAssignmentExpression,
	ParenthesizedExpression,
	AssignmentExpression,
	CallExpression,
	PostfixExpression,
};

constexpr size_t SYNTAXKIND_COUNT
= 1 + std::underlying_type_t<SyntaxKind>(SyntaxKind::PostfixExpression);

// NOTE global constant
extern "C" MCF_API const std::array<SyntaxKind, SYNTAXKIND_COUNT> & AllSyntaxKinds;

MCF_API string_view nameof(SyntaxKind kind)noexcept;

SyntaxKind GetKeywordKind(string_view text) noexcept;
MCF_API string_view GetText(SyntaxKind kind);

MCF_API bool IsComment(SyntaxKind kind)noexcept;
bool IsTrivia(SyntaxKind kind)noexcept;
MCF_API bool IsKeyword(SyntaxKind kind);
MCF_API bool IsToken(SyntaxKind kind);

MCF_API int GetUnaryOperatorPrecedence(SyntaxKind kind)noexcept;
MCF_API int GetBinaryOperatorPrecedence(SyntaxKind kind)noexcept;
MCF_API const vector<SyntaxKind>& GetUnaryOperatorKinds();
MCF_API const vector<SyntaxKind>& GetBinaryOperatorKinds();

SyntaxKind GetBinaryOperatorOfAssignmentOperator(SyntaxKind kind);

}//MCF