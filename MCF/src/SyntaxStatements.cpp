#include "SyntaxStatements.h"

#include "ReflectionHelper.h"

namespace MCF {

const vector<const SyntaxNode*> BlockStatementSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<SyntaxNode>(OpenBraceToken);
	auto rest = MakeVecOfRaw<SyntaxNode, StatementSyntax>(
		Statements.begin(), Statements.end());
	result.insert(result.end(), rest.begin(), rest.end());
	auto close = MakeVecOfRaw<SyntaxNode>(CloseBraceToken);
	result.insert(result.end(), close.begin(), close.end());
	return result;
}

const vector<const SyntaxNode*> TypeClauseSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(ColonToken, Identifier);
}

const vector<const SyntaxNode*> VariableDeclarationSyntax::GetChildren() const
{
	if (TypeClause.has_value())
		return MakeVecOfRaw<SyntaxNode>(Keyword, Identifier, *TypeClause,
										EqualsToken, Initializer);
	else
		return MakeVecOfRaw<SyntaxNode>(Keyword, Identifier,
										EqualsToken, Initializer);
}

const vector<const SyntaxNode*> ElseClauseSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(ElseKeyword, ElseStatement);
}

const vector<const SyntaxNode*> IfStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(IfKeyword, Condition,
									ThenStatement, ElseClause);
}

const vector<const SyntaxNode*> WhileStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(WhileKeyword, Condition, Body);
}

const vector<const SyntaxNode*> DoWhileStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(DoKeyword, Body, WhileKeyword, Condition);
}

const vector<const SyntaxNode*> ForStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(Keyword, Identifier, EqualsToken,
									LowerBound, ToKeyword, UpperBound, Body);
}

const vector<const SyntaxNode*> BreakStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(Keyword);
}

const vector<const SyntaxNode*> ContinueStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(Keyword);
}

const vector<const SyntaxNode*> ReturnStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(Keyword, Expression);
}

const vector<const SyntaxNode*> ExpressionStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(Expression);
}

}//MCF
