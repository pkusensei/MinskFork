#include "stdafx.h"
#include "SyntaxStatements.h"

#include "ReflectionHelper.h"

namespace MCF {

const vector<const SyntaxNode*> BlockStatementSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<const SyntaxNode>(_openBraceToken);
	auto rest = MakeVecOfRaw<const SyntaxNode, const StatementSyntax>(
		_statements.begin(), _statements.end());
	result.insert(result.end(), rest.begin(), rest.end());
	auto close = MakeVecOfRaw<const SyntaxNode>(_closeBraceToken);
	result.insert(result.end(), close.begin(), close.end());
	return result;
}

const vector<const StatementSyntax*> BlockStatementSyntax::Statements() const
{
	auto result = vector<const StatementSyntax*>();
	for (const auto& it : _statements)
		result.emplace_back(it.get());
	return result;
}

const vector<const SyntaxNode*> TypeClauseSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_colonToken, _identifier);
}

const vector<const SyntaxNode*> VariableDeclarationSyntax::GetChildren() const
{
	if (_typeClause.has_value())
		return MakeVecOfRaw<const SyntaxNode>(_keyword, _identifier, *_typeClause,
			_equalsToken, _initializer);
	else
		return MakeVecOfRaw<const SyntaxNode>(_keyword, _identifier,
			_equalsToken, _initializer);
}

const vector<const SyntaxNode*> ElseClauseSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_elseKeyword, _elseStatement);
}

const vector<const SyntaxNode*> IfStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_ifKeyword, _condition,
		_thenStatement, _elseClause);
}

const vector<const SyntaxNode*> WhileStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_whileKeyword, _condition, _body);
}

const vector<const SyntaxNode*> DoWhileStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_doKeyword, _body, _whileKeyword, _condition);
}

const vector<const SyntaxNode*> ForStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_keyword, _identifier, _equalsToken,
		_lowerBound, _toKeyword, _upperBound, _body);
}

const vector<const SyntaxNode*> BreakStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_keyword);
}

const vector<const SyntaxNode*> ContinueStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_keyword);
}

const vector<const SyntaxNode*> ReturnStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_keyword, _expression);
}

const vector<const SyntaxNode*> ExpressionStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_expression);
}

}//MCF
