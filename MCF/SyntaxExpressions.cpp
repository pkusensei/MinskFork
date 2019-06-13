#include "stdafx.h"
#include "SyntaxExpressions.h"

#include "ReflectionHelper.h"

namespace MCF {

const vector<const SyntaxNode*> AssignmentExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_identifierToken, _equalsToken, _expression);
}

const vector<const SyntaxNode*> UnaryExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_operatorToken, _operand);
}

const vector<const SyntaxNode*> BinaryExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_left, _operatorToken, _right);
}

const vector<const SyntaxNode*> ParenthesizedExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_openParenthesisToken, _expression,
		_closeParenthesisToken);
}

const vector<const SyntaxNode*> LiteralExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_literalToken);
}

const vector<const SyntaxNode*> NameExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_identifierToken);
}

const vector<const SyntaxNode*> CallExpressionSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<const SyntaxNode>(_identifier, _openParenthesisToken);
	auto nodes = _arguments.GetWithSeparators();
	result.insert(result.end(), nodes.begin(), nodes.end());
	auto rest = MakeVecOfRaw<const SyntaxNode>(_closeParenthesisToken);
	result.insert(result.end(), rest.begin(), rest.end());
	return result;
}

const vector<const SyntaxNode*> PostfixExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_identifier, _op, _expression);
}

}//MCF
