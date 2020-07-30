#include "SyntaxExpressions.h"

#include "ReflectionHelper.h"

namespace MCF {

const vector<const SyntaxNode*> AssignmentExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(IdentifierToken, AssignmentToken, Expression);
}

const vector<const SyntaxNode*> UnaryExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(OperatorToken, Operand);
}

const vector<const SyntaxNode*> BinaryExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(Left, OperatorToken, Right);
}

const vector<const SyntaxNode*> ParenthesizedExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(OpenParenthesisToken, Expression,
									CloseParenthesisToken);
}

const vector<const SyntaxNode*> LiteralExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(LiteralToken);
}

const vector<const SyntaxNode*> NameExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(IdentifierToken);
}

const vector<const SyntaxNode*> CallExpressionSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<SyntaxNode>(Identifier, OpenParenthesisToken);
	auto nodes = Arguments.GetWithSeparators();
	result.insert(result.end(), nodes.begin(), nodes.end());
	auto rest = MakeVecOfRaw<SyntaxNode>(CloseParenthesisToken);
	result.insert(result.end(), rest.begin(), rest.end());
	return result;
}

const vector<const SyntaxNode*> PostfixExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(Identifier, Op, Expression);
}

}//MCF
