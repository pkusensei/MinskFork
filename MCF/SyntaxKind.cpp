#include "stdafx.h"

#include "SyntaxKind.h"

namespace MCF {

SyntaxKind& operator++(SyntaxKind& kind)
{
	auto tmp = std::underlying_type<SyntaxKind>::type(kind);
	kind = static_cast<SyntaxKind>(tmp + 1);
	return kind;
}

SyntaxKind & operator++(SyntaxKind & kind, int c)
{
	return ++kind;
}

const vector<SyntaxKind> GetAllSyntaxKinds()
{
	auto result = vector<SyntaxKind>();
	for (auto kind = MCF::SyntaxKind::BadToken;
		 kind != MCF::SyntaxKind::PostfixExpression; kind++)
		result.emplace_back(kind);
	result.shrink_to_fit();
	return result;
}

const vector<SyntaxKind> AllSyntaxKinds = GetAllSyntaxKinds();

string GetSyntaxKindName(const SyntaxKind& kind)
{
	switch (kind)
	{
		case SyntaxKind::BadToken:
			return "BadToken";
		case SyntaxKind::EndOfFileToken:
			return "EndOfFileToken";
		case SyntaxKind::WhitespaceToken:
			return "WhitespaceToken";
		case SyntaxKind::NumberToken:
			return "NumberToken";
		case SyntaxKind::StringToken:
			return "StringToken";
		case SyntaxKind::PlusToken:
			return "PlusToken";
		case SyntaxKind::MinusToken:
			return "MinusToken";
		case SyntaxKind::StarToken:
			return "StarToken";
		case SyntaxKind::SlashToken:
			return "SlashToken";
		case SyntaxKind::PercentToken:
			return "PercentToken";
		case SyntaxKind::BangToken:
			return "BangToken";
		case SyntaxKind::PlusPlusToken:
			return "PlusPlusToken";
		case SyntaxKind::MinusMinusToken:
			return "MinusMinusToken";
		case SyntaxKind::EqualsToken:
			return "EqualsToken";
		case SyntaxKind::TildeToken:
			return "TildeToken";
		case SyntaxKind::HatToken:
			return "HatToken";
		case SyntaxKind::AmpersandToken:
			return "AmpersandToken";
		case SyntaxKind::AmpersandAmpersandToken:
			return "AmpersandAmpersandToken";
		case SyntaxKind::PipeToken:
			return "PipeToken";
		case SyntaxKind::PipePipeToken:
			return "PipePipeToken";
		case SyntaxKind::EqualsEqualsToken:
			return "EqualsEqualsToken";
		case SyntaxKind::BangEqualsToken:
			return "BangEqualsToken";
		case SyntaxKind::LessToken:
			return "LessToken";
		case SyntaxKind::LessOrEqualsToken:
			return "LessOrEqualsToken";
		case SyntaxKind::GreaterToken:
			return "GreaterToken";
		case SyntaxKind::GreaterOrEqualsToken:
			return "GreaterOrEqualsToken";
		case SyntaxKind::OpenParenthesisToken:
			return "OpenParenthesisToken";
		case SyntaxKind::CloseParenthesisToken:
			return "CloseParenthesisToken";
		case SyntaxKind::OpenBraceToken:
			return "OpenBraceToken";
		case SyntaxKind::CloseBraceToken:
			return "CloseBraceToken";
		case SyntaxKind::CommaToken:
			return "CommaToken";
		case SyntaxKind::IdentifierToken:
			return "IdentifierToken";

		case SyntaxKind::ElseKeyword:
			return "ElseKeyword";
		case SyntaxKind::FalseKeyword:
			return "FalseKeyword";
		case SyntaxKind::ForKeyword:
			return "ForKeyword";
		case SyntaxKind::IfKeyword:
			return "IfKeyword";
		case SyntaxKind::LetKeyword:
			return "LetKeyword";
		case SyntaxKind::ToKeyword:
			return "ToKeyword";
		case SyntaxKind::TrueKeyword:
			return "TrueKeyword";
		case SyntaxKind::VarKeyword:
			return "VarKeyword";
		case SyntaxKind::WhileKeyword:
			return "WhileKeyword";

		case SyntaxKind::CompilationUnit:
			return "CompilationUnit";
		case SyntaxKind::ElseClause:
			return "ElseClause";

		case SyntaxKind::BlockStatement:
			return "BlockStatement";
		case SyntaxKind::VariableDeclaration:
			return "VariableDeclaration";
		case SyntaxKind::IfStatement:
			return "IfStatement";
		case SyntaxKind::WhileStatement:
			return "WhileStatement";
		case SyntaxKind::ForStatement:
			return "ForStatement";
		case SyntaxKind::ExpressionStatement:
			return "ExpressionStatement";

		case SyntaxKind::LiteralExpression:
			return "LiteralExpression";
		case SyntaxKind::NameExpression:
			return "NameExpression";
		case SyntaxKind::UnaryExpression:
			return "UnaryExpression";
		case SyntaxKind::BinaryExpression:
			return "BinaryExpression";
		case SyntaxKind::ParenthesizedExpression:
			return "ParenthesizedExpression";
		case SyntaxKind::CallExpression:
			return "CallExpression";
		case SyntaxKind::AssignmentExpression:
			return "AssignmentExpression";
		case SyntaxKind::PostfixExpression:
			return "PostfixExpression";

		default:
			throw std::invalid_argument("Invalid syntax; no such syntax kind.");
	}
}

}//MCF