#include "stdafx.h"
#include "SyntaxKind.h"

#include "EnumHelper.h"

namespace MCF {

const vector<SyntaxKind> GetAllSyntaxKinds()
{
	return GetAllEnumValue<SyntaxKind>(SyntaxKind::BadToken, SyntaxKind::PostfixExpression);
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

SyntaxKind GetKeywordKind(const string & text) noexcept
{
	if (text == "else")
		return SyntaxKind::ElseKeyword;
	else if (text == "false")
		return SyntaxKind::FalseKeyword;
	else if (text == "for")
		return SyntaxKind::ForKeyword;
	else if (text == "if")
		return SyntaxKind::IfKeyword;
	else if (text == "let")
		return SyntaxKind::LetKeyword;
	else if (text == "to")
		return SyntaxKind::ToKeyword;
	else if (text == "true")
		return SyntaxKind::TrueKeyword;
	else if (text == "var")
		return SyntaxKind::VarKeyword;
	else if (text == "while")
		return SyntaxKind::WhileKeyword;
	else return SyntaxKind::IdentifierToken;
}

string GetText(const SyntaxKind& kind)
{
	switch (kind)
	{
		case SyntaxKind::PlusToken: return "+";
		case SyntaxKind::MinusToken: return "-";
		case SyntaxKind::StarToken: return "*";
		case SyntaxKind::SlashToken: return "/";
		case SyntaxKind::PercentToken: return "%";
		case SyntaxKind::BangToken: return "!";
		case SyntaxKind::PlusPlusToken: return "++";
		case SyntaxKind::MinusMinusToken: return "--";
		case SyntaxKind::EqualsToken: return "=";
		case SyntaxKind::TildeToken: return "~";
		case SyntaxKind::HatToken: return "^";
		case SyntaxKind::AmpersandToken: return "&";
		case SyntaxKind::AmpersandAmpersandToken: return "&&";
		case SyntaxKind::PipeToken: return "|";
		case SyntaxKind::PipePipeToken: return "||";
		case SyntaxKind::EqualsEqualsToken: return "==";
		case SyntaxKind::BangEqualsToken: return "!=";
		case SyntaxKind::LessToken: return "<";
		case SyntaxKind::LessOrEqualsToken: return "<=";
		case SyntaxKind::GreaterToken: return ">";
		case SyntaxKind::GreaterOrEqualsToken: return ">=";
		case SyntaxKind::OpenParenthesisToken: return "(";
		case SyntaxKind::CloseParenthesisToken: return ")";
		case SyntaxKind::OpenBraceToken: return "{";
		case SyntaxKind::CloseBraceToken: return "}";
		case SyntaxKind::CommaToken: return ",";
		case SyntaxKind::ElseKeyword: return "else";
		case SyntaxKind::FalseKeyword: return "false";
		case SyntaxKind::ForKeyword: return "for";
		case SyntaxKind::IfKeyword: return "if";
		case SyntaxKind::LetKeyword: return "let";
		case SyntaxKind::ToKeyword: return "to";
		case SyntaxKind::TrueKeyword: return "true";
		case SyntaxKind::VarKeyword: return "var";
		case SyntaxKind::WhileKeyword: return "while";
		default: return string();
	}
}

int GetUnaryOperatorPrecedence(const SyntaxKind& kind) noexcept
{
	switch (kind)
	{
		case SyntaxKind::PlusToken:
		case SyntaxKind::MinusToken:
		case SyntaxKind::BangToken:
			//case SyntaxKind::PlusPlusToken:
			//case SyntaxKind::MinusMinusToken:
		case SyntaxKind::TildeToken:
			return 6;
		default:
			return 0;
	}
}

int GetBinaryOperatorPrecedence(const SyntaxKind& kind) noexcept
{
	switch (kind)
	{
		case SyntaxKind::StarToken:
		case SyntaxKind::SlashToken:
		case SyntaxKind::PercentToken:
			return 5;
		case SyntaxKind::PlusToken:
		case SyntaxKind::MinusToken:
			return 4;
		case SyntaxKind::EqualsEqualsToken:
		case SyntaxKind::BangEqualsToken:
		case SyntaxKind::LessToken:
		case SyntaxKind::LessOrEqualsToken:
		case SyntaxKind::GreaterToken:
		case SyntaxKind::GreaterOrEqualsToken:
			return 3;
		case SyntaxKind::AmpersandToken:
		case SyntaxKind::AmpersandAmpersandToken:
			return 2;
		case SyntaxKind::PipeToken:
		case SyntaxKind::PipePipeToken:
		case SyntaxKind::HatToken:
			return 1;
		default:
			return 0;
	}
}

vector<SyntaxKind> GetUnaryOperatorKinds()
{
	auto result = vector<SyntaxKind>();
	for (const auto& it : AllSyntaxKinds)
		if (GetUnaryOperatorPrecedence(it) > 0)
			result.emplace_back(it);
	return result;
}

vector<SyntaxKind> GetBinaryOperatorKinds()
{
	auto result = vector<SyntaxKind>();
	for (const auto& it : AllSyntaxKinds)
		if (GetBinaryOperatorPrecedence(it) > 0)
			result.emplace_back(it);
	return result;
}

}//MCF