#include "SyntaxKind.h"

#include <stdexcept>

#include "EnumHelper.h"

namespace MCF {

const vector<SyntaxKind> AllSyntaxKinds
= GetAllEnumValue<SyntaxKind>(SyntaxKind::BadToken, SyntaxKind::PostfixExpression);

string_view GetSyntaxKindName(const SyntaxKind& kind)
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
		case SyntaxKind::ColonToken:
			return "ColonToken";
		case SyntaxKind::CommaToken:
			return "CommaToken";
		case SyntaxKind::IdentifierToken:
			return "IdentifierToken";

		case SyntaxKind::BreakKeyword:
			return "BreakKeyword";
		case SyntaxKind::ContinueKeyword:
			return "ContinueKeyword";
		case SyntaxKind::ElseKeyword:
			return "ElseKeyword";
		case SyntaxKind::FalseKeyword:
			return "FalseKeyword";
		case SyntaxKind::ForKeyword:
			return "ForKeyword";
		case SyntaxKind::FunctionKeyword:
			return "FunctionKeyword";
		case SyntaxKind::IfKeyword:
			return "IfKeyword";
		case SyntaxKind::LetKeyword:
			return "LetKeyword";
		case SyntaxKind::ReturnKeyword:
			return "ReturnKeyword";
		case SyntaxKind::ToKeyword:
			return "ToKeyword";
		case SyntaxKind::TrueKeyword:
			return "TrueKeyword";
		case SyntaxKind::VarKeyword:
			return "VarKeyword";
		case SyntaxKind::WhileKeyword:
			return "WhileKeyword";
		case SyntaxKind::DoKeyword:
			return "DoKeyword";

		case SyntaxKind::CompilationUnit:
			return "CompilationUnit";
		case SyntaxKind::FunctionDeclaration:
			return "FunctionDeclaration";
		case SyntaxKind::GlobalStatement:
			return "GlobalStatement";
		case SyntaxKind::Parameter:
			return "Parameter";
		case SyntaxKind::TypeClause:
			return "TypeClause";
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
		case SyntaxKind::DoWhileStatement:
			return "DoWhileStatement";
		case SyntaxKind::ForStatement:
			return "ForStatement";
		case SyntaxKind::BreakStatement:
			return "BreakStatement";
		case SyntaxKind::ContinueStatement:
			return "ContinueStatement";
		case SyntaxKind::ReturnStatement:
			return "ReturnStatement";
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

SyntaxKind GetKeywordKind(string_view text) noexcept
{
	if (text == "break")
		return SyntaxKind::BreakKeyword;
	else if (text == "continue")
		return SyntaxKind::ContinueKeyword;
	else if (text == "else")
		return SyntaxKind::ElseKeyword;
	else if (text == "false")
		return SyntaxKind::FalseKeyword;
	else if (text == "for")
		return SyntaxKind::ForKeyword;
	else if (text == "function")
		return SyntaxKind::FunctionKeyword;
	else if (text == "if")
		return SyntaxKind::IfKeyword;
	else if (text == "let")
		return SyntaxKind::LetKeyword;
	else if (text == "return")
		return SyntaxKind::ReturnKeyword;
	else if (text == "to")
		return SyntaxKind::ToKeyword;
	else if (text == "true")
		return SyntaxKind::TrueKeyword;
	else if (text == "var")
		return SyntaxKind::VarKeyword;
	else if (text == "while")
		return SyntaxKind::WhileKeyword;
	else if (text == "do")
		return SyntaxKind::DoKeyword;
	else return SyntaxKind::IdentifierToken;
}

string_view GetText(const SyntaxKind& kind)
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
		case SyntaxKind::ColonToken: return ":";
		case SyntaxKind::CommaToken: return ",";
		case SyntaxKind::BreakKeyword: return "break";
		case SyntaxKind::ContinueKeyword: return "continue";
		case SyntaxKind::ElseKeyword: return "else";
		case SyntaxKind::FalseKeyword: return "false";
		case SyntaxKind::ForKeyword: return "for";
		case SyntaxKind::FunctionKeyword: return "function";
		case SyntaxKind::IfKeyword: return "if";
		case SyntaxKind::LetKeyword: return "let";
		case SyntaxKind::ReturnKeyword: return "return";
		case SyntaxKind::ToKeyword: return "to";
		case SyntaxKind::TrueKeyword: return "true";
		case SyntaxKind::VarKeyword: return "var";
		case SyntaxKind::WhileKeyword: return "while";
		case SyntaxKind::DoKeyword: return "do";
		default: return string_view();
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

const vector<SyntaxKind>& GetUnaryOperatorKinds()
{
	auto build = [kinds = AllSyntaxKinds]()
	{
		auto result = vector<SyntaxKind>();
		for (const auto& it : kinds)
			if (GetUnaryOperatorPrecedence(it) > 0)
				result.emplace_back(it);
		result.shrink_to_fit();
		return result;
	};
	static const auto result = build();
	return result;
}

const vector<SyntaxKind>& GetBinaryOperatorKinds()
{
	auto build = [kinds = AllSyntaxKinds]()
	{
		auto result = vector<SyntaxKind>();
		for (const auto& it : kinds)
			if (GetBinaryOperatorPrecedence(it) > 0)
				result.emplace_back(it);
		result.shrink_to_fit();
		return result;
	};
	static const auto result = build();
	return result;
}

}//MCF