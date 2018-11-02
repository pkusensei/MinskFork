#include "stdafx.h"
#include "common.h"

#include <unordered_map>

namespace MCF {

SyntaxKind GetKeywordKind(const string & text)
{
	if (text == "true")
		return SyntaxKind::TrueKeyword;
	else if (text == "false")
		return SyntaxKind::FalseKeyword;
	else return SyntaxKind::IdentifierToken;
}

string GetText(SyntaxKind kind)
{
	switch (kind)
	{
		case SyntaxKind::PlusToken: return "+";
		case SyntaxKind::MinusToken: return "-";
		case SyntaxKind::StarToken: return "*";
		case SyntaxKind::SlashToken: return "/";
		case SyntaxKind::BangToken: return "!";
		case SyntaxKind::EqualsToken: return "=";
		case SyntaxKind::AmpersandAmpersandToken: return "&&";
		case SyntaxKind::PipePipeToken: return "||";
		case SyntaxKind::EqualsEqualsToken: return "==";
		case SyntaxKind::BangEqualsToken: return "!=";
		case SyntaxKind::OpenParenthesisToken: return "(";
		case SyntaxKind::CloseParenthesisToken: return ")";
		case SyntaxKind::FalseKeyword: return "false";
		case SyntaxKind::TrueKeyword: return "true";
		default: return "";
	}
}

int GetUnaryOperatorPrecedence(SyntaxKind kind)
{
	switch (kind)
	{
		case MCF::SyntaxKind::PlusToken:
		case MCF::SyntaxKind::MinusToken:
		case MCF::SyntaxKind::BangToken:
			return 6;
		default:
			return 0;
	}
}

int GetBinaryOperatorPrecedence(SyntaxKind kind)
{
	switch (kind)
	{
		case MCF::SyntaxKind::StarToken:
		case MCF::SyntaxKind::SlashToken:
			return 5;
		case MCF::SyntaxKind::PlusToken:
		case MCF::SyntaxKind::MinusToken:
			return 4;
		case MCF::SyntaxKind::EqualsEqualsToken:
		case MCF::SyntaxKind::BangEqualsToken:
			return 3;
		case MCF::SyntaxKind::AmpersandAmpersandToken:
			return 2;
		case MCF::SyntaxKind::PipePipeToken:
			return 1;
		default:
			return 0;
	}
}

int GetValueTypeId(const type_index & inType)
{
	static std::unordered_map<type_index, int> types = {{typeid(std::monostate), 0},
	{typeid(long), 1},{typeid(bool), 2}};

	return types[inType];
}

VariableSymbol::VariableSymbol(const string& name, const type_index& type)
	:_name(name), _type(type)
{
}

VariableSymbol::VariableSymbol(const string & name, const std::type_info & type)
	:VariableSymbol(name, type_index(type))
{
}

}