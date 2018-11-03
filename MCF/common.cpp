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

string GetTypeName(const type_index & inType)
{
	auto id = GetValueTypeId(inType);
	switch (id)
	{
		case 1: return "long";
		case 2:return "bool";
		case 0:
		default:
			return "std::monostate";
	}
}

string GetSyntaxKindName(SyntaxKind kind)
{
	switch (kind)
	{
		case MCF::SyntaxKind::BadToken:
			return "BadToken";
		case MCF::SyntaxKind::EndOfFileToken:
			return "EndOfFileToken";
		case MCF::SyntaxKind::WhitespaceToken:
			return "WhitespaceToken";
		case MCF::SyntaxKind::NumberToken:
			return "NumberToken";
		case MCF::SyntaxKind::PlusToken:
			return "PlusToken";
		case MCF::SyntaxKind::MinusToken:
			return "MinusToken";
		case MCF::SyntaxKind::StarToken:
			return "StarToken";
		case MCF::SyntaxKind::SlashToken:
			return "SlashToken";
		case MCF::SyntaxKind::BangToken:
			return "BangToken";
		case MCF::SyntaxKind::EqualsToken:
			return "EqualsToken";
		case MCF::SyntaxKind::AmpersandAmpersandToken:
			return "AmpersandAmpersandToken";
		case MCF::SyntaxKind::PipePipeToken:
			return "PipePipeToken";
		case MCF::SyntaxKind::EqualsEqualsToken:
			return "EqualsEqualsToken";
		case MCF::SyntaxKind::BangEqualsToken:
			return "BangEqualsToken";
		case MCF::SyntaxKind::OpenParenthesisToken:
			return "OpenParenthesisToken";
		case MCF::SyntaxKind::CloseParenthesisToken:
			return "CloseParenthesisToken";
		case MCF::SyntaxKind::IdentifierToken:
			return "IdentifierToken";
		case MCF::SyntaxKind::FalseKeyword:
			return "FalseKeyword";
		case MCF::SyntaxKind::TrueKeyword:
			return "TrueKeyword";
		case MCF::SyntaxKind::LiteralExpression:
			return "LiteralExpression";
		case MCF::SyntaxKind::NameExpression:
			return "NameExpression";
		case MCF::SyntaxKind::UnaryExpression:
			return "UnaryExpression";
		case MCF::SyntaxKind::BinaryExpression:
			return "BinaryExpression";
		case MCF::SyntaxKind::ParenthesizedExpression:
			return "ParenthesizedExpression";
		case MCF::SyntaxKind::AssignmentExpression:
			return "AssignmentExpression";
		default:
			return "Unknown"; // HACK
	}
}

TextSpan::TextSpan(size_t start, size_t length)
	:_span(start, length)
{
}

TextSpan TextSpan::FromBounds(size_t start, size_t end)
{
	return TextSpan(start, end - start);
}

type_index ValueType::Type() const
{
	switch (_inner.index())
	{
		case 1:
			return typeid(long);
		case 2:
			return typeid(bool);
		case 0:
		default:
			return typeid(std::monostate);
	}
}

VariableSymbol::VariableSymbol(const string& name, const type_index& type)
	:_name(name), _type(type)
{
}

VariableSymbol::VariableSymbol(const string & name, const std::type_info & type)
	: VariableSymbol(name, type_index(type))
{
}

VariableSymbol::VariableSymbol()
	: VariableSymbol("", typeid(std::monostate))
{
}

bool VariableSymbol::operator==(const VariableSymbol & other) const
{
	return _name == other._name && _type == other._type;
}

bool VariableSymbol::operator!=(const VariableSymbol & other) const
{
	return !(*this == other);
}

}