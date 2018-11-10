#include "stdafx.h"
#include "common.h"

#include <unordered_map>

namespace MCF {

SyntaxKind GetKeywordKind(const string & text)
{
	if (text == "false")
		return SyntaxKind::FalseKeyword;
	else if (text == "let")
		return SyntaxKind::LetKeyword;
	else if (text == "true")
		return SyntaxKind::TrueKeyword;
	else if (text == "var")
		return SyntaxKind::VarKeyword;
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
		case SyntaxKind::OpenBraceToken: return "{";
		case SyntaxKind::CloseBraceToken: return "}";
		case SyntaxKind::FalseKeyword: return "false";
		case SyntaxKind::LetKeyword: return "let";
		case SyntaxKind::TrueKeyword: return "true";
		case SyntaxKind::VarKeyword: return "var";
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
		case 2: return "bool";
		case 0:
		default:
			return "std::monostate";
	}
}

string GetSyntaxKindName(SyntaxKind kind)
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
		case SyntaxKind::PlusToken:
			return "PlusToken";
		case SyntaxKind::MinusToken:
			return "MinusToken";
		case SyntaxKind::StarToken:
			return "StarToken";
		case SyntaxKind::SlashToken:
			return "SlashToken";
		case SyntaxKind::BangToken:
			return "BangToken";
		case SyntaxKind::EqualsToken:
			return "EqualsToken";
		case SyntaxKind::AmpersandAmpersandToken:
			return "AmpersandAmpersandToken";
		case SyntaxKind::PipePipeToken:
			return "PipePipeToken";
		case SyntaxKind::EqualsEqualsToken:
			return "EqualsEqualsToken";
		case SyntaxKind::BangEqualsToken:
			return "BangEqualsToken";
		case SyntaxKind::OpenParenthesisToken:
			return "OpenParenthesisToken";
		case SyntaxKind::CloseParenthesisToken:
			return "CloseParenthesisToken";
		case SyntaxKind::OpenBraceToken:
			return "OpenBraceToken";
		case SyntaxKind::CloseBraceToken:
			return "CloseBraceToken";
		case SyntaxKind::IdentifierToken:
			return "IdentifierToken";

		case SyntaxKind::FalseKeyword:
			return "FalseKeyword";
		case SyntaxKind::LetKeyword:
			return "LetKeyword";
		case SyntaxKind::TrueKeyword:
			return "TrueKeyword";
		case SyntaxKind::VarKeyword:
			return "VarKeyword";

		case SyntaxKind::CompilationUnit:
			return "CompilationUnit";

		case SyntaxKind::BlockStatement:
			return "BlockStatement";
		case SyntaxKind::VariableDeclaration:
			return "VariableDeclaration";
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
		case SyntaxKind::AssignmentExpression:
			return "AssignmentExpression";

		default:
			throw std::invalid_argument("Invalid syntax; no such token.");
	}
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

VariableSymbol::VariableSymbol(const string& name, bool readOnly, const type_index& type)
	:_name(name), _isReadOnly(readOnly), _type(type)
{
}

VariableSymbol::VariableSymbol(const string & name, bool readOnly, const std::type_info & type)
	: VariableSymbol(name, readOnly, type_index(type))
{
}

VariableSymbol::VariableSymbol()
	: VariableSymbol("", true, typeid(std::monostate))
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