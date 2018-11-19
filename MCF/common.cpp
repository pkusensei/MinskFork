#include "stdafx.h"
#include "common.h"

#include <algorithm>
#include <cctype>
#include <unordered_map>

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
		 kind != MCF::SyntaxKind::AssignmentExpression; kind++)
		result.emplace_back(kind);
	result.shrink_to_fit();
	return result;
}

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
		case SyntaxKind::AssignmentExpression:
			return "AssignmentExpression";

		default:
			throw std::invalid_argument("Invalid syntax; no such syntax kind.");
	}
}

bool StringEndsWith(const string & sample, const string & ending)
{
	if (sample.length() < ending.length()) return false;
	return std::equal(ending.rbegin(), ending.rend(), sample.rbegin());
}

string TrimString(const string & text)
{
	return TrimStringStart(TrimStringEnd(text));
}

string TrimStringStart(const string & text)
{
	auto result = text;
	result.erase(result.begin(), std::find_if(result.begin(), result.end(),
											  [](char ch) {return !std::isspace(ch); }));
	return result;
}

string TrimStringEnd(const string & text)
{
	auto result = text;
	result.erase(std::find_if(result.rbegin(), result.rend(),
							  [](char ch) {return !std::isspace(ch); }).base(), result.end());
	return result;
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
		case SyntaxKind::BangToken: return "!";
		case SyntaxKind::EqualsToken: return "=";
		case SyntaxKind::AmpersandAmpersandToken: return "&&";
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
		case SyntaxKind::AmpersandAmpersandToken:
			return 2;
		case SyntaxKind::PipePipeToken:
			return 1;
		default:
			return 0;
	}
}

vector<SyntaxKind> GetUnaryOperatorKinds()
{
	auto kinds = GetAllSyntaxKinds();
	auto result = vector<SyntaxKind>();
	for (const auto& it : kinds)
		if (GetUnaryOperatorPrecedence(it) > 0)
			result.emplace_back(it);
	return result;
}

vector<SyntaxKind> GetBinaryOperatorKinds()
{
	auto kinds = GetAllSyntaxKinds();
	auto result = vector<SyntaxKind>();
	for (const auto& it : kinds)
		if (GetBinaryOperatorPrecedence(it) > 0)
			result.emplace_back(it);
	return result;
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

void ValueType::WriteTo(std::ostream & out) const
{
	auto id = GetValueTypeId(Type());
	out << "\n" << "Result value is ";
	switch (id)
	{
		case 1:
			out << GetTypeName(Type()) << " " << GetValue<long>() << std::endl;
			break;
		case 2:
			out << GetTypeName(Type()) << " " << static_cast<bool>(GetValue<bool>()) << std::endl;
			break;
		default:
			out << "Not valid value or type.\n";
			break;
	}
	out << "\n";
}

int ValueType::GetValueTypeId(const type_index & inType)
{
	static std::unordered_map<type_index, int> types = {{typeid(std::monostate), 0},
		{typeid(long), 1},{typeid(bool), 2}};

	return types[inType];
}

string ValueType::GetTypeName(const type_index & inType)
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

VariableSymbol::VariableSymbol(const string& name, bool readOnly, const type_index& type)
	:_name(name), _isReadOnly(readOnly), _type(type)
{
}

VariableSymbol::VariableSymbol(const string & name, bool readOnly, const std::type_info & type)
	: VariableSymbol(name, readOnly, type_index(type))
{
}

VariableSymbol::VariableSymbol()
	: VariableSymbol(string(), true, typeid(std::monostate))
{
}

VariableSymbol::VariableSymbol(VariableSymbol && other)
	: _name(std::move(other._name)), _isReadOnly(other._isReadOnly),
	_type(other._type)
{
	other._type = typeid(std::monostate);
}

VariableSymbol & VariableSymbol::operator=(VariableSymbol && other)
{
	if (this == &other) return *this;

	_name = std::move(other._name);
	_isReadOnly = other._isReadOnly;
	_type = other._type;
	other._type = typeid(std::monostate);
	return *this;
}

bool VariableSymbol::operator==(const VariableSymbol & other) const noexcept
{
	return _name == other._name && _type == other._type;
}

bool VariableSymbol::operator!=(const VariableSymbol & other) const noexcept
{
	return !(*this == other);
}

size_t VariableHash::operator()(const VariableSymbol & variable) const noexcept
{
	auto h1 = std::hash<string>{}(variable.Name());
	auto h2 = variable.Type().hash_code();
	return h1 ^ (h2 << 1);
}

}//MCF