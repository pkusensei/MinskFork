#include "stdafx.h"
#include "common.h"

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

const auto AllSyntaxKinds = GetAllSyntaxKinds();

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
		case SyntaxKind::PostfixExpression:
			return "PostfixExpression";
		case SyntaxKind::AssignmentExpression:
			return "AssignmentExpression";

		default:
			throw std::invalid_argument("Invalid syntax; no such syntax kind.");
	}
}

std::ostream & operator<<(std::ostream & out, const ValueType & value)
{
	if (value.HasValue())
		out << value.ToString();
	else
		out << "Not valid value or type.\n";
	return out;
}

type_index ValueType::Type() const
{
	switch (_inner.index())
	{
		case 1:
			return typeid(IntegerType);
		case 2:
			return typeid(bool);
		case 0:
		default:
			return typeid(std::monostate);
	}
}

string ValueType::ToString() const
{
	auto id = ValueType::GetValueTypeId(Type());
	auto result = string();
	switch (id)
	{
		case 1:
			result = std::to_string(GetValue<IntegerType>());
			break;
		case 2:
			result = GetValue<bool>() ? "True" : "False";
			break;
		default:
			break;
	}

	return result;
}

int ValueType::GetValueTypeId(const type_index & inType)
{
	static std::unordered_map<type_index, int> types = {
		{typeid(std::monostate), 0},
		{typeid(IntegerType), 1},
		{typeid(bool), 2},
	};

	return types[inType];
}

string ValueType::GetTypeName(const type_index & inType)
{
	auto id = GetValueTypeId(inType);
	switch (id)
	{
		case 1: return "IntegerType";
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