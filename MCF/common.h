#pragma once

#include <string>
#include <tuple>
#include <typeindex>
#include <variant>
#include <vector>

#ifdef MCF_EXPORTS
#define MCF_API __declspec(dllexport)
#else
#define MCF_API __declspec(dllimport)
#endif

namespace MCF {

using std::string;
using std::type_index;
using std::unique_ptr;
using std::vector;
using TextSpan = std::tuple<int, int, int>;

enum class SyntaxKind
{
	// Tokens
	BadToken,
	EndOfFileToken,
	WhitespaceToken,
	NumberToken,
	PlusToken,
	MinusToken,
	StarToken,
	SlashToken,
	BangToken,
	EqualsToken,
	AmpersandAmpersandToken,
	PipePipeToken,
	EqualsEqualsToken,
	BangEqualsToken,
	OpenParenthesisToken,
	CloseParenthesisToken,
	IdentifierToken,

	// Keywords
	FalseKeyword,
	TrueKeyword,

	// Expressions
	LiteralExpression,
	NameExpression,
	UnaryExpression,
	BinaryExpression,
	ParenthesizedExpression,
	AssignmentExpression
};

/// namespace-scope functions
SyntaxKind GetKeywordKind(const string& text);
string GetText(SyntaxKind kind);
int GetUnaryOperatorPrecedence(SyntaxKind kind);
int GetBinaryOperatorPrecedence(SyntaxKind kind);
int GetValueTypeId(const type_index& inType);

class ValueType final
{
private:
	std::variant<std::monostate, long, bool> _inner;

public:
	ValueType() {}
	~ValueType() = default;
	ValueType(const ValueType& other) :_inner(other._inner) {}

	/// stays implicit
	ValueType(const long& value) :_inner(value) {}
	ValueType(const bool& value) :_inner(value) {}

	bool HasValue()const { return !std::holds_alternative<std::monostate>(_inner); }

	template<typename T>
	decltype(auto) GetValue() const
	{
		return std::get<T>(_inner);
	}
};

class VariableSymbol final
{
private:
	string _name;
	type_index _type;
public:
	VariableSymbol(const string& name, const type_index& type);
	VariableSymbol(const string& name, const std::type_info& type);
	~VariableSymbol() = default;

	string Name()const { return _name; }
	type_index Type()const { return _type; }
};

}//MCF