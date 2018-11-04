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
MCF_API int GetValueTypeId(const type_index& inType);
MCF_API string GetTypeName(const type_index& inType);
MCF_API string GetSyntaxKindName(SyntaxKind kind);

class TextSpan final
{
private:
	std::tuple<size_t, size_t> _span;
public:
	TextSpan(size_t start, size_t length);
	~TextSpan() = default;
	TextSpan(const TextSpan&) = default;
	TextSpan(TextSpan&&) = default;
	TextSpan& operator=(const TextSpan&);
	TextSpan& operator=(TextSpan&&) = default;

	size_t Start()const { return std::get<0>(_span); }
	size_t Length()const { return std::get<1>(_span); }
	size_t End()const { return std::get<0>(_span) + std::get<1>(_span); }

	static TextSpan FromBounds(size_t start, size_t end);
};

class MCF_API ValueType final
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
	type_index Type()const;

	bool operator==(const ValueType& other)const { return _inner == other._inner; }
	bool operator!=(const ValueType& other)const { return !(_inner == other._inner); }

	template<typename T>
	decltype(auto) GetValue() const
	{
		return std::get<T>(_inner);
	}
};

class MCF_API VariableSymbol final
{
private:
	string _name;
	type_index _type; // no move
public:
	VariableSymbol(const string& name, const type_index& type);
	VariableSymbol(const string& name, const std::type_info& type);
	VariableSymbol();
	~VariableSymbol() = default;

	string Name()const { return _name; }
	type_index Type()const { return _type; }

	bool operator==(const VariableSymbol& other) const;
	bool operator!=(const VariableSymbol& other) const;
};

struct MCF_API VariableHash
{
	size_t operator()(const VariableSymbol& variable) const noexcept
	{
		auto h1 = std::hash<string>{}(variable.Name());
		auto h2 = variable.Type().hash_code();
		return h1 ^ (h2 << 1);
	}
};


}//MCF