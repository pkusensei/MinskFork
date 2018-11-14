#pragma once

#include <string>
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
	OpenBraceToken,
	CloseBraceToken,
	IdentifierToken,

	// Keywords
	FalseKeyword,
	LetKeyword,
	TrueKeyword,
	VarKeyword,

	// Node
	CompilationUnit,

	// Statements
	BlockStatement,
	VariableDeclaration,
	ExpressionStatement,

	// Expressions
	LiteralExpression,
	NameExpression,
	UnaryExpression,
	BinaryExpression,
	ParenthesizedExpression,
	AssignmentExpression,
};

MCF_API SyntaxKind& operator++(SyntaxKind& kind);
MCF_API SyntaxKind& operator++(SyntaxKind& kind, int c);
MCF_API const vector<SyntaxKind> GetAllSyntaxKinds();
MCF_API string GetSyntaxKindName(SyntaxKind kind);

/// helpers
MCF_API bool StringEndsWith(const string& sample, const string& ending);

/// namespace-scope functions
SyntaxKind GetKeywordKind(const string& text);
MCF_API string GetText(SyntaxKind kind);
MCF_API int GetUnaryOperatorPrecedence(SyntaxKind kind);
MCF_API int GetBinaryOperatorPrecedence(SyntaxKind kind);
MCF_API vector<SyntaxKind> GetUnaryOperatorKinds();
MCF_API vector<SyntaxKind> GetBinaryOperatorKinds();

class MCF_API ValueType final
{
private:
	std::variant<std::monostate, long, bool> _inner;

public:
	ValueType() {}
	~ValueType() = default;

	/// stays implicit
	ValueType(const long& value) :_inner(value) {}
	ValueType(const int& value):_inner(static_cast<long>(value)){}
	ValueType(const bool& value) :_inner(value) {}

	bool HasValue()const { return !std::holds_alternative<std::monostate>(_inner); }
	type_index Type()const;

	void WriteTo(std::ostream& out) const;

	bool operator==(const ValueType& other)const { return _inner == other._inner; }
	bool operator!=(const ValueType& other)const { return !(_inner == other._inner); }

	template<typename T>
	decltype(auto) GetValue() const
	{
		return std::get<T>(_inner);
	}

	static int GetValueTypeId(const type_index & inType);
	static string GetTypeName(const type_index& inType);
};

class MCF_API VariableSymbol final
{
private:
	string _name;
	bool _isReadOnly;
	type_index _type; // no move
public:
	VariableSymbol(const string& name, bool readOnly, const type_index& type);
	VariableSymbol(const string& name, bool readOnly, const std::type_info& type);
	VariableSymbol();
	~VariableSymbol() = default;
	VariableSymbol(const VariableSymbol&) = default;
	VariableSymbol(VariableSymbol&& other);
	VariableSymbol& operator=(const VariableSymbol&) = default;
	VariableSymbol& operator=(VariableSymbol&& other);

	string Name()const { return _name; }
	bool IsReadOnly()const { return _isReadOnly; }
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