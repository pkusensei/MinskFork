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
	LessToken,
	LessOrEqualsToken,
	GreaterToken,
	GreaterOrEqualsToken,
	OpenParenthesisToken,
	CloseParenthesisToken,
	OpenBraceToken,
	CloseBraceToken,
	IdentifierToken,

	// Keywords
	ElseKeyword,
	FalseKeyword,
	ForKeyword,
	IfKeyword,
	LetKeyword,
	ToKeyword,
	TrueKeyword,
	VarKeyword,
	WhileKeyword,

	// Nodes
	CompilationUnit,
	ElseClause,

	// Statements
	BlockStatement,
	VariableDeclaration,
	IfStatement,
	WhileStatement,
	ForStatement,
	ExpressionStatement,

	// Expressions
	LiteralExpression,
	NameExpression,
	UnaryExpression,
	BinaryExpression,
	ParenthesizedExpression,
	AssignmentExpression,
};

SyntaxKind& operator++(SyntaxKind& kind);
SyntaxKind& operator++(SyntaxKind& kind, int c);
MCF_API const vector<SyntaxKind> GetAllSyntaxKinds();
MCF_API string GetSyntaxKindName(SyntaxKind kind);

/// helpers
MCF_API bool StringEndsWith(const string& sample, const string& ending);
MCF_API string TrimString(const string& text);
MCF_API string TrimStringStart(const string& text);
MCF_API string TrimStringEnd(const string& text);

/// namespace-scope functions
SyntaxKind GetKeywordKind(const string& text) noexcept;
MCF_API string GetText(SyntaxKind kind);
MCF_API int GetUnaryOperatorPrecedence(SyntaxKind kind)noexcept;
MCF_API int GetBinaryOperatorPrecedence(SyntaxKind kind)noexcept;
MCF_API vector<SyntaxKind> GetUnaryOperatorKinds();
MCF_API vector<SyntaxKind> GetBinaryOperatorKinds();

class MCF_API ValueType final
{
private:
	std::variant<std::monostate, long, bool> _inner;

public:
	constexpr ValueType() noexcept {}
	~ValueType() = default;

	/// stays implicit
	constexpr ValueType(const long& value)noexcept :_inner(value) {}
	constexpr ValueType(const int& value)noexcept :_inner(static_cast<long>(value)){}
	constexpr ValueType(const bool& value)noexcept :_inner(value) {}

	constexpr bool HasValue()const noexcept{ return !std::holds_alternative<std::monostate>(_inner); }
	type_index Type()const;

	void WriteTo(std::ostream& out) const;

	constexpr bool operator==(const ValueType& other)const { return _inner == other._inner; }
	constexpr bool operator!=(const ValueType& other)const { return !(_inner == other._inner); }

	template<typename T>
	constexpr decltype(auto) GetValue() const
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
	VariableSymbol(const VariableSymbol&)= default;
	VariableSymbol(VariableSymbol&& other);
	VariableSymbol& operator=(const VariableSymbol&) = default;
	VariableSymbol& operator=(VariableSymbol&& other);

	string Name()const { return _name; }
	constexpr bool IsReadOnly()const noexcept { return _isReadOnly; }
	type_index Type()const { return _type; }

	bool operator==(const VariableSymbol& other) const noexcept;
	bool operator!=(const VariableSymbol& other) const noexcept;
};

struct MCF_API VariableHash
{
	size_t operator()(const VariableSymbol& variable) const noexcept;
};

}//MCF