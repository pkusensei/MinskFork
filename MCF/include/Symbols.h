#pragma once

#include <array>
#include <unordered_map>

#include "Values.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

struct FunctionDeclarationSyntax;

enum class SymbolKind
{
	Function,
	GlobalVariable,
	LocalVariable,
	Parameter,
	Type,
};

string_view nameof(SymbolKind kind);

struct MCF_API Symbol
{
	string_view Name;

protected:
	constexpr explicit Symbol(string_view name) noexcept
		:Name(name)
	{
	}

public:
	virtual ~Symbol() = default;

	virtual SymbolKind Kind() const noexcept = 0;
	void WriteTo(std::ostream& out)const;
	string ToString() const;

	bool operator==(const Symbol& other)const noexcept
	{
		return Name == other.Name;
	}
	bool operator!=(const Symbol& other)const noexcept
	{
		return !(*this == other);
	}

	bool IsVariableSymbol()const noexcept
	{
		return Kind() == SymbolKind::GlobalVariable
			|| Kind() == SymbolKind::LocalVariable
			|| Kind() == SymbolKind::Parameter;
	}
};

template<typename T>
struct SymbolHash
{
	auto operator()(const T& s)const noexcept
		->std::enable_if_t<std::is_base_of_v<Symbol, T>, size_t>
	{
		return std::hash<string_view>{}(s.Name);
	}
};

template<typename T>
struct SymbolHash<T*>
{
	auto operator()(const T* p)const noexcept
		->typename std::enable_if_t<std::is_base_of_v<Symbol, T>, size_t>
	{
		return std::hash<string_view>{}(p->Name);
	}
};

template<typename...>
struct SymbolEqual;

template<typename T>
struct SymbolEqual<T*>
{
	auto operator()(const T* p1, const T* p2)const noexcept
		->typename std::enable_if_t<std::is_base_of_v<Symbol, T>, bool>
	{
		return (*p1) == (*p2);
	}
};

template<typename Key, typename Value, template<typename...>typename KeyEqual = std::equal_to>
using SymbolMap = std::unordered_map<Key, Value, SymbolHash<Key>, KeyEqual<Key>>;

enum class TypeEnum
{
	Error, Any, Bool, Int, String, Void
};

struct TypeSymbol final :public Symbol
{
	// sizeof( string_view + vtable ) == 24
public:
	constexpr explicit TypeSymbol(string_view name)noexcept
		:Symbol(name)
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::Type; }
};

inline const auto TYPE_ERROR = TypeSymbol("?");
inline const auto TYPE_ANY = TypeSymbol("any");
inline const auto TYPE_BOOL = TypeSymbol("bool");
inline const auto TYPE_INT = TypeSymbol("int");
inline const auto TYPE_STRING = TypeSymbol("string");
inline const auto TYPE_VOID = TypeSymbol("void");

struct MCF_API VariableSymbol : public Symbol
{
	BoundConstant Constant;
	TypeSymbol Type;
	bool IsReadOnly;

public:
	explicit VariableSymbol(string_view name, bool isReadOnly, TypeSymbol type,
							BoundConstant constant) noexcept
		:Symbol(name),
		Constant(isReadOnly ? std::move(constant) : NULL_VALUE),
		Type(std::move(type)), IsReadOnly(isReadOnly)
	{
	}

};

struct GlobalVariableSymbol final :public VariableSymbol
{
public:
	explicit GlobalVariableSymbol(string_view name, bool isReadOnly, TypeSymbol type,
								  BoundConstant constant)noexcept
		:VariableSymbol(name, isReadOnly, std::move(type), std::move(constant))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::GlobalVariable; }
};

struct LocalVariableSymbol :public VariableSymbol
{
public:
	explicit LocalVariableSymbol(string_view name, bool isReadOnly, TypeSymbol type,
								 BoundConstant constant)noexcept
		:VariableSymbol(name, isReadOnly, std::move(type), std::move(constant))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::LocalVariable; }
};

struct ParameterSymbol final : public LocalVariableSymbol
{
public:
	explicit ParameterSymbol(string_view name, TypeSymbol type)noexcept
		:LocalVariableSymbol(name, true, std::move(type), NULL_VALUE)
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::Parameter; }
};

struct FunctionSymbol final :public Symbol
{
	vector<ParameterSymbol> Parameters;
	TypeSymbol Type;
	const FunctionDeclarationSyntax* Declaration;

public:
	explicit FunctionSymbol(string_view name, vector<ParameterSymbol> params,
							TypeSymbol type,
							const FunctionDeclarationSyntax* declaration)noexcept
		:Symbol(name),
		Parameters(std::move(params)), Type(std::move(type)), Declaration(declaration)
	{
	}
	FunctionSymbol(string_view name, vector<ParameterSymbol> params, TypeSymbol type)
		:FunctionSymbol(name, std::move(params), type, nullptr)
	{
	}
	FunctionSymbol()
		:FunctionSymbol("", vector<ParameterSymbol>(), TYPE_ERROR)
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::Function; }

};

inline const auto BUILTIN_INPUT = FunctionSymbol("input", {}, TYPE_STRING);
inline const auto BUILTIN_PRINT = FunctionSymbol("print",
												 { ParameterSymbol("text", TYPE_ANY) },
												 TYPE_VOID);
inline const auto BUILTIN_RND = FunctionSymbol("rnd",
											   { ParameterSymbol("max", TYPE_INT) },
											   TYPE_INT);

inline const auto AllBuiltinFunctions = std::array{
	&BUILTIN_INPUT, &BUILTIN_PRINT, &BUILTIN_RND
};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
