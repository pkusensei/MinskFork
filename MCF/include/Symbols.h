#pragma once

#include <array>
#include <unordered_map>

// HACK force intellisense to work with <concepts>
#ifndef __cpp_lib_concepts
#define __cpp_lib_concepts
#include <concepts>
#undef __cpp_lib_concepts
#endif

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

	virtual unique_ptr<Symbol> Clone()const = 0;

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

	template<typename T>
	requires std::derived_from<T, Symbol>
		unique_ptr<T> UniqueCloneAs() const
	{
		if constexpr (std::is_same_v<T, Symbol>)
			return Clone();
		return StaticUniquePtrCast<T>(Clone());
	}
};

template<typename T>
concept Derefable = requires(const T & ptr) { *ptr; };

template<Derefable T>
struct PointeeType
{
	using type = std::remove_cvref_t<decltype(*std::declval<T>())>;
};

template<typename T>
using PointeeType_t = typename PointeeType<T>::type;

template<typename T>
struct SymbolHash
{
	size_t operator()(const T& s)const noexcept
	{
		if constexpr (std::derived_from<T, Symbol>)
			return std::hash<string_view>{}(s.Name);
		else if constexpr (std::derived_from<PointeeType_t<T>, Symbol>)
			return std::hash<string_view>{}(s->Name);
	}

};

template<typename T>
struct SymbolEqual
{
	bool operator()(const T& s1, const T& s2)const noexcept
	{
		if constexpr (std::derived_from<T, Symbol>)
			return s1 == s2;
		else if constexpr (std::derived_from<PointeeType_t<T>, Symbol>)
			return *s1 == *s2;
	}
};

template<typename Key, typename Value, template<typename...>typename KeyEqual = SymbolEqual>
using SymbolMap = std::unordered_map<Key, Value, SymbolHash<Key>, KeyEqual<Key>>;

enum class TypeEnum
{
	Error, Any, Bool, Int, String, Void
};

struct TypeSymbol final :public Symbol
{
	// sizeof( string_view + vtable ) == 24
	constexpr explicit TypeSymbol(string_view name)noexcept
		:Symbol(name)
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::Type; }

protected:
	unique_ptr<Symbol> Clone()const override
	{
		return make_unique<TypeSymbol>(Name);
	}
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

template<typename Derived>
struct VarSymbolT :public VariableSymbol
{
protected:
	explicit VarSymbolT(string_view name, bool isReadOnly, TypeSymbol type,
						BoundConstant constant)noexcept
		:VariableSymbol(name, isReadOnly, std::move(type), std::move(constant))
	{
	}

	unique_ptr<Symbol> Clone()const final
	{
		return UniqueClone<Derived, Symbol>(*this);
	}
};

struct GlobalVariableSymbol final :public VarSymbolT<GlobalVariableSymbol>
{
	explicit GlobalVariableSymbol(string_view name, bool isReadOnly, TypeSymbol type,
								  BoundConstant constant)noexcept
		:VarSymbolT<GlobalVariableSymbol>(name, isReadOnly, std::move(type), std::move(constant))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::GlobalVariable; }
};

struct LocalVariableSymbol :public VarSymbolT<LocalVariableSymbol>
{
public:
	explicit LocalVariableSymbol(string_view name, bool isReadOnly, TypeSymbol type,
								 BoundConstant constant)noexcept
		:VarSymbolT<LocalVariableSymbol>(name, isReadOnly, std::move(type), std::move(constant))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::LocalVariable; }
};

struct ParameterSymbol final : public VarSymbolT<ParameterSymbol>
{
	explicit ParameterSymbol(string_view name, TypeSymbol type)noexcept
		:VarSymbolT<ParameterSymbol>(name, true, std::move(type), NULL_VALUE)
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
protected:
	unique_ptr<Symbol> Clone()const override
	{
		return make_unique<FunctionSymbol>(*this);
	}

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
