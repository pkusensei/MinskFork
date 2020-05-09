#pragma once

#include <array>

#include "Values.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

class FunctionDeclarationSyntax;

enum class SymbolKind
{
	Function,
	GlobalVariable,
	LocalVariable,
	Parameter,
	Type,
};

string_view nameof(SymbolKind kind);

class MCF_API Symbol
{
private:
	string_view _name;

protected:
	constexpr explicit Symbol(string_view name) noexcept
		:_name(name)
	{
	}

public:
	virtual ~Symbol() = default;

	virtual SymbolKind Kind() const noexcept = 0;
	constexpr string_view Name() const { return _name; }
	void WriteTo(std::ostream& out)const;
	string ToString() const;

	bool operator==(const Symbol& other)const noexcept
	{
		return Name() == other.Name();
	}
	bool operator!=(const Symbol& other)const noexcept
	{
		return !(*this == other);
	}

};

struct MCF_API SymbolHash
{
	template<typename Ptr,
		typename = std::enable_if_t<!std::is_base_of_v<Symbol, Ptr>>>
		size_t operator()(const Ptr& p)const noexcept
	{
		return std::hash<string_view>{}(p->Name());
	}

	size_t operator()(const Symbol& s)const noexcept
	{
		return std::hash<string_view>{}(s.Name());
	}
};

struct SymbolEqual
{
	template<typename P1, typename P2,
		typename = std::enable_if_t<!std::is_base_of_v<Symbol, P1> && !std::is_base_of_v<Symbol, P2>>>
		bool operator()(const P1& p1, const P2& p2)const noexcept
	{
		return (*p1) == (*p2);
	}

	bool operator()(const Symbol& lhs, const Symbol& rhs) const noexcept
	{
		return lhs == rhs;
	}
};

enum class TypeEnum
{
	Error, Any, Bool, Int, String, Void
};

class TypeSymbol final :public Symbol
{
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

class MCF_API VariableSymbol : public Symbol
{
private:
	bool _isReadOnly;
	TypeSymbol _type;
	BoundConstant _constant;

public:
	VariableSymbol(string_view name, bool isReadOnly, TypeSymbol type,
		BoundConstant constant) noexcept
		:Symbol(name),
		_isReadOnly(isReadOnly), _type(type),
		_constant(isReadOnly ? std::move(constant) : NULL_VALUE)
	{
	}

	bool IsReadOnly()const noexcept { return _isReadOnly; }

	constexpr const TypeSymbol& Type()const noexcept { return _type; }
	constexpr const BoundConstant& Constant()const noexcept { return _constant; }
};

class GlobalVariableSymbol final :public VariableSymbol
{
public:
	GlobalVariableSymbol(string_view name, bool isReadOnly, TypeSymbol type,
		BoundConstant constant)noexcept
		:VariableSymbol(name, isReadOnly, type, std::move(constant))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::GlobalVariable; }
};

class LocalVariableSymbol :public VariableSymbol
{
public:
	LocalVariableSymbol(string_view name, bool isReadOnly, TypeSymbol type,
		BoundConstant constant)noexcept
		:VariableSymbol(name, isReadOnly, type, std::move(constant))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::LocalVariable; }
};

class ParameterSymbol final : public LocalVariableSymbol
{
public:
	ParameterSymbol(string_view name, TypeSymbol type)noexcept
		:LocalVariableSymbol(name, true, type, NULL_VALUE)
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::Parameter; }
};

class FunctionSymbol final :public Symbol
{
private:
	vector<ParameterSymbol> _params;
	TypeSymbol _type;
	const FunctionDeclarationSyntax* _declaration;

public:
	FunctionSymbol(string_view name, vector<ParameterSymbol> params,
		TypeSymbol type, const FunctionDeclarationSyntax* declaration)
		:Symbol(name), _params(std::move(params)), _type(type), _declaration(declaration)
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
	constexpr const vector<ParameterSymbol>& Parameters()const noexcept { return _params; }
	constexpr const TypeSymbol& Type()const noexcept { return _type; }
	constexpr const FunctionDeclarationSyntax* Declaration()const noexcept { return  _declaration; }

};

inline const auto BUILTIN_INPUT = FunctionSymbol("input",
	vector<ParameterSymbol>(),
	TYPE_STRING);
inline const auto BUILTIN_PRINT = FunctionSymbol("print",
	vector<ParameterSymbol>{ParameterSymbol("text", TYPE_ANY)},
	TYPE_VOID);
inline const auto BUILTIN_RND = FunctionSymbol("rnd",
	vector<ParameterSymbol>{ParameterSymbol("max", TYPE_INT)},
	TYPE_INT);

const std::array<FunctionSymbol, 3>& GetAllBuiltinFunctions();

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
