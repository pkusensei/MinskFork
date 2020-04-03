#pragma once

#include <array>
#include <optional>
#include <variant>

#include "common.h"

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

	bool operator==(const Symbol& other)const noexcept;
	bool operator!=(const Symbol& other)const noexcept;
};

struct MCF_API SymbolHash
{
	size_t operator()(const Symbol& s)const noexcept;
	size_t operator()(const shared_ptr<Symbol>& s)const noexcept;
};

struct SymbolEqual
{
	bool operator()(const Symbol& lhs, const Symbol& rhs) const;
	bool operator()(const shared_ptr<Symbol>& lhs, const shared_ptr<Symbol>& rhs)const;
};

enum class TypeEnum
{
	Error, Bool, Int, String, Void
};

class TypeSymbol final :public Symbol
{
private:
	constexpr explicit TypeSymbol(string_view name)noexcept
		:Symbol(name)
	{
	}

public:
	SymbolKind Kind() const noexcept override { return SymbolKind::Type; }

	static std::reference_wrapper<const TypeSymbol> Get(TypeEnum kind);
};
using ConstTypeRef = std::reference_wrapper<const TypeSymbol>;

class MCF_API VariableSymbol : public Symbol
{
private:
	bool _isReadOnly;
	std::optional<ConstTypeRef> _type;

public:
	VariableSymbol(string_view name, bool isReadOnly, std::optional<ConstTypeRef> type) noexcept
		:Symbol(name), _isReadOnly(isReadOnly), _type(std::move(type))
	{
	}

	bool IsReadOnly()const noexcept { return _isReadOnly; }

	// HACK 
	// Variable could have no TypeSymbol associated
	// std::optional unwraps as the real TypeSymbol or Void
	ConstTypeRef Type()const { return _type.value_or(TypeSymbol::Get(TypeEnum::Void)); }
};

class GlobalVariableSymbol final :public VariableSymbol
{
public:
	GlobalVariableSymbol(string_view name, bool isReadOnly, std::optional<ConstTypeRef> type)noexcept
		:VariableSymbol(name, isReadOnly, std::move(type))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::GlobalVariable; }
};

class LocalVariableSymbol :public VariableSymbol
{
public:
	LocalVariableSymbol(string_view name, bool isReadOnly, std::optional<ConstTypeRef> type)noexcept
		:VariableSymbol(name, isReadOnly, std::move(type))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::LocalVariable; }
};

class ParameterSymbol final : public LocalVariableSymbol
{
public:
	ParameterSymbol(string_view name, std::optional<ConstTypeRef> type)noexcept
		:LocalVariableSymbol(name, true, std::move(type))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::Parameter; }
};

struct ParameterHash final
{
	size_t operator()(const ParameterSymbol& ps)const noexcept;
};

class FunctionSymbol final :public Symbol
{
private:
	vector<ParameterSymbol> _params;
	ConstTypeRef _type;
	const FunctionDeclarationSyntax* _declaration;

public:
	FunctionSymbol(string_view name, vector<ParameterSymbol> params,
		ConstTypeRef type, const FunctionDeclarationSyntax* declaration)
		:Symbol(name), _params(std::move(params)), _type(type), _declaration(declaration)
	{
	}
	FunctionSymbol(string_view name, vector<ParameterSymbol> params, ConstTypeRef type)
		:FunctionSymbol(name, std::move(params), type, nullptr)
	{
	}
	FunctionSymbol()
		:FunctionSymbol("", vector<ParameterSymbol>(), TypeSymbol::Get(TypeEnum::Error))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::Function; }
	constexpr const vector<ParameterSymbol>& Parameters()const noexcept { return _params; }
	ConstTypeRef Type()const noexcept { return _type; }
	constexpr const FunctionDeclarationSyntax* Declaration()const noexcept { return  _declaration; }

};

struct FunctionHash final
{
	size_t operator()(const FunctionSymbol& fs)const noexcept;
	size_t operator()(const shared_ptr<FunctionSymbol>& fs)const noexcept;
};

enum class BuiltinFuncEnum
{
	Input, Print, Rnd
};

const FunctionSymbol& GetBuiltinFunction(BuiltinFuncEnum kind);
const std::array<FunctionSymbol, 3>& GetAllBuiltinFunctions();

class MCF_API ValueType final
{
private:
	std::variant<std::monostate, bool, IntegerType, string> _inner;

public:
	constexpr ValueType() noexcept :_inner(std::monostate()) {}

	/// stays implicit
	constexpr ValueType(const IntegerType& value)noexcept :_inner(value) {}
	constexpr ValueType(const int value)noexcept :_inner(static_cast<IntegerType>(value)) {}
	constexpr ValueType(const bool value)noexcept :_inner(value) {}

	constexpr ValueType(const string& s) : _inner(s) {}
	ValueType(const char* s) : _inner(string(s)) {}

	constexpr bool HasValue()const noexcept { return !std::holds_alternative<std::monostate>(_inner); }
	ConstTypeRef Type()const noexcept;

	constexpr bool operator==(const ValueType& other)const { return _inner == other._inner; }
	constexpr bool operator!=(const ValueType& other)const { return !(_inner == other._inner); }
	bool ToBoolean()const;
	IntegerType ToInteger()const;
	string ToString()const;

	template<typename T>
	constexpr decltype(auto) GetValue() const
	{
		return std::get<T>(_inner);
	}
};

const auto NullValue = ValueType(); // NOTE global constant
MCF_API std::ostream& operator<<(std::ostream& out, const ValueType& value);

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
