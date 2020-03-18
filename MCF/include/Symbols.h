#pragma once

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

string_view GetEnumText(SymbolKind kind);

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

struct SymbolHash
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

	friend const TypeSymbol& GetTypeSymbol(const TypeEnum& kind);

};

const TypeSymbol& GetTypeSymbol(const TypeEnum& kind);

class MCF_API VariableSymbol : public Symbol
{
private:
	bool _isReadOnly;
	TypeSymbol _type;

public:
	constexpr VariableSymbol(string_view name, bool isReadOnly, const TypeSymbol& type) noexcept
		:Symbol(name), _isReadOnly(isReadOnly), _type(type)
	{
	}

	bool IsReadOnly()const noexcept { return _isReadOnly; }
	const TypeSymbol& Type()const noexcept { return _type; }
};

class GlobalVariableSymbol final :public VariableSymbol
{
public:
	constexpr GlobalVariableSymbol(string_view name, bool isReadOnly, const TypeSymbol& type)noexcept
		:VariableSymbol(name, isReadOnly, type)
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::GlobalVariable; }
};

class LocalVariableSymbol :public VariableSymbol
{
public:
	constexpr LocalVariableSymbol(string_view name, bool isReadOnly, const TypeSymbol& type)noexcept
		:VariableSymbol(name, isReadOnly, type)
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::LocalVariable; }
};

class ParameterSymbol final : public LocalVariableSymbol
{
public:
	constexpr ParameterSymbol(string_view name, const TypeSymbol& type)noexcept
		:LocalVariableSymbol(name, true, type)
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
	TypeSymbol _type;
	const FunctionDeclarationSyntax* _declaration;

public:
	FunctionSymbol(string_view name, const vector<ParameterSymbol>& params,
		const TypeSymbol& type, const FunctionDeclarationSyntax* declaration)
		:Symbol(name), _params(params), _type(type), _declaration(declaration)
	{
	}
	FunctionSymbol(string_view name, const vector<ParameterSymbol>& params,
		const TypeSymbol& type)
		:FunctionSymbol(name, params, type, nullptr)
	{
	}
	FunctionSymbol()
		:FunctionSymbol("", vector<ParameterSymbol>(), GetTypeSymbol(TypeEnum::Error))
	{
	}

	SymbolKind Kind() const noexcept override { return SymbolKind::Function; }
	const vector<ParameterSymbol>& Parameters()const noexcept { return _params; }
	const TypeSymbol& Type()const noexcept { return _type; }
	const FunctionDeclarationSyntax* Declaration()const noexcept { return  _declaration; }

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

const FunctionSymbol& GetBuiltinFunction(const BuiltinFuncEnum& kind);
const vector<FunctionSymbol>& GetAllBuiltinFunctions();

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
	const TypeSymbol& Type()const noexcept;

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

	static size_t GetValueTypeId(const TypeSymbol& inType);
};

const auto NullValue = ValueType(); // NOTE global constant
MCF_API std::ostream& operator<<(std::ostream& out, const ValueType& value);

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
