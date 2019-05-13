#pragma once

#include <variant>

#include "common.h"

namespace MCF {

enum class SymbolKind
{
	Function,
	Variable,
	Parameter,
	Type,
};

class MCF_API Symbol
{
private:
	string _name;

protected:
	explicit Symbol(const string& name)
		:_name(name)
	{
	}

public:
	virtual ~Symbol() = default;

	virtual SymbolKind Kind() const noexcept = 0;
	string Name() const { return _name; }
	string ToString() const { return Name(); }

	bool operator==(const Symbol& other)const noexcept;
	bool operator!=(const Symbol& other)const noexcept;
};

enum class TypeEnum
{
	Error, Bool, Int, String, Void
};

class TypeSymbol final :public Symbol
{
private:
	explicit TypeSymbol(const string & name)
		:Symbol(name)
	{
	}

public:
	TypeSymbol(const TypeSymbol& other) = default;
	TypeSymbol& operator=(const TypeSymbol& other) = default;

	SymbolKind Kind() const noexcept override { return SymbolKind::Type; }

	static const TypeSymbol GetType(const TypeEnum& kind);
};

struct TypeHash
{
	size_t operator()(const TypeSymbol& ts)const noexcept;
};

class MCF_API VariableSymbol : public Symbol
{
private:
	bool _isReadOnly;
	TypeSymbol _type;

public:
	VariableSymbol(const string & name, bool isReadOnly, const TypeSymbol & type)
		:Symbol(name), _isReadOnly(isReadOnly), _type(type)
	{
	}
	VariableSymbol()
		: VariableSymbol("", true, TypeSymbol::GetType(TypeEnum::Error))
	{
	}
	VariableSymbol(const VariableSymbol& other) = default;
	VariableSymbol& operator=(const VariableSymbol& other) = default;

	SymbolKind Kind() const noexcept override { return SymbolKind::Variable; }
	bool IsReadOnly()const noexcept { return _isReadOnly; }
	TypeSymbol Type()const { return _type; }
};

struct MCF_API VariableHash
{
	size_t operator()(const VariableSymbol& vs)const noexcept;
};

class ParameterSymbol final : public VariableSymbol
{
public:
	ParameterSymbol(const string & name, const TypeSymbol & type)
		:VariableSymbol(name, true, type)
	{
	}
	ParameterSymbol(const ParameterSymbol& other) = default;
	ParameterSymbol& operator=(const ParameterSymbol& other) = default;

	SymbolKind Kind() const noexcept override { return SymbolKind::Parameter; }
};

class FunctionSymbol final :public Symbol
{
private:
	vector<ParameterSymbol> _params;
	TypeSymbol _type;

public:
	FunctionSymbol(const string& name, const vector<ParameterSymbol>& params, const TypeSymbol& type)
		:Symbol(name), _params(params), _type(type)
	{
	}
	FunctionSymbol()
		:FunctionSymbol("", vector<ParameterSymbol>(), TypeSymbol::GetType(TypeEnum::Error))
	{
	}
	FunctionSymbol(const FunctionSymbol& other) = default;
	FunctionSymbol& operator=(const FunctionSymbol& other) = default;

	SymbolKind Kind() const noexcept override { return SymbolKind::Function; }
	const vector<ParameterSymbol>& Parameters()const noexcept { return _params; }
	TypeSymbol Type()const { return _type; }
};

enum class BuiltinFuncEnum
{
	Input, Print, Rnd
};

const FunctionSymbol GetBuiltinFunction(const BuiltinFuncEnum& kind);
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

	constexpr bool HasValue()const noexcept { return !std::holds_alternative<std::monostate>(_inner); }
	TypeSymbol Type()const;

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

	static size_t GetValueTypeId(const TypeSymbol & inType);
};

const auto NullValue = ValueType(); // NOTE global constant
MCF_API std::ostream& operator<<(std::ostream& out, const ValueType& value);

}//MCF
