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
	explicit Symbol(const string& name);

public:
	virtual ~Symbol() = default;

	virtual SymbolKind Kind() const noexcept = 0;
	string Name() const { return _name; }
	string ToString() const { return Name(); }

	bool operator==(const Symbol& other)const noexcept;
	bool operator!=(const Symbol& other)const noexcept;
};

enum class TypeKind
{
	Error, Bool, Int, String, Void
};

class TypeSymbol final :public Symbol
{
private:
	explicit TypeSymbol(const string& name);

public:
	SymbolKind Kind() const noexcept override { return SymbolKind::Type; }

	//NOTE C++ static variables are initialized in undefined order
	static const TypeSymbol GetType(const TypeKind& kind);
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
	VariableSymbol(const string& name, bool isReadOnly, const TypeSymbol& type);
	VariableSymbol();

	SymbolKind Kind() const noexcept override { return SymbolKind::Variable; }
	bool IsReadOnly()const noexcept { return _isReadOnly; }
	TypeSymbol Type()const { return _type; }
};

struct MCF_API VariableHash
{
	size_t operator()(const VariableSymbol& vs)const noexcept;
};

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
	string ToString()const;

	template<typename T>
	constexpr decltype(auto) GetValue() const
	{
		return std::get<T>(_inner);
	}

	static int GetValueTypeId(const TypeSymbol & inType);
};

const auto NullValue = ValueType(); // NOTE global constant
MCF_API std::ostream& operator<<(std::ostream& out, const ValueType& value);

}//MCF
