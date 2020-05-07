#pragma once

#include <variant>

#include "common.h"

namespace MCF {

class TypeSymbol;

class MCF_API ValueType final
{
private:
	std::variant<std::monostate, bool, IntegerType, string> _inner;

public:
	constexpr ValueType() noexcept :_inner(std::monostate()) {}

	/// stays implicit
	constexpr ValueType(bool value)noexcept :_inner(value) {}
	constexpr ValueType(IntegerType value)noexcept :_inner(value) {}

	ValueType(string s) : _inner(std::move(s)) {}
	ValueType(const char* s) : _inner(string(s)) {}

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
};

inline const auto NULL_VALUE = ValueType(); // NOTE global constant
MCF_API std::ostream& operator<<(std::ostream& out, const ValueType& value);

using BoundConstant = ValueType;

} //MCF