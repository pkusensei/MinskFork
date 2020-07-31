#pragma once

#include <cassert>
#include <variant>

#include "common.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

struct TypeSymbol;

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
	const TypeSymbol& Type()const noexcept;

	constexpr bool operator==(const ValueType& other)const { return _inner == other._inner; }
	constexpr bool operator!=(const ValueType& other)const { return !(_inner == other._inner); }
	bool ToBoolean()const;
	IntegerType ToInteger()const;
	string ToString()const;

	template<typename T>
	constexpr decltype(auto) GetValue() const
	{
		auto p = std::get_if<T>(&_inner);
		assert(p && "Variant should contain valid value; otherwise logic ouside this class is wrong.");
		return *p;
	}
};

inline const auto NULL_VALUE = ValueType(); // NOTE global constant
MCF_API std::ostream& operator<<(std::ostream& out, const ValueType& value);

using BoundConstant = ValueType;

} //MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
