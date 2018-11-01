#pragma once
#include <tuple>
#include <variant>

#ifdef MCF_EXPORTS
#define MCF_API __declspec(dllexport)
#else
#define MCF_API __declspec(dllimport)
#endif

namespace MCF {

using std::string;
using std::unique_ptr;
using std::vector;

using TextSpan = std::tuple<int, int, int>;

class ValueType final
{
private:
	std::variant<std::monostate, long, bool> _inner;

public:
	ValueType() { }
	~ValueType() = default;
	ValueType(const ValueType& other) :_inner(other._inner) {}

	// stays implicit
	ValueType(const long& value):_inner(value) { }
	ValueType(const bool& value):_inner(value) { }

	bool HasValue()const { return !std::holds_alternative<std::monostate>(_inner); }

	template<typename T>
	decltype(auto) GetValue() const
	{
		return std::get<T>(_inner);
	}	
};

}//MCF