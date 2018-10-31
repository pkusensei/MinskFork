#pragma once
#include <tuple>
#include <any>

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
	std::any _inner;

public:
	ValueType() { _inner.reset(); }
	// stays implicit
	ValueType(const std::any& value) :_inner(value) {}
	~ValueType() = default;
	ValueType(const ValueType& other) :_inner(other._inner) {}

	// Hack
	size_t TypeHash()const { return HasValue() ? _inner.type().hash_code() : NULL; }
	bool HasValue()const { return _inner.has_value(); }

	template<typename T>
	decltype(auto) GetValue() const
	{
		return std::any_cast<T>(_inner);
	}	
};

}//MCF