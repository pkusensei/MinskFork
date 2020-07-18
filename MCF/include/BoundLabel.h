#pragma once

#ifdef _DEBUG
#include <atomic>
#endif // _DEBUG

#include "common.h"

namespace MCF {

class [[nodiscard]] BoundLabel final
{
private:
	string _name;

#ifdef _DEBUG
	inline static std::atomic_size_t _count{ 0 };
#endif // _DEBUG

public:

	explicit BoundLabel(string name)
		:_name(std::move(name))
	{
#ifdef _DEBUG
		_name += std::to_string(_count.fetch_add(1));
#endif // _DEBUG
	}
	explicit BoundLabel() :BoundLabel(string()) {}

	bool operator==(const BoundLabel& other) const noexcept { return _name == other._name; }
	bool operator!=(const BoundLabel& other) const noexcept { return !(*this == other); }

	string_view Name()const noexcept { return _name; }
	string_view ToString()const noexcept { return Name(); }
};

struct LabelHash final
{
	size_t operator()(const BoundLabel& label) const noexcept
	{
		return std::hash<string_view>{}(label.Name());
	}
};

}//MCF
