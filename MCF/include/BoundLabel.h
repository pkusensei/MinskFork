#pragma once

#ifdef _DEBUG
#include <atomic>
#endif // _DEBUG

#include "common.h"

namespace MCF {

struct [[nodiscard]] BoundLabel final
{
	string Name;

#ifdef _DEBUG
private:
	inline static std::atomic_size_t _count{ 0 };
#endif // _DEBUG

public:

	explicit BoundLabel(string name)
#ifndef _DEBUG
		noexcept
#endif // !_DEBUG
		: Name(std::move(name))
	{
#ifdef _DEBUG
		Name += std::to_string(_count.fetch_add(1));
#endif // _DEBUG
	}
	explicit BoundLabel()
#ifndef _DEBUG
		noexcept
#endif // !_DEBUG
		:BoundLabel(string())
	{
	}

	bool operator==(const BoundLabel& other) const noexcept { return Name == other.Name; }
	bool operator!=(const BoundLabel& other) const noexcept { return !(*this == other); }

	string_view ToString()const noexcept { return Name; }
};

struct LabelHash final
{
	size_t operator()(const BoundLabel& label) const noexcept
	{
		return std::hash<string>{}(label.Name);
	}
};

}//MCF
