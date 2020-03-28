#pragma once

#include "common.h"

namespace MCF {

class BoundLabel final
{
private:
	string _name;

public:
	explicit BoundLabel(string name) :_name(std::move(name)) {}
	BoundLabel() :BoundLabel(string()) {}

	bool operator==(const BoundLabel& other) const noexcept { return _name == other._name; }
	bool operator!=(const BoundLabel& other) const noexcept { return !(*this == other); }

	string_view Name()const { return _name; }
	string_view ToString()const { return Name(); }
};

struct LabelHash final
{
	size_t operator()(const BoundLabel& label) const noexcept
	{
		return std::hash<string_view>{}(label.Name());
	}
};

}//MCF
