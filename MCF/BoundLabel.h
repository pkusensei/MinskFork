#pragma once

#include "common.h"

namespace MCF {

class BoundLabel final
{
private:
	string _name;
public:
	explicit BoundLabel(const string& name) :_name(name) {}

	bool operator==(const BoundLabel& other) const noexcept { return _name == other._name; }
	bool operator!=(const BoundLabel& other) const noexcept { return !(*this == other); }

	string Name()const { return _name; }
	string ToString()const { return Name(); }
};

struct LabelHash final
{
	size_t operator()(const BoundLabel& label) const noexcept
	{
		return std::hash<string>{}(label.Name());
	}
};

}//MCF
