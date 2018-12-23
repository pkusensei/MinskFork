#pragma once

#include "common.h"

namespace MCF {

class MCF_API VariableSymbol final
{
private:
	string _name;
	bool _isReadOnly;
	type_index _type; // no move
public:
	VariableSymbol(const string& name, bool readOnly, const type_index& type);
	VariableSymbol();
	VariableSymbol(const VariableSymbol&) = default;
	VariableSymbol(VariableSymbol&& other);
	VariableSymbol& operator=(const VariableSymbol&) = default;
	VariableSymbol& operator=(VariableSymbol&& other);

	string Name()const { return _name; }
	constexpr bool IsReadOnly()const noexcept { return _isReadOnly; }
	type_index Type()const { return _type; }
	string ToString()const { return Name(); }

	bool operator==(const VariableSymbol& other) const noexcept;
	bool operator!=(const VariableSymbol& other) const noexcept;
};

struct MCF_API VariableHash final
{
	size_t operator()(const VariableSymbol& variable) const noexcept;
};

}//MCF