#include "stdafx.h"
#include "VariableSymbol.h"

namespace MCF {

VariableSymbol::VariableSymbol(const string& name, bool readOnly, const type_index& type)
	:_name(name), _isReadOnly(readOnly), _type(type)
{
}

VariableSymbol::VariableSymbol()
	: VariableSymbol(string(), true, typeid(std::monostate))
{
}

VariableSymbol::VariableSymbol(VariableSymbol && other)
	: _name(std::move(other._name)), _isReadOnly(other._isReadOnly),
	_type(other._type)
{
	other._type = typeid(std::monostate);
}

VariableSymbol & VariableSymbol::operator=(VariableSymbol && other)
{
	if (this == &other) return *this;

	_name = std::move(other._name);
	_isReadOnly = other._isReadOnly;
	_type = other._type;
	other._type = typeid(std::monostate);
	return *this;
}

bool VariableSymbol::operator==(const VariableSymbol & other) const noexcept
{
	return _name == other._name && _type == other._type;
}

bool VariableSymbol::operator!=(const VariableSymbol & other) const noexcept
{
	return !(*this == other);
}

size_t VariableHash::operator()(const VariableSymbol & variable) const noexcept
{
	auto h1 = std::hash<string>{}(variable.Name());
	auto h2 = variable.Type().hash_code();
	return h1 ^ (h2 << 1);
}

}//MCF