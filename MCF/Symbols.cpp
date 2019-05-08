#include "stdafx.h"
#include "Symbols.h"

#include <unordered_map>

namespace MCF {

Symbol::Symbol(const string & name)
	:_name(name)
{
}

bool Symbol::operator==(const Symbol & other) const noexcept
{
	return Name() == other.Name() && Kind() == other.Kind();
}

bool Symbol::operator!=(const Symbol & other) const noexcept
{
	return !(*this == other);
}

TypeSymbol::TypeSymbol(const string & name)
	:Symbol(name)
{
}

const TypeSymbol TypeSymbol::GetType(const TypeKind& kind)
{
	switch (kind)
	{
		case TypeKind::Error: return TypeSymbol("?");
		case TypeKind::Bool: return TypeSymbol("bool");
		case TypeKind::Int: return TypeSymbol("int");
		case TypeKind::String: return TypeSymbol("string");
		case TypeKind::Void: return TypeSymbol("void");
		default:
			throw std::invalid_argument("Unexpected TypeKind enum value.");
	}
}

size_t TypeHash::operator()(const TypeSymbol & ts) const noexcept
{
	return std::hash<string>{}(ts.Name());
}

VariableSymbol::VariableSymbol(const string & name, bool isReadOnly, const TypeSymbol & type)
	:Symbol(name), _isReadOnly(isReadOnly), _type(type)
{
}

VariableSymbol::VariableSymbol() : VariableSymbol("", true, TypeSymbol::GetType(TypeKind::Error))
{
}

size_t VariableHash::operator()(const VariableSymbol & vs) const noexcept
{
	auto h1 = std::hash<string>{}(vs.Name());
	auto h2 = TypeHash{}(vs.Type());
	return h1 ^ (h2 << 1);
}

TypeSymbol ValueType::Type() const
{
	switch (_inner.index())
	{
		case 1:
			return TypeSymbol::GetType(TypeKind::Bool);
		case 2:
			return TypeSymbol::GetType(TypeKind::Int);
		case 3:
			return TypeSymbol::GetType(TypeKind::String);
		case 0:
		default:
			return TypeSymbol::GetType(TypeKind::Error);
	}
}

string ValueType::ToString() const
{
	auto id = ValueType::GetValueTypeId(Type());
	auto result = string();
	switch (id)
	{
		case 1:
			result = GetValue<bool>() ? "True" : "False";
			break;
		case 2:
			result = std::to_string(GetValue<IntegerType>());
			break;
		case 3:
			result = GetValue<string>();
		default:
			break;
	}

	return result;
}

int ValueType::GetValueTypeId(const TypeSymbol & inType)
{
	static std::unordered_map<TypeSymbol, int, TypeHash> types = {
		{TypeSymbol::GetType(TypeKind::Error), 0},
		{TypeSymbol::GetType(TypeKind::Bool), 1},
		{TypeSymbol::GetType(TypeKind::Int), 2},
		{TypeSymbol::GetType(TypeKind::String), 3}
	};

	return types.at(inType);
}

std::ostream & operator<<(std::ostream & out, const ValueType & value)
{
	if (value.HasValue())
		out << value.ToString();
	else
		out << "Not valid value or type.\n";
	return out;
}

}