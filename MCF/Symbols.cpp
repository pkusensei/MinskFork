#include "stdafx.h"
#include "Symbols.h"

#include <unordered_map>

#include "EnumHelper.h"

namespace MCF {

bool Symbol::operator==(const Symbol & other) const noexcept
{
	return Name() == other.Name();
}

bool Symbol::operator!=(const Symbol & other) const noexcept
{
	return !(*this == other);
}

const TypeSymbol TypeSymbol::GetType(const TypeEnum& kind)
{
	switch (kind)
	{
		case TypeEnum::Error: return TypeSymbol("?");
		case TypeEnum::Bool: return TypeSymbol("bool");
		case TypeEnum::Int: return TypeSymbol("int");
		case TypeEnum::String: return TypeSymbol("string");
		case TypeEnum::Void: return TypeSymbol("void");
		default:
			throw std::invalid_argument("Unexpected TypeEnum enum value.");
	}
}

size_t TypeHash::operator()(const TypeSymbol & ts) const noexcept
{
	return std::hash<string>{}(ts.Name());
}

size_t VariableHash::operator()(const VariableSymbol & vs) const noexcept
{
	auto h1 = std::hash<string>{}(vs.Name());
	auto h2 = TypeHash{}(vs.Type());
	return h1 ^ (h2 << 1);
}

const FunctionSymbol GetBuiltinFunction(const BuiltinFuncEnum & kind)
{
	switch (kind)
	{
		case BuiltinFuncEnum::Print:
			return FunctionSymbol("print",
								  vector<ParameterSymbol>{ParameterSymbol("text", TypeSymbol::GetType(TypeEnum::String))},
								  TypeSymbol::GetType(TypeEnum::Void));
		case BuiltinFuncEnum::Input:
			return FunctionSymbol("input", vector<ParameterSymbol>(),
								  TypeSymbol::GetType(TypeEnum::String));
		case BuiltinFuncEnum::Rnd:
			return FunctionSymbol("rnd",
								  vector<ParameterSymbol>{ParameterSymbol("max", TypeSymbol::GetType(TypeEnum::Int))},
								  TypeSymbol::GetType(TypeEnum::Int));
		default:
			throw std::invalid_argument("Unexpected BuiltinFuncEnum enum value.");
	}
}

namespace {

const vector<FunctionSymbol> GetAllBuiltinFunctionsImpl()
{
	auto enums = GetAllEnumValue<BuiltinFuncEnum>(BuiltinFuncEnum::Input, BuiltinFuncEnum::Rnd);
	auto funcs = vector<FunctionSymbol>();
	for (const auto& it:enums)
		funcs.emplace_back(GetBuiltinFunction(it));
	funcs.shrink_to_fit();
	return funcs;
}

}

const vector<FunctionSymbol>& GetAllBuiltinFunctions()
{
	static auto funcs = GetAllBuiltinFunctionsImpl();
	return funcs;
}

TypeSymbol ValueType::Type() const
{
	switch (_inner.index())
	{
		case 1:
			return TypeSymbol::GetType(TypeEnum::Bool);
		case 2:
			return TypeSymbol::GetType(TypeEnum::Int);
		case 3:
			return TypeSymbol::GetType(TypeEnum::String);
		case 0:
		default:
			return TypeSymbol::GetType(TypeEnum::Error);
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
		{TypeSymbol::GetType(TypeEnum::Error), 0},
		{TypeSymbol::GetType(TypeEnum::Bool), 1},
		{TypeSymbol::GetType(TypeEnum::Int), 2},
		{TypeSymbol::GetType(TypeEnum::String), 3}
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