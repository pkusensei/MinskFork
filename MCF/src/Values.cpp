#include "Values.h"

#include <iostream>

#include "Symbols.h"

namespace MCF {

TypeSymbol ValueType::Type() const
{
	switch (_inner.index())
	{
		case 1:
			return TYPE_BOOL;
		case 2:
			return TYPE_INT;
		case 3:
			return TYPE_STRING;
		case 0:
		default:
			return TYPE_ERROR;
	}
}

bool ValueType::ToBoolean() const
{
	switch (_inner.index())
	{
		case 1:
			return GetValue<bool>();
		case 2:
			return GetValue<IntegerType>() > 0;
		default:
			throw std::invalid_argument("Type cannot convert to bool");
	}
}

IntegerType ValueType::ToInteger() const
{
	switch (_inner.index())
	{
		case 1:
			return GetValue<bool>() ? 1 : 0;
		case 2:
			return GetValue<IntegerType>();
		case 3:
			try
			{
				return StringToInteger(GetValue<string>());
			} catch (...)
			{
				[[fallthrough]];
			}
		default:
			throw std::invalid_argument("Type cannot convert to IntegerType");
	}
}

template<typename... Ts> struct overloaded :Ts...{using Ts::operator()...; };
template<typename... Ts> overloaded(Ts...)->overloaded<Ts...>;

string ValueType::ToString() const
{
	using namespace std::string_literals;
	return std::visit(overloaded{
		[](bool arg) { return arg ? "True"s : "False"s; },
		[](IntegerType arg) { return std::to_string(arg); },
		[](const string& arg) { return arg; },
		[](auto&&) { return "Null"s; }
		}, _inner);
}

std::ostream& operator<<(std::ostream& out, const ValueType& value)
{
	if (value.HasValue())
		out << value.ToString();
	else
		out << "Not valid value or type." << NEW_LINE;
	return out;
}

} //MCF