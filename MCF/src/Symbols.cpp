#include "Symbols.h"

#include <sstream>
#include <unordered_map>

#include "Parsing.h"
#include "SymbolPrinter.h"

namespace MCF {

void Symbol::WriteTo(std::ostream& out) const
{
	Write(*this, out);
}

string Symbol::ToString() const
{
	std::ostringstream ss;
	WriteTo(ss);
	return ss.str();
}

string_view TypeSymbol::nameof(TypeEnum kind)
{
	switch (kind)
	{
		case MCF::TypeEnum::Error:
			return "?";
		case MCF::TypeEnum::Any:
			return "any";
		case MCF::TypeEnum::Bool:
			return "bool";
		case MCF::TypeEnum::Int:
			return "int";
		case MCF::TypeEnum::String:
			return "string";
		case MCF::TypeEnum::Void:
			return "void";
		default:
			throw std::invalid_argument("Unexpected TypeEnum value.");
	}
}

string_view nameof(SymbolKind kind)
{
	switch (kind)
	{
		case SymbolKind::Function:
			return "Function";
		case SymbolKind::GlobalVariable:
			return "GlobalVariable";
		case SymbolKind::LocalVariable:
			return "LocalVariable";
		case SymbolKind::Parameter:
			return "Parameter";
		case SymbolKind::Type:
			return "Type";
		default:
			return string_view();
	}
}

const FunctionSymbol& GetBuiltinFunction(BuiltinFuncEnum kind)
{
	static const auto& funcs = GetAllBuiltinFunctions();

	switch (kind)
	{
		case BuiltinFuncEnum::Input: return funcs[0];
		case BuiltinFuncEnum::Print: return funcs[1];
		case BuiltinFuncEnum::Rnd: return funcs[2];
		default:
			throw std::invalid_argument("Unexpected BuiltinFuncEnum enum value.");
	}
}

const std::array<FunctionSymbol, 3>& GetAllBuiltinFunctions()
{
	static const auto funcs = std::array{
		FunctionSymbol("input",
			vector<ParameterSymbol>(),
			TypeSymbol(TypeEnum::String)),
		FunctionSymbol("print",
			vector<ParameterSymbol>{ParameterSymbol("text",
				TypeSymbol(TypeEnum::String))},
			TypeSymbol(TypeEnum::Void)),
		FunctionSymbol("rnd",
			vector<ParameterSymbol>{ParameterSymbol("max",
				TypeSymbol(TypeEnum::Int))},
			TypeSymbol(TypeEnum::Int))
	};
	return funcs;
}

TypeSymbol ValueType::Type() const
{
	switch (_inner.index())
	{
		case 1:
			return TypeSymbol(TypeEnum::Bool);
		case 2:
			return TypeSymbol(TypeEnum::Int);
		case 3:
			return TypeSymbol(TypeEnum::String);
		case 0:
		default:
			return TypeSymbol(TypeEnum::Error);
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

}//MCF