#include "Symbols.h"

#include <sstream>
#include <unordered_map>

#include "Parsing.h"
#include "SymbolPrinter.h"

namespace MCF {

void Symbol::WriteTo(std::ostream& out) const
{
	auto printer = SymbolPrinter(out);
	printer.Write(this);
}

string Symbol::ToString() const
{
	std::ostringstream ss;
	WriteTo(ss);
	return ss.str();
}

bool Symbol::operator==(const Symbol& other) const noexcept
{
	return Name() == other.Name();
}

bool Symbol::operator!=(const Symbol& other) const noexcept
{
	return !(*this == other);
}

size_t SymbolHash::operator()(const Symbol& s) const noexcept
{
	return std::hash<string>{}(s.Name());
}

size_t SymbolHash::operator()(const shared_ptr<Symbol>& s) const noexcept
{
	return SymbolHash{}(*s);
}

bool SymbolEqual::operator()(const Symbol& lhs, const Symbol& rhs) const
{
	return lhs == rhs;
}

bool SymbolEqual::operator()(const shared_ptr<Symbol>& lhs,
	const shared_ptr<Symbol>& rhs) const
{
	return (*lhs) == (*rhs);
}

const TypeSymbol& GetTypeSymbol(const TypeEnum& kind)
{
	static const auto types = std::make_tuple(TypeSymbol("?"), TypeSymbol("bool"),
		TypeSymbol("int"), TypeSymbol("string"), TypeSymbol("void"));

	switch (kind)
	{
		case TypeEnum::Error: return std::get<0>(types);
		case TypeEnum::Bool: return std::get<1>(types);
		case TypeEnum::Int: return std::get<2>(types);
		case TypeEnum::String: return std::get<3>(types);
		case TypeEnum::Void: return std::get<4>(types);
		default:
			throw std::invalid_argument("Unexpected TypeEnum value.");
	}
}

size_t ParameterHash::operator()(const ParameterSymbol& ps) const noexcept
{
	return std::hash<string>{}(ps.Type().Name());
}

size_t FunctionHash::operator()(const FunctionSymbol& fs) const noexcept
{
	auto result = SymbolHash{}(fs);
	for (const auto& it : fs.Parameters())
	{
		auto h = ParameterHash{}(it);
		result = result ^ (h << 1);
	}
	return result;
}

size_t FunctionHash::operator()(const shared_ptr<FunctionSymbol>& fs) const noexcept
{
	return (*this)(*fs);
}

string GetEnumText(const SymbolKind& kind)
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
			return string();
	}
}

const FunctionSymbol& GetBuiltinFunction(const BuiltinFuncEnum& kind)
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

const vector<FunctionSymbol>& GetAllBuiltinFunctions()
{
	static const auto funcs = std::vector<FunctionSymbol>{
		FunctionSymbol("input",
			vector<ParameterSymbol>(),
			GetTypeSymbol(TypeEnum::String)),
		FunctionSymbol("print",
			vector<ParameterSymbol>{ParameterSymbol("text",
				GetTypeSymbol(TypeEnum::String))},
			GetTypeSymbol(TypeEnum::Void)),
		FunctionSymbol("rnd",
			vector<ParameterSymbol>{ParameterSymbol("max",
				GetTypeSymbol(TypeEnum::Int))},
			GetTypeSymbol(TypeEnum::Int))
	};
	return funcs;
}

const TypeSymbol& ValueType::Type() const noexcept
{
	switch (_inner.index())
	{
		case 1:
			return GetTypeSymbol(TypeEnum::Bool);
		case 2:
			return GetTypeSymbol(TypeEnum::Int);
		case 3:
			return GetTypeSymbol(TypeEnum::String);
		case 0:
		default:
			return GetTypeSymbol(TypeEnum::Error);
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
				[[fallthrough]] ;
			}
		default:
			throw std::invalid_argument("Type cannot convert to IntegerType");
	}
}

string ValueType::ToString() const
{
	auto id = _inner.index();
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

size_t ValueType::GetValueTypeId(const TypeSymbol& inType)
{
	static std::unordered_map<TypeSymbol, size_t, SymbolHash> types = {
		{GetTypeSymbol(TypeEnum::Error), 0},
		{GetTypeSymbol(TypeEnum::Bool), 1},
		{GetTypeSymbol(TypeEnum::Int), 2},
		{GetTypeSymbol(TypeEnum::String), 3}
	};

	return types.at(inType);
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