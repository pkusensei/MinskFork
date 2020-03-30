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
	return std::hash<string_view>{}(s.Name());
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

std::reference_wrapper<const TypeSymbol> TypeSymbol::Get(TypeEnum kind)
{
	static const auto error = TypeSymbol("?");
	static const auto bool_ = TypeSymbol("bool");
	static const auto int_ = TypeSymbol("int");
	static const auto string_ = TypeSymbol("string");
	static const auto void_ = TypeSymbol("void");

	switch (kind)
	{
		case TypeEnum::Error: return error;
		case TypeEnum::Bool: return bool_;
		case TypeEnum::Int: return int_;
		case TypeEnum::String: return string_;
		case TypeEnum::Void: return void_;
		default:
			throw std::invalid_argument("Unexpected TypeEnum value.");
	}
}

size_t ParameterHash::operator()(const ParameterSymbol& ps) const noexcept
{
	return std::hash<string_view>{}(ps.Type().get().Name());
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
			TypeSymbol::Get(TypeEnum::String)),
		FunctionSymbol("print",
			vector<ParameterSymbol>{ParameterSymbol("text",
				TypeSymbol::Get(TypeEnum::String))},
			TypeSymbol::Get(TypeEnum::Void)),
		FunctionSymbol("rnd",
			vector<ParameterSymbol>{ParameterSymbol("max",
				TypeSymbol::Get(TypeEnum::Int))},
			TypeSymbol::Get(TypeEnum::Int))
	};
	return funcs;
}

ConstTypeRef ValueType::Type() const noexcept
{
	switch (_inner.index())
	{
		case 1:
			return TypeSymbol::Get(TypeEnum::Bool);
		case 2:
			return TypeSymbol::Get(TypeEnum::Int);
		case 3:
			return TypeSymbol::Get(TypeEnum::String);
		case 0:
		default:
			return TypeSymbol::Get(TypeEnum::Error);
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