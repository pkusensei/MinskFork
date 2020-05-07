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

const std::array<FunctionSymbol, 3>& GetAllBuiltinFunctions()
{
	static const auto funcs = std::array{
		BUILTIN_INPUT, BUILTIN_PRINT, BUILTIN_RND
	};
	return funcs;
}

}//MCF