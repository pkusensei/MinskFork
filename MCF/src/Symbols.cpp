#include "Symbols.h"

#include <sstream>

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
#define NAME(kind)\
case SymbolKind::kind: return #kind;

	switch (kind)
	{
		NAME(Function);
		NAME(GlobalVariable);
		NAME(LocalVariable);
		NAME(Parameter);
		NAME(Type);

		default:
			return string_view();
	}

#undef NAME
}

const std::array<FunctionSymbol, 3>& GetAllBuiltinFunctions()
{
	static const auto funcs = std::array{
		BUILTIN_INPUT, BUILTIN_PRINT, BUILTIN_RND
	};
	return funcs;
}

}//MCF