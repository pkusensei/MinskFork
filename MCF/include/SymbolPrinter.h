#pragma once

#include "IO.h"

namespace MCF{

class Symbol;
class FunctionSymbol;
class GlobalVariableSymbol;
class LocalVariableSymbol;
class ParameterSymbol;
class TypeSymbol;

class SymbolPrinter
{
private:
	TextWriter _writer;

	void WriteFunction(const FunctionSymbol* symbol);
	void WriteGlobalVariable(const GlobalVariableSymbol* symbol);
	void WriteLocalVariable(const LocalVariableSymbol* symbol);
	void WriteParameter(const ParameterSymbol* symbol);
	void WriteType(const TypeSymbol* symbol);

public:
	constexpr explicit SymbolPrinter(std::ostream& out)noexcept 
		:_writer(out)
	{
	}

	void Write(const Symbol* symbol);
};

}//MCF