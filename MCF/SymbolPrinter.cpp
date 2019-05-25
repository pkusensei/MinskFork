#include "stdafx.h"
#include "SymbolPrinter.h"

#include "Symbols.h"

namespace MCF {

SymbolPrinter::SymbolPrinter(std::ostream & out)
	:_writer(out)
{
}

void SymbolPrinter::Write(const Symbol * symbol)
{
	switch (symbol->Kind())
	{
		case SymbolKind::Function:
		{
			auto p = dynamic_cast<const FunctionSymbol*>(symbol);
			if (p) WriteFunction(p);
			break;
		}
		case SymbolKind::GlobalVariable:
		{
			auto p = dynamic_cast<const GlobalVariableSymbol*> (symbol);
			if (p) WriteGlobalVariable(p);
			break;
		}
		case SymbolKind::LocalVariable:
		{
			auto p = dynamic_cast<const LocalVariableSymbol*>(symbol);
			if (p) WriteLocalVariable(p);
			break;
		}
		case SymbolKind::Parameter:
		{
			auto p = dynamic_cast<const ParameterSymbol*>(symbol);
			if (p) WriteParameter(p);
			break;
		}
		case SymbolKind::Type:
		{
			auto p = dynamic_cast<const TypeSymbol*>(symbol);
			if (p) WriteType(p);
			break;
		}
		default:
			throw std::invalid_argument("Unexpected symbol: "
				+ GetEnumText(symbol->Kind()));
	}
}

void SymbolPrinter::WriteFunction(const FunctionSymbol * symbol)
{
	_writer.WriteKeyword("function ");
	_writer.WriteIdentifier(symbol->Name());
	_writer.WritePunctuation("(");

	for (int i = 0; i < symbol->Parameters().size(); ++i)
	{
		if (i > 0)
			_writer.WritePunctuation(", ");

		Write(&(symbol->Parameters()[i]));
	}

	_writer.WritePunctuation(")");
	_writer.WriteLine();
}

void SymbolPrinter::WriteGlobalVariable(const GlobalVariableSymbol * symbol)
{
	_writer.WriteKeyword(symbol->IsReadOnly() ? "let " : "var ");
	_writer.WriteIdentifier(symbol->Name());
	_writer.WritePunctuation(": ");
	Write(&(symbol->Type()));
}

void SymbolPrinter::WriteLocalVariable(const LocalVariableSymbol * symbol)
{
	_writer.WriteKeyword(symbol->IsReadOnly() ? "let " : "var ");
	_writer.WriteIdentifier(symbol->Name());
	_writer.WritePunctuation(": ");
	Write(&(symbol->Type()));
}

void SymbolPrinter::WriteParameter(const ParameterSymbol * symbol)
{
	_writer.WriteIdentifier(symbol->Name());
	_writer.WritePunctuation(": ");
	Write(&(symbol->Type()));
}

void SymbolPrinter::WriteType(const TypeSymbol * symbol)
{
	_writer.WriteIdentifier(symbol->Name());
}

}//MCF
