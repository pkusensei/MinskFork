#include "stdafx.h"
#include "SymbolPrinter.h"

#include "Symbols.h"
#include "SyntaxKind.h"
#include "helpers.h"

namespace MCF {

void SymbolPrinter::Write(const Symbol* symbol)
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
			throw std::invalid_argument(BuildStringFrom("Unexpected symbol: "
				, nameof(symbol->Kind())));
	}
}

void SymbolPrinter::WriteFunction(const FunctionSymbol* symbol)
{
	_writer.WriteKeyword(SyntaxKind::FunctionKeyword);
	_writer.WriteSpace();
	_writer.WriteIdentifier(symbol->Name());
	_writer.WritePunctuation(SyntaxKind::OpenParenthesisToken);

	for (size_t i = 0; i < symbol->Parameters().size(); ++i)
	{
		if (i > 0)
		{
			_writer.WriteKeyword(SyntaxKind::CommaToken);
			_writer.WriteSpace();
		}
		Write(&(symbol->Parameters()[i]));
	}

	_writer.WritePunctuation(SyntaxKind::CloseParenthesisToken);
	_writer.WriteLine();
}

void SymbolPrinter::WriteGlobalVariable(const GlobalVariableSymbol* symbol)
{
	_writer.WriteKeyword(symbol->IsReadOnly() ? SyntaxKind::LetKeyword : SyntaxKind::VarKeyword);
	_writer.WriteSpace();
	_writer.WriteIdentifier(symbol->Name());
	_writer.WritePunctuation(SyntaxKind::ColonToken);
	_writer.WriteSpace();
	auto type = symbol->Type();
	Write(&type.get());
}

void SymbolPrinter::WriteLocalVariable(const LocalVariableSymbol* symbol)
{
	_writer.WriteKeyword(symbol->IsReadOnly() ?
		SyntaxKind::LetKeyword : SyntaxKind::VarKeyword);
	_writer.WriteSpace();
	_writer.WriteIdentifier(symbol->Name());
	_writer.WritePunctuation(SyntaxKind::ColonToken);
	_writer.WriteSpace();
	auto type = symbol->Type();
	Write(&type.get());
}

void SymbolPrinter::WriteParameter(const ParameterSymbol* symbol)
{
	_writer.WriteIdentifier(symbol->Name());
	_writer.WritePunctuation(SyntaxKind::ColonToken);
	_writer.WriteSpace();
	auto type = symbol->Type();
	Write(&type.get());
}

void SymbolPrinter::WriteType(const TypeSymbol* symbol)
{
	_writer.WriteIdentifier(symbol->Name());
}

}//MCF
