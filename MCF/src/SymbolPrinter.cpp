#include "stdafx.h"
#include "SymbolPrinter.h"

#include "Symbols.h"
#include "SyntaxKind.h"
#include "StringHelper.h"

namespace MCF {

class SymbolPrinter
{
private:
	TextWriter _writer;

	void WriteFunction(const FunctionSymbol& symbol);
	void WriteGlobalVariable(const GlobalVariableSymbol& symbol);
	void WriteLocalVariable(const LocalVariableSymbol& symbol);
	void WriteParameter(const ParameterSymbol& symbol);
	void WriteType(const TypeSymbol& symbol);

public:
	constexpr explicit SymbolPrinter(std::ostream& out)noexcept
		:_writer(out)
	{
	}

	void Write(const Symbol& symbol);
};

void Write(const Symbol& symbol, std::ostream& out)
{
	auto w = SymbolPrinter(out);
	w.Write(symbol);
}

void SymbolPrinter::Write(const Symbol& symbol)
{
#define WRITE_SYMBOL(kind) \
case SymbolKind::kind:                                  \
{                                                       \
	auto& p = static_cast<const kind##Symbol&>(symbol); \
	Write##kind(p); break;                              \
}

	switch (symbol.Kind())
	{
		WRITE_SYMBOL(Function);
		WRITE_SYMBOL(GlobalVariable);
		WRITE_SYMBOL(LocalVariable);
		WRITE_SYMBOL(Parameter);
		WRITE_SYMBOL(Type);

		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected symbol: "
				, nameof(symbol.Kind())));
	}

#undef WRITE_SYMBOL
}

void SymbolPrinter::WriteFunction(const FunctionSymbol& symbol)
{
	_writer.WriteKeyword(SyntaxKind::FunctionKeyword);
	_writer.WriteSpace();
	_writer.WriteIdentifier(symbol.Name());
	_writer.WritePunctuation(SyntaxKind::OpenParenthesisToken);

	for (size_t i = 0; i < symbol.Parameters().size(); ++i)
	{
		if (i > 0)
		{
			_writer.WriteKeyword(SyntaxKind::CommaToken);
			_writer.WriteSpace();
		}
		Write(symbol.Parameters()[i]);
	}

	_writer.WritePunctuation(SyntaxKind::CloseParenthesisToken);

	if (symbol.Type() != TYPE_VOID)
	{
		_writer.WritePunctuation(SyntaxKind::ColonToken);
		_writer.WriteSpace();
		WriteType(symbol.Type());
	}
}

void SymbolPrinter::WriteGlobalVariable(const GlobalVariableSymbol& symbol)
{
	_writer.WriteKeyword(symbol.IsReadOnly() ? SyntaxKind::LetKeyword : SyntaxKind::VarKeyword);
	_writer.WriteSpace();
	_writer.WriteIdentifier(symbol.Name());
	_writer.WritePunctuation(SyntaxKind::ColonToken);
	_writer.WriteSpace();
	auto type = symbol.Type();
	Write(type);
}

void SymbolPrinter::WriteLocalVariable(const LocalVariableSymbol& symbol)
{
	_writer.WriteKeyword(symbol.IsReadOnly() ?
		SyntaxKind::LetKeyword : SyntaxKind::VarKeyword);
	_writer.WriteSpace();
	_writer.WriteIdentifier(symbol.Name());
	_writer.WritePunctuation(SyntaxKind::ColonToken);
	_writer.WriteSpace();
	auto type = symbol.Type();
	Write(type);
}

void SymbolPrinter::WriteParameter(const ParameterSymbol& symbol)
{
	_writer.WriteIdentifier(symbol.Name());
	_writer.WritePunctuation(SyntaxKind::ColonToken);
	_writer.WriteSpace();
	auto type = symbol.Type();
	Write(type);
}

void SymbolPrinter::WriteType(const TypeSymbol& symbol)
{
	_writer.WriteIdentifier(symbol.Name());
}

}//MCF
