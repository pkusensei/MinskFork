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
	switch (symbol.Kind())
	{
		case SymbolKind::Function:
		{
			auto& p = static_cast<const FunctionSymbol&>(symbol);
			WriteFunction(p);
			break;
		}
		case SymbolKind::GlobalVariable:
		{
			auto& p = static_cast<const GlobalVariableSymbol&>(symbol);
			WriteGlobalVariable(p);
			break;
		}
		case SymbolKind::LocalVariable:
		{
			auto& p = static_cast<const LocalVariableSymbol&>(symbol);
			WriteLocalVariable(p);
			break;
		}
		case SymbolKind::Parameter:
		{
			auto& p = static_cast<const ParameterSymbol&>(symbol);
			WriteParameter(p);
			break;
		}
		case SymbolKind::Type:
		{
			auto& p = static_cast<const TypeSymbol&>(symbol);
			WriteType(p);
			break;
		}
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected symbol: "
				, nameof(symbol.Kind())));
	}
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
