#pragma once

#include "Diagnostic.h"
#include "Symbols.h"

namespace MCF {

struct BoundStatement;
struct BoundBlockStatement;

class DiagnosticBag;
class SyntaxTree;

struct [[nodiscard]] BoundGlobalScope final
{
	vector<unique_ptr<FunctionSymbol>> Functions;
	vector<unique_ptr<VariableSymbol>> Variables;
	vector<shared_ptr<BoundStatement>> Statements;
	unique_ptr<DiagnosticBag> Diagnostics;
	unique_ptr<FunctionSymbol> MainFunc;
	unique_ptr<FunctionSymbol> ScriptFunc;
	const BoundGlobalScope* Previous;

public:
	BoundGlobalScope(const BoundGlobalScope* previous,
					 unique_ptr<DiagnosticBag> diagnostics,
					 unique_ptr<FunctionSymbol> mainFunc,
					 unique_ptr<FunctionSymbol> scriptFunc,
					 vector<unique_ptr<FunctionSymbol>> functions,
					 vector<unique_ptr<VariableSymbol>> variables,
					 vector<shared_ptr<BoundStatement>> statements)
		:Functions(std::move(functions)),
		Variables(std::move(variables)),
		Statements(std::move(statements)),
		Diagnostics(std::move(diagnostics)),
		MainFunc(std::move(mainFunc)),
		ScriptFunc(std::move(scriptFunc)),
		Previous(previous)
	{
	}

};

struct [[nodiscard]] BoundProgram final
{
	using FuncMap = SymbolMap<const FunctionSymbol*, unique_ptr<BoundBlockStatement>,
		SymbolEqual>;

	FuncMap Functions;
	unique_ptr<BoundProgram> Previous;
	unique_ptr<DiagnosticBag> Diagnostics;
	const FunctionSymbol* MainFunc;
	const FunctionSymbol* ScriptFunc;

public:
	BoundProgram(unique_ptr<BoundProgram> previous,
				 unique_ptr<DiagnosticBag> diagnostics,
				 const FunctionSymbol* mainFunc,
				 const FunctionSymbol* scriptFunc,
				 FuncMap functions)
		:Functions(std::move(functions)),
		Previous(std::move(previous)),
		Diagnostics(std::move(diagnostics)),
		MainFunc(mainFunc),
		ScriptFunc(scriptFunc)
	{
	}

};

BoundGlobalScope BindGlobalScope(bool isScript,
								 const BoundGlobalScope* previous,
								 const vector<const SyntaxTree*>& trees);

BoundProgram BindProgram(bool isScript,
						 unique_ptr<BoundProgram> preious,
						 const BoundGlobalScope* globalScope);

}//MCF
