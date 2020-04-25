#pragma once

#include <unordered_map>

#include "Symbols.h"

namespace MCF {

class BoundStatement;
class BoundBlockStatement;

class DiagnosticBag;
class SyntaxTree;

class BoundGlobalScope final
{
private:
	const BoundGlobalScope* _previous;
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<FunctionSymbol> _main;
	unique_ptr<FunctionSymbol> _script;
	vector<shared_ptr<FunctionSymbol>> _functions;
	vector<shared_ptr<VariableSymbol>> _variables;
	vector<shared_ptr<BoundStatement>> _statements;

public:
	BoundGlobalScope(const BoundGlobalScope* previous,
		unique_ptr<DiagnosticBag> diagnostics,
		unique_ptr<FunctionSymbol> mainFunc,
		unique_ptr<FunctionSymbol> scriptFunc,
		vector<shared_ptr<FunctionSymbol>> functions,
		vector<shared_ptr<VariableSymbol>> variables,
		vector<shared_ptr<BoundStatement>> statements)
		:_previous(previous), _diagnostics(std::move(diagnostics)),
		_main(std::move(mainFunc)),
		_script(std::move(scriptFunc)),
		_functions(std::move(functions)), _variables(std::move(variables)),
		_statements(std::move(statements))
	{
	}

	constexpr const BoundGlobalScope* Previous()const noexcept { return _previous; }
	DiagnosticBag& Diagnostics()const noexcept { return *_diagnostics; }
	const FunctionSymbol* MainFunc()const noexcept { return _main.get(); }
	const FunctionSymbol* ScriptFunc()const noexcept { return _script.get(); }
	constexpr const vector<shared_ptr<FunctionSymbol>>& Functions()const noexcept { return _functions; }
	constexpr const vector<shared_ptr<VariableSymbol>>& Variables()const noexcept { return _variables; }
	constexpr const vector<shared_ptr<BoundStatement>>& Statements()const noexcept { return _statements; }
};

class BoundProgram final
{
public:
	using FuncMap = std::unordered_map<const FunctionSymbol*, unique_ptr<BoundBlockStatement>,
		SymbolHash, SymbolEqual>;

private:
	unique_ptr<BoundProgram> _previous;
	unique_ptr<DiagnosticBag> _diagnostics;
	const FunctionSymbol* _main;
	const FunctionSymbol* _script;
	FuncMap _functions;

public:
	BoundProgram(unique_ptr<BoundProgram> previous,
		unique_ptr<DiagnosticBag> diagnostics,
		const FunctionSymbol* mainFunc,
		const FunctionSymbol* scriptFunc,
		FuncMap functions)
		:_previous(std::move(previous)),
		_diagnostics(std::move(diagnostics)),
		_main(mainFunc),
		_script(scriptFunc),
		_functions(std::move(functions))
	{
	}

	const BoundProgram* Previous()const noexcept { return _previous.get(); }
	DiagnosticBag& Diagnostics()const noexcept { return *_diagnostics; }
	constexpr const FunctionSymbol* MainFunc()const noexcept { return _main; }
	constexpr const FunctionSymbol* ScriptFunc()const noexcept { return _script; }
	constexpr const FuncMap& Functions()const noexcept { return _functions; }
};

unique_ptr<BoundGlobalScope> BindGlobalScope(bool isScript,
	const BoundGlobalScope* previous,
	const vector<const SyntaxTree*>& synTrees);

unique_ptr<BoundProgram> BindProgram(bool isScript,
	unique_ptr<BoundProgram> preious,
	const BoundGlobalScope* globalScope);

}//MCF
