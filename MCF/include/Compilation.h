#pragma once

#include <filesystem>
#include <mutex>
#include <unordered_map>

#include "Symbols.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

namespace fs = std::filesystem;

class DiagnosticBag;

class BoundGlobalScope;
class BoundProgram;
class SyntaxTree;

extern "C" using VarMap = std::unordered_map<const VariableSymbol*, ValueType,
	SymbolHash, SymbolEqual>;

class MCF_API EvaluationResult final
{
private:
	DiagnosticBag& _diagnostics;
	ValueType _value;

public:
	EvaluationResult(DiagnosticBag& diagnostics, ValueType value)
		:_diagnostics(diagnostics), _value(std::move(value))
	{
	}

	constexpr DiagnosticBag& Diagnostics() const noexcept { return _diagnostics; }
	constexpr const ValueType& Value()const noexcept { return _value; }
};

class MCF_API Compilation final
{
private:
	bool _isScript;
	unique_ptr<Compilation> _previous;
	vector<unique_ptr<SyntaxTree>> _syntaxTrees;
	unique_ptr<BoundGlobalScope> _globalScope;
	unique_ptr<DiagnosticBag> _diagnostics; //NOTE one bag to rule them all

	std::mutex _mtx;

	unique_ptr<BoundProgram> GetProgram();

	explicit Compilation(bool isScript, unique_ptr<Compilation> previous = nullptr,
		vector<unique_ptr<SyntaxTree>> trees = {});

public:

	Compilation(Compilation&& other) noexcept;
	Compilation& operator=(Compilation&& other) = delete;
	Compilation(const Compilation&) = delete;
	Compilation& operator=(const Compilation&) = delete;
	~Compilation();

	[[nodiscard]] static unique_ptr<Compilation> Create(unique_ptr<SyntaxTree> tree);
	[[nodiscard]] static unique_ptr<Compilation> CreateScript(unique_ptr<Compilation> previous,
		unique_ptr<SyntaxTree> tree);

	constexpr bool IsScript()const noexcept { return _isScript; }
	Compilation* Previous() noexcept { return _previous.get(); }
	const Compilation* Previous()const noexcept { return _previous.get(); }
	const vector<const SyntaxTree*> SynTrees()const noexcept;

	const BoundGlobalScope* GlobalScope();
	const vector<shared_ptr<FunctionSymbol>>& Functions();
	const vector<shared_ptr<VariableSymbol>>& Variables();
	const vector<const Symbol*> GetSymbols();

	[[nodiscard]] EvaluationResult Evaluate(VarMap& variables);
	void EmitTree(std::ostream& out);
	void EmitTree(const FunctionSymbol* symbol, std::ostream& out);
	DiagnosticBag Emit(const string& moduleName, const fs::path& outPath);
};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
