#pragma once

#include <filesystem>

#include "Symbols.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

namespace fs = std::filesystem;

class Diagnostic;
class DiagnosticBag;

class BoundGlobalScope;
class BoundProgram;
class SyntaxTree;

extern "C" using VarMap = SymbolMap<const VariableSymbol*, ValueType,
	SymbolEqual>;

class MCF_API [[nodiscard]] EvaluationResult final
{
private:
	ValueType _value;
	const DiagnosticBag& _diagnostics;

public:
	EvaluationResult(const DiagnosticBag& diagnostics, ValueType value)
		: _value(std::move(value)), _diagnostics(diagnostics)
	{
	}

	constexpr const DiagnosticBag& Diagnostics() const noexcept { return _diagnostics; };
	constexpr const ValueType& Value()const noexcept { return _value; }
};

class MCF_API [[nodiscard]] Compilation final
{
private:
	vector<unique_ptr<SyntaxTree>> _syntaxTrees;
	unique_ptr<Compilation> _previous;
	unique_ptr<BoundGlobalScope> _globalScope;
	unique_ptr<DiagnosticBag> _diagnostics;
	bool _isScript;

	[[nodiscard]] unique_ptr<BoundProgram> GetProgram();

	explicit Compilation(bool isScript, unique_ptr<Compilation> previous = nullptr,
						 vector<unique_ptr<SyntaxTree>> trees = {});

public:

	Compilation(Compilation&& other);
	Compilation& operator=(Compilation&&) = delete;
	Compilation(const Compilation&) = delete;
	Compilation& operator=(const Compilation&) = delete;
	~Compilation();

	static Compilation Create(unique_ptr<SyntaxTree> tree);
	static Compilation CreateScript(unique_ptr<Compilation> previous,
									unique_ptr<SyntaxTree> tree);

	constexpr bool IsScript()const noexcept { return _isScript; }
	Compilation* Previous() noexcept { return _previous.get(); }
	const Compilation* Previous()const noexcept { return _previous.get(); }
	const vector<const SyntaxTree*> SyntaxTrees()const noexcept;

	[[nodiscard]] const BoundGlobalScope* GlobalScope();
	const vector<shared_ptr<FunctionSymbol>>& Functions();
	const vector<shared_ptr<VariableSymbol>>& Variables();
	const vector<const Symbol*> GetSymbols();

	EvaluationResult Evaluate(VarMap& variables);
	void EmitTree(std::ostream& out);
	void EmitTree(const FunctionSymbol* symbol, std::ostream& out);
	DiagnosticBag Emit(const string& moduleName, const fs::path& srcPath,
					   const fs::path& outPath);
};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
