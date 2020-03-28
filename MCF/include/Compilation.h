#pragma once

#include <mutex>
#include <stack>
#include <unordered_map>

#include "Symbols.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

class DiagnosticBag;

class BoundExpression;
class BoundLiteralExpression;
class BoundVariableExpression;
class BoundAssignmentExpression;
class BoundUnaryExpression;
class BoundBinaryExpression;
class BoundCallExpression;
class BoundConversionExpression;
class BoundPostfixExpression;

class BoundBlockStatement;
class BoundVariableDeclaration;
class BoundExpressionStatement;

class BoundGlobalScope;
class BoundProgram;
class SyntaxTree;

extern "C" using VarMap = std::unordered_map<shared_ptr<VariableSymbol>,
	ValueType, SymbolHash, SymbolEqual>;

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

	DiagnosticBag& Diagnostics() const noexcept { return _diagnostics; }
	constexpr const ValueType& Value()const noexcept { return _value; }
};

class Evaluator final
{
private:
	unique_ptr<BoundProgram> _program;
	VarMap* _globals;
	std::stack<VarMap> _locals;
	ValueType _lastValue;

	ValueType EvaluateStatement(const BoundBlockStatement* body);
	void EvaluateVariableDeclaration(const BoundVariableDeclaration* node);
	void EvaluateExpressionStatement(const BoundExpressionStatement* node);

	ValueType EvaluateExpression(const BoundExpression* node);
	ValueType EvaluateLiteralExpression(const BoundLiteralExpression* node)const;
	ValueType EvaluateVariableExpression(const BoundVariableExpression* node);
	ValueType EvaluateAssignmentExpression(const BoundAssignmentExpression* node);
	ValueType EvaluateUnaryExpression(const BoundUnaryExpression* node);
	ValueType EvaluateBinaryExpression(const BoundBinaryExpression* node);
	ValueType EvaluateCallExpression(const BoundCallExpression* node);
	ValueType EvaluateConversionExpression(const BoundConversionExpression* node);
	ValueType EvaluatePostfixExpression(const BoundPostfixExpression* node);

	void Assign(const shared_ptr<VariableSymbol>& variable, const ValueType& value);

public:
	Evaluator(unique_ptr<BoundProgram>& program, VarMap& variables);
	ValueType Evaluate();
};

class MCF_API Compilation final
{
private:
	unique_ptr<Compilation> _previous;
	vector<unique_ptr<SyntaxTree>> _syntaxTrees;
	unique_ptr<BoundGlobalScope> _globalScope;
	unique_ptr<DiagnosticBag> _diagnostics; //NOTE one bag to rule them all

	std::mutex _mtx;

public:
	Compilation(unique_ptr<Compilation> previous, vector<unique_ptr<SyntaxTree>> trees);
	explicit Compilation(vector<unique_ptr<SyntaxTree>> trees);
	Compilation(unique_ptr<Compilation> previous, unique_ptr<SyntaxTree> tree);
	explicit Compilation(unique_ptr<SyntaxTree> tree);

	~Compilation();
	Compilation(Compilation&& other) noexcept;
	Compilation& operator=(Compilation&& other) = delete;

	const Compilation* Previous()const noexcept { return _previous.get(); }
	const vector<const SyntaxTree*> SynTrees()const noexcept;

	const BoundGlobalScope* GlobalScope();
	[[nodiscard]] static unique_ptr<Compilation> ContinueWith(unique_ptr<Compilation> previous,
		unique_ptr<SyntaxTree> tree);

	EvaluationResult Evaluate(VarMap& variables);
	void EmitTree(std::ostream& out);

};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
