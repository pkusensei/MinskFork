#pragma once

#include <mutex>
#include <unordered_map>

#include "common.h"

namespace MCF {

class DiagnosticBag;
class BoundExpression;
class BoundStatement;
class BoundGlobalScope;
class SyntaxTree;

class MCF_API EvaluationResult final
{
private:
	const DiagnosticBag* _diagnostics;
	ValueType _value;
public:
	EvaluationResult(const DiagnosticBag* diagnostics, const ValueType& value);
	EvaluationResult(EvaluationResult&&) = default;
	EvaluationResult& operator=(EvaluationResult&&) = default;

	const DiagnosticBag* Diagnostics() const noexcept { return _diagnostics; }
	ValueType Value()const { return _value; }
};

class Evaluator final
{
private:
	const BoundStatement* _root;
	std::unordered_map<VariableSymbol, ValueType, VariableHash>* _variables;
	ValueType _lastValue;

	void EvaluateStatement(const BoundStatement* node);
	void EvaluateBlockStatement(const BoundStatement* node);
	void EvaluateVariableDeclaration(const BoundStatement* node);
	void EvaluateIfStatement(const BoundStatement* node);
	void EvaluateWhileStatement(const BoundStatement* node);
	void EvaluateForStatement(const BoundStatement* node);
	void EvaluateExpressionStatement(const BoundStatement* node);

	ValueType EvaluateExpression(const BoundExpression* node)const;
	ValueType EvaluateLiteralExpression(const BoundExpression* node)const;
	ValueType EvaluateVariableExpression(const BoundExpression* node)const;
	ValueType EvaluateAssignmentExpression(const BoundExpression* node)const;
	ValueType EvaluateUnaryExpression(const BoundExpression* node)const;
	ValueType EvaluateBinaryExpression(const BoundExpression* node)const;

public:
	Evaluator(const BoundStatement* root, const std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables);

	ValueType Evaluate();
};

class MCF_API Compilation final
{
private:
	unique_ptr<Compilation> _previous;
	const SyntaxTree* _syntaxTree;
	unique_ptr<BoundGlobalScope> _globalScope;

	std::mutex _mtx;

public:
	Compilation(const unique_ptr<Compilation>& previous, const SyntaxTree& tree);
	Compilation(const unique_ptr<Compilation>& previous, const unique_ptr<SyntaxTree>& tree);
	Compilation();
	explicit Compilation(const SyntaxTree& tree);
	explicit Compilation(const unique_ptr<SyntaxTree>& tree);
	~Compilation();
	Compilation(Compilation&&) = default;
	Compilation& operator=(Compilation&&) = default;

	const Compilation* Previous()const noexcept { return _previous.get(); }
	const SyntaxTree* Syntax()const noexcept { return _syntaxTree; }

	const BoundGlobalScope* GlobalScope();
	static unique_ptr<Compilation> ContinueWith(const unique_ptr<Compilation>& previous, const SyntaxTree& tree);
	static unique_ptr<Compilation> ContinueWith(const unique_ptr<Compilation>& previous, const unique_ptr<SyntaxTree>& tree);

	EvaluationResult Evaluate(std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables);

	void EmitTree(std::ostream& out);
};

}//MCF

