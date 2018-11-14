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
	DiagnosticBag* _diagnostics;
	ValueType _value;
public:
	EvaluationResult(DiagnosticBag* diagnostics, const ValueType& value);
	~EvaluationResult() = default;
	EvaluationResult(EvaluationResult&& other);

	DiagnosticBag* Diagnostics() const { return _diagnostics; }
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
	void EvaluateExpressionStatement(const BoundStatement* node);

	ValueType EvaluateExpression(const BoundExpression* node)const;
	ValueType EvaluateLiteralExpression(const BoundExpression* node)const;
	ValueType EvaluateVariableExpression(const BoundExpression* node)const;
	ValueType EvaluateAssignmentExpression(const BoundExpression* node)const;
	ValueType EvaluateUnaryExpression(const BoundExpression* node)const;
	ValueType EvaluateBinaryExpression(const BoundExpression* node)const;

public:
	Evaluator(const BoundStatement* root, std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables);

	ValueType Evaluate();
};

class MCF_API Compilation final
{
private:
	unique_ptr<Compilation> _previous;
	const SyntaxTree* _syntaxTree;
	std::shared_ptr<BoundGlobalScope> _globalScope;

	std::mutex _mtx;

public:
	Compilation(const unique_ptr<Compilation>& previous, const SyntaxTree& tree);
	Compilation(const unique_ptr<Compilation>& previous, const unique_ptr<SyntaxTree>& tree);
	Compilation();
	explicit Compilation(const SyntaxTree& tree);
	explicit Compilation(const unique_ptr<SyntaxTree>& tree);
	~Compilation();
	Compilation(Compilation&& other);
	Compilation& operator=(Compilation&& other);

	Compilation*  Previous()const { return _previous.get(); }
	const SyntaxTree* Syntax()const { return _syntaxTree; }

	std::weak_ptr<BoundGlobalScope> GlobalScope();
	static unique_ptr<Compilation> ContinueWith(const unique_ptr<Compilation>& previous, const SyntaxTree& tree);
	static unique_ptr<Compilation> ContinueWith(const unique_ptr<Compilation>& previous, const unique_ptr<SyntaxTree>& tree);

	EvaluationResult Evaluate(std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables);
};

}//MCF

