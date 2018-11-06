#pragma once

#include <unordered_map>
#include "common.h"

namespace MCF {

class DiagnosticBag;
class BoundExpression;
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
	const BoundExpression* _root;
	std::unordered_map<VariableSymbol, ValueType, VariableHash>* _variables;

	ValueType EvaluateExpression(const BoundExpression* node)const;
	ValueType EvaluateLiteralExpression(const BoundExpression* node)const;
	ValueType EvaluateVariableExpression(const BoundExpression* node)const;
	ValueType EvaluateAssignmentExpression(const BoundExpression* node)const;
	ValueType EvaluateUnaryExpression(const BoundExpression* node)const;
	ValueType EvaluateBinaryExpression(const BoundExpression* node)const;

public:
	Evaluator(const unique_ptr<BoundExpression>& root, std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables);

	ValueType Evaluate() { return EvaluateExpression(_root); }
};

class MCF_API Compilation final
{
private:
	const SyntaxTree* _syntax;
public:
	explicit Compilation(const SyntaxTree& tree);
	explicit Compilation(const unique_ptr<SyntaxTree>& tree);
	~Compilation() = default;

	const SyntaxTree* Syntax()const { return _syntax; }
	EvaluationResult Evaluate(std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables);
};

}//MCF

