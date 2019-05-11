#pragma once

#include <mutex>
#include <unordered_map>

#include "Symbols.h"

namespace MCF {

class DiagnosticBag;

class BoundExpression;
class BoundLiteralExpression;
class BoundVariableExpression;
class BoundAssignmentExpression;
class BoundUnaryExpression;
class BoundBinaryExpression;
class BoundCallExpression;
class BoundPostfixExpression;

class BoundStatement;
class BoundBlockStatement;
class BoundVariableDeclaration;
class BoundExpressionStatement;
class BoundGotoStatement;
class BoundConditionalGotoStatement;

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
	const BoundBlockStatement* _root;
	std::unordered_map<VariableSymbol, ValueType, VariableHash>* _variables;
	ValueType _lastValue;

	void EvaluateVariableDeclaration(const BoundVariableDeclaration* node);
	void EvaluateExpressionStatement(const BoundExpressionStatement* node);

	ValueType EvaluateExpression(const BoundExpression* node)const;
	ValueType EvaluateLiteralExpression(const BoundLiteralExpression* node)const;
	ValueType EvaluateVariableExpression(const BoundVariableExpression* node)const;
	ValueType EvaluateAssignmentExpression(const BoundAssignmentExpression* node)const;
	ValueType EvaluateUnaryExpression(const BoundUnaryExpression* node)const;
	ValueType EvaluateBinaryExpression(const BoundBinaryExpression* node)const;
	ValueType EvaluateCallExpression(const BoundCallExpression* node)const;
	ValueType EvaluatePostfixExpression(const BoundPostfixExpression* node)const;

public:
	Evaluator(const BoundBlockStatement* root, const std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables);

	ValueType Evaluate();
};

class MCF_API Compilation final
{
private:
	unique_ptr<Compilation> _previous;
	const SyntaxTree* _syntaxTree;
	unique_ptr<BoundGlobalScope> _globalScope;

	std::mutex _mtx;

	unique_ptr<BoundBlockStatement> GetStatement();

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

