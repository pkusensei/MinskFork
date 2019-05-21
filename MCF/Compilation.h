#pragma once

#include <mutex>
#include <stack>
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
class BoundConversionExpression;
class BoundPostfixExpression;

class BoundStatement;
class BoundBlockStatement;
class BoundVariableDeclaration;
class BoundExpressionStatement;
class BoundGotoStatement;
class BoundConditionalGotoStatement;

class BoundGlobalScope;
class BoundProgram;
class SyntaxTree;

extern "C" using VarMap = std::unordered_map<shared_ptr<VariableSymbol>, ValueType,
	SymbolHash, SymbolEqual>;

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
	unique_ptr<SyntaxTree> _syntaxTree;
	unique_ptr<BoundGlobalScope> _globalScope;

	std::mutex _mtx;

public:
	Compilation(unique_ptr<Compilation>& previous, unique_ptr<SyntaxTree>& tree);
	explicit Compilation(unique_ptr<SyntaxTree>& tree);
	~Compilation();
	Compilation(Compilation&&) = default;
	Compilation& operator=(Compilation&&) = default;

	const Compilation* Previous()const noexcept { return _previous.get(); }
	const SyntaxTree* Syntax()const noexcept { return _syntaxTree.get(); }

	const BoundGlobalScope* GlobalScope();
	static unique_ptr<Compilation> ContinueWith(unique_ptr<Compilation>& previous,
												unique_ptr<SyntaxTree>& tree);

	EvaluationResult Evaluate(VarMap& variables);
	void EmitTree(std::ostream& out);
};

}//MCF

