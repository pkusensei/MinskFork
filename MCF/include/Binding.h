#pragma once

#include <optional>
#include <stack>
#include <unordered_map>

#include "BoundNode.h"
#include "BoundLabel.h"
#include "SourceText.h"
#include "Symbols.h"

namespace MCF {

class DiagnosticBag;
class TextSpan;
class SyntaxTree;

class SyntaxToken;
class ExpressionSyntax;
class ParenthesizedExpressionSyntax;
class LiteralExpressionSyntax;
class NameExpressionSyntax;
class AssignmentExpressionSyntax;
class UnaryExpressionSyntax;
class BinaryExpressionSyntax;
class CallExpressionSyntax;
class BoundConversionExpression;
class PostfixExpressionSyntax;

class StatementSyntax;
class BlockStatementSyntax;
class VariableDeclarationSyntax;
class TypeClauseSyntax;
class IfStatementSyntax;
class WhileStatementSyntax;
class DoWhileStatementSyntax;
class ForStatementSyntax;
class BreakStatementSyntax;
class ContinueStatementSyntax;
class ReturnStatementSyntax;
class ExpressionStatementSyntax;

class CompilationUnitSyntax;

class BoundExpression;
class BoundErrorExpression;
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
class BoundIfStatement;
class BoundWhileStatement;
class BoundDoWhileStatement;
class BoundForStatement;
class BoundLabelStatement;
class BoundExpressionStatement;
class BoundGotoStatement;
class BoundConditionalGotoStatement;

class BoundScope final
{
private:
	std::unordered_map<string_view, shared_ptr<Symbol>> _symbols;
	unique_ptr<BoundScope> _parent;

	template<typename T, typename = std::enable_if_t<std::is_base_of_v<Symbol, T>>>
	bool TryDeclareSymbol(shared_ptr<T> symbol)
	{
		auto name = symbol->Name();
		if (_symbols.find(name) == _symbols.end() && !name.empty())
		{
			_symbols.emplace(name, std::move(symbol));
			return true;
		}
		return false;
	}

	template<typename T, typename = std::enable_if_t<std::is_base_of_v<Symbol, T>>>
	const vector<shared_ptr<T>> GetDeclaredSymbols() const
	{
		auto result = vector<shared_ptr<T>>();
		for (const auto& [_, symbol] : _symbols)
		{
			auto p = std::dynamic_pointer_cast<T>(symbol);
			if (p)
				result.push_back(std::move(p));
		}
		return result;
	}

public:
	explicit BoundScope(unique_ptr<BoundScope> parent = nullptr)
		: _parent(std::move(parent))
	{
	}

	// NOTE copy ctor vs shared_ptr? 
	BoundScope(const BoundScope& other)
		:_symbols(other._symbols),
		_parent(other._parent ? make_unique<BoundScope>(*other._parent) : nullptr)
	{
	}
	BoundScope& operator=(const BoundScope&) = delete;

	BoundScope(BoundScope&&) = default;
	BoundScope& operator=(BoundScope&&) = default;
	~BoundScope() = default;

	const BoundScope* Parent()const noexcept { return _parent.get(); }

	std::optional<shared_ptr<Symbol>> TryLookupSymbol(string_view name)const;

	bool TryDeclareVariable(shared_ptr<VariableSymbol> variable)
	{
		return TryDeclareSymbol(std::move(variable));
	}
	bool TryDeclareFunction(shared_ptr<FunctionSymbol> function)
	{
		return TryDeclareSymbol(std::move(function));
	}

	const vector<shared_ptr<VariableSymbol>> GetDeclaredVariables()const
	{
		return GetDeclaredSymbols<VariableSymbol>();
	}
	const vector<shared_ptr<FunctionSymbol>> GetDeclaredFunctions()const
	{
		return GetDeclaredSymbols<FunctionSymbol>();
	}

	static void ResetToParent(unique_ptr<BoundScope>& current)noexcept;
};

class BoundGlobalScope final
{
private:
	const BoundGlobalScope* _previous;
	unique_ptr<DiagnosticBag> _diagnostics;
	vector<shared_ptr<FunctionSymbol>> _functions;
	vector<shared_ptr<VariableSymbol>> _variables;
	vector<shared_ptr<BoundStatement>> _statements;

public:
	BoundGlobalScope(const BoundGlobalScope* previous,
		unique_ptr<DiagnosticBag> diagnostics,
		vector<shared_ptr<FunctionSymbol>> functions,
		vector<shared_ptr<VariableSymbol>> variables,
		vector<shared_ptr<BoundStatement>> statements)
		:_previous(previous), _diagnostics(std::move(diagnostics)),
		_functions(std::move(functions)), _variables(std::move(variables)),
		_statements(std::move(statements))
	{
	}

	constexpr const BoundGlobalScope* Previous()const noexcept { return _previous; }
	DiagnosticBag& Diagnostics()const noexcept { return *_diagnostics; }
	constexpr const vector<shared_ptr<FunctionSymbol>>& Functions()const noexcept { return _functions; }
	constexpr const vector<shared_ptr<VariableSymbol>>& Variables()const noexcept { return _variables; }
	constexpr const vector<shared_ptr<BoundStatement>>& Statements()const noexcept { return _statements; }
};

class BoundProgram final
{
public:
	using FuncMap = std::unordered_map<const FunctionSymbol*, unique_ptr<BoundBlockStatement>>;

private:
	unique_ptr<BoundProgram> _previous;
	unique_ptr<DiagnosticBag> _diagnostics;
	FuncMap _functions;
	unique_ptr<BoundBlockStatement> _statement;

public:
	BoundProgram(unique_ptr<BoundProgram> previous,
		unique_ptr<DiagnosticBag> diagnostics,
		FuncMap functions,
		unique_ptr<BoundBlockStatement> statement)
		:_previous(std::move(previous)),
		_diagnostics(std::move(diagnostics)),
		_functions(std::move(functions)),
		_statement(std::move(statement))
	{
	}

	const BoundProgram* Previous()const noexcept { return _previous.get(); }
	DiagnosticBag& Diagnostics()const noexcept { return *_diagnostics; }
	constexpr const FuncMap& Functions()const noexcept { return _functions; }
	const BoundBlockStatement* Statement()const noexcept { return _statement.get(); }
};

class Binder final
{
private:
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<BoundScope> _scope;
	const FunctionSymbol* _function;
	std::stack<std::pair<BoundLabel, BoundLabel>> _loopStack; //break-continue pair
	size_t _labelCount;

	void BindFunctionDeclaration(const FunctionDeclarationSyntax* syntax);

	shared_ptr<BoundStatement> BindErrorStatement();
	shared_ptr<BoundStatement> BindStatement(const StatementSyntax* syntax);
	shared_ptr<BoundStatement> BindBlockStatement(const BlockStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindVariableDeclaration(const VariableDeclarationSyntax* syntax);
	shared_ptr<BoundStatement> BindIfStatement(const IfStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindWhileStatement(const WhileStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindDoWhileStatement(const DoWhileStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindForStatement(const ForStatementSyntax* syntax);
	auto BindLoopBody(const StatementSyntax* syntax)
		->std::tuple<shared_ptr<BoundStatement>, BoundLabel, BoundLabel>;

	shared_ptr<BoundStatement> BindBreakStatement(const BreakStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindContinueStatement(const ContinueStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindReturnStatement(const ReturnStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindExpressionStatement(const ExpressionStatementSyntax* syntax);

	shared_ptr<BoundExpression> BindExpression(const ExpressionSyntax* syntax,
		ConstTypeRef targetType);
	shared_ptr<BoundExpression> BindExpression(const ExpressionSyntax* syntax,
		bool canBeVoid = false);
	shared_ptr<BoundExpression> BindExpressionInternal(const ExpressionSyntax* syntax);
	shared_ptr<BoundExpression> BindParenthesizedExpression(const ParenthesizedExpressionSyntax* syntax);
	shared_ptr<BoundExpression> BindLiteralExpression(const LiteralExpressionSyntax* syntax);
	shared_ptr<BoundExpression> BindNameExpression(const NameExpressionSyntax* syntax);
	shared_ptr<BoundExpression> BindAssignmentExpression(const AssignmentExpressionSyntax* syntax);
	shared_ptr<BoundExpression> BindUnaryExpression(const UnaryExpressionSyntax* syntax);
	shared_ptr<BoundExpression> BindBinaryExpression(const BinaryExpressionSyntax* syntax);
	shared_ptr<BoundExpression> BindCallExpression(const CallExpressionSyntax* syntax);
	shared_ptr<BoundExpression> BindPostfixExpression(const PostfixExpressionSyntax* syntax);

	shared_ptr<BoundExpression> BindConversion(const ExpressionSyntax* syntax,
		ConstTypeRef type, bool allowExplicit = false);
	shared_ptr<BoundExpression> BindConversion(
		TextLocation diagLocation, shared_ptr<BoundExpression> syntax,
		ConstTypeRef type, bool allowExplicit = false);
	shared_ptr<VariableSymbol> BindVariableDeclaration(const SyntaxToken& identifier,
		bool isReadOnly, ConstTypeRef type);
	std::optional<shared_ptr<VariableSymbol>> BindVariableReference(const SyntaxToken& identifier);
	std::optional<ConstTypeRef> BindTypeClause(const std::optional<TypeClauseSyntax>& syntax);
	std::optional<ConstTypeRef> LookupType(string_view name)const;

	[[nodiscard]] static unique_ptr<BoundScope> CreateParentScope(const BoundGlobalScope* previous);
	[[nodiscard]] static unique_ptr<BoundScope> CreateRootScope();

public:
	Binder(unique_ptr<BoundScope> parent, const FunctionSymbol* function);

	DiagnosticBag& Diagnostics()const noexcept { return *_diagnostics; }

	static unique_ptr<BoundGlobalScope> BindGlobalScope(const BoundGlobalScope* previous,
		const vector<const SyntaxTree*>& synTrees);
	static unique_ptr<BoundProgram> BindProgram(unique_ptr<BoundProgram> preious,
		const BoundGlobalScope* globalScope);
};

}//MCF
