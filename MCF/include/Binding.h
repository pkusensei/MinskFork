#pragma once

#include <optional>
#include <stack>
#include <unordered_map>

#include "BoundNode.h"
#include "BoundLabel.h"
#include "Symbols.h"

namespace MCF {

class DiagnosticBag;
class TextSpan;

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
	bool TryDeclareSymbol(const shared_ptr<T>& symbol)
	{
		auto name = symbol->Name();
		if (_symbols.find(name) == _symbols.end() && !name.empty())
		{
			_symbols.emplace(name, symbol);
			return true;
		}
		return false;
	}

	template<typename T, typename = std::enable_if_t<std::is_base_of_v<Symbol, T>>>
	std::optional<shared_ptr<T>> TryLookupSymbol(string_view name)const
	{
		if (_symbols.find(name) != _symbols.end())
		{
			auto p = std::dynamic_pointer_cast<T>(_symbols.at(name));
			if (p) return std::make_optional(p);
			else return std::nullopt;
		}
		if (_parent == nullptr) return std::nullopt;
		return _parent->TryLookupSymbol<T>(name);
	}

	template<typename T, typename = std::enable_if_t<std::is_base_of_v<Symbol, T>>>
	const vector<shared_ptr<T>> GetDeclaredSymbols() const
	{
		auto result = vector<shared_ptr<T>>();
		for (const auto& it : _symbols)
		{
			auto p = std::dynamic_pointer_cast<T>(it.second);
			if (p) result.emplace_back(p);
		}
		return result;
	}

public:
	explicit BoundScope(std::nullptr_t)
		:_parent(nullptr)
	{
	}
	explicit BoundScope(unique_ptr<BoundScope>& parent)
		: _parent(std::move(parent))
	{
	}

	const BoundScope* Parent()const noexcept { return _parent.get(); }

	bool TryDeclareVariable(const shared_ptr<VariableSymbol>& variable)
	{
		return TryDeclareSymbol(variable);
	}
	bool TryDeclareFunction(const shared_ptr<FunctionSymbol>& function)
	{
		return TryDeclareSymbol(function);
	}

	decltype(auto) TryLookupVariable(string_view name)const
	{
		return TryLookupSymbol<VariableSymbol>(name);
	}
	decltype(auto) TryLookupFunction(string_view name)const
	{
		return TryLookupSymbol<FunctionSymbol>(name);
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
		unique_ptr<DiagnosticBag>& diagnostics,
		const vector<shared_ptr<FunctionSymbol>>& functions,
		const vector<shared_ptr<VariableSymbol>>& variables,
		const vector<shared_ptr<BoundStatement>>& statements)
		:_previous(previous), _diagnostics(std::move(diagnostics)),
		_functions(functions), _variables(variables), _statements(statements)
	{
	}

	constexpr const BoundGlobalScope* Previous()const noexcept { return _previous; }
	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }
	constexpr const vector<shared_ptr<FunctionSymbol>>& Functions()const { return _functions; }
	constexpr const vector<shared_ptr<VariableSymbol>>& Variables()const { return _variables; }
	constexpr const vector<shared_ptr<BoundStatement>>& Statements()const noexcept { return _statements; }
};

class BoundProgram final
{
public:
	using FuncMap = std::unordered_map<shared_ptr<FunctionSymbol>, unique_ptr<BoundBlockStatement>>;

private:
	unique_ptr<DiagnosticBag> _diagnostics;
	FuncMap _functions;
	unique_ptr<BoundBlockStatement> _statement;

public:
	BoundProgram(unique_ptr<DiagnosticBag>& diagnostics, FuncMap& functions,
		unique_ptr<BoundBlockStatement>& statement)
		:_diagnostics(std::move(diagnostics)), _functions(std::move(functions)),
		_statement(std::move(statement))
	{
	}

	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }
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
		const TypeSymbol& targetType);
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
		const TypeSymbol& type, bool allowExplicit = false);
	shared_ptr<BoundExpression> BindConversion(const TextSpan& diagnosticSpan,
		const shared_ptr<BoundExpression>& syntax,
		const TypeSymbol& type, bool allowExplicit = false);
	shared_ptr<VariableSymbol> BindVariable(const SyntaxToken& identifier,
		bool isReadOnly, const TypeSymbol& type);
	std::optional<TypeSymbol> BindTypeClause(const std::optional<TypeClauseSyntax>& syntax);
	std::optional<TypeSymbol> LookupType(string_view name)const;

	[[nodiscard]] static unique_ptr<BoundScope> CreateParentScope(const BoundGlobalScope* previous);
	[[nodiscard]] static unique_ptr<BoundScope> CreateRootScope();

public:
	Binder(unique_ptr<BoundScope>& parent, const FunctionSymbol* function);

	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }

	static unique_ptr<BoundGlobalScope> BindGlobalScope(const BoundGlobalScope* previous,
		const CompilationUnitSyntax* syntax);
	static unique_ptr<BoundProgram> BindProgram(const BoundGlobalScope* globalScope);
};

}//MCF
