#pragma once

#include <optional>
#include <unordered_map>

#include "BoundNode.h"
#include "Symbols.h"

namespace MCF {

class DiagnosticBag;
class BoundLabel;
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
	std::unordered_map<string, shared_ptr<Symbol>> _symbols;
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
	bool TryLookupSymbol(const string& name, shared_ptr<T>& symbol)const
	{
		if (_symbols.find(name) != _symbols.end())
		{
			auto p = std::dynamic_pointer_cast<T>(_symbols.at(name));
			if (p)
			{
				symbol = p;
				return true;
			}
			return false;
		}
		if (_parent == nullptr) return false;
		return _parent->TryLookupSymbol<T>(name, symbol);
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
	explicit BoundScope(std::nullptr_t);
	explicit BoundScope(unique_ptr<BoundScope>& parent);

	const BoundScope* Parent()const noexcept { return _parent.get(); }

	bool TryDeclareVariable(const shared_ptr<VariableSymbol>& variable);
	bool TryDeclareFunction(const shared_ptr<FunctionSymbol>& function);

	bool TryLookupVariable(const string& name, shared_ptr<VariableSymbol>& variable)const;
	bool TryLookupFunction(const string& name, shared_ptr<FunctionSymbol>& function)const;

	const vector<shared_ptr<VariableSymbol>> GetDeclaredVariables()const;
	const vector<shared_ptr<FunctionSymbol>> GetDeclaredFunctions()const;

	static void ResetToParent(unique_ptr<BoundScope>& current);
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
					 const vector<shared_ptr<BoundStatement>>& statements);

	const BoundGlobalScope* Previous()const noexcept { return _previous; }
	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }
	const vector<shared_ptr<FunctionSymbol>>& Functions()const { return _functions; }
	const vector<shared_ptr<VariableSymbol>>& Variables()const { return _variables; }
	const vector<shared_ptr<BoundStatement>>& Statements()const noexcept { return _statements; }
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
				 unique_ptr<BoundBlockStatement>& statement);
	BoundProgram(BoundProgram&&) = default;
	BoundProgram& operator=(BoundProgram&&) = default;

	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }
	decltype(auto) Functions()const noexcept { return &_functions; }
	const BoundBlockStatement* Statement()const noexcept { return _statement.get(); }
};

class Binder final
{
private:
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<BoundScope> _scope;
	const FunctionSymbol* _function;

	void BindFunctionDeclaration(const FunctionDeclarationSyntax* syntax);

	shared_ptr<BoundStatement> BindStatement(const StatementSyntax* syntax);
	shared_ptr<BoundStatement> BindBlockStatement(const BlockStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindVariableDeclaration(const VariableDeclarationSyntax* syntax);
	shared_ptr<BoundStatement> BindIfStatement(const IfStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindWhileStatement(const WhileStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindDoWhileStatement(const DoWhileStatementSyntax* syntax);
	shared_ptr<BoundStatement> BindForStatement(const ForStatementSyntax* syntax);
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
											   const TypeSymbol& type,
											   bool allowExplicit = false);
	shared_ptr<BoundExpression> BindConversion(const TextSpan& diagnosticSpan,
											   const shared_ptr<BoundExpression>& syntax,
											   const TypeSymbol& type,
											   bool allowExplicit = false);
	shared_ptr<VariableSymbol> BindVariable(const SyntaxToken& identifier, bool isReadOnly,
											const TypeSymbol& type);
	std::optional<TypeSymbol> BindTypeClause(const std::optional<TypeClauseSyntax>& syntax);
	std::optional<TypeSymbol> LookupType(const string& name)const;

	static unique_ptr<BoundScope> CreateParentScope(const BoundGlobalScope* previous);
	static unique_ptr<BoundScope> CreateRootScope();

public:
	explicit Binder(unique_ptr<BoundScope>& parent, const FunctionSymbol* function);

	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }

	static unique_ptr<BoundGlobalScope> BindGlobalScope(const BoundGlobalScope* previous,
														const CompilationUnitSyntax* syntax);
	static unique_ptr<BoundProgram> BindProgram(const BoundGlobalScope* globalScope);
};

class BoundTreeRewriter
{
	/*
	* HACK The BoundTreeRewriter class takes the existing bound tree and analyzes
	* its structure. Its return value is a completely newly constructed bound tree.
	* The original C# version tries its best to reduce memory allocation. This one
	* does not.
	*/
protected:
	virtual shared_ptr<BoundStatement> RewriteBlockStatement(const shared_ptr<BoundBlockStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteVariableDeclaration(const shared_ptr<BoundVariableDeclaration>& node);
	virtual shared_ptr<BoundStatement> RewriteIfStatement(const shared_ptr<BoundIfStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteWhileStatement(const shared_ptr<BoundWhileStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteDoWhileStatement(const shared_ptr<BoundDoWhileStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteForStatement(const shared_ptr<BoundForStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteLabelStatement(const shared_ptr<BoundLabelStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteGotoStatement(const shared_ptr<BoundGotoStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteConditionalGotoStatement(const shared_ptr<BoundConditionalGotoStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteExpressionStatement(const shared_ptr<BoundExpressionStatement>& node);

	virtual shared_ptr<BoundExpression> RewriteErrorExpression(const shared_ptr<BoundErrorExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteLiteralExpression(const shared_ptr<BoundLiteralExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteVariableExpression(const shared_ptr<BoundVariableExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteAssignmentExpression(const shared_ptr<BoundAssignmentExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteUnaryExpression(const shared_ptr<BoundUnaryExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteBinaryExpression(const shared_ptr<BoundBinaryExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteCallExpression(const shared_ptr<BoundCallExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteConversionExpression(const shared_ptr<BoundConversionExpression>& node);
	virtual shared_ptr<BoundExpression> RewritePostfixExpression(const shared_ptr<BoundPostfixExpression>& node);

public:
	virtual ~BoundTreeRewriter() = default;

	virtual shared_ptr<BoundStatement> RewriteStatement(const shared_ptr<BoundStatement>& node);
	virtual shared_ptr<BoundExpression> RewriteExpression(const shared_ptr<BoundExpression>& node);
};

class Lowerer final :public BoundTreeRewriter
{
private:
	size_t _labelCount{0};

	Lowerer() = default;
	BoundLabel GenerateLabel();
	static unique_ptr<BoundBlockStatement> Flatten(const shared_ptr<BoundStatement>& statement);

protected:
	shared_ptr<BoundStatement> RewriteIfStatement(const shared_ptr<BoundIfStatement>& node)override;
	shared_ptr<BoundStatement> RewriteWhileStatement(const shared_ptr<BoundWhileStatement>& node)override;
	shared_ptr<BoundStatement> RewriteDoWhileStatement(const shared_ptr<BoundDoWhileStatement>& node)override;
	shared_ptr<BoundStatement> RewriteForStatement(const shared_ptr<BoundForStatement>& node)override;

public:
	static unique_ptr<BoundBlockStatement> Lower(const shared_ptr<BoundStatement>& statement);
};

}//MCF
