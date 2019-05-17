#pragma once

#include <unordered_map>

#include "BoundNode.h"
#include "Symbols.h"

namespace MCF {

class DiagnosticBag;
class BoundLabel;

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
	std::unordered_map<string, VariableSymbol> _variables;
	std::unordered_map<string, FunctionSymbol> _functions;
	unique_ptr<BoundScope> _parent;

public:
	explicit BoundScope(const unique_ptr<BoundScope>& parent);

	const BoundScope* Parent()const noexcept { return _parent.get(); }

	bool TryDeclareVariable(const VariableSymbol& variable);
	bool TryLookupVariable(const string& name, VariableSymbol& variable)const;
	const vector<VariableSymbol> GetDeclaredVariables()const;

	bool TryDeclareFunction(const FunctionSymbol& function);
	bool TryLookupFunction(const string& name, FunctionSymbol& function)const;
	const vector<FunctionSymbol> GetDeclaredFunctions()const;

	static void ResetToParent(unique_ptr<BoundScope>& current);
};

class BoundGlobalScope final
{
private:
	const BoundGlobalScope* _previous;
	unique_ptr<DiagnosticBag> _diagnostics;
	vector<VariableSymbol> _variables;
	unique_ptr<BoundStatement> _statement;
public:
	BoundGlobalScope(const BoundGlobalScope* previous, const unique_ptr<DiagnosticBag>& diagnostics,
					 const vector<VariableSymbol>& variables, const unique_ptr<BoundStatement>& statement);

	const BoundGlobalScope* Previous()const noexcept { return _previous; }
	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }
	const vector<VariableSymbol> Variables()const { return _variables; }
	const BoundStatement* Statement()const noexcept { return _statement.get(); }
};

class Binder final
{
private:
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<BoundScope> _scope;

	unique_ptr<BoundStatement> BindStatement(const StatementSyntax* syntax);
	unique_ptr<BoundStatement> BindBlockStatement(const BlockStatementSyntax* syntax);
	unique_ptr<BoundStatement> BindVariableDeclaration(const VariableDeclarationSyntax* syntax);
	unique_ptr<BoundStatement> BindIfStatement(const IfStatementSyntax* syntax);
	unique_ptr<BoundStatement> BindWhileStatement(const WhileStatementSyntax* syntax);
	unique_ptr<BoundStatement> BindDoWhileStatement(const DoWhileStatementSyntax* syntax);
	unique_ptr<BoundStatement> BindForStatement(const ForStatementSyntax* syntax);
	unique_ptr<BoundStatement> BindExpressionStatement(const ExpressionStatementSyntax* syntax);

	unique_ptr<BoundExpression> BindExpression(const ExpressionSyntax* syntax, const TypeSymbol& targetType);
	unique_ptr<BoundExpression> BindExpression(const ExpressionSyntax* syntax, bool canBeVoid = false);
	unique_ptr<BoundExpression> BindExpressionInternal(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindParenthesizedExpression(const ParenthesizedExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindLiteralExpression(const LiteralExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindNameExpression(const NameExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindAssignmentExpression(const AssignmentExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindUnaryExpression(const UnaryExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindBinaryExpression(const BinaryExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindCallExpression(const CallExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindPostfixExpression(const PostfixExpressionSyntax* syntax);

	unique_ptr<BoundExpression> BindConversion(const TypeSymbol& type, const ExpressionSyntax* syntax);
	VariableSymbol BindVariable(const SyntaxToken& identifier, bool isReadOnly, const TypeSymbol& type);
	TypeSymbol LookupType(const string& name)const;

	static unique_ptr<BoundScope> CreateParentScope(const BoundGlobalScope* previous);
	static unique_ptr<BoundScope> CreateRootScope();

public:
	explicit Binder(const unique_ptr<BoundScope>& parent);

	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }

	static unique_ptr<BoundGlobalScope> BindGlobalScope(const BoundGlobalScope* previous, const CompilationUnitSyntax* syntax);
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
	virtual unique_ptr<BoundStatement> RewriteBlockStatement(const BoundBlockStatement* node);
	virtual unique_ptr<BoundStatement> RewriteVariableDeclaration(const BoundVariableDeclaration* node);
	virtual unique_ptr<BoundStatement> RewriteIfStatement(const BoundIfStatement* node);
	virtual unique_ptr<BoundStatement> RewriteWhileStatement(const BoundWhileStatement* node);
	virtual unique_ptr<BoundStatement> RewriteDoWhileStatement(const BoundDoWhileStatement* node);
	virtual unique_ptr<BoundStatement> RewriteForStatement(const BoundForStatement* node);
	virtual unique_ptr<BoundStatement> RewriteLabelStatement(const BoundLabelStatement* node);
	virtual unique_ptr<BoundStatement> RewriteGotoStatement(const BoundGotoStatement* node);
	virtual unique_ptr<BoundStatement> RewriteConditionalGotoStatement(const BoundConditionalGotoStatement* node);
	virtual unique_ptr<BoundStatement> RewriteExpressionStatement(const BoundExpressionStatement* node);

	virtual unique_ptr<BoundExpression> RewriteErrorExpression(const BoundErrorExpression* node);
	virtual unique_ptr<BoundExpression> RewriteLiteralExpression(const BoundLiteralExpression* node);
	virtual unique_ptr<BoundExpression> RewriteVariableExpression(const BoundVariableExpression* node);
	virtual unique_ptr<BoundExpression> RewriteAssignmentExpression(const BoundAssignmentExpression* node);
	virtual unique_ptr<BoundExpression> RewriteUnaryExpression(const BoundUnaryExpression* node);
	virtual unique_ptr<BoundExpression> RewriteBinaryExpression(const BoundBinaryExpression* node);
	virtual unique_ptr<BoundExpression> RewriteCallExpression(const BoundCallExpression* node);
	virtual unique_ptr<BoundExpression> RewriteConversionExpression(const BoundConversionExpression* node);
	virtual unique_ptr<BoundExpression> RewritePostfixExpression(const BoundPostfixExpression* node);

public:
	virtual ~BoundTreeRewriter() = default;

	virtual unique_ptr<BoundStatement> RewriteStatement(const BoundStatement* node);
	virtual unique_ptr<BoundExpression> RewriteExpression(const BoundExpression* node);
};

class Lowerer final :public BoundTreeRewriter
{
private:
	size_t _labelCount{0};

	Lowerer() = default;
	BoundLabel GenerateLabel();
	unique_ptr<BoundBlockStatement> Flatten(unique_ptr<BoundStatement>& statement);

protected:
	unique_ptr<BoundStatement> RewriteIfStatement(const BoundIfStatement* node)override;
	unique_ptr<BoundStatement> RewriteWhileStatement(const BoundWhileStatement* node)override;
	unique_ptr<BoundStatement> RewriteDoWhileStatement(const BoundDoWhileStatement* node)override;
	unique_ptr<BoundStatement> RewriteForStatement(const BoundForStatement* node)override;

public:
	static unique_ptr<BoundBlockStatement> Lower(const BoundStatement* statement);
};

}//MCF
