#include "stdafx.h"
#include "Binding.h"

#include <stack>
#include <unordered_set>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "Conversion.h"
#include "Diagnostic.h"
#include "Parsing.h"
#include "ReflectionHelper.h"
#include "SourceText.h"

namespace MCF {

BoundScope::BoundScope(std::nullptr_t n)
	:_parent(nullptr)
{
}

BoundScope::BoundScope(unique_ptr<BoundScope>& parent)
	: _parent(std::move(parent))
{
}

bool BoundScope::TryDeclareVariable(const shared_ptr<VariableSymbol>& variable)
{
	return TryDeclareSymbol(variable);
}

bool BoundScope::TryDeclareFunction(const shared_ptr<FunctionSymbol>& function)
{
	return TryDeclareSymbol(function);
}

bool BoundScope::TryLookupVariable(const string & name, shared_ptr<VariableSymbol>& variable) const
{
	return TryLookupSymbol(name, variable);
}

bool BoundScope::TryLookupFunction(const string & name, shared_ptr<FunctionSymbol>& function) const
{
	return TryLookupSymbol(name, function);
}

const vector<shared_ptr<VariableSymbol>> BoundScope::GetDeclaredVariables() const
{
	return GetDeclaredSymbols<VariableSymbol>();
}

const vector<shared_ptr<FunctionSymbol>> BoundScope::GetDeclaredFunctions() const
{
	return GetDeclaredSymbols<FunctionSymbol>();
}

void BoundScope::ResetToParent(unique_ptr<BoundScope>& current)
{
	if (current->Parent() == nullptr) return;
	current.swap(current->_parent);
}


BoundGlobalScope::BoundGlobalScope(const BoundGlobalScope* previous,
								   unique_ptr<DiagnosticBag>& diagnostics,
								   const vector<shared_ptr<FunctionSymbol>>& functions,
								   const vector<shared_ptr<VariableSymbol>>& variables,
								   const vector<shared_ptr<BoundStatement>>& statements)
	:_previous(previous), _diagnostics(std::move(diagnostics)),
	_functions(functions), _variables(variables), _statements(statements)
{
}

BoundProgram::BoundProgram(unique_ptr<DiagnosticBag>& diagnostics,
						   FuncMap& functions,
						   unique_ptr<BoundBlockStatement>& statement)
	: _diagnostics(std::move(diagnostics)), _functions(std::move(functions)),
	_statement(std::move(statement))
{
}

Binder::Binder(unique_ptr<BoundScope>& parent, const FunctionSymbol* function)
	: _diagnostics(make_unique<DiagnosticBag>()),
	_scope(make_unique<BoundScope>(parent)), _function(function)
{
	if (function != nullptr)
		for (const auto& p : function->Parameters())
		{
			shared_ptr<VariableSymbol> v = make_shared<ParameterSymbol>(p);
			_scope->TryDeclareVariable(v);
		}
}

void Binder::BindFunctionDeclaration(const FunctionDeclarationSyntax * syntax)
{
	auto parameters = vector<ParameterSymbol>();
	auto seenParamNames = std::unordered_set<string>();

	for (const auto& it : *(syntax->Parameters()))
	{
		auto p = dynamic_cast<ParameterSyntax*>(it.get());
		if (p)
		{
			auto paramName = p->Identifier().Text();
			auto paramType = BindTypeClause(p->Type());
			auto[_, success] = seenParamNames.emplace(paramName);

			if (success && paramType.has_value())
			{
				auto param = ParameterSymbol(paramName, *paramType);
				parameters.emplace_back(param);
			} else
				_diagnostics->ReportSymbolAlreadyDeclared(p->Span(), paramName);
		}
		auto type = BindTypeClause(syntax->Type())
			.value_or(TypeSymbol::GetType(TypeEnum::Void));
		auto function = make_shared<FunctionSymbol>(syntax->Identifier().Text(),
													parameters, type, syntax);

		if (!_scope->TryDeclareFunction(function))
			_diagnostics->ReportSymbolAlreadyDeclared(syntax->Identifier().Span(),
													  function->Name());
	}
}

shared_ptr<BoundStatement> Binder::BindStatement(const StatementSyntax * syntax)
{
	switch (syntax->Kind())
	{
		case SyntaxKind::BlockStatement:
		{
			auto p = dynamic_cast<const BlockStatementSyntax*>(syntax);
			if (p) return BindBlockStatement(p);
			else break;
		}
		case SyntaxKind::VariableDeclaration:
		{
			auto p = dynamic_cast<const VariableDeclarationSyntax*>(syntax);
			if (p) return BindVariableDeclaration(p);
			else break;
		}
		case SyntaxKind::IfStatement:
		{
			auto p = dynamic_cast<const IfStatementSyntax*>(syntax);
			if (p) return BindIfStatement(p);
			else break;
		}
		case SyntaxKind::WhileStatement:
		{
			auto p = dynamic_cast<const WhileStatementSyntax*>(syntax);
			if (p) return BindWhileStatement(p);
			else break;
		}
		case SyntaxKind::DoWhileStatement:
		{
			auto p = dynamic_cast<const DoWhileStatementSyntax*>(syntax);
			if (p) return BindDoWhileStatement(p);
			else break;
		}

		case SyntaxKind::ForStatement:
		{
			auto p = dynamic_cast<const ForStatementSyntax*>(syntax);
			if (p) return BindForStatement(p);
			else break;
		}
		case SyntaxKind::ExpressionStatement:
		{
			auto p = dynamic_cast<const ExpressionStatementSyntax*>(syntax);
			if (p) return BindExpressionStatement(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Unexpected syntax " + GetSyntaxKindName(syntax->Kind()));
}

shared_ptr<BoundStatement> Binder::BindBlockStatement(const BlockStatementSyntax * syntax)
{
	auto result = vector<shared_ptr<BoundStatement>>();
	_scope = make_unique<BoundScope>(_scope);
	auto statements = syntax->Statements();
	for (const auto& it : statements)
		result.emplace_back(BindStatement(it));
	BoundScope::ResetToParent(_scope);
	return make_shared<BoundBlockStatement>(result);
}

shared_ptr<BoundStatement> Binder::BindVariableDeclaration(const VariableDeclarationSyntax * syntax)
{
	auto readOnly = syntax->Keyword().Kind() == SyntaxKind::LetKeyword;
	auto type = BindTypeClause(syntax->TypeClause());
	auto init = BindExpression(syntax->Initializer());
	auto variableType = type.value_or(init->Type());
	auto variable = BindVariable(syntax->Identifier(), readOnly, variableType);
	auto convertInitializer = BindConversion(syntax->Identifier().Span(), init, variableType);

	return make_shared<BoundVariableDeclaration>(variable, convertInitializer);
}

shared_ptr<BoundStatement> Binder::BindIfStatement(const IfStatementSyntax * syntax)
{
	auto condition = BindExpression(syntax->Condition(), TypeSymbol::GetType(TypeEnum::Bool));
	auto thenStatement = BindStatement(syntax->ThenStatement());
	auto elseStatement =
		syntax->ElseClause() == nullptr ?
		nullptr : BindStatement(syntax->ElseClause()->ElseStatement());
	return make_shared<BoundIfStatement>(condition, thenStatement, elseStatement);
}

shared_ptr<BoundStatement> Binder::BindWhileStatement(const WhileStatementSyntax * syntax)
{
	auto condition = BindExpression(syntax->Condition(), TypeSymbol::GetType(TypeEnum::Bool));
	auto body = BindStatement(syntax->Body());
	return make_shared<BoundWhileStatement>(condition, body);
}

shared_ptr<BoundStatement> Binder::BindDoWhileStatement(const DoWhileStatementSyntax * syntax)
{
	auto body = BindStatement(syntax->Body());
	auto condition = BindExpression(syntax->Condition());
	return make_shared<BoundDoWhileStatement>(body, condition);
}

shared_ptr<BoundStatement> Binder::BindForStatement(const ForStatementSyntax * syntax)
{
	auto lowerBound = BindExpression(syntax->LowerBound(), TypeSymbol::GetType(TypeEnum::Int));
	auto upperBound = BindExpression(syntax->UpperBound(), TypeSymbol::GetType(TypeEnum::Int));

	_scope = make_unique<BoundScope>(_scope);

	auto variable = BindVariable(syntax->Identifier(), true, TypeSymbol::GetType(TypeEnum::Int));
	auto body = BindStatement(syntax->Body());

	BoundScope::ResetToParent(_scope);
	return make_shared<BoundForStatement>(variable, lowerBound, upperBound, body);
}

shared_ptr<BoundStatement> Binder::BindExpressionStatement(const ExpressionStatementSyntax * syntax)
{
	auto expression = BindExpression(syntax->Expression(), true);
	return make_shared<BoundExpressionStatement>(expression);
}

shared_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax * syntax,
												   const TypeSymbol & targetType)
{
	return BindConversion(syntax, targetType);
}

shared_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax * syntax,
												   bool canBeVoid)
{
	auto result = BindExpressionInternal(syntax);
	if (!canBeVoid && result->Type() == TypeSymbol::GetType(TypeEnum::Void))
	{
		_diagnostics->ReportExpressionMustHaveValue(syntax->Span());
		return make_shared<BoundErrorExpression>();
	}
	return result;
}

shared_ptr<BoundExpression> Binder::BindExpressionInternal(const ExpressionSyntax * syntax)
{
	switch (syntax->Kind())
	{
		case SyntaxKind::ParenthesizedExpression:
		{
			auto p = dynamic_cast<const ParenthesizedExpressionSyntax*>(syntax);
			if (p) return BindParenthesizedExpression(p);
			else break;
		}
		case SyntaxKind::LiteralExpression:
		{
			auto p = dynamic_cast<const LiteralExpressionSyntax*>(syntax);
			if (p) return BindLiteralExpression(p);
			else break;
		}
		case SyntaxKind::NameExpression:
		{
			auto p = dynamic_cast<const NameExpressionSyntax*>(syntax);
			if (p) return BindNameExpression(p);
			else break;
		}
		case SyntaxKind::AssignmentExpression:
		{
			auto p = dynamic_cast<const AssignmentExpressionSyntax*>(syntax);
			if (p) return BindAssignmentExpression(p);
			else break;
		}
		case SyntaxKind::UnaryExpression:
		{
			auto p = dynamic_cast<const UnaryExpressionSyntax*>(syntax);
			if (p) return BindUnaryExpression(p);
			else break;
		}
		case SyntaxKind::BinaryExpression:
		{
			auto p = dynamic_cast<const BinaryExpressionSyntax*>(syntax);
			if (p) return BindBinaryExpression(p);
			else break;
		}
		case SyntaxKind::CallExpression:
		{
			auto p = dynamic_cast<const CallExpressionSyntax*>(syntax);
			if (p) return BindCallExpression(p);
			else break;
		}
		case SyntaxKind::PostfixExpression:
		{
			auto p = dynamic_cast<const PostfixExpressionSyntax*>(syntax);
			if (p) return BindPostfixExpression(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Invalid expression " + GetSyntaxKindName(syntax->Kind()));

}

shared_ptr<BoundExpression> Binder::BindParenthesizedExpression(const ParenthesizedExpressionSyntax * syntax)
{
	return BindExpression(syntax->Expression());
}

shared_ptr<BoundExpression> Binder::BindLiteralExpression(const LiteralExpressionSyntax * syntax)
{
	return make_shared<BoundLiteralExpression>(syntax->Value());
}

shared_ptr<BoundExpression> Binder::BindNameExpression(const NameExpressionSyntax * syntax)
{
	auto name = syntax->IdentifierToken().Text();
	if (syntax->IdentifierToken().IsMissing()) // NOTE this token was injected by Parser::MatchToken
		return make_shared<BoundErrorExpression>();

	auto variable = shared_ptr<VariableSymbol>(nullptr);
	if (!_scope->TryLookupVariable(name, variable))
	{
		_diagnostics->ReportUndefinedName(syntax->IdentifierToken().Span(), name);
		return make_shared<BoundErrorExpression>();
	}
	return make_shared<BoundVariableExpression>(variable);
}

shared_ptr<BoundExpression> Binder::BindAssignmentExpression(const AssignmentExpressionSyntax * syntax)
{
	auto name = syntax->IdentifierToken().Text();
	auto boundExpression = BindExpression(syntax->Expression());

	auto variable = shared_ptr<VariableSymbol>(nullptr);
	if (!_scope->TryLookupVariable(name, variable))
	{
		_diagnostics->ReportUndefinedName(syntax->IdentifierToken().Span(), name);
		return boundExpression;
	}
	if (variable->IsReadOnly())
		_diagnostics->ReportCannotAssign(syntax->EqualsToken().Span(), name);
	auto convertExpression = BindConversion(syntax->Expression()->Span(),
											boundExpression, variable->Type());
	return make_shared<BoundAssignmentExpression>(variable, convertExpression);
}

shared_ptr<BoundExpression> Binder::BindUnaryExpression(const UnaryExpressionSyntax * syntax)
{
	auto boundOperand = BindExpression(syntax->Operand());
	if (boundOperand->Type() == TypeSymbol::GetType(TypeEnum::Error))
		return make_shared<BoundErrorExpression>();

	auto boundOperator = BoundUnaryOperator::Bind(syntax->OperatorToken().Kind(),
												  boundOperand->Type());
	if (boundOperator.IsUseful())
	{
		return make_shared<BoundUnaryExpression>(boundOperator, boundOperand);
	} else
	{
		_diagnostics->ReportUndefinedUnaryOperator(syntax->OperatorToken().Span(),
												   syntax->OperatorToken().Text(),
												   boundOperand->Type());
		return make_shared<BoundErrorExpression>();
	}
}

shared_ptr<BoundExpression> Binder::BindBinaryExpression(const BinaryExpressionSyntax * syntax)
{
	auto boundLeft = BindExpression(syntax->Left());
	auto boundRight = BindExpression(syntax->Right());
	if (boundLeft->Type() == TypeSymbol::GetType(TypeEnum::Error)
		|| boundRight->Type() == TypeSymbol::GetType(TypeEnum::Error))
		return make_shared<BoundErrorExpression>();

	auto boundOperator = BoundBinaryOperator::Bind(syntax->OperatorToken().Kind(),
												   boundLeft->Type(), boundRight->Type());
	if (boundOperator.IsUseful())
	{
		return make_shared<BoundBinaryExpression>(boundLeft, boundOperator, boundRight);
	} else
	{
		_diagnostics->ReportUndefinedBinaryOperator(syntax->OperatorToken().Span(),
													syntax->OperatorToken().Text(),
													boundLeft->Type(), boundRight->Type());
		return make_shared<BoundErrorExpression>();
	}
}

shared_ptr<BoundExpression> Binder::BindCallExpression(const CallExpressionSyntax * syntax)
{
	if (syntax->Arguments()->size() == 1)
	{
		auto type = LookupType(syntax->Identifier().Text());
		if (type.has_value())
			return BindConversion((*syntax->Arguments())[0], *type, true);
	}

	auto boundArguments = vector<shared_ptr<BoundExpression>>();
	for (const auto& arg : *(syntax->Arguments()))
	{
		auto boundArgument = BindExpression(dynamic_cast<ExpressionSyntax*>(arg.get()));
		boundArguments.emplace_back(std::move(boundArgument));
	}

	auto function = shared_ptr<FunctionSymbol>(nullptr);
	if (!_scope->TryLookupFunction(syntax->Identifier().Text(), function))
	{
		_diagnostics->ReportUndefinedFunction(syntax->Identifier().Span(),
											  syntax->Identifier().Text());
		return make_shared<BoundErrorExpression>();
	}
	if (syntax->Arguments()->size() != function->Parameters().size())
	{
		_diagnostics->ReportWrongArgumentCount(syntax->Span(), function->Name(),
											   function->Parameters().size(),
											   syntax->Arguments()->size());
		return make_shared<BoundErrorExpression>();
	}

	for (auto i = 0; i < syntax->Arguments()->size(); ++i)
	{
		auto arg = boundArguments[i].get();
		auto param = function->Parameters()[i];
		if (arg->Type() != param.Type())
		{
			_diagnostics->ReportWrongArgumentType(syntax->Span(), param.Name(),
												  param.Type(), arg->Type());
			return make_shared<BoundErrorExpression>();
		}
	}
	return make_shared<BoundCallExpression>(function, boundArguments);
}

shared_ptr<BoundExpression> Binder::BindPostfixExpression(const PostfixExpressionSyntax * syntax)
{
	auto name = syntax->IdentifierToken().Text();
	auto boundExpression = BindExpression(syntax->Expression());

	auto variable = shared_ptr<VariableSymbol>(nullptr);

	if (!_scope->TryLookupVariable(name, variable))
	{
		_diagnostics->ReportUndefinedName(syntax->IdentifierToken().Span(), name);
		return make_shared<BoundErrorExpression>();
	}
	if (variable->IsReadOnly())
	{
		_diagnostics->ReportCannotAssign(syntax->Op().Span(), name);
		return make_shared<BoundErrorExpression>();
	}
	if (boundExpression->Type() != variable->Type())
	{
		_diagnostics->ReportCannotConvert(syntax->Expression()->Span(),
										  boundExpression->Type(), variable->Type());
		return make_shared<BoundErrorExpression>();
	}
	if (variable->Type() != TypeSymbol::GetType(TypeEnum::Int))
	{
		_diagnostics->ReportVariableNotSupportPostfixOperator(syntax->Expression()->Span(),
															  syntax->Op().Text(),
															  variable->Type());
		return make_shared<BoundErrorExpression>();
	}
	switch (syntax->Op().Kind())
	{
		case SyntaxKind::PlusPlusToken:
			return make_shared<BoundPostfixExpression>(variable,
													   BoundPostfixOperatorEnum::Increment,
													   boundExpression);
		case SyntaxKind::MinusMinusToken:
			return make_shared<BoundPostfixExpression>(variable,
													   BoundPostfixOperatorEnum::Decrement,
													   boundExpression);
		default:
			throw std::invalid_argument("Unexpected operator token "
										+ GetSyntaxKindName(syntax->Op().Kind()));
	}
}

shared_ptr<BoundExpression> Binder::BindConversion(const ExpressionSyntax* syntax,
												   const TypeSymbol& type,
												   bool allowExplicit)
{
	auto expression = BindExpression(syntax);
	return BindConversion(syntax->Span(), expression, type, allowExplicit);
}

shared_ptr<BoundExpression> Binder::BindConversion(const TextSpan & diagnosticSpan,
												   const shared_ptr<BoundExpression>& expression,
												   const TypeSymbol & type,
												   bool allowExplicit)
{
	auto conversion = Conversion::Classify(expression->Type(), type);
	if (!conversion.Exists())
	{
		if (expression->Type() != TypeSymbol::GetType(TypeEnum::Error)
			&& type != TypeSymbol::GetType(TypeEnum::Error))
			_diagnostics->ReportCannotConvert(diagnosticSpan, expression->Type(), type);
		return make_shared<BoundErrorExpression>();
	}
	if (!allowExplicit&&conversion.IsExplicit())
		_diagnostics->ReportCannotConvertImplicitly(diagnosticSpan, expression->Type(), type);
	if (conversion.IsIdentity())
		return expression;
	return make_shared<BoundConversionExpression>(type, expression);
}

shared_ptr<VariableSymbol> Binder::BindVariable(const SyntaxToken & identifier, bool isReadOnly,
												const TypeSymbol & type)
{
	auto name = identifier.Text().empty() ? "?" : identifier.Text();
	auto declare = !identifier.IsMissing();
	shared_ptr<VariableSymbol> variable;
	if (_function == nullptr)
		variable = make_shared<GlobalVariableSymbol>(name, isReadOnly, type);
	else
		variable = make_shared<LocalVariableSymbol>(name, isReadOnly, type);

	if (declare && !_scope->TryDeclareVariable(variable))
		_diagnostics->ReportSymbolAlreadyDeclared(identifier.Span(), name);
	return variable;
}

std::optional<TypeSymbol> Binder::BindTypeClause(const TypeClauseSyntax * syntax)
{
	if (syntax == nullptr) return std::nullopt;

	auto type = LookupType(syntax->Identifier().Text());
	if (!type.has_value())
		_diagnostics->ReportUndefinedType(syntax->Identifier().Span(),
										  syntax->Identifier().Text());
	return type;
}

std::optional<TypeSymbol> Binder::LookupType(const string & name) const
{
	if (name == "bool") return TypeSymbol::GetType(TypeEnum::Bool);
	else if (name == "int") return TypeSymbol::GetType(TypeEnum::Int);
	else if (name == "string") return TypeSymbol::GetType(TypeEnum::String);
	else return std::nullopt;
}

unique_ptr<BoundScope> Binder::CreateParentScope(const BoundGlobalScope* previous)
{
	auto stack = std::stack<const BoundGlobalScope*>();
	while (previous != nullptr)
	{
		stack.emplace(previous);
		previous = previous->Previous();
	}
	auto parent = CreateRootScope();
	while (!stack.empty())
	{
		auto current = stack.top();
		auto scope = make_unique<BoundScope>(parent);
		for (const auto& it : current->Functions())
			scope->TryDeclareFunction(it);
		for (const auto& it : current->Variables())
			scope->TryDeclareVariable(it);
		parent.swap(scope);
		stack.pop();
	}
	return parent;
}

unique_ptr<BoundScope> Binder::CreateRootScope()
{
	auto result = make_unique<BoundScope>(nullptr);
	for (const auto& f : GetAllBuiltinFunctions())
		result->TryDeclareFunction(make_shared<FunctionSymbol>(f));
	return result;
}

unique_ptr<BoundGlobalScope> Binder::BindGlobalScope(const BoundGlobalScope* previous,
													 const CompilationUnitSyntax* syntax)
{
	auto parentScope = CreateParentScope(previous);
	auto binder = Binder(parentScope, nullptr);
	auto statements = vector<shared_ptr<BoundStatement>>();
	for (const auto& it : syntax->Members())
	{
		auto func = dynamic_cast<FunctionDeclarationSyntax*>(it.get());
		auto globalStatement = dynamic_cast<GlobalStatementSyntax*>(it.get());
		if (func)
			binder.BindFunctionDeclaration(func);
		if (globalStatement)
		{
			statements.emplace_back(binder.BindStatement(globalStatement->Statement()));
		}
	}
	auto functions = binder._scope->GetDeclaredFunctions();
	auto variables = binder._scope->GetDeclaredVariables();
	auto diagnostics = binder.Diagnostics();
	if (previous != nullptr)
		diagnostics->AddRangeFront(*previous->Diagnostics());
	return make_unique<BoundGlobalScope>(previous, binder._diagnostics,
										 functions, variables, statements);
}

unique_ptr<BoundProgram> Binder::BindProgram(const BoundGlobalScope * globalScope)
{
	auto parentScope = CreateParentScope(globalScope);

	auto funcBodies = BoundProgram::FuncMap();
	auto diag = make_unique<DiagnosticBag>();

	auto scope = globalScope;
	while (scope != nullptr)
	{
		for (const auto& it : scope->Functions())
		{
			auto binder = Binder(parentScope, it.get());
			auto body = binder.BindStatement(it->Declaration()->Body());
			auto lowerBody = Lowerer::Lower(body);
			funcBodies.emplace(it, std::move(lowerBody));

			diag->AddRange(*binder.Diagnostics());
		}
		scope = scope->Previous();
	}
	auto s = make_shared<BoundBlockStatement>(globalScope->Statements());
	auto statement = Lowerer::Lower(s);
	return make_unique<BoundProgram>(diag, funcBodies, statement);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteStatement(const shared_ptr<BoundStatement>& node)
{
	switch (node->Kind())
	{
		case BoundNodeKind::BlockStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundBlockStatement>(node);
			if (p) return RewriteBlockStatement(p);
			else break;
		}
		case BoundNodeKind::VariableDeclaration:
		{
			auto p = std::dynamic_pointer_cast<BoundVariableDeclaration> (node);
			if (p) return RewriteVariableDeclaration(p);
			else break;
		}
		case BoundNodeKind::IfStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundIfStatement> (node);
			if (p) return RewriteIfStatement(p);
			else break;
		}
		case BoundNodeKind::WhileStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundWhileStatement> (node);
			if (p) return RewriteWhileStatement(p);
			else break;
		}
		case BoundNodeKind::DoWhileStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundDoWhileStatement> (node);
			if (p) return RewriteDoWhileStatement(p);
			else break;
		}
		case BoundNodeKind::ForStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundForStatement> (node);
			if (p) return RewriteForStatement(p);
			else break;
		}
		case BoundNodeKind::LabelStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundLabelStatement> (node);
			if (p) return RewriteLabelStatement(p);
			else break;
		}
		case BoundNodeKind::GotoStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundGotoStatement> (node);
			if (p) return RewriteGotoStatement(p);
			else break;
		}
		case BoundNodeKind::ConditionalGotoStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundConditionalGotoStatement> (node);
			if (p) return RewriteConditionalGotoStatement(p);
			else break;
		}
		case BoundNodeKind::ExpressionStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundExpressionStatement>(node);
			if (p) return RewriteExpressionStatement(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Unexpected node: " + GetEnumText(node->Kind()));
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteBlockStatement(const shared_ptr<BoundBlockStatement>& node)
{
	auto result = vector<shared_ptr<BoundStatement>>();
	auto statements = node->Statements();
	for (const auto& it : statements)
		result.emplace_back(RewriteStatement(it));
	if (result.empty())
		return node;
	return make_shared<BoundBlockStatement>(result);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteVariableDeclaration(const shared_ptr<BoundVariableDeclaration>& node)
{
	auto initializer = RewriteExpression(node->Initializer());
	if (initializer == node->Initializer())
		return node;
	return make_shared<BoundVariableDeclaration>(node->Variable(), initializer);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteIfStatement(const shared_ptr<BoundIfStatement>& node)
{
	auto condition = RewriteExpression(node->Condition());
	auto thenStatement = RewriteStatement(node->ThenStatement());
	auto elseStatement =
		node->ElseStatement() == nullptr ?
		nullptr : RewriteStatement(node->ElseStatement());
	if (condition == node->Condition()
		&& thenStatement == node->ThenStatement()
		&& elseStatement == node->ElseStatement())
		return node;
	return make_shared<BoundIfStatement>(condition, thenStatement, elseStatement);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteWhileStatement(const shared_ptr<BoundWhileStatement>& node)
{
	auto condition = RewriteExpression(node->Condition());
	auto body = RewriteStatement(node->Body());
	if (condition == node->Condition() && body == node->Body())
		return node;
	return make_shared<BoundWhileStatement>(condition, body);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteDoWhileStatement(const shared_ptr<BoundDoWhileStatement>& node)
{
	auto body = RewriteStatement(node->Body());
	auto condition = RewriteExpression(node->Condition());
	if (body == node->Body() && condition == node->Condition())
		return node;
	return make_shared<BoundDoWhileStatement>(body, condition);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteForStatement(const shared_ptr<BoundForStatement>& node)
{
	auto lowerBound = RewriteExpression(node->LowerBound());
	auto upperBound = RewriteExpression(node->UpperBound());
	auto body = RewriteStatement(node->Body());
	if (lowerBound == node->LowerBound()
		&& upperBound == node->UpperBound()
		&& body == node->Body())
		return node;
	return make_shared<BoundForStatement>(node->Variable(), lowerBound, upperBound, body);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteLabelStatement(const shared_ptr<BoundLabelStatement>& node)
{
	return node;
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteGotoStatement(const shared_ptr<BoundGotoStatement>& node)
{
	return node;
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteConditionalGotoStatement(const shared_ptr<BoundConditionalGotoStatement>& node)
{
	auto condition = RewriteExpression(node->Condition());
	if (condition == node->Condition())
		return node;
	return make_shared<BoundConditionalGotoStatement>(node->Label(), condition, node->JumpIfTrue());
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteExpressionStatement(const shared_ptr<BoundExpressionStatement>& node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundExpressionStatement>(expression);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteExpression(const shared_ptr<BoundExpression>& node)
{
	switch (node->Kind())
	{
		case BoundNodeKind::ErrorExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundErrorExpression>(node);
			if (p) return RewriteErrorExpression(p);
			else break;
		}
		case BoundNodeKind::LiteralExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundLiteralExpression>(node);
			if (p) return RewriteLiteralExpression(p);
			else break;
		}
		case BoundNodeKind::VariableExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundVariableExpression>(node);
			if (p) return RewriteVariableExpression(p);
			else break;
		}
		case BoundNodeKind::AssignmentExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundAssignmentExpression>(node);
			if (p) return RewriteAssignmentExpression(p);
			else break;
		}
		case BoundNodeKind::UnaryExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundUnaryExpression>(node);
			if (p) return RewriteUnaryExpression(p);
			else break;
		}
		case BoundNodeKind::BinaryExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundBinaryExpression>(node);
			if (p) return RewriteBinaryExpression(p);
			else break;
		}
		case BoundNodeKind::CallExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundCallExpression>(node);
			if (p) return RewriteCallExpression(p);
			else break;
		}
		case BoundNodeKind::ConversionExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundConversionExpression>(node);
			if (p) return RewriteConversionExpression(p);
			else break;
		}
		case BoundNodeKind::PostfixExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundPostfixExpression>(node);
			if (p) return RewritePostfixExpression(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Unexpected node: " + GetEnumText(node->Kind()));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteErrorExpression(const shared_ptr<BoundErrorExpression>& node)
{
	return node;
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteLiteralExpression(const shared_ptr<BoundLiteralExpression>& node)
{
	return node;
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteVariableExpression(const shared_ptr<BoundVariableExpression>& node)
{
	return node;
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteAssignmentExpression(const shared_ptr<BoundAssignmentExpression>& node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundAssignmentExpression>(node->Variable(), expression);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteUnaryExpression(const shared_ptr<BoundUnaryExpression>& node)
{
	auto operand = RewriteExpression(node->Operand());
	if (operand == node->Operand())
		return node;
	return make_shared<BoundUnaryExpression>(*(node->Op()), operand);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteBinaryExpression(const shared_ptr<BoundBinaryExpression>& node)
{
	auto left = RewriteExpression(node->Left());
	auto right = RewriteExpression(node->Right());
	if (left == node->Left() && right == node->Right())
		return node;
	return make_shared<BoundBinaryExpression>(left, *(node->Op()), right);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteCallExpression(const shared_ptr<BoundCallExpression>& node)
{
	auto result = vector<shared_ptr<BoundExpression>>();
	for (auto i = 0; i < node->Arguments().size(); ++i)
	{
		auto oldArg = node->Arguments()[i];
		auto newArg = RewriteExpression(oldArg);
		if (newArg != oldArg)
		{
			if (result.empty())
				for (auto j = 0; j < i; ++j)
					result.emplace_back(node->Arguments()[i]);
		}
		if (!result.empty())
			result.emplace_back(newArg);
	}
	if (result.empty())
		return node;
	return make_shared<BoundCallExpression>(node->Function(), result);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteConversionExpression(const shared_ptr<BoundConversionExpression>& node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundConversionExpression>(node->Type(), expression);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewritePostfixExpression(const shared_ptr<BoundPostfixExpression>& node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundPostfixExpression>(node->Variable(), node->OperatorKind(), expression);
}

BoundLabel Lowerer::GenerateLabel()
{
	++_labelCount;
	string name("Label" + std::to_string(_labelCount));
	return BoundLabel(name);
}

unique_ptr<BoundBlockStatement> Lowerer::Lower(const shared_ptr<BoundStatement>& statement)
{
	auto lowerer = Lowerer();
	auto result = lowerer.RewriteStatement(statement);
	return Flatten(result);
}

unique_ptr<BoundBlockStatement> Lowerer::Flatten(const shared_ptr<BoundStatement>& statement)
{
	auto result = vector<shared_ptr<BoundStatement>>();
	auto stack = std::stack<shared_ptr<BoundStatement>>();
	stack.emplace(statement);

	while (!stack.empty())
	{
		auto current = stack.top();
		stack.pop();

		auto p = std::dynamic_pointer_cast<BoundBlockStatement>(current);
		if (p)
		{
			auto& statements = p->Statements();
			for (auto it = statements.rbegin(); it != statements.rend(); ++it)
				stack.emplace(*it);
		} else
		{
			result.emplace_back(current);
		}
	}
	return make_unique<BoundBlockStatement>(result);
}

shared_ptr<BoundStatement> Lowerer::RewriteIfStatement(const shared_ptr<BoundIfStatement>& node)
{
	if (node->ElseStatement() == nullptr)
	{
		auto endLabel = GenerateLabel();
		auto endLabelStatement = make_shared<BoundLabelStatement>(endLabel);
		auto gotoFalse = make_shared<BoundConditionalGotoStatement>(
			endLabel, node->Condition(), false
			);

		auto statements = vector<shared_ptr<BoundStatement>>{
			gotoFalse, node->ThenStatement(), endLabelStatement
		};
		auto result = make_shared<BoundBlockStatement>(statements);
		return RewriteStatement(result);
	} else
	{
		auto elseLabel = GenerateLabel();
		auto endLabel = GenerateLabel();
		auto gotoFalse = make_shared<BoundConditionalGotoStatement>(
			elseLabel, node->Condition(), false
			);
		auto gotoEndStatement = make_shared<BoundGotoStatement>(endLabel);
		auto elseLabelStatement = make_shared<BoundLabelStatement>(elseLabel);
		auto endLabelStatement = make_shared<BoundLabelStatement>(endLabel);

		auto statements = vector<shared_ptr<BoundStatement>>{
			gotoFalse, node->ThenStatement(), gotoEndStatement,
			elseLabelStatement,node->ElseStatement(),	endLabelStatement
		};
		auto result = make_shared<BoundBlockStatement>(statements);
		return RewriteStatement(result);
	}
}

shared_ptr<BoundStatement> Lowerer::RewriteWhileStatement(const shared_ptr<BoundWhileStatement>& node)
{
	auto continueLabel = GenerateLabel();
	auto checkLabel = GenerateLabel();
	auto endLabel = GenerateLabel();

	auto gotoCheck = make_shared<BoundGotoStatement>(checkLabel);
	auto continueLabelStatement = make_shared<BoundLabelStatement>(continueLabel);
	auto checkLabelStatement = make_shared<BoundLabelStatement>(checkLabel);
	auto gotoTrue = make_shared<BoundConditionalGotoStatement>(continueLabel,
															   node->Condition());
	auto endLabelStatement = make_shared<BoundLabelStatement>(endLabel);

	auto statements = vector<shared_ptr<BoundStatement>>{
		gotoCheck, continueLabelStatement, node->Body(),
		checkLabelStatement, gotoTrue, endLabelStatement
	};

	auto result = make_shared<BoundBlockStatement>(statements);
	return RewriteStatement(result);
}

shared_ptr<BoundStatement> Lowerer::RewriteDoWhileStatement(const shared_ptr<BoundDoWhileStatement>& node)
{
	auto continueLabel = GenerateLabel();

	auto continueLabelStatement = make_shared<BoundLabelStatement>(continueLabel);
	auto gotoTrue = make_shared<BoundConditionalGotoStatement>(continueLabel,
															   node->Condition());

	auto statements = vector<shared_ptr<BoundStatement>>{
		continueLabelStatement, node->Body(), gotoTrue
	};
	auto result = make_shared<BoundBlockStatement>(statements);
	return RewriteStatement(result);
}

shared_ptr<BoundStatement> Lowerer::RewriteForStatement(const shared_ptr<BoundForStatement>& node)
{
	auto variableDeclaration = make_shared<BoundVariableDeclaration>(node->Variable(),
																	 node->LowerBound());
	auto variableExpression = make_shared<BoundVariableExpression>(node->Variable());
	auto upperBoundSymbol = make_shared<LocalVariableSymbol>(
		"upperBound", true, TypeSymbol::GetType(TypeEnum::Int)
		);
	auto upperBoundDeclaration = make_shared<BoundVariableDeclaration>(upperBoundSymbol,
																	   node->UpperBound());

	auto condition = make_shared<BoundBinaryExpression>(
		RewriteExpression(variableExpression),
		BoundBinaryOperator::Bind(SyntaxKind::LessOrEqualsToken,
								  TypeSymbol::GetType(TypeEnum::Int),
								  TypeSymbol::GetType(TypeEnum::Int)),
		make_shared<BoundVariableExpression>(upperBoundSymbol)
		);

	auto increment = make_shared<BoundExpressionStatement>(
		make_shared<BoundAssignmentExpression>(
			node->Variable(),
			make_shared<BoundBinaryExpression>(
				RewriteExpression(variableExpression),
				BoundBinaryOperator::Bind(SyntaxKind::PlusToken,
										  TypeSymbol::GetType(TypeEnum::Int),
										  TypeSymbol::GetType(TypeEnum::Int)),
				make_shared<BoundLiteralExpression>(1)
				)
			)
		);

	auto statements = vector<shared_ptr<BoundStatement>>{node->Body(),increment};
	auto whileBody = make_shared<BoundBlockStatement>(statements);
	auto whileStatement = make_shared<BoundWhileStatement>(condition, whileBody);

	statements = {variableDeclaration, upperBoundDeclaration, whileStatement};
	auto result = make_shared<BoundBlockStatement>(statements);
	return RewriteStatement(result);
}

}//MCF
