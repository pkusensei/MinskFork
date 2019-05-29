#include "stdafx.h"
#include "Binding.h"

#include <stack>
#include <unordered_set>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "Conversion.h"
#include "Diagnostic.h"
#include "Lowering.h"
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
	_scope(make_unique<BoundScope>(parent)), _function(function),
	_loopStack(), _labelCount(0)
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
	}
	auto type = BindTypeClause(syntax->Type())
		.value_or(GetTypeSymbol(TypeEnum::Void));
	auto function = make_shared<FunctionSymbol>(syntax->Identifier().Text(),
		parameters, type, syntax);

	if (!_scope->TryDeclareFunction(function))
		_diagnostics->ReportSymbolAlreadyDeclared(syntax->Identifier().Span(),
			function->Name());

}

shared_ptr<BoundStatement> Binder::BindErrorStatement()
{
	return make_shared<BoundExpressionStatement>(make_shared<BoundErrorExpression>());
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
		case SyntaxKind::BreakStatement:
		{
			auto p = dynamic_cast<const BreakStatementSyntax*>(syntax);
			if (p) return BindBreakStatement(p);
			else break;
		}
		case SyntaxKind::ContinueStatement:
		{
			auto p = dynamic_cast<const ContinueStatementSyntax*>(syntax);
			if (p) return BindContinueStatement(p);
			else break;
		}
		case SyntaxKind::ReturnStatement:
		{
			auto p = dynamic_cast<const ReturnStatementSyntax*>(syntax);
			if (p) return BindReturnStatement(p);
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
	auto condition = BindExpression(syntax->Condition(), GetTypeSymbol(TypeEnum::Bool));
	auto thenStatement = BindStatement(syntax->ThenStatement());
	auto elseStatement =
		syntax->ElseClause() == nullptr ?
		nullptr : BindStatement(syntax->ElseClause()->ElseStatement());
	return make_shared<BoundIfStatement>(condition, thenStatement, elseStatement);
}

shared_ptr<BoundStatement> Binder::BindWhileStatement(const WhileStatementSyntax * syntax)
{
	auto condition = BindExpression(syntax->Condition(),
		GetTypeSymbol(TypeEnum::Bool));
	BoundLabel breakLabel;
	BoundLabel continueLabel;
	auto body = BindLoopBody(syntax->Body(), breakLabel, continueLabel);
	return make_shared<BoundWhileStatement>(condition, body, breakLabel, continueLabel);
}

shared_ptr<BoundStatement> Binder::BindDoWhileStatement(const DoWhileStatementSyntax * syntax)
{
	BoundLabel breakLabel;
	BoundLabel continueLabel;
	auto body = BindLoopBody(syntax->Body(), breakLabel, continueLabel);
	auto condition = BindExpression(syntax->Condition(),
		GetTypeSymbol(TypeEnum::Bool));
	return make_shared<BoundDoWhileStatement>(body, condition, breakLabel, continueLabel);
}

shared_ptr<BoundStatement> Binder::BindForStatement(const ForStatementSyntax * syntax)
{
	auto lowerBound = BindExpression(syntax->LowerBound(),
		GetTypeSymbol(TypeEnum::Int));
	auto upperBound = BindExpression(syntax->UpperBound(),
		GetTypeSymbol(TypeEnum::Int));

	_scope = make_unique<BoundScope>(_scope);

	auto variable = BindVariable(syntax->Identifier(), true,
		GetTypeSymbol(TypeEnum::Int));
	BoundLabel breakLabel;
	BoundLabel continueLabel;
	auto body = BindLoopBody(syntax->Body(), breakLabel, continueLabel);

	BoundScope::ResetToParent(_scope);
	return make_shared<BoundForStatement>(variable, lowerBound, upperBound,
		body, breakLabel, continueLabel);
}

shared_ptr<BoundStatement> Binder::BindLoopBody(const StatementSyntax * syntax,
	BoundLabel & breakLabel, BoundLabel & continueLabel)
{
	++_labelCount;
	auto brLabel = BoundLabel("break" + std::to_string(_labelCount));
	breakLabel = brLabel;
	auto conLabel = BoundLabel("continue" + std::to_string(_labelCount));
	continueLabel = conLabel;

	_loopStack.emplace(brLabel, conLabel);
	auto boundBody = BindStatement(syntax);
	_loopStack.pop();
	return boundBody;
}

shared_ptr<BoundStatement> Binder::BindBreakStatement(const BreakStatementSyntax * syntax)
{
	if (_loopStack.empty())
	{
		_diagnostics->ReportInvalidBreakOrContinue(syntax->Keyword().Span(),
			syntax->Keyword().Text());
		return BindErrorStatement();
	}
	auto brLabel = _loopStack.top().first;
	return make_shared<BoundGotoStatement>(brLabel);
}

shared_ptr<BoundStatement> Binder::BindContinueStatement(const ContinueStatementSyntax * syntax)
{
	if (_loopStack.empty())
	{
		_diagnostics->ReportInvalidBreakOrContinue(syntax->Keyword().Span(),
			syntax->Keyword().Text());
		return BindErrorStatement();
	}
	auto conLabel = _loopStack.top().second;
	return make_shared<BoundGotoStatement>(conLabel);
}

shared_ptr<BoundStatement> Binder::BindReturnStatement(const ReturnStatementSyntax * syntax)
{
	auto expression = syntax->Expression() == nullptr ?
		nullptr : BindExpression(syntax->Expression());
	if (_function == nullptr)
	{
		_diagnostics->ReportInvalidReturn(syntax->Keyword().Span());
	} else
	{
		if (_function->Type() == GetTypeSymbol(TypeEnum::Void))
		{
			if (expression != nullptr)
				_diagnostics->ReportInvalidReturnExpression(syntax->Expression()->Span(),
					_function->Name());
		} else
		{
			if (expression == nullptr)
				_diagnostics->ReportMissingReturnExpression(syntax->Keyword().Span(),
					_function->Type());
			else
				expression = BindConversion(syntax->Expression()->Span(),
					expression, _function->Type());
		}
	}
	return make_shared<BoundReturnStatement>(expression);
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
	if (!canBeVoid && result->Type() == GetTypeSymbol(TypeEnum::Void))
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
	if (boundOperand->Type() == GetTypeSymbol(TypeEnum::Error))
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
	if (boundLeft->Type() == GetTypeSymbol(TypeEnum::Error)
		|| boundRight->Type() == GetTypeSymbol(TypeEnum::Error))
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
		//NOTE here the difference comes from generics vs C++ template
		auto p = dynamic_cast<ExpressionSyntax*>(arg.get());
		if (p)
			boundArguments.emplace_back(BindExpression(p));
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
		TextSpan span;
		if (syntax->Arguments()->size() > function->Parameters().size())
		{
			const SyntaxNode* firstExceedingNode = nullptr;
			if (function->Parameters().size() > 0)
				firstExceedingNode = syntax->Arguments()->GetSeparator(function->Parameters().size() - 1);
			else
				firstExceedingNode = (*syntax->Arguments())[0];
			auto lastExceedingArg = (*syntax->Arguments())[syntax->Arguments()->size() - 1];

			span = TextSpan::FromBounds(firstExceedingNode->Span().Start(),
				lastExceedingArg->Span().End());
		} else
		{
			span = syntax->CloseParenthesisToken().Span();
		}
		_diagnostics->ReportWrongArgumentCount(span, function->Name(),
			function->Parameters().size(), syntax->Arguments()->size());
		return make_shared<BoundErrorExpression>();
	}

	auto hasError = false;
	for (auto i = 0; i < syntax->Arguments()->size(); ++i)
	{
		auto arg = boundArguments[i].get();
		auto param = function->Parameters()[i];
		if (arg->Type() != param.Type())
		{
			_diagnostics->ReportWrongArgumentType((*syntax->Arguments())[i]->Span(),
				param.Name(), param.Type(), arg->Type());
			hasError = true;
		}
	}
	if (hasError)
		return make_shared<BoundErrorExpression>();

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
	if (variable->Type() != GetTypeSymbol(TypeEnum::Int))
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
		if (expression->Type() != GetTypeSymbol(TypeEnum::Error)
			&& type != GetTypeSymbol(TypeEnum::Error))
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

std::optional<TypeSymbol> Binder::BindTypeClause(const std::optional<TypeClauseSyntax>& syntax)
{
	if (!syntax.has_value()) return std::nullopt;

	auto type = LookupType(syntax->Identifier().Text());
	if (!type.has_value())
		_diagnostics->ReportUndefinedType(syntax->Identifier().Span(),
			syntax->Identifier().Text());
	return type;
}

std::optional<TypeSymbol> Binder::LookupType(const string & name) const
{
	if (name == "bool") return GetTypeSymbol(TypeEnum::Bool);
	else if (name == "int") return GetTypeSymbol(TypeEnum::Int);
	else if (name == "string") return GetTypeSymbol(TypeEnum::String);
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


}//MCF
