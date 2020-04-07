#include "Binding.h"

#include <stack>
#include <stdexcept>
#include <unordered_set>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "ControlFlowGraph.h"
#include "Conversion.h"
#include "Diagnostic.h"
#include "helpers.h"
#include "Lowering.h"
#include "Parsing.h"

namespace MCF {

std::optional<shared_ptr<Symbol>> BoundScope::TryLookupSymbol(string_view name)const
{
	try
	{
		return _symbols.at(name);
	} catch (const std::out_of_range&)
	{
		if (_parent == nullptr) return std::nullopt;
		return _parent->TryLookupSymbol(name);
	}
}

void BoundScope::ResetToParent(unique_ptr<BoundScope>& current)noexcept
{
	if (current->Parent() == nullptr) return;
	current.swap(current->_parent);
}

Binder::Binder(unique_ptr<BoundScope> parent, const FunctionSymbol* function)
	: _diagnostics(make_unique<DiagnosticBag>()),
	_scope(make_unique<BoundScope>(std::move(parent))), _function(function),
	_loopStack(), _labelCount(0)
{
	if (function != nullptr)
		for (const auto& p : function->Parameters())
		{
			shared_ptr<VariableSymbol> v = make_shared<ParameterSymbol>(p);
			_scope->TryDeclareVariable(std::move(v));
		}
}

void Binder::BindFunctionDeclaration(const FunctionDeclarationSyntax* syntax)
{
	auto parameters = vector<ParameterSymbol>();
	auto seenParamNames = std::unordered_set<string_view>();

	for (const auto& it : *(syntax->Parameters()))
	{
		if (it->Kind()==SyntaxKind::Parameter)
		{
			auto p = static_cast<ParameterSyntax*>(it.get());
			auto paramName = p->Identifier().Text();
			auto paramType = BindTypeClause(p->Type());
			auto [_, success] = seenParamNames.emplace(paramName);

			if (success)
			{
				auto param = ParameterSymbol(paramName, *paramType);
				parameters.push_back(std::move(param));
			} else
				_diagnostics->ReportParameterAlreadyDeclared(p->Location(), paramName);
		}
	}
	auto type = BindTypeClause(syntax->Type())
		.value_or(TypeSymbol::Get(TypeEnum::Void));
	auto function = make_shared<FunctionSymbol>(syntax->Identifier().Text(),
		std::move(parameters), type, syntax);

	if (!function->Declaration()->Identifier().Text().empty()
		&& !_scope->TryDeclareFunction(function))
	{
		_diagnostics->ReportSymbolAlreadyDeclared(syntax->Identifier().Location(),
			function->Name());
	}
}

shared_ptr<BoundStatement> Binder::BindErrorStatement()
{
	return make_shared<BoundExpressionStatement>(make_shared<BoundErrorExpression>());
}

shared_ptr<BoundStatement> Binder::BindStatement(const StatementSyntax* syntax)
{
	switch (syntax->Kind())
	{
		case SyntaxKind::BlockStatement:
		{
			auto p = static_cast<const BlockStatementSyntax*>(syntax);
			return BindBlockStatement(p);
		}
		case SyntaxKind::VariableDeclaration:
		{
			auto p = static_cast<const VariableDeclarationSyntax*>(syntax);
			return BindVariableDeclaration(p);
		}
		case SyntaxKind::IfStatement:
		{
			auto p = static_cast<const IfStatementSyntax*>(syntax);
			return BindIfStatement(p);
		}
		case SyntaxKind::WhileStatement:
		{
			auto p = static_cast<const WhileStatementSyntax*>(syntax);
			return BindWhileStatement(p);
		}
		case SyntaxKind::DoWhileStatement:
		{
			auto p = static_cast<const DoWhileStatementSyntax*>(syntax);
			return BindDoWhileStatement(p);
		}
		case SyntaxKind::ForStatement:
		{
			auto p = static_cast<const ForStatementSyntax*>(syntax);
			return BindForStatement(p);
		}
		case SyntaxKind::BreakStatement:
		{
			auto p = static_cast<const BreakStatementSyntax*>(syntax);
			return BindBreakStatement(p);
		}
		case SyntaxKind::ContinueStatement:
		{
			auto p = static_cast<const ContinueStatementSyntax*>(syntax);
			return BindContinueStatement(p);
		}
		case SyntaxKind::ReturnStatement:
		{
			auto p = static_cast<const ReturnStatementSyntax*>(syntax);
			return BindReturnStatement(p);
		}
		case SyntaxKind::ExpressionStatement:
		{
			auto p = static_cast<const ExpressionStatementSyntax*>(syntax);
			return BindExpressionStatement(p);
		}
		default:
			break;
	}
	throw std::invalid_argument(BuildStringFrom("Unexpected syntax ", nameof(syntax->Kind())));
}

shared_ptr<BoundStatement> Binder::BindBlockStatement(const BlockStatementSyntax* syntax)
{
	auto result = vector<shared_ptr<BoundStatement>>();
	_scope = make_unique<BoundScope>(std::move(_scope));
	auto statements = syntax->Statements();
	for (const auto& it : statements)
		result.push_back(BindStatement(it));
	BoundScope::ResetToParent(_scope);
	return make_shared<BoundBlockStatement>(std::move(result));
}

shared_ptr<BoundStatement> Binder::BindVariableDeclaration(const VariableDeclarationSyntax* syntax)
{
	auto readOnly = syntax->Keyword().Kind() == SyntaxKind::LetKeyword;
	auto type = BindTypeClause(syntax->TypeClause());
	auto init = BindExpression(syntax->Initializer());
	auto variableType = type.value_or(init->Type());
	auto variable = BindVariableDeclaration(syntax->Identifier(), readOnly, variableType);
	auto convertInitializer = BindConversion(syntax->Identifier().Location(),
		std::move(init), variableType);

	return make_shared<BoundVariableDeclaration>(std::move(variable), std::move(convertInitializer));
}

shared_ptr<BoundStatement> Binder::BindIfStatement(const IfStatementSyntax* syntax)
{
	auto condition = BindExpression(syntax->Condition(), TypeSymbol::Get(TypeEnum::Bool));
	auto thenStatement = BindStatement(syntax->ThenStatement());
	auto elseStatement =
		syntax->ElseClause() == nullptr ?
		nullptr : BindStatement(syntax->ElseClause()->ElseStatement());
	return make_shared<BoundIfStatement>(std::move(condition),
		std::move(thenStatement), std::move(elseStatement));
}

shared_ptr<BoundStatement> Binder::BindWhileStatement(const WhileStatementSyntax* syntax)
{
	auto condition = BindExpression(syntax->Condition(),
		TypeSymbol::Get(TypeEnum::Bool));
	auto [body, breakLabel, continueLabel] = BindLoopBody(syntax->Body());
	return make_shared<BoundWhileStatement>(std::move(condition), body,
		breakLabel, continueLabel);
}

shared_ptr<BoundStatement> Binder::BindDoWhileStatement(const DoWhileStatementSyntax* syntax)
{
	auto [body, breakLabel, continueLabel] = BindLoopBody(syntax->Body());
	auto condition = BindExpression(syntax->Condition(),
		TypeSymbol::Get(TypeEnum::Bool));
	return make_shared<BoundDoWhileStatement>(body, std::move(condition),
		breakLabel, continueLabel);
}

shared_ptr<BoundStatement> Binder::BindForStatement(const ForStatementSyntax* syntax)
{
	auto lowerBound = BindExpression(syntax->LowerBound(),
		TypeSymbol::Get(TypeEnum::Int));
	auto upperBound = BindExpression(syntax->UpperBound(),
		TypeSymbol::Get(TypeEnum::Int));

	_scope = make_unique<BoundScope>(std::move(_scope));

	auto variable = BindVariableDeclaration(syntax->Identifier(), true,
		TypeSymbol::Get(TypeEnum::Int));
	auto [body, breakLabel, continueLabel] = BindLoopBody(syntax->Body());

	BoundScope::ResetToParent(_scope);
	return make_shared<BoundForStatement>(std::move(variable),
		std::move(lowerBound), std::move(upperBound),
		body, breakLabel, continueLabel);
}

auto Binder::BindLoopBody(const StatementSyntax* syntax)
->std::tuple<shared_ptr<BoundStatement>, BoundLabel, BoundLabel>
{
	++_labelCount;
	auto breakLabel = BoundLabel("break" + std::to_string(_labelCount));
	auto continueLabel = BoundLabel("continue" + std::to_string(_labelCount));

	_loopStack.emplace(breakLabel, continueLabel);
	auto boundBody = BindStatement(syntax);
	_loopStack.pop();
	return std::make_tuple(std::move(boundBody), std::move(breakLabel),
		std::move(continueLabel));
}

shared_ptr<BoundStatement> Binder::BindBreakStatement(const BreakStatementSyntax* syntax)
{
	if (_loopStack.empty())
	{
		_diagnostics->ReportInvalidBreakOrContinue(syntax->Keyword().Location(),
			syntax->Keyword().Text());
		return BindErrorStatement();
	}
	auto [brLabel, _] = _loopStack.top();
	return make_shared<BoundGotoStatement>(brLabel);
}

shared_ptr<BoundStatement> Binder::BindContinueStatement(const ContinueStatementSyntax* syntax)
{
	if (_loopStack.empty())
	{
		_diagnostics->ReportInvalidBreakOrContinue(syntax->Keyword().Location(),
			syntax->Keyword().Text());
		return BindErrorStatement();
	}
	auto [_, conLabel] = _loopStack.top();
	return make_shared<BoundGotoStatement>(conLabel);
}

shared_ptr<BoundStatement> Binder::BindReturnStatement(const ReturnStatementSyntax* syntax)
{
	auto expression = syntax->Expression() == nullptr ?
		nullptr : BindExpression(syntax->Expression());
	if (_function == nullptr)
	{
		_diagnostics->ReportInvalidReturn(syntax->Keyword().Location());
	} else
	{
		if (_function->Type().get() == TypeSymbol::Get(TypeEnum::Void))
		{
			if (expression != nullptr)
				_diagnostics->ReportInvalidReturnExpression(syntax->Expression()->Location(),
					_function->Name());
		} else
		{
			if (expression == nullptr)
				_diagnostics->ReportMissingReturnExpression(syntax->Keyword().Location(),
					_function->Type());
			else
				expression = BindConversion(syntax->Expression()->Location(),
					std::move(expression), _function->Type());
		}
	}
	return make_shared<BoundReturnStatement>(std::move(expression));
}

shared_ptr<BoundStatement> Binder::BindExpressionStatement(const ExpressionStatementSyntax* syntax)
{
	auto expression = BindExpression(syntax->Expression(), true);
	return make_shared<BoundExpressionStatement>(std::move(expression));
}

shared_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax* syntax,
	ConstTypeRef targetType)
{
	return BindConversion(syntax, targetType);
}

shared_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax* syntax,
	bool canBeVoid)
{
	auto result = BindExpressionInternal(syntax);
	if (!canBeVoid && result->Type().get() == TypeSymbol::Get(TypeEnum::Void))
	{
		_diagnostics->ReportExpressionMustHaveValue(syntax->Location());
		return make_shared<BoundErrorExpression>();
	}
	return result;
}

shared_ptr<BoundExpression> Binder::BindExpressionInternal(const ExpressionSyntax* syntax)
{
	switch (syntax->Kind())
	{
		case SyntaxKind::ParenthesizedExpression:
		{
			auto p = static_cast<const ParenthesizedExpressionSyntax*>(syntax);
			return BindParenthesizedExpression(p);
		}
		case SyntaxKind::LiteralExpression:
		{
			auto p = static_cast<const LiteralExpressionSyntax*>(syntax);
			return BindLiteralExpression(p);
		}
		case SyntaxKind::NameExpression:
		{
			auto p = static_cast<const NameExpressionSyntax*>(syntax);
			return BindNameExpression(p);
		}
		case SyntaxKind::AssignmentExpression:
		{
			auto p = static_cast<const AssignmentExpressionSyntax*>(syntax);
			return BindAssignmentExpression(p);
		}
		case SyntaxKind::UnaryExpression:
		{
			auto p = static_cast<const UnaryExpressionSyntax*>(syntax);
			return BindUnaryExpression(p);
		}
		case SyntaxKind::BinaryExpression:
		{
			auto p = static_cast<const BinaryExpressionSyntax*>(syntax);
			return BindBinaryExpression(p);
		}
		case SyntaxKind::CallExpression:
		{
			auto p = static_cast<const CallExpressionSyntax*>(syntax);
			return BindCallExpression(p);
		}
		case SyntaxKind::PostfixExpression:
		{
			auto p = static_cast<const PostfixExpressionSyntax*>(syntax);
			return BindPostfixExpression(p);
		}
		default:
			break;
	}
	throw std::invalid_argument(BuildStringFrom("Invalid expression ", nameof(syntax->Kind())));

}

shared_ptr<BoundExpression> Binder::BindParenthesizedExpression(const ParenthesizedExpressionSyntax* syntax)
{
	return BindExpression(syntax->Expression());
}

shared_ptr<BoundExpression> Binder::BindLiteralExpression(const LiteralExpressionSyntax* syntax)
{
	return make_shared<BoundLiteralExpression>(syntax->Value());
}

shared_ptr<BoundExpression> Binder::BindNameExpression(const NameExpressionSyntax* syntax)
{
	if (syntax->IdentifierToken().IsMissing()) // NOTE this token was injected by Parser::MatchToken
		return make_shared<BoundErrorExpression>();

	auto opt = BindVariableReference(syntax->IdentifierToken());
	if (!opt.has_value())
		return make_shared<BoundErrorExpression>();
	return make_shared<BoundVariableExpression>(opt.value());
}

shared_ptr<BoundExpression> Binder::BindAssignmentExpression(const AssignmentExpressionSyntax* syntax)
{
	auto name = syntax->IdentifierToken().Text();
	auto boundExpression = BindExpression(syntax->Expression());

	auto opt = BindVariableReference(syntax->IdentifierToken());
	if (!opt.has_value())
		return boundExpression;

	auto variable = opt.value();
	if (variable->IsReadOnly())
		_diagnostics->ReportCannotAssign(syntax->EqualsToken().Location(), name);
	auto convertExpression = BindConversion(syntax->Expression()->Location(),
		std::move(boundExpression), variable->Type());
	return make_shared<BoundAssignmentExpression>(std::move(variable), std::move(convertExpression));
}

shared_ptr<BoundExpression> Binder::BindUnaryExpression(const UnaryExpressionSyntax* syntax)
{
	auto boundOperand = BindExpression(syntax->Operand());
	if (boundOperand->Type().get() == TypeSymbol::Get(TypeEnum::Error))
		return make_shared<BoundErrorExpression>();

	auto boundOperator = BoundUnaryOperator::Bind(syntax->OperatorToken().Kind(),
		boundOperand->Type());
	if (boundOperator.IsUseful())
	{
		return make_shared<BoundUnaryExpression>(boundOperator, std::move(boundOperand));
	} else
	{
		_diagnostics->ReportUndefinedUnaryOperator(syntax->OperatorToken().Location(),
			syntax->OperatorToken().Text(), boundOperand->Type());
		return make_shared<BoundErrorExpression>();
	}
}

shared_ptr<BoundExpression> Binder::BindBinaryExpression(const BinaryExpressionSyntax* syntax)
{
	auto boundLeft = BindExpression(syntax->Left());
	auto boundRight = BindExpression(syntax->Right());
	if (boundLeft->Type().get() == TypeSymbol::Get(TypeEnum::Error)
		|| boundRight->Type().get() == TypeSymbol::Get(TypeEnum::Error))
		return make_shared<BoundErrorExpression>();

	auto boundOperator = BoundBinaryOperator::Bind(syntax->OperatorToken().Kind(),
		boundLeft->Type(), boundRight->Type());
	if (boundOperator.IsUseful())
	{
		return make_shared<BoundBinaryExpression>(std::move(boundLeft),
			boundOperator, std::move(boundRight));
	} else
	{
		_diagnostics->ReportUndefinedBinaryOperator(syntax->OperatorToken().Location(),
			syntax->OperatorToken().Text(), boundLeft->Type(), boundRight->Type());
		return make_shared<BoundErrorExpression>();
	}
}

shared_ptr<BoundExpression> Binder::BindCallExpression(const CallExpressionSyntax* syntax)
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
			boundArguments.push_back(BindExpression(p));
	}

	auto opt = _scope->TryLookupSymbol(syntax->Identifier().Text());
	if (!opt.has_value())
	{
		_diagnostics->ReportUndefinedFunction(syntax->Identifier().Location(),
			syntax->Identifier().Text());
		return make_shared<BoundErrorExpression>();
	}

	auto function = std::dynamic_pointer_cast<FunctionSymbol>(opt.value());
	if (function == nullptr)
	{
		_diagnostics->ReportNotAFunction(syntax->Identifier().Location(),
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
		auto location = TextLocation(syntax->SynTree().Text(), span);
		_diagnostics->ReportWrongArgumentCount(
			std::move(location), function->Name(),
			function->Parameters().size(), syntax->Arguments()->size());
		return make_shared<BoundErrorExpression>();
	}

	auto hasError = false;
	for (size_t i = 0; i < syntax->Arguments()->size(); ++i)
	{
		auto arg = boundArguments[i].get();
		auto param = function->Parameters()[i];
		if (arg->Type().get() != param.Type())
		{
			_diagnostics->ReportWrongArgumentType(
				(*syntax->Arguments())[i]->Location(),
				param.Name(), param.Type(), arg->Type());
			hasError = true;
		}
	}
	if (hasError)
		return make_shared<BoundErrorExpression>();

	return make_shared<BoundCallExpression>(function, boundArguments);
}

shared_ptr<BoundExpression> Binder::BindPostfixExpression(const PostfixExpressionSyntax* syntax)
{
	auto name = syntax->IdentifierToken().Text();
	auto boundExpression = BindExpression(syntax->Expression());

	auto opt = BindVariableReference(syntax->IdentifierToken());
	if (!opt.has_value())
		return make_shared<BoundErrorExpression>();

	auto variable = opt.value();
	if (variable->IsReadOnly())
	{
		_diagnostics->ReportCannotAssign(syntax->Op().Location(), name);
		return make_shared<BoundErrorExpression>();
	}
	if (boundExpression->Type().get() != variable->Type())
	{
		_diagnostics->ReportCannotConvert(syntax->Expression()->Location(),
			boundExpression->Type(), variable->Type());
		return make_shared<BoundErrorExpression>();
	}
	if (variable->Type().get() != TypeSymbol::Get(TypeEnum::Int))
	{
		_diagnostics->ReportVariableNotSupportPostfixOperator(
			syntax->Expression()->Location(), syntax->Op().Text(), variable->Type());
		return make_shared<BoundErrorExpression>();
	}
	switch (syntax->Op().Kind())
	{
		case SyntaxKind::PlusPlusToken:
			return make_shared<BoundPostfixExpression>(std::move(variable),
				BoundPostfixOperatorEnum::Increment, std::move(boundExpression));
		case SyntaxKind::MinusMinusToken:
			return make_shared<BoundPostfixExpression>(std::move(variable),
				BoundPostfixOperatorEnum::Decrement, std::move(boundExpression));
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected operator token "
				, nameof(syntax->Op().Kind())));
	}
}

shared_ptr<BoundExpression> Binder::BindConversion(const ExpressionSyntax* syntax,
	ConstTypeRef type,
	bool allowExplicit)
{
	auto expression = BindExpression(syntax);
	return BindConversion(syntax->Location(), std::move(expression), type, allowExplicit);
}

shared_ptr<BoundExpression> Binder::BindConversion(TextLocation diagLocation,
	shared_ptr<BoundExpression> expression, ConstTypeRef type, bool allowExplicit)
{
	auto conversion = Conversion::Classify(expression->Type(), type);
	if (!conversion.Exists())
	{
		if (expression->Type().get() != TypeSymbol::Get(TypeEnum::Error)
			&& type.get() != TypeSymbol::Get(TypeEnum::Error))
			_diagnostics->ReportCannotConvert(
				std::move(diagLocation), expression->Type(), type);
		return make_shared<BoundErrorExpression>();
	}
	if (!allowExplicit && conversion.IsExplicit())
		_diagnostics->ReportCannotConvertImplicitly(
			std::move(diagLocation), expression->Type(), type);
	if (conversion.IsIdentity())
		return expression;
	return make_shared<BoundConversionExpression>(type, std::move(expression));
}

shared_ptr<VariableSymbol> Binder::BindVariableDeclaration(const SyntaxToken& identifier, bool isReadOnly,
	ConstTypeRef type)
{
	auto name = identifier.Text().empty() ? "?" : identifier.Text();
	auto declare = !identifier.IsMissing();
	shared_ptr<VariableSymbol> variable = nullptr;
	if (_function == nullptr)
		variable = make_shared<GlobalVariableSymbol>(name, isReadOnly, type);
	else
		variable = make_shared<LocalVariableSymbol>(name, isReadOnly, type);

	if (declare && !_scope->TryDeclareVariable(variable))
		_diagnostics->ReportSymbolAlreadyDeclared(identifier.Location(), name);
	return variable;
}

std::optional<shared_ptr<VariableSymbol>> Binder::BindVariableReference(const SyntaxToken& identifier)
{
	auto name = identifier.Text();
	auto var = _scope->TryLookupSymbol(name).value_or(nullptr);
	if (var == nullptr)
	{
		_diagnostics->ReportUndefinedVariable(identifier.Location(), name);
		return std::nullopt;
	} else
	{
		auto p = std::dynamic_pointer_cast<VariableSymbol>(var);
		if (p)
			return p;
		else
		{
			_diagnostics->ReportNotAVariable(identifier.Location(), name);
			return std::nullopt;
		}
	}
}

std::optional<ConstTypeRef> Binder::BindTypeClause(const std::optional<TypeClauseSyntax>& syntax)
{
	if (!syntax.has_value()) return std::nullopt;

	auto type = LookupType(syntax->Identifier().Text());
	if (!type.has_value())
		_diagnostics->ReportUndefinedType(syntax->Identifier().Location(),
			syntax->Identifier().Text());
	return type;
}

std::optional<ConstTypeRef> Binder::LookupType(string_view name) const
{
	if (name == "bool") return TypeSymbol::Get(TypeEnum::Bool);
	else if (name == "int") return TypeSymbol::Get(TypeEnum::Int);
	else if (name == "string") return TypeSymbol::Get(TypeEnum::String);
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
		auto scope = make_unique<BoundScope>(std::move(parent));
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
	const vector<const SyntaxTree*>& synTrees)
{
	auto parentScope = CreateParentScope(previous);
	auto binder = Binder(std::move(parentScope), nullptr);
	auto statements = vector<shared_ptr<BoundStatement>>();

	for (const auto& tree : synTrees)
	{
		auto syntax = tree->Root();
		for (const auto& it : syntax->Members())
		{
			if (it->Kind() == SyntaxKind::FunctionDeclaration)
			{
				auto func = static_cast<FunctionDeclarationSyntax*>(it.get());
				binder.BindFunctionDeclaration(func);
			}
			if (it->Kind() == SyntaxKind::GlobalStatement)
			{
				auto globalStatement = static_cast<GlobalStatementSyntax*>(it.get());
				statements.push_back(binder.BindStatement(globalStatement->Statement()));
			}
		}
	}

	auto functions = binder._scope->GetDeclaredFunctions();
	auto variables = binder._scope->GetDeclaredVariables();
	auto& diagnostics = binder.Diagnostics();
	if (previous != nullptr)
		diagnostics.AddRangeFront(previous->Diagnostics());
	return make_unique<BoundGlobalScope>(previous, std::move(binder._diagnostics),
		std::move(functions), std::move(variables), std::move(statements));
}

unique_ptr<BoundProgram> Binder::BindProgram(const BoundGlobalScope* globalScope)
{
	auto parentScope = CreateParentScope(globalScope);

	auto funcBodies = BoundProgram::FuncMap();
	auto diag = make_unique<DiagnosticBag>();

	auto scope = globalScope;
	while (scope != nullptr)
	{
		for (const auto& it : scope->Functions())
		{
			auto binder = Binder(std::make_unique<BoundScope>(*parentScope), it.get());
			auto body = binder.BindStatement(it->Declaration()->Body());
			auto lowerBody = Lowerer::Lower(std::move(body));

			if (it->Type().get() != TypeSymbol::Get(TypeEnum::Void)
				&& !ControlFlowGraph::AllPathsReturn(lowerBody.get()))
			{
				binder._diagnostics->ReportAllPathsMustReturn(
					it->Declaration()->Identifier().Location());
			}
			funcBodies.emplace(it.get(), std::move(lowerBody));

			diag->AddRange(binder.Diagnostics());
		}
		scope = scope->Previous();
	}
	auto s = make_shared<BoundBlockStatement>(globalScope->Statements());
	auto statement = Lowerer::Lower(std::move(s));
	return make_unique<BoundProgram>(std::move(diag), std::move(funcBodies),
		std::move(statement));
}

}//MCF
