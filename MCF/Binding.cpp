#include "stdafx.h"
#include "Binding.h"

#include <stack>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "Conversion.h"
#include "Diagnostic.h"
#include "Parsing.h"
#include "SourceText.h"

namespace MCF {

BoundScope::BoundScope(const unique_ptr<BoundScope>& parent)
	: _parent(std::move(std::remove_const_t<unique_ptr<BoundScope>&>(parent)))
{
}

bool BoundScope::TryDeclareVariable(const VariableSymbol & variable)
{
	if (_variables.find(variable.Name()) == _variables.end()
		&& !variable.Name().empty())
	{
		_variables.emplace(variable.Name(), variable);
		return true;
	}
	return false;
}

bool BoundScope::TryLookupVariable(const string & name, VariableSymbol & variable)const
{
	if (_variables.find(name) != _variables.end()
		&& !name.empty())
	{
		variable = _variables.at(name);
		return true;
	}
	if (_parent == nullptr)
		return false;
	return _parent->TryLookupVariable(name, variable);
}

const vector<VariableSymbol> BoundScope::GetDeclaredVariables() const
{
	auto result = vector<VariableSymbol>();
	for (const auto& it : _variables)
		result.emplace_back(it.second);
	return result;
}

bool BoundScope::TryDeclareFunction(const FunctionSymbol & function)
{
	if (_functions.find(function.Name()) == _functions.end() &&
		!function.Name().empty())
	{
		_functions.emplace(function.Name(), function);
		return true;
	}
	return false;
}

bool BoundScope::TryLookupFunction(const string & name, FunctionSymbol & function) const
{
	if (_functions.find(name) != _functions.end()
		&& !name.empty())
	{
		function = _functions.at(name);
		return true;
	}
	if (_parent == nullptr)
		return false;
	return _parent->TryLookupFunction(name, function);
}

const vector<FunctionSymbol> BoundScope::GetDeclaredFunctions() const
{
	auto result = vector<FunctionSymbol>();
	for (const auto& it : _functions)
		result.emplace_back(it.second);
	return result;
}

void BoundScope::ResetToParent(unique_ptr<BoundScope>& current)
{
	if (current->Parent() == nullptr) return;
	current.swap(current->_parent);
}


BoundGlobalScope::BoundGlobalScope(const BoundGlobalScope* previous, const unique_ptr<DiagnosticBag>& diagnostics,
								   const vector<VariableSymbol>& variables, const unique_ptr<BoundStatement>& statement)
	:_previous(previous),
	_diagnostics(std::move(std::remove_const_t<unique_ptr<DiagnosticBag>&>(diagnostics))),
	_variables(variables),
	_statement(std::move(std::remove_const_t<unique_ptr<BoundStatement>&>(statement)))
{
}

Binder::Binder(const unique_ptr<BoundScope>& parent)
	: _diagnostics(make_unique<DiagnosticBag>()),
	_scope(make_unique<BoundScope>(parent))
{
}

unique_ptr<BoundStatement> Binder::BindStatement(const StatementSyntax * syntax)
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

unique_ptr<BoundStatement> Binder::BindBlockStatement(const BlockStatementSyntax * syntax)
{
	auto result = vector<unique_ptr<BoundStatement>>();
	_scope = make_unique<BoundScope>(_scope);
	auto statements = syntax->Statements();
	for (const auto& it : statements)
		result.emplace_back(BindStatement(it));
	BoundScope::ResetToParent(_scope);
	return make_unique<BoundBlockStatement>(result);
}

unique_ptr<BoundStatement> Binder::BindVariableDeclaration(const VariableDeclarationSyntax * syntax)
{
	auto readOnly = syntax->Keyword().Kind() == SyntaxKind::LetKeyword;
	auto init = BindExpression(syntax->Initializer());
	auto variable = BindVariable(syntax->Identifier(), readOnly, init->Type());

	return make_unique<BoundVariableDeclaration>(variable, init);
}

unique_ptr<BoundStatement> Binder::BindIfStatement(const IfStatementSyntax * syntax)
{
	auto condition = BindExpression(syntax->Condition(), TypeSymbol::GetType(TypeEnum::Bool));
	auto thenStatement = BindStatement(syntax->ThenStatement());
	auto elseStatement = syntax->ElseClause() == nullptr ? nullptr
		: BindStatement(syntax->ElseClause()->ElseStatement());
	return make_unique<BoundIfStatement>(condition, thenStatement, elseStatement);
}

unique_ptr<BoundStatement> Binder::BindWhileStatement(const WhileStatementSyntax * syntax)
{
	auto condition = BindExpression(syntax->Condition(), TypeSymbol::GetType(TypeEnum::Bool));
	auto body = BindStatement(syntax->Body());
	return make_unique<BoundWhileStatement>(condition, body);
}

unique_ptr<BoundStatement> Binder::BindDoWhileStatement(const DoWhileStatementSyntax * syntax)
{
	auto body = BindStatement(syntax->Body());
	auto condition = BindExpression(syntax->Condition());
	return make_unique<BoundDoWhileStatement>(body, condition);
}

unique_ptr<BoundStatement> Binder::BindForStatement(const ForStatementSyntax * syntax)
{
	auto lowerBound = BindExpression(syntax->LowerBound(), TypeSymbol::GetType(TypeEnum::Int));
	auto upperBound = BindExpression(syntax->UpperBound(), TypeSymbol::GetType(TypeEnum::Int));

	_scope = make_unique<BoundScope>(_scope);

	auto variable = BindVariable(syntax->Identifier(), true, TypeSymbol::GetType(TypeEnum::Int));
	auto body = BindStatement(syntax->Body());

	BoundScope::ResetToParent(_scope);
	return make_unique<BoundForStatement>(variable, lowerBound, upperBound, body);
}

unique_ptr<BoundStatement> Binder::BindExpressionStatement(const ExpressionStatementSyntax * syntax)
{
	auto expression = BindExpression(syntax->Expression(), true);
	return make_unique<BoundExpressionStatement>(expression);
}

unique_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax * syntax, const TypeSymbol & targetType)
{
	auto result = BindExpression(syntax);
	if (targetType != TypeSymbol::GetType(TypeEnum::Error)
		&& result->Type() != TypeSymbol::GetType(TypeEnum::Error)
		&& result->Type() != targetType)
	{
		_diagnostics->ReportCannotConvert(syntax->Span(), result->Type(), targetType);
		return make_unique<BoundErrorExpression>();
	}
	return result;
}

unique_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax * syntax, bool canBeVoid)
{
	auto result = BindExpressionInternal(syntax);
	if (!canBeVoid && result->Type() == TypeSymbol::GetType(TypeEnum::Void))
	{
		_diagnostics->ReportExpressionMustHaveValue(syntax->Span());
		return make_unique<BoundErrorExpression>();
	}
	return result;
}

unique_ptr<BoundExpression> Binder::BindExpressionInternal(const ExpressionSyntax * syntax)
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

unique_ptr<BoundExpression> Binder::BindParenthesizedExpression(const ParenthesizedExpressionSyntax * syntax)
{
	return BindExpression(syntax->Expression());
}

unique_ptr<BoundExpression> Binder::BindLiteralExpression(const LiteralExpressionSyntax * syntax)
{
	return make_unique<BoundLiteralExpression>(syntax->Value());
}

unique_ptr<BoundExpression> Binder::BindNameExpression(const NameExpressionSyntax * syntax)
{
	auto name = syntax->IdentifierToken().Text();
	if (syntax->IdentifierToken().IsMissing()) // NOTE this token was injected by Parser::MatchToken
		return make_unique<BoundErrorExpression>();

	VariableSymbol variable;
	if (!_scope->TryLookupVariable(name, variable))
	{
		_diagnostics->ReportUndefinedName(syntax->IdentifierToken().Span(), name);
		return make_unique<BoundErrorExpression>();
	}
	return make_unique<BoundVariableExpression>(variable);
}

unique_ptr<BoundExpression> Binder::BindAssignmentExpression(const AssignmentExpressionSyntax * syntax)
{
	auto name = syntax->IdentifierToken().Text();
	auto boundExpression = BindExpression(syntax->Expression());

	VariableSymbol variable;

	if (!_scope->TryLookupVariable(name, variable))
	{
		_diagnostics->ReportUndefinedName(syntax->IdentifierToken().Span(), name);
		return boundExpression;
	}
	if (variable.IsReadOnly())
		_diagnostics->ReportCannotAssign(syntax->EqualsToken().Span(), name);
	if (boundExpression->Type() != variable.Type())
	{
		_diagnostics->ReportCannotConvert(syntax->Expression()->Span(), boundExpression->Type(), variable.Type());
		return boundExpression;
	}
	return make_unique<BoundAssignmentExpression>(variable, boundExpression);
}

unique_ptr<BoundExpression> Binder::BindUnaryExpression(const UnaryExpressionSyntax * syntax)
{
	auto boundOperand = BindExpression(syntax->Operand());
	if (boundOperand->Type() == TypeSymbol::GetType(TypeEnum::Error))
		return make_unique<BoundErrorExpression>();

	auto boundOperator = BoundUnaryOperator::Bind(syntax->OperatorToken().Kind(), boundOperand->Type());
	if (boundOperator.IsUseful())
	{
		return make_unique<BoundUnaryExpression>(boundOperator, boundOperand);
	} else
	{
		_diagnostics->ReportUndefinedUnaryOperator(syntax->OperatorToken().Span(),
												   syntax->OperatorToken().Text(), boundOperand->Type());
		return make_unique<BoundErrorExpression>();
	}
}

unique_ptr<BoundExpression> Binder::BindBinaryExpression(const BinaryExpressionSyntax * syntax)
{
	auto boundLeft = BindExpression(syntax->Left());
	auto boundRight = BindExpression(syntax->Right());
	if (boundLeft->Type() == TypeSymbol::GetType(TypeEnum::Error)
		|| boundRight->Type() == TypeSymbol::GetType(TypeEnum::Error))
		return make_unique<BoundErrorExpression>();

	auto boundOperator = BoundBinaryOperator::Bind(syntax->OperatorToken().Kind(), boundLeft->Type(), boundRight->Type());
	if (boundOperator.IsUseful())
	{
		return make_unique<BoundBinaryExpression>(boundLeft, boundOperator, boundRight);
	} else
	{
		_diagnostics->ReportUndefinedBinaryOperator(syntax->OperatorToken().Span(), syntax->OperatorToken().Text(),
													boundLeft->Type(), boundRight->Type());
		return make_unique<BoundErrorExpression>();
	}
}

unique_ptr<BoundExpression> Binder::BindCallExpression(const CallExpressionSyntax * syntax)
{
	if (syntax->Arguments()->size() == 1)
	{
		auto type = LookupType(syntax->Identifier().Text());
		if (type != TypeSymbol::GetType(TypeEnum::Error))
			return BindConversion(type, (*syntax->Arguments())[0]);
	}

	auto boundArguments = vector<unique_ptr<BoundExpression>>();
	for (const auto& arg : *(syntax->Arguments()))
	{
		auto boundArgument = BindExpression(dynamic_cast<ExpressionSyntax*>(arg.get()));
		boundArguments.emplace_back(std::move(boundArgument));
	}

	FunctionSymbol function;
	if (!_scope->TryLookupFunction(syntax->Identifier().Text(), function))
	{
		_diagnostics->ReportUndefinedFunction(syntax->Identifier().Span(), syntax->Identifier().Text());
		return make_unique<BoundErrorExpression>();
	}
	if (syntax->Arguments()->size() != function.Parameters().size())
	{
		_diagnostics->ReportWrongArgumentCount(syntax->Span(), function.Name(),
											   function.Parameters().size(), syntax->Arguments()->size());
		return make_unique<BoundErrorExpression>();
	}

	for (auto i = 0; i < syntax->Arguments()->size(); ++i)
	{
		auto arg = boundArguments[i].get();
		auto param = function.Parameters()[i];
		if (arg->Type() != param.Type())
		{
			_diagnostics->ReportWrongArgumentType(syntax->Span(), param.Name(),
												  param.Type(), arg->Type());
			return make_unique<BoundErrorExpression>();
		}
	}
	return make_unique<BoundCallExpression>(function, boundArguments);
}

unique_ptr<BoundExpression> Binder::BindPostfixExpression(const PostfixExpressionSyntax * syntax)
{
	auto name = syntax->IdentifierToken().Text();
	auto boundExpression = BindExpression(syntax->Expression());

	VariableSymbol variable;

	if (!_scope->TryLookupVariable(name, variable))
	{
		_diagnostics->ReportUndefinedName(syntax->IdentifierToken().Span(), name);
		return make_unique<BoundErrorExpression>();
	}
	if (variable.IsReadOnly())
	{
		_diagnostics->ReportCannotAssign(syntax->Op().Span(), name);
		return make_unique<BoundErrorExpression>();
	}
	if (boundExpression->Type() != variable.Type())
	{
		_diagnostics->ReportCannotConvert(syntax->Expression()->Span(),
										  boundExpression->Type(), variable.Type());
		return make_unique<BoundErrorExpression>();
	}
	if (variable.Type() != TypeSymbol::GetType(TypeEnum::Int))
	{
		_diagnostics->ReportVariableNotSupportPostfixOperator(syntax->Expression()->Span(),
															  syntax->Op().Text(), variable.Type());
		return make_unique<BoundErrorExpression>();
	}
	switch (syntax->Op().Kind())
	{
		case SyntaxKind::PlusPlusToken:
			return make_unique<BoundPostfixExpression>(variable, BoundPostfixOperatorEnum::Increment,
													   boundExpression);
		case SyntaxKind::MinusMinusToken:
			return make_unique<BoundPostfixExpression>(variable, BoundPostfixOperatorEnum::Decrement,
													   boundExpression);
		default:
			throw std::invalid_argument("Unexpected operator token " + GetSyntaxKindName(syntax->Op().Kind()));
	}
}

unique_ptr<BoundExpression> Binder::BindConversion(const TypeSymbol & type, const ExpressionSyntax * syntax)
{
	auto expression = BindExpression(syntax);
	auto conversion = Conversion::Classify(expression->Type(), type);
	if (!conversion.Exists())
	{
		_diagnostics->ReportCannotConvert(syntax->Span(), expression->Type(), type);
		return make_unique<BoundErrorExpression>();
	}
	return make_unique<BoundConversionExpression>(type, expression);
}

VariableSymbol Binder::BindVariable(const SyntaxToken & identifier, bool isReadOnly, const TypeSymbol & type)
{
	auto name = identifier.Text().empty() ? "?" : identifier.Text();
	auto declare = !identifier.IsMissing();
	auto variable = VariableSymbol(name, isReadOnly, type);

	if (declare && !_scope->TryDeclareVariable(variable))
		_diagnostics->ReportVariableAlreadyDeclared(identifier.Span(), name);
	return variable;
}

TypeSymbol Binder::LookupType(const string & name) const
{
	if (name == "bool") return TypeSymbol::GetType(TypeEnum::Bool);
	else if (name == "int") return TypeSymbol::GetType(TypeEnum::Int);
	else if (name == "string") return TypeSymbol::GetType(TypeEnum::String);
	else return TypeSymbol::GetType(TypeEnum::Error);
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
		result->TryDeclareFunction(f);
	return result;
}

unique_ptr<BoundGlobalScope> Binder::BindGlobalScope(const BoundGlobalScope* previous, const CompilationUnitSyntax* syntax)
{
	auto parentScope = CreateParentScope(previous);
	auto binder = Binder(parentScope);
	auto expression = binder.BindStatement(syntax->Statement());
	auto variables = binder._scope->GetDeclaredVariables();
	auto diagnostics = binder.Diagnostics();
	if (previous != nullptr)
		diagnostics->AddRangeFront(*previous->Diagnostics());
	return make_unique<BoundGlobalScope>(previous, binder._diagnostics, variables, expression);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteStatement(const BoundStatement * node)
{
	switch (node->Kind())
	{
		case BoundNodeKind::BlockStatement:
		{
			auto p = dynamic_cast<const BoundBlockStatement*>(node);
			if (p) return RewriteBlockStatement(p);
			else break;
		}
		case BoundNodeKind::VariableDeclaration:
		{
			auto p = dynamic_cast<const BoundVariableDeclaration*>(node);
			if (p) return RewriteVariableDeclaration(p);
			else break;
		}
		case BoundNodeKind::IfStatement:
		{
			auto p = dynamic_cast<const BoundIfStatement*>(node);
			if (p) return RewriteIfStatement(p);
			else break;
		}
		case BoundNodeKind::WhileStatement:
		{
			auto p = dynamic_cast<const BoundWhileStatement*>(node);
			if (p) return RewriteWhileStatement(p);
			else break;
		}
		case BoundNodeKind::DoWhileStatement:
		{
			auto p = dynamic_cast<const BoundDoWhileStatement*>(node);
			if (p) return RewriteDoWhileStatement(p);
			else break;
		}
		case BoundNodeKind::ForStatement:
		{
			auto p = dynamic_cast<const BoundForStatement*>(node);
			if (p) return RewriteForStatement(p);
			else break;
		}
		case BoundNodeKind::LabelStatement:
		{
			auto p = dynamic_cast<const BoundLabelStatement*>(node);
			if (p) return RewriteLabelStatement(p);
			else break;
		}
		case BoundNodeKind::GotoStatement:
		{
			auto p = dynamic_cast<const BoundGotoStatement*>(node);
			if (p) return RewriteGotoStatement(p);
			else break;
		}
		case BoundNodeKind::ConditionalGotoStatement:
		{
			auto p = dynamic_cast<const BoundConditionalGotoStatement*>(node);
			if (p) return RewriteConditionalGotoStatement(p);
			else break;
		}
		case BoundNodeKind::ExpressionStatement:
		{
			auto p = dynamic_cast<const BoundExpressionStatement*>(node);
			if (p) return RewriteExpressionStatement(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Unexpected node: " + GetEnumText(node->Kind()));
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteBlockStatement(const BoundBlockStatement * node)
{
	auto result = vector<unique_ptr<BoundStatement>>();
	auto statements = node->Statements();
	for (const auto& it : statements)
		result.emplace_back(RewriteStatement(it));
	return make_unique<BoundBlockStatement>(result);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteVariableDeclaration(const BoundVariableDeclaration * node)
{
	auto initializer = RewriteExpression(node->Initializer());
	return make_unique<BoundVariableDeclaration>(node->Variable(), initializer);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteIfStatement(const BoundIfStatement * node)
{
	auto condition = RewriteExpression(node->Condition());
	auto thenStatement = RewriteStatement(node->ThenStatement());
	auto elseStatement = node->ElseStatement() == nullptr ? nullptr : RewriteStatement(node->ElseStatement());
	return make_unique<BoundIfStatement>(condition, thenStatement, elseStatement);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteWhileStatement(const BoundWhileStatement * node)
{
	auto condition = RewriteExpression(node->Condition());
	auto body = RewriteStatement(node->Body());
	return make_unique<BoundWhileStatement>(condition, body);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteDoWhileStatement(const BoundDoWhileStatement * node)
{
	auto body = RewriteStatement(node->Body());
	auto condition = RewriteExpression(node->Condition());
	return make_unique<BoundDoWhileStatement>(body, condition);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteForStatement(const BoundForStatement * node)
{
	auto lowerBound = RewriteExpression(node->LowerBound());
	auto upperBound = RewriteExpression(node->UpperBound());
	auto body = RewriteStatement(node->Body());
	return make_unique<BoundForStatement>(node->Variable(), lowerBound, upperBound, body);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteLabelStatement(const BoundLabelStatement * node)
{
	return make_unique<BoundLabelStatement>(node->Label());
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteGotoStatement(const BoundGotoStatement * node)
{
	return make_unique<BoundGotoStatement>(node->Label());
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteConditionalGotoStatement(const BoundConditionalGotoStatement * node)
{
	auto condition = RewriteExpression(node->Condition());
	return make_unique<BoundConditionalGotoStatement>(node->Label(), condition, node->JumpIfTrue());
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteExpressionStatement(const BoundExpressionStatement * node)
{
	auto expression = RewriteExpression(node->Expression());
	return make_unique<BoundExpressionStatement>(expression);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteExpression(const BoundExpression * node)
{
	switch (node->Kind())
	{
		case BoundNodeKind::ErrorExpression:
		{
			auto p = dynamic_cast<const BoundErrorExpression*>(node);
			if (p) return RewriteErrorExpression(p);
			else break;
		}
		case BoundNodeKind::LiteralExpression:
		{
			auto p = dynamic_cast<const BoundLiteralExpression*>(node);
			if (p) return RewriteLiteralExpression(p);
			else break;
		}
		case BoundNodeKind::VariableExpression:
		{
			auto p = dynamic_cast<const BoundVariableExpression*>(node);
			if (p) return RewriteVariableExpression(p);
			else break;
		}
		case BoundNodeKind::AssignmentExpression:
		{
			auto p = dynamic_cast<const BoundAssignmentExpression*>(node);
			if (p) return RewriteAssignmentExpression(p);
			else break;
		}
		case BoundNodeKind::UnaryExpression:
		{
			auto p = dynamic_cast<const BoundUnaryExpression*>(node);
			if (p) return RewriteUnaryExpression(p);
			else break;
		}
		case BoundNodeKind::BinaryExpression:
		{
			auto p = dynamic_cast<const BoundBinaryExpression*>(node);
			if (p) return RewriteBinaryExpression(p);
			else break;
		}
		case BoundNodeKind::CallExpression:
		{
			auto p = dynamic_cast<const BoundCallExpression*>(node);
			if (p) return RewriteCallExpression(p);
			else break;
		}
		case BoundNodeKind::ConversionExpression:
		{
			auto p = dynamic_cast<const BoundConversionExpression*>(node);
			if (p) return RewriteConversionExpression(p);
			else break;
		}
		case BoundNodeKind::PostfixExpression:
		{
			auto p = dynamic_cast<const BoundPostfixExpression*>(node);
			if (p) return RewritePostfixExpression(p);
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument("Unexpected node: " + GetEnumText(node->Kind()));
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteErrorExpression(const BoundErrorExpression * node)
{
	return make_unique<BoundErrorExpression>();
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteLiteralExpression(const BoundLiteralExpression * node)
{
	return make_unique<BoundLiteralExpression>(node->Value());
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteVariableExpression(const BoundVariableExpression * node)
{
	return make_unique<BoundVariableExpression>(node->Variable());
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteAssignmentExpression(const BoundAssignmentExpression * node)
{
	auto expression = RewriteExpression(node->Expression());
	return make_unique<BoundAssignmentExpression>(node->Variable(), expression);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteUnaryExpression(const BoundUnaryExpression * node)
{
	auto operand = RewriteExpression(node->Operand());
	return make_unique<BoundUnaryExpression>(*(node->Op()), operand);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteBinaryExpression(const BoundBinaryExpression * node)
{
	auto left = RewriteExpression(node->Left());
	auto right = RewriteExpression(node->Right());
	return make_unique<BoundBinaryExpression>(left, *(node->Op()), right);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteCallExpression(const BoundCallExpression * node)
{
	auto result = vector<unique_ptr<BoundExpression>>();
	for (auto i = 0; i < node->Arguments().size(); ++i)
	{
		auto oldArg = node->Arguments()[i];
		auto newArg = RewriteExpression(oldArg);
		result.emplace_back(std::move(newArg));
	}
	return make_unique<BoundCallExpression>(node->Function(), result);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteConversionExpression(const BoundConversionExpression * node)
{
	auto expression = RewriteExpression(node->Expression());
	return make_unique<BoundConversionExpression>(node->Type(), expression);
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewritePostfixExpression(const BoundPostfixExpression * node)
{
	auto expression = RewriteExpression(node->Expression());
	return make_unique<BoundPostfixExpression>(node->Variable(), node->OperatorKind(), expression);
}

BoundLabel Lowerer::GenerateLabel()
{
	++_labelCount;
	string name("Label" + std::to_string(_labelCount));
	return BoundLabel(name);
}

unique_ptr<BoundBlockStatement> Lowerer::Lower(const BoundStatement * statement)
{
	auto lowerer = Lowerer();
	auto result = lowerer.RewriteStatement(statement);
	return lowerer.Flatten(result);
}

unique_ptr<BoundBlockStatement> Lowerer::Flatten(unique_ptr<BoundStatement>& statement)
{
	auto result = vector<unique_ptr<BoundStatement>>();
	auto stack = std::stack<unique_ptr<BoundStatement>>();
	stack.emplace(std::move(statement));

	while (!stack.empty())
	{
		auto current = std::move(stack.top());
		stack.pop();

		if (auto p = dynamic_cast<BoundBlockStatement*>(current.get()))
		{
			auto statements = p->Statements();
			for (auto it = statements.rbegin(); it != statements.rend(); ++it)
				stack.emplace(RewriteStatement(*it));
		} else
		{
			result.emplace_back(std::move(current));
		}
	}
	return make_unique<BoundBlockStatement>(result);
}

unique_ptr<BoundStatement> Lowerer::RewriteIfStatement(const BoundIfStatement * node)
{
	if (node->ElseStatement() == nullptr)
	{
		auto endLabel = GenerateLabel();
		auto endLabelStatement = make_unique<BoundLabelStatement>(endLabel);
		// HACK construct new BoundExpression
		auto condition = RewriteExpression(node->Condition());
		auto thenStatement = RewriteStatement(node->ThenStatement());
		auto gotoFalse = make_unique<BoundConditionalGotoStatement>(endLabel, condition, false);

		auto statements = vector<unique_ptr<BoundStatement>>();
		statements.emplace_back(std::move(gotoFalse));
		statements.emplace_back(std::move(thenStatement));
		statements.emplace_back(std::move(endLabelStatement));

		auto result = make_unique<BoundBlockStatement>(statements);
		return RewriteStatement(result.get());
	} else
	{
		auto elseLabel = GenerateLabel();
		auto endLabel = GenerateLabel();
		auto elseLabelStatement = make_unique<BoundLabelStatement>(elseLabel);
		auto endLabelStatement = make_unique<BoundLabelStatement>(endLabel);
		auto gotoEndStatement = make_unique<BoundGotoStatement>(endLabel);

		auto condition = RewriteExpression(node->Condition());
		auto thenStatement = RewriteStatement(node->ThenStatement());
		auto elseStatement = RewriteStatement(node->ElseStatement());
		auto gotoFalse = make_unique<BoundConditionalGotoStatement>(elseLabel, condition, false);

		auto statements = vector<unique_ptr<BoundStatement>>();
		statements.emplace_back(std::move(gotoFalse));
		statements.emplace_back(std::move(thenStatement));
		statements.emplace_back(std::move(gotoEndStatement));
		statements.emplace_back(std::move(elseLabelStatement));
		statements.emplace_back(std::move(elseStatement));
		statements.emplace_back(std::move(endLabelStatement));

		auto result = make_unique<BoundBlockStatement>(statements);
		return RewriteStatement(result.get());
	}
}

unique_ptr<BoundStatement> Lowerer::RewriteWhileStatement(const BoundWhileStatement * node)
{
	auto continueLabel = GenerateLabel();
	auto checkLabel = GenerateLabel();
	auto endLabel = GenerateLabel();
	auto continueLabelStatement = make_unique<BoundLabelStatement>(continueLabel);
	auto checkLabelStatement = make_unique<BoundLabelStatement>(checkLabel);
	auto gotoCheck = make_unique<BoundGotoStatement>(checkLabel);
	auto endLabelStatement = make_unique<BoundLabelStatement>(endLabel);

	auto condition = RewriteExpression(node->Condition());
	auto body = RewriteStatement(node->Body());
	auto gotoTrue = make_unique<BoundConditionalGotoStatement>(continueLabel, condition);

	auto statements = vector<unique_ptr<BoundStatement>>();
	statements.emplace_back(std::move(gotoCheck));
	statements.emplace_back(std::move(continueLabelStatement));
	statements.emplace_back(std::move(body));
	statements.emplace_back(std::move(checkLabelStatement));
	statements.emplace_back(std::move(gotoTrue));
	statements.emplace_back(std::move(endLabelStatement));

	auto result = make_unique<BoundBlockStatement>(statements);
	return RewriteStatement(result.get());
}

unique_ptr<BoundStatement> Lowerer::RewriteDoWhileStatement(const BoundDoWhileStatement * node)
{
	auto continueLabel = GenerateLabel();
	auto continueLabelStatement = make_unique<BoundLabelStatement>(continueLabel);
	auto condition = RewriteExpression(node->Condition());
	auto gotoTrue = make_unique<BoundConditionalGotoStatement>(continueLabel, condition);

	auto statements = vector<unique_ptr<BoundStatement>>();
	statements.emplace_back(std::move(continueLabelStatement));
	statements.emplace_back(RewriteStatement(node->Body()));
	statements.emplace_back(std::move(gotoTrue));
	auto result = make_unique<BoundBlockStatement>(statements);
	return RewriteStatement(result.get());
}

unique_ptr<BoundStatement> Lowerer::RewriteForStatement(const BoundForStatement * node)
{
	auto lowerBound = RewriteExpression(node->LowerBound());
	auto upperBound = RewriteExpression(node->UpperBound());
	auto body = RewriteStatement(node->Body());

	auto variableDeclaration = make_unique<BoundVariableDeclaration>(node->Variable(), lowerBound);
	auto variableExpression = make_unique<BoundVariableExpression>(node->Variable());
	auto upperBoundSymbol = VariableSymbol("upperBound", true, TypeSymbol::GetType(TypeEnum::Int));
	auto upperBoundDeclaration = make_unique<BoundVariableDeclaration>(upperBoundSymbol, upperBound);
	auto condition = make_unique<BoundBinaryExpression>(
		RewriteExpression(variableExpression.get()),
		BoundBinaryOperator::Bind(SyntaxKind::LessOrEqualsToken, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Int)),
		make_unique<BoundVariableExpression>(upperBoundSymbol)
		);

	auto increment = make_unique<BoundExpressionStatement>(
		make_unique<BoundAssignmentExpression>(
			node->Variable(),
			make_unique<BoundBinaryExpression>(
				RewriteExpression(variableExpression.get()),
				BoundBinaryOperator::Bind(SyntaxKind::PlusToken, TypeSymbol::GetType(TypeEnum::Int), TypeSymbol::GetType(TypeEnum::Int)),
				make_unique<BoundLiteralExpression>(1)
				)
			)
		);

	auto statements = vector<unique_ptr<BoundStatement>>();
	statements.emplace_back(std::move(body));
	statements.emplace_back(std::move(increment));
	auto whileStatement = make_unique<BoundWhileStatement>(
		RewriteExpression(condition.get()),
		make_unique<BoundBlockStatement>(statements) // whileBody
		);

	statements.clear();
	statements.emplace_back(std::move(variableDeclaration));
	statements.emplace_back(std::move(upperBoundDeclaration));
	statements.emplace_back(std::move(whileStatement));
	auto result = make_unique<BoundBlockStatement>(statements);
	return RewriteStatement(result.get());
}

}//MCF
