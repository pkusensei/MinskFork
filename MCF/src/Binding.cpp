#include "Binding.h"

#include <cassert>
#include <stack>
#include <stdexcept>
#include <unordered_set>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "ControlFlowGraph.h"
#include "Lowering.h"
#include "Parsing.h"
#include "StringHelper.h"

namespace MCF {

namespace {
// NOTE CRT calls this function by name
constexpr auto ENTRY_NAME = "main";

enum class ConversionEnum
{
	None, Identity, Implicit, Explicit
};

ConversionEnum Classify(const TypeSymbol& from, const TypeSymbol& to)
{
	if (from == to)
		return ConversionEnum::Identity;
	if (from != TYPE_VOID && to == TYPE_ANY)
	{
		return ConversionEnum::Implicit;
	}
	if (from == TYPE_ANY && to != TYPE_VOID)
	{
		return ConversionEnum::Explicit;
	}
	if (from == TYPE_INT || from == TYPE_BOOL)
	{
		if (to == TYPE_STRING)
			return ConversionEnum::Explicit;
	}
	if (from == TYPE_STRING)
	{
		if (to == TYPE_INT || to == TYPE_BOOL)
			return ConversionEnum::Explicit;
	}
	return ConversionEnum::None;
}

} //namespace

class BoundScope final
{
private:
	std::unordered_map<string_view, unique_ptr<Symbol>> _symbols;
	unique_ptr<BoundScope> _parent;

	template<typename T>
	requires std::derived_from<T, Symbol>
		bool TryDeclareSymbol(unique_ptr<T> symbol)
	{
		auto name = symbol->Name;
		if (!name.empty())
		{
			auto [_, success] = _symbols.emplace(name, std::move(symbol));
			return success;
		}
		return false;
	}

	template<typename T, typename Pred>
	requires std::derived_from<T, Symbol>
		const vector<unique_ptr<T>> GetDeclaredSymbols(Pred&& pred) const
	{
		auto result = vector<unique_ptr<T>>();
		for (const auto& [_, symbol] : _symbols)
		{
			if (std::forward<Pred>(pred)(symbol))
			{
				auto p = symbol->UniqueCloneAs<T>();
				result.push_back(std::move(p));
			}
		}
		return result;
	}

public:
	explicit BoundScope(unique_ptr<BoundScope> parent = nullptr)noexcept
		: _parent(std::move(parent))
	{
	}

	// NOTE copy ctor vs shared_ptr? 
	BoundScope(const BoundScope& other)
		:_symbols{},
		_parent(other._parent ? make_unique<BoundScope>(*other._parent) : nullptr)
	{
		for (const auto& [key, value] : other._symbols)
			_symbols.emplace(key, value->UniqueCloneAs<Symbol>());
	}
	BoundScope& operator=(const BoundScope&) = delete;

	BoundScope(BoundScope&&) = default;
	BoundScope& operator=(BoundScope&&) = default;
	~BoundScope() = default;

	const BoundScope* Parent()const noexcept { return _parent.get(); }

	unique_ptr<Symbol> TryLookupSymbol(string_view name)const;

	bool TryDeclareVariable(unique_ptr<VariableSymbol> variable)
	{
		return TryDeclareSymbol(std::move(variable));
	}
	bool TryDeclareFunction(unique_ptr<FunctionSymbol> function)
	{
		return TryDeclareSymbol(std::move(function));
	}

	const vector<unique_ptr<VariableSymbol>> GetDeclaredVariables()const
	{
		auto pred = [](const auto& ptr)
		{
			return ptr->IsVariableSymbol();
		};
		return GetDeclaredSymbols<VariableSymbol>(pred);
	}
	const vector<unique_ptr<FunctionSymbol>> GetDeclaredFunctions()const
	{
		auto pred = [](const auto& ptr)
		{
			return ptr->Kind() == SymbolKind::Function;
		};

		return GetDeclaredSymbols<FunctionSymbol>(pred);
	}

	static void ResetToParent(unique_ptr<BoundScope>& current)noexcept;
};

unique_ptr<Symbol> BoundScope::TryLookupSymbol(string_view name)const
{
	if (!_symbols.contains(name))
	{
		if (_parent == nullptr)
			return nullptr;
		return _parent->TryLookupSymbol(name);
	}
	return _symbols.at(name)->UniqueCloneAs<Symbol>();
}

void BoundScope::ResetToParent(unique_ptr<BoundScope>& current)noexcept
{
	if (current->Parent() == nullptr) return;
	current.swap(current->_parent);
}

class Binder final
{
private:
	DiagnosticBag _diagnostics;
	std::stack<std::pair<BoundLabel, BoundLabel>> _loopStack; //break-continue pair
	const FunctionSymbol* _function;
	unique_ptr<BoundScope> _scope;
	size_t _labelCount;
	bool _isScript;

	Binder(bool isScript, unique_ptr<BoundScope> parent, const FunctionSymbol* function);

	void BindFunctionDeclaration(const FunctionDeclarationSyntax& syntax);

	shared_ptr<BoundStatement> BindErrorStatement(const SyntaxNode& syntax);
	shared_ptr<BoundStatement> BindGlobalStatement(const StatementSyntax& syntax);
	shared_ptr<BoundStatement> BindStatement(const StatementSyntax& syntax, bool isGlobal = false);
	shared_ptr<BoundStatement> BindStatementInternal(const StatementSyntax& syntax);
	shared_ptr<BoundStatement> BindBlockStatement(const BlockStatementSyntax& syntax);
	shared_ptr<BoundStatement> BindVariableDeclaration(const VariableDeclarationSyntax& syntax);
	shared_ptr<BoundStatement> BindIfStatement(const IfStatementSyntax& syntax);
	shared_ptr<BoundStatement> BindWhileStatement(const WhileStatementSyntax& syntax);
	shared_ptr<BoundStatement> BindDoWhileStatement(const DoWhileStatementSyntax& syntax);
	shared_ptr<BoundStatement> BindForStatement(const ForStatementSyntax& syntax);
	auto BindLoopBody(const StatementSyntax& syntax)
		->std::tuple<shared_ptr<BoundStatement>, BoundLabel, BoundLabel>;

	shared_ptr<BoundStatement> BindBreakStatement(const BreakStatementSyntax& syntax);
	shared_ptr<BoundStatement> BindContinueStatement(const ContinueStatementSyntax& syntax);
	shared_ptr<BoundStatement> BindReturnStatement(const ReturnStatementSyntax& syntax);
	shared_ptr<BoundStatement> BindExpressionStatement(const ExpressionStatementSyntax& syntax);

	shared_ptr<BoundExpression> BindExpression(const ExpressionSyntax& syntax,
											   const TypeSymbol& targetType);
	shared_ptr<BoundExpression> BindExpression(const ExpressionSyntax& syntax,
											   bool canBeVoid = false);
	shared_ptr<BoundExpression> BindExpressionInternal(const ExpressionSyntax& syntax);
	shared_ptr<BoundExpression> BindParenthesizedExpression(const ParenthesizedExpressionSyntax& syntax);
	shared_ptr<BoundExpression> BindLiteralExpression(const LiteralExpressionSyntax& syntax);
	shared_ptr<BoundExpression> BindNameExpression(const NameExpressionSyntax& syntax);
	shared_ptr<BoundExpression> BindAssignmentExpression(const AssignmentExpressionSyntax& syntax);
	shared_ptr<BoundExpression> BindUnaryExpression(const UnaryExpressionSyntax& syntax);
	shared_ptr<BoundExpression> BindBinaryExpression(const BinaryExpressionSyntax& syntax);
	shared_ptr<BoundExpression> BindCallExpression(const CallExpressionSyntax& syntax);
	shared_ptr<BoundExpression> BindPostfixExpression(const PostfixExpressionSyntax& syntax);

	shared_ptr<BoundExpression> BindConversion(const ExpressionSyntax& syntax,
											   const TypeSymbol& type, bool allowExplicit = false);
	shared_ptr<BoundExpression> BindConversion(
		TextLocation diagLocation, shared_ptr<BoundExpression> syntax,
		const TypeSymbol& type, bool allowExplicit = false);
	unique_ptr<VariableSymbol> BindVariableDeclaration(const SyntaxToken& identifier,
													   bool isReadOnly, const TypeSymbol& type,
													   BoundConstant constant = NULL_VALUE);
	unique_ptr<VariableSymbol> FindVariableInScope(const SyntaxToken& identifier);
	std::optional<TypeSymbol> BindTypeClause(const std::optional<TypeClauseSyntax>& syntax);
	std::optional<TypeSymbol> LookupType(string_view name)const;

	[[nodiscard]] static unique_ptr<BoundScope> CreateParentScope(const BoundGlobalScope* previous);
	[[nodiscard]] static unique_ptr<BoundScope> CreateRootScope();

public:

	constexpr const DiagnosticBag& Diagnostics()const& noexcept { return _diagnostics; }
	DiagnosticBag Diagnostics() && noexcept { return std::move(_diagnostics); }

	static BoundGlobalScope BindGlobalScope(bool isScript,
											const BoundGlobalScope* previous,
											const vector<const SyntaxTree*>& trees);
	static BoundProgram BindProgram(bool isScript,
									unique_ptr<BoundProgram> preious,
									const BoundGlobalScope* globalScope);
};

Binder::Binder(bool isScript, unique_ptr<BoundScope> parent, const FunctionSymbol* function)
	:_diagnostics(), _loopStack(),
	_function(function),
	_scope(make_unique<BoundScope>(std::move(parent))),
	_labelCount(0), _isScript(isScript)
{
	if (function != nullptr)
		for (const auto& p : function->Parameters)
		{
			unique_ptr<VariableSymbol> v = make_unique<ParameterSymbol>(p);
			_scope->TryDeclareVariable(std::move(v));
		}
}

void Binder::BindFunctionDeclaration(const FunctionDeclarationSyntax& syntax)
{
	auto parameters = vector<ParameterSymbol>();
	auto seenParamNames = std::unordered_set<string_view>();

	for (const auto& it : syntax.Parameters)
	{
		if (it->Kind() == SyntaxKind::Parameter)
		{
			auto& p = static_cast<const ParameterSyntax&>(*it);
			auto paramName = p.Identifier.Text;
			auto paramType = BindTypeClause(p.Type);
			auto [_, success] = seenParamNames.emplace(paramName);

			if (success)
			{
				auto param = ParameterSymbol(paramName, *paramType);
				parameters.push_back(std::move(param));
			} else
				_diagnostics.ReportParameterAlreadyDeclared(p.Location(), paramName);
		}
	}
	auto type = BindTypeClause(syntax.Type)
		.value_or(TYPE_VOID);
	auto function = make_unique<FunctionSymbol>(syntax.Identifier.Text,
												std::move(parameters), type, &syntax);
	auto name = function->Name;
	if (!function->Declaration->Identifier.Text.empty()
		&& !_scope->TryDeclareFunction(std::move(function)))
	{
		_diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Location(),
												 name);
	}
}

shared_ptr<BoundStatement> Binder::BindErrorStatement(const SyntaxNode& syntax)
{
	return make_shared<BoundExpressionStatement>(syntax,
												 make_shared<BoundErrorExpression>(syntax));
}

shared_ptr<BoundStatement> Binder::BindGlobalStatement(const StatementSyntax& syntax)
{
	return BindStatement(syntax, true);
}

shared_ptr<BoundStatement> Binder::BindStatement(const StatementSyntax& syntax, bool isGlobal)
{
	auto result = BindStatementInternal(syntax);
	if (!_isScript || !isGlobal)
		if (result->Kind() == BoundNodeKind::ExpressionStatement)
		{
			auto& es = static_cast<const BoundExpressionStatement&>(*result);
			auto allowExpression = es.Expression->Kind() == BoundNodeKind::ErrorExpression
				|| es.Expression->Kind() == BoundNodeKind::AssignmentExpression
				|| es.Expression->Kind() == BoundNodeKind::CallExpression
				|| es.Expression->Kind() == BoundNodeKind::CompoundAssignmentExpression
				|| es.Expression->Kind() == BoundNodeKind::PostfixExpression;
			if (!allowExpression)
				_diagnostics.ReportInvalidExpressionStatement(syntax.Location());
		}
	return result;
}

shared_ptr<BoundStatement> Binder::BindStatementInternal(const StatementSyntax& syntax)
{
#define BIND_STMT(kind) \
case SyntaxKind::kind:                                  \
{                                                       \
	auto& p = static_cast<const kind##Syntax&>(syntax); \
	return Bind##kind(p);                               \
}

	switch (syntax.Kind())
	{
		BIND_STMT(BlockStatement);
		BIND_STMT(VariableDeclaration);
		BIND_STMT(IfStatement);
		BIND_STMT(WhileStatement);
		BIND_STMT(DoWhileStatement);
		BIND_STMT(ForStatement);
		BIND_STMT(BreakStatement);
		BIND_STMT(ContinueStatement);
		BIND_STMT(ReturnStatement);
		BIND_STMT(ExpressionStatement);

		default:
			break;
	}
	throw std::invalid_argument(BuildStringFrom("Unexpected syntax: ", nameof(syntax.Kind())));

#undef BIND_STMT
}

shared_ptr<BoundStatement> Binder::BindBlockStatement(const BlockStatementSyntax& syntax)
{
	auto result = vector<shared_ptr<BoundStatement>>();
	_scope = make_unique<BoundScope>(std::move(_scope));
	auto& statements = syntax.Statements;
	for (const auto& it : statements)
		result.push_back(BindStatement(*it));
	BoundScope::ResetToParent(_scope);
	return make_shared<BoundBlockStatement>(syntax, std::move(result));
}

shared_ptr<BoundStatement> Binder::BindVariableDeclaration(const VariableDeclarationSyntax& syntax)
{
	auto readOnly = syntax.Keyword.Kind() == SyntaxKind::LetKeyword;
	auto type = BindTypeClause(syntax.TypeClause);
	auto init = BindExpression(*syntax.Initializer);
	auto variableType = type.value_or(init->Type());
	auto variable = BindVariableDeclaration(syntax.Identifier, readOnly,
											variableType, init->ConstantValue());
	auto convertInitializer = BindConversion(syntax.Identifier.Location(),
											 std::move(init), variableType);

	return make_shared<BoundVariableDeclaration>(syntax, std::move(variable),
												 std::move(convertInitializer));
}

shared_ptr<BoundStatement> Binder::BindIfStatement(const IfStatementSyntax& syntax)
{
	auto condition = BindExpression(*syntax.Condition, TYPE_BOOL);

	if (condition->ConstantValue() != NULL_VALUE)
	{
		if (!condition->ConstantValue().GetValue<bool>())
			_diagnostics.ReportUnreachableCode(*syntax.ThenStatement);
		else if (syntax.ElseClause != nullptr)
			_diagnostics.ReportUnreachableCode(*syntax.ElseClause->ElseStatement);
	}

	auto thenStatement = BindStatement(*syntax.ThenStatement);
	auto elseStatement =
		syntax.ElseClause == nullptr ?
		nullptr : BindStatement(*syntax.ElseClause->ElseStatement);
	return make_shared<BoundIfStatement>(syntax, std::move(condition),
										 std::move(thenStatement), std::move(elseStatement));
}

shared_ptr<BoundStatement> Binder::BindWhileStatement(const WhileStatementSyntax& syntax)
{
	auto condition = BindExpression(*syntax.Condition, TYPE_BOOL);

	if (condition->ConstantValue() != NULL_VALUE)
	{
		if (!condition->ConstantValue().GetValue<bool>())
			_diagnostics.ReportUnreachableCode(*syntax.Body);
	}

	auto [body, breakLabel, continueLabel] = BindLoopBody(*syntax.Body);
	return make_shared<BoundWhileStatement>(syntax,
											std::move(condition), body,
											breakLabel, continueLabel);
}

shared_ptr<BoundStatement> Binder::BindDoWhileStatement(const DoWhileStatementSyntax& syntax)
{
	auto [body, breakLabel, continueLabel] = BindLoopBody(*syntax.Body);
	auto condition = BindExpression(*syntax.Condition, TYPE_BOOL);
	return make_shared<BoundDoWhileStatement>(syntax,
											  body, std::move(condition),
											  breakLabel, continueLabel);
}

shared_ptr<BoundStatement> Binder::BindForStatement(const ForStatementSyntax& syntax)
{
	auto lowerBound = BindExpression(*syntax.LowerBound, TYPE_INT);
	auto upperBound = BindExpression(*syntax.UpperBound, TYPE_INT);

	_scope = make_unique<BoundScope>(std::move(_scope));

	auto variable = BindVariableDeclaration(syntax.Identifier, true, TYPE_INT);
	auto [body, breakLabel, continueLabel] = BindLoopBody(*syntax.Body);

	BoundScope::ResetToParent(_scope);
	return make_shared<BoundForStatement>(syntax,
										  std::move(variable),
										  std::move(lowerBound), std::move(upperBound),
										  body, breakLabel, continueLabel);
}

auto Binder::BindLoopBody(const StatementSyntax& syntax)
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

shared_ptr<BoundStatement> Binder::BindBreakStatement(const BreakStatementSyntax& syntax)
{
	if (_loopStack.empty())
	{
		_diagnostics.ReportInvalidBreakOrContinue(syntax.Keyword.Location(),
												  syntax.Keyword.Text);
		return BindErrorStatement(syntax);
	}
	auto [brLabel, _] = _loopStack.top();
	return make_shared<BoundGotoStatement>(syntax, brLabel);
}

shared_ptr<BoundStatement> Binder::BindContinueStatement(const ContinueStatementSyntax& syntax)
{
	if (_loopStack.empty())
	{
		_diagnostics.ReportInvalidBreakOrContinue(syntax.Keyword.Location(),
												  syntax.Keyword.Text);
		return BindErrorStatement(syntax);
	}
	auto [_, conLabel] = _loopStack.top();
	return make_shared<BoundGotoStatement>(syntax, conLabel);
}

shared_ptr<BoundStatement> Binder::BindReturnStatement(const ReturnStatementSyntax& syntax)
{
	auto expression = syntax.Expression ?
		BindExpression(*syntax.Expression) : nullptr;
	if (_function == nullptr)
	{
		if (_isScript)
		{
			if (expression == nullptr)
				expression = make_shared<BoundLiteralExpression>(syntax, "");
		} else if (expression != nullptr)
			_diagnostics.ReportInvalidReturnWithValueInGlobalStatements(
				syntax.Expression->Location());
	} else
	{
		if (_function->Type == TYPE_VOID)
		{
			if (expression != nullptr)
				_diagnostics.ReportInvalidReturnExpression(syntax.Expression->Location(),
														   _function->Name);
		} else
		{
			if (expression == nullptr)
				_diagnostics.ReportMissingReturnExpression(syntax.Keyword.Location(),
														   _function->Type);
			else
				expression = BindConversion(syntax.Expression->Location(),
											std::move(expression), _function->Type);
		}
	}
	return make_shared<BoundReturnStatement>(syntax, std::move(expression));
}

shared_ptr<BoundStatement> Binder::BindExpressionStatement(const ExpressionStatementSyntax& syntax)
{
	auto expression = BindExpression(*syntax.Expression, true);
	return make_shared<BoundExpressionStatement>(syntax, std::move(expression));
}

shared_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax& syntax,
												   const TypeSymbol& targetType)
{
	return BindConversion(syntax, targetType);
}

shared_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax& syntax,
												   bool canBeVoid)
{
	auto result = BindExpressionInternal(syntax);
	if (!canBeVoid && result->Type() == TYPE_VOID)
	{
		_diagnostics.ReportExpressionMustHaveValue(syntax.Location());
		return make_shared<BoundErrorExpression>(syntax);
	}
	return result;
}

shared_ptr<BoundExpression> Binder::BindExpressionInternal(const ExpressionSyntax& syntax)
{
#define BIND_EXPR(kind) \
case SyntaxKind::kind:                                  \
{                                                       \
	auto& p = static_cast<const kind##Syntax&>(syntax); \
	return Bind##kind(p);                               \
}

	switch (syntax.Kind())
	{
		BIND_EXPR(ParenthesizedExpression);
		BIND_EXPR(LiteralExpression);
		BIND_EXPR(NameExpression);
		BIND_EXPR(AssignmentExpression);
		BIND_EXPR(UnaryExpression);
		BIND_EXPR(BinaryExpression);
		BIND_EXPR(CallExpression);
		BIND_EXPR(PostfixExpression);

		default:
			break;
	}
	throw std::invalid_argument(BuildStringFrom("Invalid expression: ", nameof(syntax.Kind())));

#undef BIND_EXPR
}

shared_ptr<BoundExpression> Binder::BindParenthesizedExpression(const ParenthesizedExpressionSyntax& syntax)
{
	return BindExpression(*syntax.Expression);
}

shared_ptr<BoundExpression> Binder::BindLiteralExpression(const LiteralExpressionSyntax& syntax)
{
	return make_shared<BoundLiteralExpression>(syntax, syntax.Value);
}

shared_ptr<BoundExpression> Binder::BindNameExpression(const NameExpressionSyntax& syntax)
{
	if (syntax.IdentifierToken.IsMissing()) // NOTE this token was injected by Parser::MatchToken
		return make_shared<BoundErrorExpression>(syntax);

	auto var = FindVariableInScope(syntax.IdentifierToken);
	if (var == nullptr)
		return make_shared<BoundErrorExpression>(syntax);
	return make_shared<BoundVariableExpression>(syntax, std::move(var));
}

shared_ptr<BoundExpression> Binder::BindAssignmentExpression(const AssignmentExpressionSyntax& syntax)
{
	auto name = syntax.IdentifierToken.Text;
	auto boundExpression = BindExpression(*syntax.Expression);

	auto variable = FindVariableInScope(syntax.IdentifierToken);
	if (variable == nullptr)
		return boundExpression;

	if (variable->IsReadOnly)
		_diagnostics.ReportCannotAssign(syntax.AssignmentToken.Location(), name);

	if (syntax.AssignmentToken.Kind() != SyntaxKind::EqualsToken)
	{
		auto equivalentOpKind =
			GetBinaryOperatorOfAssignmentOperator(syntax.AssignmentToken.Kind());
		auto op = BoundBinaryOperator::Bind(equivalentOpKind,
											variable->Type,
											boundExpression->Type());
		if (!op.IsUseful)
		{
			_diagnostics.ReportUndefinedBinaryOperator(syntax.AssignmentToken.Location(),
													   syntax.AssignmentToken.Text,
													   variable->Type,
													   boundExpression->Type());
			return make_shared<BoundErrorExpression>(syntax);
		}
		auto convertExpression = BindConversion(syntax.Expression->Location(),
												std::move(boundExpression), variable->Type);
		return make_shared<BoundCompoundAssignmentExpression>(syntax,
															  std::move(variable),
															  std::move(op),
															  std::move(convertExpression));
	} else
	{
		auto convertExpression = BindConversion(syntax.Expression->Location(),
												std::move(boundExpression), variable->Type);
		return make_shared<BoundAssignmentExpression>(syntax,
													  std::move(variable),
													  std::move(convertExpression));
	}
}

shared_ptr<BoundExpression> Binder::BindUnaryExpression(const UnaryExpressionSyntax& syntax)
{
	auto boundOperand = BindExpression(*syntax.Operand);
	if (boundOperand->Type() == TYPE_ERROR)
		return make_shared<BoundErrorExpression>(syntax);

	auto boundOperator = BoundUnaryOperator::Bind(syntax.OperatorToken.Kind(),
												  boundOperand->Type());
	if (boundOperator.IsUseful)
	{
		return make_shared<BoundUnaryExpression>(syntax, boundOperator, std::move(boundOperand));
	} else
	{
		_diagnostics.ReportUndefinedUnaryOperator(syntax.OperatorToken.Location(),
												  syntax.OperatorToken.Text, boundOperand->Type());
		return make_shared<BoundErrorExpression>(syntax);
	}
}

shared_ptr<BoundExpression> Binder::BindBinaryExpression(const BinaryExpressionSyntax& syntax)
{
	auto boundLeft = BindExpression(*syntax.Left);
	auto boundRight = BindExpression(*syntax.Right);
	if (boundLeft->Type() == TYPE_ERROR
		|| boundRight->Type() == TYPE_ERROR)
		return make_shared<BoundErrorExpression>(syntax);

	auto boundOperator = BoundBinaryOperator::Bind(syntax.OperatorToken.Kind(),
												   boundLeft->Type(), boundRight->Type());
	if (boundOperator.IsUseful)
	{
		return make_shared<BoundBinaryExpression>(syntax, std::move(boundLeft),
												  boundOperator, std::move(boundRight));
	} else
	{
		_diagnostics.ReportUndefinedBinaryOperator(syntax.OperatorToken.Location(),
												   syntax.OperatorToken.Text, boundLeft->Type(),
												   boundRight->Type());
		return make_shared<BoundErrorExpression>(syntax);
	}
}

shared_ptr<BoundExpression> Binder::BindCallExpression(const CallExpressionSyntax& syntax)
{
	if (syntax.Arguments.size() == 1)
	{
		auto type = LookupType(syntax.Identifier.Text);
		if (type.has_value())
			return BindConversion(*syntax.Arguments[0], *type, true);
	}

	auto boundArguments = vector<shared_ptr<BoundExpression>>();
	for (const auto& arg : syntax.Arguments)
	{
		auto& p = static_cast<ExpressionSyntax&>(*arg);
		boundArguments.push_back(BindExpression(p));
	}

	auto sp = _scope->TryLookupSymbol(syntax.Identifier.Text);
	if (sp == nullptr)
	{
		_diagnostics.ReportUndefinedFunction(syntax.Identifier.Location(),
											 syntax.Identifier.Text);
		return make_shared<BoundErrorExpression>(syntax);
	}

	if (sp->Kind() != SymbolKind::Function)
	{
		_diagnostics.ReportNotAFunction(syntax.Identifier.Location(),
										syntax.Identifier.Text);
		return make_shared<BoundErrorExpression>(syntax);
	}

	auto function = StaticUniquePtrCast<FunctionSymbol>(std::move(sp));

	if (syntax.Arguments.size() != function->Parameters.size())
	{
		TextSpan span;
		if (syntax.Arguments.size() > function->Parameters.size())
		{
			const SyntaxNode* firstExceedingNode = nullptr;
			if (function->Parameters.size() > 0)
				firstExceedingNode = syntax.Arguments.GetSeparator(function->Parameters.size() - 1);
			else
				firstExceedingNode = syntax.Arguments[0];
			auto lastExceedingArg = syntax.Arguments[syntax.Arguments.size() - 1];

			span = TextSpan::FromBounds(firstExceedingNode->Span().Start,
										lastExceedingArg->Span().End());
		} else
		{
			span = syntax.CloseParenthesisToken.Span();
		}
		auto location = TextLocation(syntax.Tree().Text(), span);
		_diagnostics.ReportWrongArgumentCount(
			std::move(location), function->Name,
			function->Parameters.size(), syntax.Arguments.size());
		return make_shared<BoundErrorExpression>(syntax);
	}

	for (size_t i = 0; i < syntax.Arguments.size(); ++i)
	{
		auto argLocation = syntax.Arguments[i]->Location();
		auto& arg = boundArguments[i];
		auto& param = function->Parameters.at(i);
		boundArguments.at(i) = BindConversion(argLocation, arg, param.Type);
	}

	return make_shared<BoundCallExpression>(syntax, std::move(function), boundArguments);
}

shared_ptr<BoundExpression> Binder::BindPostfixExpression(const PostfixExpressionSyntax& syntax)
{
	auto name = syntax.Identifier.Text;
	auto boundExpression = BindExpression(*syntax.Expression);

	auto variable = FindVariableInScope(syntax.Identifier);
	if (variable == nullptr)
		return make_shared<BoundErrorExpression>(syntax);

	if (variable->IsReadOnly)
	{
		_diagnostics.ReportCannotAssign(syntax.Op.Location(), name);
		return make_shared<BoundErrorExpression>(syntax);
	}
	if (boundExpression->Type() != variable->Type)
	{
		_diagnostics.ReportCannotConvert(syntax.Expression->Location(),
										 boundExpression->Type(), variable->Type);
		return make_shared<BoundErrorExpression>(syntax);
	}
	if (variable->Type != TYPE_INT)
	{
		_diagnostics.ReportVariableNotSupportPostfixOperator(
			syntax.Expression->Location(), syntax.Op.Text, variable->Type);
		return make_shared<BoundErrorExpression>(syntax);
	}
	switch (syntax.Op.Kind())
	{
		case SyntaxKind::PlusPlusToken:
			return make_shared<BoundPostfixExpression>(syntax, std::move(variable),
													   BoundPostfixOperatorEnum::Increment,
													   std::move(boundExpression));
		case SyntaxKind::MinusMinusToken:
			return make_shared<BoundPostfixExpression>(syntax, std::move(variable),
													   BoundPostfixOperatorEnum::Decrement,
													   std::move(boundExpression));
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected operator token: "
														, nameof(syntax.Op.Kind())));
	}
}

shared_ptr<BoundExpression> Binder::BindConversion(const ExpressionSyntax& syntax,
												   const TypeSymbol& type,
												   bool allowExplicit)
{
	auto expression = BindExpression(syntax);
	return BindConversion(syntax.Location(), std::move(expression), type, allowExplicit);
}

shared_ptr<BoundExpression> Binder::BindConversion(TextLocation diagLocation,
												   shared_ptr<BoundExpression> expression,
												   const TypeSymbol& type,
												   bool allowExplicit)
{
	auto conversion = Classify(expression->Type(), type);
	if (conversion == ConversionEnum::None)
	{
		if (expression->Type() != TYPE_ERROR
			&& type != TYPE_ERROR)
			_diagnostics.ReportCannotConvert(
				std::move(diagLocation), expression->Type(), type);
		return make_shared<BoundErrorExpression>(expression->Syntax());
	}
	if (!allowExplicit && conversion == ConversionEnum::Explicit)
		_diagnostics.ReportCannotConvertImplicitly(std::move(diagLocation),
												   expression->Type(), type);
	if (conversion == ConversionEnum::Identity)
		return expression;
	return make_shared<BoundConversionExpression>(expression->Syntax(), type, std::move(expression));
}

unique_ptr<VariableSymbol> Binder::BindVariableDeclaration(const SyntaxToken& identifier,
														   bool isReadOnly,
														   const TypeSymbol& type,
														   BoundConstant constant)
{
	auto name = identifier.Text.empty() ? "?" : identifier.Text;
	auto declare = !identifier.IsMissing();
	unique_ptr<VariableSymbol> variable{ nullptr };
	if (_function == nullptr)
		variable = make_unique<GlobalVariableSymbol>(name, isReadOnly, type, std::move(constant));
	else
		variable = make_unique<LocalVariableSymbol>(name, isReadOnly, type, std::move(constant));

	if (declare &&
		!_scope->TryDeclareVariable(variable->UniqueCloneAs<VariableSymbol>()))
		_diagnostics.ReportSymbolAlreadyDeclared(identifier.Location(), name);
	return variable;
}

unique_ptr<VariableSymbol> Binder::FindVariableInScope(const SyntaxToken& identifier)
{
	auto name = identifier.Text;
	auto var = _scope->TryLookupSymbol(name);
	if (var == nullptr)
	{
		_diagnostics.ReportUndefinedVariable(identifier.Location(), name);
		return nullptr;
	} else
	{
		if (var->IsVariableSymbol())
		{
			return StaticUniquePtrCast<VariableSymbol>(std::move(var));
		} else
		{
			_diagnostics.ReportNotAVariable(identifier.Location(), name);
			return nullptr;
		}
	}
}

std::optional<TypeSymbol> Binder::BindTypeClause(const std::optional<TypeClauseSyntax>& syntax)
{
	if (!syntax.has_value()) return std::nullopt;

	auto type = LookupType(syntax->Identifier.Text);
	if (!type.has_value())
		_diagnostics.ReportUndefinedType(syntax->Identifier.Location(),
										 syntax->Identifier.Text);
	return type;
}

std::optional<TypeSymbol> Binder::LookupType(string_view name) const
{
	if (name == "bool") return TYPE_BOOL;
	else if (name == "any") return TYPE_ANY;
	else if (name == "int") return TYPE_INT;
	else if (name == "string") return TYPE_STRING;
	else return std::nullopt;
}

unique_ptr<BoundScope> Binder::CreateParentScope(const BoundGlobalScope* previous)
{
	auto stack = std::stack<const BoundGlobalScope*>();
	while (previous != nullptr)
	{
		stack.emplace(previous);
		previous = previous->Previous;
	}
	auto parent = CreateRootScope();
	while (!stack.empty())
	{
		auto current = stack.top();
		auto scope = make_unique<BoundScope>(std::move(parent));
		for (const auto& it : current->Functions)
			scope->TryDeclareFunction(it->UniqueCloneAs<FunctionSymbol>());
		for (const auto& it : current->Variables)
			scope->TryDeclareVariable(it->UniqueCloneAs<VariableSymbol>());
		parent.swap(scope);
		stack.pop();
	}
	return parent;
}

unique_ptr<BoundScope> Binder::CreateRootScope()
{
	auto result = make_unique<BoundScope>(nullptr);
	for (const auto f : AllBuiltinFunctions)
	{
		assert(f && "Built-in functions should have been declared.");

		result->TryDeclareFunction(make_unique<FunctionSymbol>(*f));
	}
	return result;
}

BoundGlobalScope BindGlobalScope(bool isScript,
								 const BoundGlobalScope* previous,
								 const vector<const SyntaxTree*>& trees)
{
	return Binder::BindGlobalScope(isScript, previous, trees);
}

BoundGlobalScope Binder::BindGlobalScope(bool isScript,
										 const BoundGlobalScope* previous,
										 const vector<const SyntaxTree*>& trees)
{
	auto parentScope = CreateParentScope(previous);
	auto binder = Binder(isScript, std::move(parentScope), nullptr);

	std::for_each(trees.cbegin(), trees.cend(),
				  [&binder](const auto tree)
				  {
					  binder._diagnostics.AddRange(std::move(*tree).Diagnostics());
				  });

	if (!binder._diagnostics.empty())
	{
		return BoundGlobalScope(previous,
								make_unique<DiagnosticBag>(std::move(binder).Diagnostics()),
								nullptr, nullptr, {}, {}, {});
	}

	auto statements = vector<shared_ptr<BoundStatement>>();
	auto globalStmts = vector<const GlobalStatementSyntax*>();

	for (const auto& tree : trees)
	{
		auto syntax = tree->Root();
		for (const auto& it : syntax->Members)
		{
			if (it->Kind() == SyntaxKind::FunctionDeclaration)
			{
				auto& func = static_cast<const FunctionDeclarationSyntax&>(*it);
				binder.BindFunctionDeclaration(func);
			}
		}
	}

	for (const auto& tree : trees)
	{
		auto syntax = tree->Root();
		for (const auto& it : syntax->Members)
		{
			if (it->Kind() == SyntaxKind::GlobalStatement)
			{
				auto& globalStatement = static_cast<const GlobalStatementSyntax&>(*it);
				globalStmts.push_back(&globalStatement);
				statements.push_back(binder.BindGlobalStatement(*globalStatement.Statement));
			}
		}
	}

	auto firstGlobalStmtPerSynTree = vector<const GlobalStatementSyntax*>();
	std::for_each(trees.cbegin(), trees.cend(),
				  [&firstGlobalStmtPerSynTree](const auto& tree)
				  {
					  auto& members = tree->Root()->Members;
					  auto it = std::find_if(members.cbegin(), members.cend(),
											 [](const auto& it)
											 {
												 return it->Kind() == SyntaxKind::GlobalStatement;
											 });
					  if (it != members.cend())
						  firstGlobalStmtPerSynTree.push_back(static_cast<GlobalStatementSyntax*>(it->get()));
				  });

	auto functions = binder._scope->GetDeclaredFunctions();
	unique_ptr<FunctionSymbol> mainFunc = nullptr;
	unique_ptr<FunctionSymbol> scriptFunc = nullptr;

	if (isScript)
	{
		if (!globalStmts.empty())
		{
			// TODO should this function also be name "main"? 
			scriptFunc = make_unique<FunctionSymbol>("$eval", vector<ParameterSymbol>(),
													 TYPE_ANY, nullptr);
		}
	} else
	{
		auto it = std::find_if(functions.cbegin(), functions.cend(),
							   [](const auto& it) { return it->Name == ENTRY_NAME; });
		if (it != functions.cend())
		{
			if ((*it)->Type != TYPE_VOID
				|| !(*it)->Parameters.empty())
				binder._diagnostics.ReportMainMustHaveCorrectSignature(
					(*it)->Declaration->Identifier.Location()
				);
			mainFunc = make_unique<FunctionSymbol>(**it);
		}
		if (!globalStmts.empty())
		{
			if (mainFunc != nullptr)
			{
				binder._diagnostics.ReportCannotMixMainAndGlobalStatements(
					mainFunc->Declaration->Identifier.Location()
				);
				for (const auto& gs : firstGlobalStmtPerSynTree)
				{
					binder._diagnostics.ReportCannotMixMainAndGlobalStatements(
						gs->Location());
				}
			} else
			{
				mainFunc = make_unique<FunctionSymbol>(ENTRY_NAME, vector<ParameterSymbol>(),
													   TYPE_VOID, nullptr);
			}
		}
	}

	auto diagnostics = std::move(binder).Diagnostics();
	auto variables = binder._scope->GetDeclaredVariables();
	if (previous != nullptr)
		diagnostics.AddRangeFront(std::move(*previous->Diagnostics));
	return BoundGlobalScope(previous,
							make_unique<DiagnosticBag>(std::move(diagnostics)),
							std::move(mainFunc), std::move(scriptFunc),
							std::move(functions), std::move(variables),
							std::move(statements));
}

BoundProgram BindProgram(bool isScript,
						 unique_ptr<BoundProgram> preious, const BoundGlobalScope* globalScope)
{
	return Binder::BindProgram(isScript, std::move(preious), globalScope);
}

BoundProgram Binder::BindProgram(bool isScript,
								 unique_ptr<BoundProgram> previous, const BoundGlobalScope* globalScope)
{
	auto parentScope = CreateParentScope(globalScope);

	if (!globalScope->Diagnostics->empty())
	{
		return BoundProgram(std::move(previous),
							make_unique<DiagnosticBag>(std::move(*globalScope->Diagnostics)),
							nullptr, nullptr,
							BoundProgram::FuncMap());
	}

	auto funcBodies = BoundProgram::FuncMap();
	auto diag = make_unique<DiagnosticBag>();

	for (const auto& it : globalScope->Functions)
	{
		auto binder = Binder(isScript, std::make_unique<BoundScope>(*parentScope), it.get());
		auto body = binder.BindStatement(*it->Declaration->Body);
		auto lowerBody = Lower(*it, std::move(body));

		if (it->Type != TYPE_VOID
			&& !ControlFlowGraph::AllPathsReturn(*lowerBody))
		{
			binder._diagnostics.ReportAllPathsMustReturn(
				it->Declaration->Identifier.Location());
		}
		funcBodies.emplace(it.get(), std::move(lowerBody));

		diag->AddRange(std::move(binder).Diagnostics());
	}

	auto cu = globalScope->Statements.empty() ? nullptr
		: globalScope->Statements.front()->Syntax().AncestorsAndSelf().back();

	if (globalScope->MainFunc != nullptr && !globalScope->Statements.empty())
	{
		assert(cu && "Local variable cu should be a valid pointer.");

		auto body = Lower(*globalScope->MainFunc,
						  make_shared<BoundBlockStatement>(*cu, globalScope->Statements));
		funcBodies.emplace(globalScope->MainFunc.get(), std::move(body));
	} else if (globalScope->ScriptFunc != nullptr)
	{
		auto statements = globalScope->Statements;

		// adds return to one-liner expression 
		// otherwise return empty string ""
		if (statements.size() == 1
			&& statements.at(0)->Kind() == BoundNodeKind::ExpressionStatement)
		{
			auto& es = static_cast<const BoundExpressionStatement&>(*statements.at(0));
			if (es.Expression->Type() != TYPE_VOID)
			{
				statements.at(0) = make_shared<BoundReturnStatement>(es.Expression->Syntax(),
																	 es.Expression);
			}
		} else if (!statements.empty()
				   && statements.back()->Kind() != BoundNodeKind::ReturnStatement)
		{
			assert(cu && "Local variable cu should be a valid pointer.");

			auto nullValue = make_shared<BoundLiteralExpression>(*cu, "");
			statements.push_back(make_shared<BoundReturnStatement>(*cu, nullValue));
		}

		assert(cu && "Local variable cu should be a valid pointer.");

		auto body = Lower(*globalScope->ScriptFunc,
						  make_shared<BoundBlockStatement>(*cu, std::move(statements)));
		funcBodies.emplace(globalScope->ScriptFunc.get(), std::move(body));
	}

	return BoundProgram(std::move(previous), std::move(diag),
						globalScope->MainFunc.get(), globalScope->ScriptFunc.get(),
						std::move(funcBodies));
}

}//MCF
