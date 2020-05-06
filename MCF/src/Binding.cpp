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

// NOTE CRT calls this function by name
constexpr auto ENTRY_NAME = "main";

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

class Binder final
{
private:
	unique_ptr<DiagnosticBag> _diagnostics;
	const bool _isScript;
	const FunctionSymbol* _function;
	unique_ptr<BoundScope> _scope;
	std::stack<std::pair<BoundLabel, BoundLabel>> _loopStack; //break-continue pair
	size_t _labelCount;

	void BindFunctionDeclaration(const FunctionDeclarationSyntax* syntax);

	shared_ptr<BoundStatement> BindErrorStatement();
	shared_ptr<BoundStatement> BindGlobalStatement(const StatementSyntax* syntax);
	shared_ptr<BoundStatement> BindStatement(const StatementSyntax* syntax, bool isGlobal = false);
	shared_ptr<BoundStatement> BindStatementInternal(const StatementSyntax* syntax);
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
	shared_ptr<BoundExpression> BindConversion(
		TextLocation diagLocation, shared_ptr<BoundExpression> syntax,
		const TypeSymbol& type, bool allowExplicit = false);
	shared_ptr<VariableSymbol> BindVariableDeclaration(const SyntaxToken& identifier,
		bool isReadOnly, const TypeSymbol& type);
	std::optional<shared_ptr<VariableSymbol>> BindVariableReference(const SyntaxToken& identifier);
	std::optional<TypeSymbol> BindTypeClause(const std::optional<TypeClauseSyntax>& syntax);
	std::optional<TypeSymbol> LookupType(string_view name)const;

	[[nodiscard]] static unique_ptr<BoundScope> CreateParentScope(const BoundGlobalScope* previous);
	[[nodiscard]] static unique_ptr<BoundScope> CreateRootScope();

public:
	Binder(bool isScript, unique_ptr<BoundScope> parent, const FunctionSymbol* function);

	DiagnosticBag& Diagnostics()const noexcept { return *_diagnostics; }

	static unique_ptr<BoundGlobalScope> BindGlobalScope(bool isScript,
		const BoundGlobalScope* previous, const vector<const SyntaxTree*>& trees);
	static unique_ptr<BoundProgram> BindProgram(bool isScript,
		unique_ptr<BoundProgram> preious, const BoundGlobalScope* globalScope);
};

Binder::Binder(bool isScript, unique_ptr<BoundScope> parent, const FunctionSymbol* function)
	: _diagnostics(make_unique<DiagnosticBag>()), _isScript(isScript),
	_function(function),
	_scope(make_unique<BoundScope>(std::move(parent))),
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
		if (it->Kind() == SyntaxKind::Parameter)
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
		.value_or(TYPE_VOID);
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

shared_ptr<BoundStatement> Binder::BindGlobalStatement(const StatementSyntax* syntax)
{
	return BindStatement(syntax, true);
}

shared_ptr<BoundStatement> Binder::BindStatement(const StatementSyntax* syntax, bool isGlobal)
{
	auto result = BindStatementInternal(syntax);
	if (!_isScript || !isGlobal)
		if (result->Kind() == BoundNodeKind::ExpressionStatement)
		{
			auto es = static_cast<BoundExpressionStatement*>(result.get());
			auto allowExpression = es->Expression()->Kind() == BoundNodeKind::ErrorExpression
				|| es->Expression()->Kind() == BoundNodeKind::AssignmentExpression
				|| es->Expression()->Kind() == BoundNodeKind::CallExpression
				|| es->Expression()->Kind() == BoundNodeKind::PostfixExpression;
			if (!allowExpression)
				_diagnostics->ReportInvalidExpressionStatement(syntax->Location());
		}
	return result;
}

shared_ptr<BoundStatement> Binder::BindStatementInternal(const StatementSyntax* syntax)
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
	throw std::invalid_argument(BuildStringFrom("Unexpected syntax: ", nameof(syntax->Kind())));
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
	auto condition = BindExpression(syntax->Condition(), TYPE_BOOL);
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
		TYPE_BOOL);
	auto [body, breakLabel, continueLabel] = BindLoopBody(syntax->Body());
	return make_shared<BoundWhileStatement>(std::move(condition), body,
		breakLabel, continueLabel);
}

shared_ptr<BoundStatement> Binder::BindDoWhileStatement(const DoWhileStatementSyntax* syntax)
{
	auto [body, breakLabel, continueLabel] = BindLoopBody(syntax->Body());
	auto condition = BindExpression(syntax->Condition(),
		TYPE_BOOL);
	return make_shared<BoundDoWhileStatement>(body, std::move(condition),
		breakLabel, continueLabel);
}

shared_ptr<BoundStatement> Binder::BindForStatement(const ForStatementSyntax* syntax)
{
	auto lowerBound = BindExpression(syntax->LowerBound(),
		TYPE_INT);
	auto upperBound = BindExpression(syntax->UpperBound(),
		TYPE_INT);

	_scope = make_unique<BoundScope>(std::move(_scope));

	auto variable = BindVariableDeclaration(syntax->Identifier(), true,
		TYPE_INT);
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
	auto expression = syntax->Expression() ?
		BindExpression(syntax->Expression()) : nullptr;
	if (_function == nullptr)
	{
		if (_isScript)
		{
			if (expression == nullptr)
				expression = make_shared<BoundLiteralExpression>("");
		} else if (expression != nullptr)
			_diagnostics->ReportInvalidReturnWithValueInGlobalStatements(
				syntax->Expression()->Location());
	} else
	{
		if (_function->Type() == TYPE_VOID)
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
	const TypeSymbol& targetType)
{
	return BindConversion(syntax, targetType);
}

shared_ptr<BoundExpression> Binder::BindExpression(const ExpressionSyntax* syntax,
	bool canBeVoid)
{
	auto result = BindExpressionInternal(syntax);
	if (!canBeVoid && result->Type() == TYPE_VOID)
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
	throw std::invalid_argument(BuildStringFrom("Invalid expression: ", nameof(syntax->Kind())));

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
	if (boundOperand->Type() == TYPE_ERROR)
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
	if (boundLeft->Type() == TYPE_ERROR
		|| boundRight->Type() == TYPE_ERROR)
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
	if (syntax->Arguments().size() == 1)
	{
		auto type = LookupType(syntax->Identifier().Text());
		if (type.has_value())
			return BindConversion(syntax->Arguments()[0], *type, true);
	}

	auto boundArguments = vector<shared_ptr<BoundExpression>>();
	for (const auto& arg : syntax->Arguments())
	{
		auto p = static_cast<ExpressionSyntax*>(arg.get());
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

	if (syntax->Arguments().size() != function->Parameters().size())
	{
		TextSpan span;
		if (syntax->Arguments().size() > function->Parameters().size())
		{
			const SyntaxNode* firstExceedingNode = nullptr;
			if (function->Parameters().size() > 0)
				firstExceedingNode = syntax->Arguments().GetSeparator(function->Parameters().size() - 1);
			else
				firstExceedingNode = syntax->Arguments()[0];
			auto lastExceedingArg = syntax->Arguments()[syntax->Arguments().size() - 1];

			span = TextSpan::FromBounds(firstExceedingNode->Span().Start(),
				lastExceedingArg->Span().End());
		} else
		{
			span = syntax->CloseParenthesisToken().Span();
		}
		auto location = TextLocation(syntax->SynTree().Text(), span);
		_diagnostics->ReportWrongArgumentCount(
			std::move(location), function->Name(),
			function->Parameters().size(), syntax->Arguments().size());
		return make_shared<BoundErrorExpression>();
	}

	for (size_t i = 0; i < syntax->Arguments().size(); ++i)
	{
		auto argLocation = syntax->Arguments()[i]->Location();
		auto& arg = boundArguments[i];
		auto param = function->Parameters()[i];
		boundArguments.at(i) = BindConversion(argLocation, arg, param.Type());
	}

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
	if (boundExpression->Type() != variable->Type())
	{
		_diagnostics->ReportCannotConvert(syntax->Expression()->Location(),
			boundExpression->Type(), variable->Type());
		return make_shared<BoundErrorExpression>();
	}
	if (variable->Type() != TYPE_INT)
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
			throw std::invalid_argument(BuildStringFrom("Unexpected operator token: "
				, nameof(syntax->Op().Kind())));
	}
}

shared_ptr<BoundExpression> Binder::BindConversion(const ExpressionSyntax* syntax,
	const TypeSymbol& type, bool allowExplicit)
{
	auto expression = BindExpression(syntax);
	return BindConversion(syntax->Location(), std::move(expression), type, allowExplicit);
}

shared_ptr<BoundExpression> Binder::BindConversion(TextLocation diagLocation,
	shared_ptr<BoundExpression> expression, const TypeSymbol& type, bool allowExplicit)
{
	auto conversion = Classify(expression->Type(), type);
	if (conversion == ConversionEnum::None)
	{
		if (expression->Type() != TYPE_ERROR
			&& type != TYPE_ERROR)
			_diagnostics->ReportCannotConvert(
				std::move(diagLocation), expression->Type(), type);
		return make_shared<BoundErrorExpression>();
	}
	if (!allowExplicit && conversion == ConversionEnum::Explicit)
		_diagnostics->ReportCannotConvertImplicitly(
			std::move(diagLocation), expression->Type(), type);
	if (conversion == ConversionEnum::Identity)
		return expression;
	return make_shared<BoundConversionExpression>(type, std::move(expression));
}

shared_ptr<VariableSymbol> Binder::BindVariableDeclaration(const SyntaxToken& identifier, bool isReadOnly,
	const TypeSymbol& type)
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

std::optional<TypeSymbol> Binder::BindTypeClause(const std::optional<TypeClauseSyntax>& syntax)
{
	if (!syntax.has_value()) return std::nullopt;

	auto type = LookupType(syntax->Identifier().Text());
	if (!type.has_value())
		_diagnostics->ReportUndefinedType(syntax->Identifier().Location(),
			syntax->Identifier().Text());
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

unique_ptr<BoundGlobalScope> BindGlobalScope(bool isScript,
	const BoundGlobalScope* previous, const vector<const SyntaxTree*>& trees)
{
	return Binder::BindGlobalScope(isScript, previous, trees);
}

unique_ptr<BoundGlobalScope> Binder::BindGlobalScope(bool isScript,
	const BoundGlobalScope* previous, const vector<const SyntaxTree*>& trees)
{
	auto parentScope = CreateParentScope(previous);
	auto binder = Binder(isScript, std::move(parentScope), nullptr);
	auto statements = vector<shared_ptr<BoundStatement>>();
	auto globalStmts = vector<const GlobalStatementSyntax*>();

	for (const auto& tree : trees)
	{
		auto syntax = tree->Root();
		for (const auto& it : syntax->Members())
		{
			if (it->Kind() == SyntaxKind::FunctionDeclaration)
			{
				auto func = static_cast<FunctionDeclarationSyntax*>(it.get());
				binder.BindFunctionDeclaration(func);
			}
		}
	}

	for (const auto& tree : trees)
	{
		auto syntax = tree->Root();
		for (const auto& it : syntax->Members())
		{
			if (it->Kind() == SyntaxKind::GlobalStatement)
			{
				auto globalStatement = static_cast<GlobalStatementSyntax*>(it.get());
				globalStmts.push_back(globalStatement);
				statements.push_back(binder.BindGlobalStatement(globalStatement->Statement()));
			}
		}
	}

	auto firstGlobalStmtPerSynTree = vector<const GlobalStatementSyntax*>();
	std::for_each(trees.cbegin(), trees.cend(),
		[&firstGlobalStmtPerSynTree](const auto& tree)
		{
			auto& members = tree->Root()->Members();
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
			scriptFunc = make_unique<FunctionSymbol>("$eval", vector<ParameterSymbol>(),
				TYPE_ANY, nullptr);
		}
	} else
	{
		auto it = std::find_if(functions.cbegin(), functions.cend(),
			[](const auto& it) { return it->Name() == ENTRY_NAME; });
		if (it != functions.cend())
		{
			if ((*it)->Type() != TYPE_VOID
				|| !(*it)->Parameters().empty())
				binder._diagnostics->ReportMainMustHaveCorrectSignature(
					(*it)->Declaration()->Identifier().Location()
				);
			mainFunc = make_unique<FunctionSymbol>(**it);
		}
		if (!globalStmts.empty())
		{
			if (mainFunc != nullptr)
			{
				binder._diagnostics->ReportCannotMixMainAndGlobalStatements(
					mainFunc->Declaration()->Identifier().Location()
				);
				for (const auto& gs : firstGlobalStmtPerSynTree)
				{
					binder._diagnostics->ReportCannotMixMainAndGlobalStatements(
						gs->Location());
				}
			} else
			{
				mainFunc = make_unique<FunctionSymbol>(ENTRY_NAME, vector<ParameterSymbol>(),
					TYPE_VOID, nullptr);
			}
		}
	}

	auto& diagnostics = binder.Diagnostics();
	auto variables = binder._scope->GetDeclaredVariables();
	if (previous != nullptr)
		diagnostics.AddRangeFront(previous->Diagnostics());
	return make_unique<BoundGlobalScope>(previous,
		std::move(binder._diagnostics),
		std::move(mainFunc), std::move(scriptFunc),
		std::move(functions), std::move(variables),
		std::move(statements));
}

unique_ptr<BoundProgram> BindProgram(bool isScript,
	unique_ptr<BoundProgram> preious, const BoundGlobalScope* globalScope)
{
	return Binder::BindProgram(isScript, std::move(preious), globalScope);
}

unique_ptr<BoundProgram> Binder::BindProgram(bool isScript,
	unique_ptr<BoundProgram> previous, const BoundGlobalScope* globalScope)
{
	auto parentScope = CreateParentScope(globalScope);

	auto funcBodies = BoundProgram::FuncMap();
	auto diag = make_unique<DiagnosticBag>();

	for (const auto& it : globalScope->Functions())
	{
		auto binder = Binder(isScript, std::make_unique<BoundScope>(*parentScope), it.get());
		auto body = binder.BindStatement(it->Declaration()->Body());
		auto lowerBody = Lower(*it, std::move(body));

		if (it->Type() != TYPE_VOID
			&& !ControlFlowGraph::AllPathsReturn(lowerBody.get()))
		{
			binder._diagnostics->ReportAllPathsMustReturn(
				it->Declaration()->Identifier().Location());
		}
		funcBodies.emplace(it.get(), std::move(lowerBody));

		diag->AddRange(binder.Diagnostics());
	}

	if (globalScope->MainFunc() != nullptr && !globalScope->Statements().empty())
	{
		auto body = Lower(*globalScope->MainFunc(),
			make_shared<BoundBlockStatement>(globalScope->Statements()));
		funcBodies.emplace(globalScope->MainFunc(), std::move(body));
	} else if (globalScope->ScriptFunc() != nullptr)
	{
		auto statements = globalScope->Statements();

		// adds return to one-liner expression 
		// otherwise return empty string ""
		if (statements.size() == 1
			&& statements.at(0)->Kind() == BoundNodeKind::ExpressionStatement)
		{
			auto es = static_cast<BoundExpressionStatement*>(statements.at(0).get());
			if (es->Expression()->Type() != TYPE_VOID)
			{
				statements.at(0) = make_shared<BoundReturnStatement>(es->Expression());
			}
		} else if (!statements.empty()
			&& statements.back()->Kind() != BoundNodeKind::ReturnStatement)
		{
			auto nullValue = make_shared<BoundLiteralExpression>("");
			statements.push_back(make_shared<BoundReturnStatement>(nullValue));
		}
		auto body = Lower(*globalScope->ScriptFunc(),
			make_shared<BoundBlockStatement>(std::move(statements)));
		funcBodies.emplace(globalScope->ScriptFunc(), std::move(body));
	}

	return make_unique<BoundProgram>(std::move(previous), std::move(diag),
		globalScope->MainFunc(), globalScope->ScriptFunc(),
		std::move(funcBodies));
}

}//MCF
