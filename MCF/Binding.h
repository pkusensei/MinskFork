#pragma once

#include <unordered_map>

#include "common.h"

namespace MCF {

class DiagnosticBag;
class ExpressionSyntax;
class StatementSyntax;
class CompilationUnitSyntax;

enum class BoundNodeKind
{
	// Statements
	BlockStatement,
	VariableDeclaration,
	IfStatement,
	WhileStatement,
	ForStatement,
	ExpressionStatement,

	// Expressions
	LiteralExpression,
	VariableExpression,
	AssignmentExpression,
	UnaryExpression,
	BinaryExpression,

	VoidExpression //HACK
};

enum class BoundUnaryOperatorKind
{
	Identity,
	Negation,
	LogicalNegation
};

enum class BoundBinaryOperatorKind
{
	Addition,
	Subtraction,
	Multiplication,
	Division,
	LogicalAnd,
	LogicalOr,
	Equals,
	NotEquals,
	Less,
	LessOrEquals,
	Greater,
	GreaterOrEquals
};

class BoundNode
{
public:
	virtual ~BoundNode() = default;
	virtual BoundNodeKind Kind() const = 0;
};

#pragma region Expression

class BoundExpression :public BoundNode
{
	// NOTE concrete type in unique_ptr
public:
	virtual ~BoundExpression() = default;
	// Inherited via BoundNode
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VoidExpression; }
	virtual type_index Type() const { return typeid(std::monostate); }
};

class BoundUnaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundUnaryOperatorKind _kind;
	type_index _operandType;
	type_index _resultType;
	bool _isUseful = true;

	BoundUnaryOperator(const SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
					   const type_index& operandType, const type_index& resultType);
	BoundUnaryOperator(const SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
					   const type_index& operandType);
	BoundUnaryOperator();
	static BoundUnaryOperator _operators[3];
public:
	BoundUnaryOperator(const BoundUnaryOperator&) = default;

	constexpr SyntaxKind SyntaxKind()const noexcept { return _syntaxKind; }
	constexpr BoundUnaryOperatorKind Kind()const noexcept { return _kind; }
	type_index OperandType()const { return _operandType; }
	type_index Type()const { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundUnaryOperator Bind(const enum SyntaxKind& synKind, const type_index& type);
};

class BoundUnaryExpression final : public BoundExpression
{
private:
	unique_ptr<BoundUnaryOperator> _op;
	unique_ptr<BoundExpression> _operand;
public:
	BoundUnaryExpression(const BoundUnaryOperator& op, const unique_ptr<BoundExpression>& operand);
	virtual ~BoundUnaryExpression() = default;
	BoundUnaryExpression(BoundUnaryExpression&& other)noexcept;

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::UnaryExpression; }
	virtual type_index Type() const override { return _op->Type(); }

	const BoundUnaryOperator* Op()const noexcept { return _op.get(); }
	const BoundExpression* Operand()const noexcept { return _operand.get(); }
};

class BoundBinaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundBinaryOperatorKind _kind;
	type_index _leftType;
	type_index _rightType;
	type_index _resultType;
	bool _isUseful = true;

	BoundBinaryOperator(const SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
						const type_index& left, const type_index& right, const type_index& result);
	BoundBinaryOperator(const SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
						const type_index& operandType, const type_index& resultType);
	BoundBinaryOperator(const SyntaxKind& synKind, const BoundBinaryOperatorKind& kind, const type_index& type);
	BoundBinaryOperator();

	static BoundBinaryOperator _operators[14];
public:
	BoundBinaryOperator(const BoundBinaryOperator&) = default;

	constexpr SyntaxKind SyntaxKind()const noexcept { return _syntaxKind; }
	constexpr BoundBinaryOperatorKind Kind()const noexcept { return _kind; }
	type_index LeftType()const { return _leftType; }
	type_index RightType()const { return _rightType; }
	type_index Type()const { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundBinaryOperator Bind(const enum SyntaxKind& synKind, const type_index& leftType, const type_index& rightType);
};

class BoundBinaryExpression final : public BoundExpression
{
private:
	unique_ptr<BoundExpression> _left;
	unique_ptr<BoundExpression> _right;
	unique_ptr<BoundBinaryOperator> _op;

public:
	BoundBinaryExpression(const unique_ptr<BoundExpression>& left, const BoundBinaryOperator& op, const unique_ptr<BoundExpression>& right);
	virtual ~BoundBinaryExpression() = default;
	BoundBinaryExpression(BoundBinaryExpression&& other)noexcept;

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BinaryExpression; }
	virtual type_index Type() const override { return _op->Type(); }

	const BoundExpression* Left()const noexcept { return _left.get(); }
	const BoundExpression* Right()const noexcept { return _right.get(); }
	const BoundBinaryOperator* Op()const noexcept { return _op.get(); }
};

class BoundAssignmentExpression final : public BoundExpression
{
private:
	VariableSymbol _variable;
	unique_ptr<BoundExpression> _expression;
public:
	BoundAssignmentExpression(const VariableSymbol& variable, const unique_ptr<BoundExpression>& expression);
	virtual ~BoundAssignmentExpression() = default;
	BoundAssignmentExpression(BoundAssignmentExpression&& other)noexcept;

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::AssignmentExpression; }
	virtual type_index Type() const override { return _expression->Type(); }

	VariableSymbol Variable()const { return _variable; }
	const BoundExpression* Expression()const noexcept { return _expression.get(); }
};

class BoundLiteralExpression final : public BoundExpression
{
private:
	ValueType _value;
public:
	BoundLiteralExpression(const ValueType& value);
	virtual ~BoundLiteralExpression() = default;
	BoundLiteralExpression(BoundLiteralExpression&& other)noexcept;

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LiteralExpression; }
	virtual type_index Type() const override { return _value.Type(); }

	ValueType Value()const { return _value; }

};

class BoundVariableExpression final : public BoundExpression
{
private:
	VariableSymbol _variable;
public:
	BoundVariableExpression(const VariableSymbol& variable);
	virtual ~BoundVariableExpression() = default;
	BoundVariableExpression(BoundVariableExpression&& other)noexcept;

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableExpression; }
	virtual type_index Type() const override { return _variable.Type(); }

	VariableSymbol Variable()const { return _variable; }
};

#pragma endregion

#pragma region Statement

class BoundStatement :public BoundNode
{
public:
	// Inherited via BoundNode
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VoidExpression; }
};

class BoundBlockStatement final : public BoundStatement
{
private:
	vector<unique_ptr<BoundStatement>> _statements;
public:
	BoundBlockStatement(const vector<unique_ptr<BoundStatement>>& statements);
	virtual ~BoundBlockStatement() = default;
	BoundBlockStatement(BoundBlockStatement&& other)noexcept;

	// Inherited via BoundStatement
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BlockStatement; }
	const vector<BoundStatement*> Statements()const;
};

class BoundVariableDeclaration final :public BoundStatement
{
private:
	VariableSymbol _variable;
	unique_ptr<BoundExpression> _initializer;
public:
	BoundVariableDeclaration(const VariableSymbol& variable, const unique_ptr<BoundExpression>& initializer);
	virtual ~BoundVariableDeclaration() = default;
	BoundVariableDeclaration(BoundVariableDeclaration&& other)noexcept;

	// Inherited via BoundStatement
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableDeclaration; }

	VariableSymbol Variable()const { return _variable; }
	const BoundExpression* Initializer()const noexcept { return _initializer.get(); }
};

class BoundIfStatement final :public BoundStatement
{
private:
	unique_ptr<BoundExpression> _condition;
	unique_ptr<BoundStatement> _thenStatement;
	unique_ptr<BoundStatement> _elseStatement;
public:
	BoundIfStatement(const unique_ptr<BoundExpression>& condition, const unique_ptr<BoundStatement>& thenStatement,
					 const unique_ptr<BoundStatement>& elseStatement);
	virtual ~BoundIfStatement() = default;
	BoundIfStatement(BoundIfStatement&&) = default;

	// Inherited via BoundStatement
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::IfStatement; }

	const BoundExpression* Condition()const noexcept { return _condition.get(); }
	const BoundStatement* ThenStatement()const noexcept { return _thenStatement.get(); }
	const BoundStatement* ElseStatement()const noexcept { return _elseStatement.get(); }
};

class BoundWhileStatement final :public BoundStatement
{
private:
	unique_ptr<BoundExpression> _condition;
	unique_ptr<BoundStatement> _body;
public:
	BoundWhileStatement(const unique_ptr<BoundExpression>& condition, const unique_ptr<BoundStatement>& body);
	virtual ~BoundWhileStatement() = default;
	BoundWhileStatement(BoundWhileStatement&&) = default;

	// Inherited via BoundStatement
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::WhileStatement; }

	const BoundExpression* Condition()const noexcept { return _condition.get(); }
	const BoundStatement* Body()const noexcept { return _body.get(); }
};

class BoundForStatement final :public BoundStatement
{
private:
	VariableSymbol _variable;
	unique_ptr<BoundExpression> _lowerBound;
	unique_ptr<BoundExpression> _upperBound;
	unique_ptr<BoundStatement> _body;
public:
	BoundForStatement(const VariableSymbol& variable, const unique_ptr<BoundExpression>& lowerBound,
					  const unique_ptr<BoundExpression>& upperBound, const unique_ptr<BoundStatement>& body);
	virtual ~BoundForStatement() = default;
	BoundForStatement(BoundForStatement&&) = default;

	// Inherited via BoundStatement
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ForStatement; }

	VariableSymbol Variable()const { return _variable; }
	const BoundExpression* LowerBound()const noexcept { return _lowerBound.get(); }
	const BoundExpression* UpperBound()const noexcept { return _upperBound.get(); }
	const BoundStatement* Body()const noexcept { return _body.get(); }
};

class BoundExpressionStatement final : public BoundStatement
{
private:
	unique_ptr<BoundExpression> _expression;

public:
	BoundExpressionStatement(const unique_ptr<BoundExpression>& expression);
	virtual ~BoundExpressionStatement() = default;
	BoundExpressionStatement(BoundExpressionStatement&& other)noexcept;

	// Inherited via BoundStatement
	virtual BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ExpressionStatement; }

	const BoundExpression* Expression()const noexcept { return _expression.get(); }
};

#pragma endregion

class BoundScope final
{
private:
	std::unordered_map<string, VariableSymbol> _variables;
	unique_ptr<BoundScope> _parent;
public:
	explicit BoundScope(const unique_ptr<BoundScope>& parent);

	const BoundScope* Parent()const noexcept { return _parent.get(); }
	bool TryDeclare(const VariableSymbol& variable);
	bool TryLookup(const string& name, VariableSymbol& variable)const;
	const vector<VariableSymbol> GetDeclaredVariables()const;
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
	BoundGlobalScope(BoundGlobalScope&& other)noexcept;

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
	unique_ptr<BoundStatement> BindBlockStatement(const StatementSyntax* syntax);
	unique_ptr<BoundStatement> BindVariableDeclaration(const StatementSyntax* syntax);
	unique_ptr<BoundStatement> BindIfStatement(const StatementSyntax* syntax);
	unique_ptr<BoundStatement> BindWhileStatement(const StatementSyntax* syntax);
	unique_ptr<BoundStatement> BindForStatement(const StatementSyntax* syntax);
	unique_ptr<BoundStatement> BindExpressionStatement(const StatementSyntax* syntax);

	unique_ptr<BoundExpression> BindExpression(const ExpressionSyntax* syntax, const type_index& targetType);
	unique_ptr<BoundExpression> BindExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindParenthesizedExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindLiteralExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindNameExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindAssignmentExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindUnaryExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindBinaryExpression(const ExpressionSyntax* syntax);

	static unique_ptr<BoundScope> CreateParentScope(const BoundGlobalScope* previous);
public:
	explicit Binder(const unique_ptr<BoundScope>& parent);
	~Binder() = default;

	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }

	static unique_ptr<BoundGlobalScope> BindGlobalScope(const BoundGlobalScope* previous, const CompilationUnitSyntax* syntax);
};

}//MCF

