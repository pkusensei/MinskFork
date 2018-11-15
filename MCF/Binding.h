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
	NotEquals
};

class BoundNode
{
public:
	virtual ~BoundNode() = default;
	virtual BoundNodeKind Kind() const = 0;
};

class BoundExpression :public BoundNode
{
	// NOTE
public:
	virtual ~BoundExpression() = default;
	// Inherited via BoundNode
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::VoidExpression; }
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

	BoundUnaryOperator(SyntaxKind synKind, BoundUnaryOperatorKind kind,
					   const type_index& operandType, const type_index& resultType);
	BoundUnaryOperator(SyntaxKind synKind, BoundUnaryOperatorKind kind,
					   const type_index& operandType);
	BoundUnaryOperator();
	static BoundUnaryOperator _operators[3];
public:
	BoundUnaryOperator(const BoundUnaryOperator&) = default;

	SyntaxKind SyntaxKind()const { return _syntaxKind; }
	BoundUnaryOperatorKind Kind()const { return _kind; }
	type_index OperandType()const { return _operandType; }
	type_index Type()const { return _resultType; }
	bool IsUseful()const { return _isUseful; }

	static BoundUnaryOperator Bind(enum SyntaxKind synKind, const type_index& type);
};

class BoundUnaryExpression final : public BoundExpression
{
private:
	unique_ptr<BoundUnaryOperator> _op;
	unique_ptr<BoundExpression> _operand;
public:
	BoundUnaryExpression(const BoundUnaryOperator& op, const unique_ptr<BoundExpression>& operand);
	virtual ~BoundUnaryExpression() = default;
	BoundUnaryExpression(BoundUnaryExpression&& other);

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::UnaryExpression; }
	virtual type_index Type() const override { return _op->Type(); }

	const BoundUnaryOperator* Op()const { return _op.get(); }
	const BoundExpression* Operand()const { return _operand.get(); }
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

	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
						const type_index& left, const type_index& right, const type_index& result);
	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
						const type_index& operandType, const type_index& resultType);
	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind, const type_index& type);
	BoundBinaryOperator();

	static BoundBinaryOperator _operators[10];
public:
	BoundBinaryOperator(const BoundBinaryOperator&) = default;

	SyntaxKind SyntaxKind()const { return _syntaxKind; }
	BoundBinaryOperatorKind Kind()const { return _kind; }
	type_index LeftType()const { return _leftType; }
	type_index RightType()const { return _rightType; }
	type_index Type()const { return _resultType; }
	bool IsUseful()const { return _isUseful; }

	static BoundBinaryOperator Bind(enum SyntaxKind synKind, type_index leftType, type_index rightType);
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
	BoundBinaryExpression(BoundBinaryExpression&& other);

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::BinaryExpression; }
	virtual type_index Type() const override { return _op->Type(); }

	const BoundExpression* Left()const { return _left.get(); }
	const BoundExpression* Right()const { return _right.get(); }
	const BoundBinaryOperator* Op()const { return _op.get(); }
};

class BoundAssignmentExpression final : public BoundExpression
{
private:
	VariableSymbol _variable;
	unique_ptr<BoundExpression> _expression;
public:
	BoundAssignmentExpression(const VariableSymbol& variable, const unique_ptr<BoundExpression>& expression);
	virtual ~BoundAssignmentExpression() = default;
	BoundAssignmentExpression(BoundAssignmentExpression&& other);

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::AssignmentExpression; }
	virtual type_index Type() const override { return _expression->Type(); }

	VariableSymbol Variable()const { return _variable; }
	const BoundExpression* Expression()const { return _expression.get(); }
};

class BoundLiteralExpression final : public BoundExpression
{
private:
	ValueType _value;
public:
	BoundLiteralExpression(const ValueType& value);
	virtual ~BoundLiteralExpression() = default;
	BoundLiteralExpression(BoundLiteralExpression&& other);

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::LiteralExpression; }
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
	BoundVariableExpression(BoundVariableExpression&& other);

	// Inherited via BoundExpression
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::VariableExpression; }
	virtual type_index Type() const override { return _variable.Type(); }

	VariableSymbol Variable()const { return _variable; }
};

class BoundStatement :public BoundNode
{
public:
	// Inherited via BoundNode
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::VoidExpression; }
};

class BoundBlockStatement final : public BoundStatement
{
private:
	vector<unique_ptr<BoundStatement>> _statements;
public:
	BoundBlockStatement(const vector<unique_ptr<BoundStatement>>& statements);
	virtual ~BoundBlockStatement() = default;
	BoundBlockStatement(BoundBlockStatement&& other);

	// Inherited via BoundStatement
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::BlockStatement; }
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
	BoundVariableDeclaration(BoundVariableDeclaration&& other);

	// Inherited via BoundStatement
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::VariableDeclaration; }

	VariableSymbol Variable()const { return _variable; }
	const BoundExpression* Initializer()const { return _initializer.get(); }
};

class BoundExpressionStatement final : public BoundStatement
{
private:
	unique_ptr<BoundExpression> _expression;

public:
	BoundExpressionStatement(const unique_ptr<BoundExpression>& expression);
	virtual ~BoundExpressionStatement() = default;
	BoundExpressionStatement(BoundExpressionStatement&& other);

	// Inherited via BoundStatement
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::ExpressionStatement; }

	const BoundExpression* Expression()const { return _expression.get(); }
};

class BoundScope final
{
private:
	std::unordered_map<string, VariableSymbol> _variables;
	std::shared_ptr<BoundScope> _parent;
public:
	explicit BoundScope(const std::weak_ptr<BoundScope>& parent);
	explicit BoundScope(const std::shared_ptr<BoundScope>& parent);

	std::weak_ptr<BoundScope> Parent()const { return _parent; }
	bool TryDeclare(const VariableSymbol& variable);
	bool TryLookup(const string& name, VariableSymbol& variable)const;
	const vector<VariableSymbol> GetDeclaredVariables()const;
};

class BoundGlobalScope final
{
private:
	std::shared_ptr<BoundGlobalScope> _previous;
	unique_ptr<DiagnosticBag> _diagnostics;
	vector<VariableSymbol> _variables;
	unique_ptr<BoundStatement> _statement;
public:
	BoundGlobalScope(const std::shared_ptr<BoundGlobalScope>& previous, const unique_ptr<DiagnosticBag>& diagnostics,
					 const vector<VariableSymbol>& variables, const unique_ptr<BoundStatement>& statement);
	BoundGlobalScope(const std::weak_ptr<BoundGlobalScope>& previous, unique_ptr<DiagnosticBag>& diagnostics,
					 const vector<VariableSymbol>& variables, const unique_ptr<BoundStatement>& statement);
	BoundGlobalScope(BoundGlobalScope&& other);

	std::weak_ptr<BoundGlobalScope> Previous()const { return _previous; }
	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
	const vector<VariableSymbol> Variables()const { return _variables; }
	const BoundStatement* Statement()const { return _statement.get(); }
};

class Binder final
{
private:
	unique_ptr<DiagnosticBag> _diagnostics;
	std::shared_ptr<BoundScope> _scope;

	unique_ptr<BoundStatement> BindStatement(const StatementSyntax* syntax);
	unique_ptr<BoundStatement> BindBlockStatement(const StatementSyntax* syntax);
	unique_ptr<BoundStatement> BindVariableDeclaration(const StatementSyntax* syntax);
	unique_ptr<BoundStatement> BindExpressionStatement(const StatementSyntax* syntax);

	unique_ptr<BoundExpression> BindExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindParenthesizedExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindLiteralExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindNameExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindAssignmentExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindUnaryExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindBinaryExpression(const ExpressionSyntax* syntax);

	static std::shared_ptr<BoundScope> CreateParentScope(const std::shared_ptr<BoundGlobalScope>& previous);
public:
	explicit Binder(const std::shared_ptr<BoundScope>& parent);
	~Binder() = default;

	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }

	static std::shared_ptr<BoundGlobalScope> BindGlobalScope(const std::weak_ptr<BoundGlobalScope>& previous, const CompilationUnitSyntax* syntax);
};

}//MCF

