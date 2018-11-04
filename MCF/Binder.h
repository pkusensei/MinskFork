#pragma once

#include <unordered_map>

#include "common.h"

namespace MCF {

class DiagnosticBag;
class ExpressionSyntax;

enum class BoundNodeKind
{
	LiteralExpression,
	VariableExpression,
	AssignmentExpression,
	UnaryExpression,
	BinaryExpression,

	EmptyExpression //HACK
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
	// HACK
public:
	virtual ~BoundExpression() = default;
	// Inherited via BoundNode
	virtual BoundNodeKind Kind() const override { return BoundNodeKind::EmptyExpression; }
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
					   type_index operandType, type_index resultType);
	BoundUnaryOperator(SyntaxKind synKind, BoundUnaryOperatorKind kind,
					   type_index operandType);
	BoundUnaryOperator();
	static BoundUnaryOperator _operators[3];
public:
	BoundUnaryOperator(const BoundUnaryOperator&) = default;

	SyntaxKind SyntaxKind()const { return _syntaxKind; }
	BoundUnaryOperatorKind Kind()const { return _kind; }
	type_index OperandType()const { return _operandType; }
	type_index Type()const { return _resultType; }
	bool IsUseful()const { return _isUseful; }

	static BoundUnaryOperator Bind(enum SyntaxKind synKind, type_index type);
};

class BoundUnaryExpression final : public BoundExpression
{
private:
	unique_ptr<BoundUnaryOperator> _op;
	unique_ptr<BoundExpression> _operand;
public:
	BoundUnaryExpression(const BoundUnaryOperator& op, unique_ptr<BoundExpression>& operand);
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
						type_index left, type_index right, type_index result);
	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind,
						type_index operandType, type_index resultType);
	BoundBinaryOperator(SyntaxKind synKind, BoundBinaryOperatorKind kind, type_index type);
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
	BoundBinaryExpression(unique_ptr<BoundExpression>& left, const BoundBinaryOperator& op, unique_ptr<BoundExpression>& right);
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
	BoundAssignmentExpression(const VariableSymbol& variable, unique_ptr<BoundExpression>& expression);
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

class Binder
{
private:
	unique_ptr<DiagnosticBag> _diagnostics;
	std::unordered_map<VariableSymbol, ValueType, VariableHash>* _variables;

	unique_ptr<BoundExpression> BindParenthesizedExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindLiteralExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindNameExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindAssignmentExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindUnaryExpression(const ExpressionSyntax* syntax);
	unique_ptr<BoundExpression> BindBinaryExpression(const ExpressionSyntax* syntax);

public:
	Binder(std::unordered_map<VariableSymbol, ValueType, VariableHash>& variables);
	~Binder() = default;

	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }

	unique_ptr<BoundExpression> BindExpression(const ExpressionSyntax* syntax);
};

}//MCF

