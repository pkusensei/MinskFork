#pragma once

#include "BoundNode.h"
#include "Symbols.h"

namespace MCF {

enum class SyntaxKind;

enum class BoundUnaryOperatorKind
{
	Identity,
	Negation,
	LogicalNegation,
	OnesComplement
};

string GetEnumText(const BoundUnaryOperatorKind& kind);

enum class BoundBinaryOperatorKind
{
	Addition,
	Subtraction,
	Multiplication,
	Division,
	Modulus,
	LogicalAnd,
	LogicalOr,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	Equals,
	NotEquals,
	Less,
	LessOrEquals,
	Greater,
	GreaterOrEquals
};

string GetEnumText(const BoundBinaryOperatorKind& kind);

enum class BoundPostfixOperatorEnum
{
	Increment,
	Decrement,
};

string GetEnumText(const BoundPostfixOperatorEnum& kind);

class BoundExpression :public BoundNode
{
public:
	// Inherited via BoundNode
	virtual TypeSymbol Type() const = 0;
	const vector<const BoundNode*> GetChildren() const override;
};

class BoundErrorExpression final :public BoundExpression
{
public:
	// Inherited via BoundExpression
	TypeSymbol Type()const override { return TypeSymbol::GetType(TypeEnum::Error); }
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ErrorExpression; }
	const vector<std::pair<string, string>> GetProperties() const override;
};

class BoundUnaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundUnaryOperatorKind _kind;
	TypeSymbol _operandType;
	TypeSymbol _resultType;
	bool _isUseful = true;

	BoundUnaryOperator(const SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
					   const TypeSymbol& operandType, const TypeSymbol& resultType);
	BoundUnaryOperator(const SyntaxKind& synKind, const BoundUnaryOperatorKind& kind,
					   const TypeSymbol& operandType);
	BoundUnaryOperator();
	static const vector<BoundUnaryOperator>& Operators();

public:
	constexpr SyntaxKind SyntaxKind()const noexcept { return _syntaxKind; }
	constexpr BoundUnaryOperatorKind Kind()const noexcept { return _kind; }
	TypeSymbol OperandType()const { return _operandType; }
	TypeSymbol Type()const { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundUnaryOperator Bind(const enum SyntaxKind& synKind, 
								   const TypeSymbol& type);
};

class BoundUnaryExpression final : public BoundExpression
{
private:
	BoundUnaryOperator _op;
	unique_ptr<BoundExpression> _operand;

public:
	BoundUnaryExpression(const BoundUnaryOperator& op, 
						 const unique_ptr<BoundExpression>& operand);
	BoundUnaryExpression(BoundUnaryExpression&&) = default;
	BoundUnaryExpression& operator=(BoundUnaryExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::UnaryExpression; }
	const vector<const BoundNode*> GetChildren() const override;
	const vector<std::pair<string, string>> GetProperties() const override;
	TypeSymbol Type() const override { return _op.Type(); }

	const BoundUnaryOperator* Op()const noexcept { return &_op; }
	const BoundExpression* Operand()const noexcept { return _operand.get(); }
};

class BoundBinaryOperator final
{
private:
	SyntaxKind _syntaxKind;
	BoundBinaryOperatorKind _kind;
	TypeSymbol _leftType;
	TypeSymbol _rightType;
	TypeSymbol _resultType;
	bool _isUseful = true;

	BoundBinaryOperator(const SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
						const TypeSymbol& left, const TypeSymbol& right, 
						const TypeSymbol& result);
	BoundBinaryOperator(const SyntaxKind& synKind, const BoundBinaryOperatorKind& kind,
						const TypeSymbol& operandType, const TypeSymbol& resultType);
	BoundBinaryOperator(const SyntaxKind& synKind, const BoundBinaryOperatorKind& kind, 
						const TypeSymbol& type);
	BoundBinaryOperator();

	static const vector<BoundBinaryOperator>& Operators();

public:
	constexpr SyntaxKind SyntaxKind()const noexcept { return _syntaxKind; }
	constexpr BoundBinaryOperatorKind Kind()const noexcept { return _kind; }
	TypeSymbol LeftType()const { return _leftType; }
	TypeSymbol RightType()const { return _rightType; }
	TypeSymbol Type()const { return _resultType; }
	constexpr bool IsUseful()const noexcept { return _isUseful; }

	static BoundBinaryOperator Bind(const enum SyntaxKind& synKind, 
									const TypeSymbol& leftType, const TypeSymbol& rightType);
};

class BoundBinaryExpression final : public BoundExpression
{
private:
	unique_ptr<BoundExpression> _left;
	unique_ptr<BoundExpression> _right;
	BoundBinaryOperator _op;

public:
	BoundBinaryExpression(const unique_ptr<BoundExpression>& left, 
						  const BoundBinaryOperator& op, 
						  const unique_ptr<BoundExpression>& right);
	BoundBinaryExpression(BoundBinaryExpression&&) = default;
	BoundBinaryExpression& operator=(BoundBinaryExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BinaryExpression; }
	const vector<const BoundNode*> GetChildren() const override;
	const vector<std::pair<string, string>> GetProperties() const override;
	TypeSymbol Type() const override { return _op.Type(); }

	const BoundExpression* Left()const noexcept { return _left.get(); }
	const BoundExpression* Right()const noexcept { return _right.get(); }
	const BoundBinaryOperator* Op()const noexcept { return &_op; }
};

class BoundAssignmentExpression final : public BoundExpression
{
private:
	VariableSymbol _variable;
	unique_ptr<BoundExpression> _expression;

public:
	BoundAssignmentExpression(const VariableSymbol& variable, 
							  const unique_ptr<BoundExpression>& expression);
	BoundAssignmentExpression(BoundAssignmentExpression&&) = default;
	BoundAssignmentExpression& operator=(BoundAssignmentExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::AssignmentExpression; }
	const vector<const BoundNode*> GetChildren() const override;
	const vector<std::pair<string, string>> GetProperties() const override;
	TypeSymbol Type() const override { return _expression->Type(); }

	VariableSymbol Variable()const { return _variable; }
	const BoundExpression* Expression()const noexcept { return _expression.get(); }
};

class BoundLiteralExpression final : public BoundExpression
{
private:
	ValueType _value;

public:
	explicit BoundLiteralExpression(const ValueType& value);
	BoundLiteralExpression(BoundLiteralExpression&&) = default;
	BoundLiteralExpression& operator=(BoundLiteralExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LiteralExpression; }
	const vector<std::pair<string, string>> GetProperties() const override;
	TypeSymbol Type() const override { return _value.Type(); }

	ValueType Value()const { return _value; }
};

class BoundVariableExpression final : public BoundExpression
{
private:
	VariableSymbol _variable;

public:
	BoundVariableExpression(const VariableSymbol& variable);
	BoundVariableExpression(BoundVariableExpression&&) = default;
	BoundVariableExpression& operator=(BoundVariableExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableExpression; }
	const vector<std::pair<string, string>> GetProperties() const override;
	TypeSymbol Type() const override { return _variable.Type(); }

	VariableSymbol Variable()const { return _variable; }
};

class BoundCallExpression final :public BoundExpression
{
private:
	FunctionSymbol _function;
	vector<unique_ptr<BoundExpression>> _arguments;

public:
	BoundCallExpression(const FunctionSymbol& function,
						const vector<unique_ptr<BoundExpression>>& arguments);
	BoundCallExpression(BoundCallExpression&&) = default;
	BoundCallExpression& operator=(BoundCallExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::CallExpression; }
	TypeSymbol Type() const override { return _function.Type(); }
	const vector<const BoundNode*> GetChildren() const override;
	const vector<std::pair<string, string>> GetProperties() const override;

	FunctionSymbol Function()const { return _function; }
	const vector<const BoundExpression*> Arguments()const;
};

class BoundConversionExpression final :public BoundExpression
{
private:
	TypeSymbol _type;
	unique_ptr<BoundExpression> _expression;

public:
	BoundConversionExpression(const TypeSymbol& type,
							  const unique_ptr<BoundExpression>& expression);
	BoundConversionExpression(BoundConversionExpression&&) = default;
	BoundConversionExpression& operator=(BoundConversionExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConversionExpression; }
	TypeSymbol Type() const override { return _type; }
	const vector<const BoundNode*> GetChildren() const override;
	const vector<std::pair<string, string>> GetProperties() const override;

	const BoundExpression* Expression()const noexcept { return _expression.get(); }
};

class BoundPostfixExpression final :public BoundExpression
{
private:
	VariableSymbol _variable;
	BoundPostfixOperatorEnum _kind;
	unique_ptr<BoundExpression> _expression;

public:
	BoundPostfixExpression(const VariableSymbol& variable, 
						   const BoundPostfixOperatorEnum& kind, 
						   const unique_ptr<BoundExpression>& expression);
	BoundPostfixExpression(BoundPostfixExpression&&) = default;
	BoundPostfixExpression& operator=(BoundPostfixExpression&&) = default;

	// Inherited via BoundExpression
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::PostfixExpression; }
	const vector<const BoundNode*> GetChildren() const override;
	const vector<std::pair<string, string>> GetProperties() const override;
	TypeSymbol Type() const override { return _variable.Type(); }

	VariableSymbol Variable()const { return _variable; }
	BoundPostfixOperatorEnum OperatorKind()const { return _kind; }
	const BoundExpression* Expression()const noexcept { return _expression.get(); }
};

}//MCF