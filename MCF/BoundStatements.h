#pragma once

#include "BoundLabel.h"
#include "BoundNode.h"
#include "Symbols.h"

namespace MCF{

class BoundExpression;

class BoundStatement :public BoundNode
{
public:
	// Inherited via BoundNode
	const vector<std::pair<string, string>> GetProperties() const override;
	const vector<const BoundNode*> GetChildren() const override;
};

class BoundBlockStatement final : public BoundStatement
{
private:
	vector<unique_ptr<BoundStatement>> _statements;

public:
	explicit BoundBlockStatement(const vector<unique_ptr<BoundStatement>>& statements);
	BoundBlockStatement(BoundBlockStatement&&) = default;
	BoundBlockStatement& operator=(BoundBlockStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BlockStatement; }
	const vector<const BoundNode*> GetChildren() const override;

	const vector<const BoundStatement*> Statements()const;
};

class BoundVariableDeclaration final :public BoundStatement
{
private:
	VariableSymbol _variable;
	unique_ptr<BoundExpression> _initializer;

public:
	BoundVariableDeclaration(const VariableSymbol& variable, 
							 const unique_ptr<BoundExpression>& initializer);
	BoundVariableDeclaration(BoundVariableDeclaration&&) = default;
	BoundVariableDeclaration& operator=(BoundVariableDeclaration&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableDeclaration; }
	const vector<std::pair<string, string>> GetProperties() const override;
	const vector<const BoundNode*> GetChildren() const override;

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
	BoundIfStatement(const unique_ptr<BoundExpression>& condition, 
					 const unique_ptr<BoundStatement>& thenStatement,
					 const unique_ptr<BoundStatement>& elseStatement);
	BoundIfStatement(BoundIfStatement&&) = default;
	BoundIfStatement& operator=(BoundIfStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::IfStatement; }
	const vector<const BoundNode*> GetChildren() const override;

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
	BoundWhileStatement(const unique_ptr<BoundExpression>& condition, 
						const unique_ptr<BoundStatement>& body);
	BoundWhileStatement(BoundWhileStatement&&) = default;
	BoundWhileStatement& operator=(BoundWhileStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::WhileStatement; }
	const vector<const BoundNode*> GetChildren() const override;

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
	BoundForStatement(const VariableSymbol& variable, 
					  const unique_ptr<BoundExpression>& lowerBound,
					  const unique_ptr<BoundExpression>& upperBound, 
					  const unique_ptr<BoundStatement>& body);
	BoundForStatement(BoundForStatement&&) = default;
	BoundForStatement& operator=(BoundForStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ForStatement; }
	const vector<std::pair<string, string>> GetProperties() const override;
	const vector<const BoundNode*> GetChildren() const override;

	VariableSymbol Variable()const { return _variable; }
	const BoundExpression* LowerBound()const noexcept { return _lowerBound.get(); }
	const BoundExpression* UpperBound()const noexcept { return _upperBound.get(); }
	const BoundStatement* Body()const noexcept { return _body.get(); }
};

class BoundLabelStatement final :public BoundStatement
{
private:
	BoundLabel _label;

public:
	explicit BoundLabelStatement(const BoundLabel& label);
	BoundLabelStatement(BoundLabelStatement&&) = default;
	BoundLabelStatement& operator=(BoundLabelStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::LabelStatement; }
	const vector<std::pair<string, string>> GetProperties() const override;

	BoundLabel Label()const { return _label; }
};

class BoundGotoStatement final :public BoundStatement
{
private:
	BoundLabel _label;

public:
	explicit BoundGotoStatement(const BoundLabel& label);
	BoundGotoStatement(BoundGotoStatement&&) = default;
	BoundGotoStatement& operator=(BoundGotoStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::GotoStatement; }
	const vector<std::pair<string, string>> GetProperties() const override;

	BoundLabel Label()const { return _label; }
};

class BoundConditionalGotoStatement final :public BoundStatement
{
private:
	BoundLabel _label;
	unique_ptr<BoundExpression> _condition;
	bool _jumpIfTrue;

public:
	BoundConditionalGotoStatement(const BoundLabel& label, 
								  const unique_ptr<BoundExpression>& condition, 
								  bool jumpIfTrue = true);
	BoundConditionalGotoStatement(BoundConditionalGotoStatement&&) = default;
	BoundConditionalGotoStatement& operator=(BoundConditionalGotoStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConditionalGotoStatement; }
	const vector<std::pair<string, string>> GetProperties() const override;
	const vector<const BoundNode*> GetChildren() const override;

	BoundLabel Label()const { return _label; }
	const BoundExpression* Condition()const { return _condition.get(); }
	bool JumpIfTrue()const noexcept { return _jumpIfTrue; }
};

class BoundExpressionStatement final : public BoundStatement
{
private:
	unique_ptr<BoundExpression> _expression;

public:
	BoundExpressionStatement(const unique_ptr<BoundExpression>& expression);
	BoundExpressionStatement(BoundExpressionStatement&&) = default;
	BoundExpressionStatement& operator=(BoundExpressionStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ExpressionStatement; }
	const vector<const BoundNode*> GetChildren() const override;

	const BoundExpression* Expression()const noexcept { return _expression.get(); }
};

}//MCF
