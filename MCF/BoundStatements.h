#pragma once

#include "BoundLabel.h"
#include "BoundNode.h"
#include "Symbols.h"

namespace MCF {

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
	vector<shared_ptr<BoundStatement>> _statements;

public:
	explicit BoundBlockStatement(const vector<shared_ptr<BoundStatement>>& statements);
	BoundBlockStatement(BoundBlockStatement&&) = default;
	BoundBlockStatement& operator=(BoundBlockStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::BlockStatement; }
	const vector<const BoundNode*> GetChildren() const override;

	const vector<shared_ptr<BoundStatement>>& Statements()const noexcept { return _statements; }
};

class BoundVariableDeclaration final :public BoundStatement
{
private:
	shared_ptr<VariableSymbol> _variable;
	shared_ptr<BoundExpression> _initializer;

public:
	BoundVariableDeclaration(const shared_ptr<VariableSymbol>& variable,
							 const shared_ptr<BoundExpression>& initializer);
	BoundVariableDeclaration(BoundVariableDeclaration&&) = default;
	BoundVariableDeclaration& operator=(BoundVariableDeclaration&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::VariableDeclaration; }
	const vector<std::pair<string, string>> GetProperties() const override;
	const vector<const BoundNode*> GetChildren() const override;

	const shared_ptr<VariableSymbol>& Variable()const { return _variable; }
	const shared_ptr<BoundExpression>& Initializer()const noexcept { return _initializer; }
};

class BoundIfStatement final :public BoundStatement
{
private:
	shared_ptr<BoundExpression> _condition;
	shared_ptr<BoundStatement> _thenStatement;
	shared_ptr<BoundStatement> _elseStatement;

public:
	BoundIfStatement(const shared_ptr<BoundExpression>& condition,
					 const shared_ptr<BoundStatement>& thenStatement,
					 const shared_ptr<BoundStatement>& elseStatement);
	BoundIfStatement(BoundIfStatement&&) = default;
	BoundIfStatement& operator=(BoundIfStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::IfStatement; }
	const vector<const BoundNode*> GetChildren() const override;

	const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
	const shared_ptr<BoundStatement>& ThenStatement()const noexcept { return _thenStatement; }
	const shared_ptr<BoundStatement>& ElseStatement()const noexcept { return _elseStatement; }
};

class BoundWhileStatement final :public BoundStatement
{
private:
	shared_ptr<BoundExpression> _condition;
	shared_ptr<BoundStatement> _body;

public:
	BoundWhileStatement(const shared_ptr<BoundExpression>& condition,
						const shared_ptr<BoundStatement>& body);
	BoundWhileStatement(BoundWhileStatement&&) = default;
	BoundWhileStatement& operator=(BoundWhileStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::WhileStatement; }
	const vector<const BoundNode*> GetChildren() const override;

	const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
	const shared_ptr<BoundStatement>& Body()const noexcept { return _body; }
};

class BoundDoWhileStatement final :public BoundStatement
{
private:
	shared_ptr<BoundStatement> _body;
	shared_ptr<BoundExpression> _condition;

public:
	BoundDoWhileStatement(const shared_ptr<BoundStatement>& body,
						  const shared_ptr<BoundExpression>& condition);
	BoundDoWhileStatement(BoundDoWhileStatement&&) = default;
	BoundDoWhileStatement& operator=(BoundDoWhileStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::DoWhileStatement; }
	const vector<const BoundNode*> GetChildren() const override;

	const shared_ptr<BoundStatement>& Body()const noexcept { return _body; }
	const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }
};

class BoundForStatement final :public BoundStatement
{
private:
	shared_ptr<VariableSymbol> _variable;
	shared_ptr<BoundExpression> _lowerBound;
	shared_ptr<BoundExpression> _upperBound;
	shared_ptr<BoundStatement> _body;

public:
	BoundForStatement(const shared_ptr<VariableSymbol>& variable,
					  const shared_ptr<BoundExpression>& lowerBound,
					  const shared_ptr<BoundExpression>& upperBound,
					  const shared_ptr<BoundStatement>& body);
	BoundForStatement(BoundForStatement&&) = default;
	BoundForStatement& operator=(BoundForStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ForStatement; }
	const vector<std::pair<string, string>> GetProperties() const override;
	const vector<const BoundNode*> GetChildren() const override;

	const shared_ptr<VariableSymbol>& Variable()const { return _variable; }
	const shared_ptr<BoundExpression>& LowerBound()const noexcept { return _lowerBound; }
	const shared_ptr<BoundExpression>& UpperBound()const noexcept { return _upperBound; }
	const shared_ptr<BoundStatement>& Body()const noexcept { return _body; }
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
	shared_ptr<BoundExpression> _condition;
	bool _jumpIfTrue;

public:
	BoundConditionalGotoStatement(const BoundLabel& label,
								  const shared_ptr<BoundExpression>& condition,
								  bool jumpIfTrue = true);
	BoundConditionalGotoStatement(BoundConditionalGotoStatement&&) = default;
	BoundConditionalGotoStatement& operator=(BoundConditionalGotoStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ConditionalGotoStatement; }
	const vector<std::pair<string, string>> GetProperties() const override;
	const vector<const BoundNode*> GetChildren() const override;

	BoundLabel Label()const { return _label; }
	const shared_ptr<BoundExpression>& Condition()const { return _condition; }
	bool JumpIfTrue()const noexcept { return _jumpIfTrue; }
};

class BoundExpressionStatement final : public BoundStatement
{
private:
	shared_ptr<BoundExpression> _expression;

public:
	BoundExpressionStatement(const shared_ptr<BoundExpression>& expression);
	BoundExpressionStatement(BoundExpressionStatement&&) = default;
	BoundExpressionStatement& operator=(BoundExpressionStatement&&) = default;

	// Inherited via BoundStatement
	BoundNodeKind Kind() const noexcept override { return BoundNodeKind::ExpressionStatement; }
	const vector<const BoundNode*> GetChildren() const override;

	const shared_ptr<BoundExpression>& Expression()const noexcept { return _expression; }
};

}//MCF
