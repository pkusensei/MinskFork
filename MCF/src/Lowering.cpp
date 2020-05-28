#include "Lowering.h"

#include <algorithm>
#include <cassert>
#include <stack>
#include <unordered_set>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "ControlFlowGraph.h"
#include "StringHelper.h"
#include "SyntaxKind.h"

namespace MCF {

namespace {

shared_ptr<BoundBlockStatement> Block(const SyntaxNode* syntax,
									  vector<shared_ptr<BoundStatement>> statements)
{
	return make_shared<BoundBlockStatement>(syntax, std::move(statements));
}

shared_ptr<BoundVariableDeclaration> VariableDeclaration(const SyntaxNode* syntax,
														 shared_ptr<VariableSymbol> symbol,
														 shared_ptr<BoundExpression> initializer)
{
	return make_shared<BoundVariableDeclaration>(syntax, std::move(symbol), std::move(initializer));
}

shared_ptr<BoundVariableDeclaration> VariableDeclarationInternal(const SyntaxNode* syntax,
																 string name,
																 shared_ptr<BoundExpression> initializer,
																 bool isReadOnly)
{
	auto local = make_shared<LocalVariableSymbol>(name, isReadOnly,
												  initializer->Type(),
												  initializer->ConstantValue());
	return make_shared<BoundVariableDeclaration>(syntax, std::move(local), std::move(initializer));
}

[[maybe_unused]] shared_ptr<BoundVariableDeclaration> VariableDeclaration(const SyntaxNode* syntax, string name,
																		  shared_ptr<BoundExpression> initializer)
{
	return VariableDeclarationInternal(syntax, std::move(name), std::move(initializer), false);
}

shared_ptr<BoundVariableDeclaration> ConstantDeclaration(const SyntaxNode* syntax,
														 string name,
														 shared_ptr<BoundExpression> initializer)
{
	return VariableDeclarationInternal(syntax, std::move(name), std::move(initializer), true);
}

shared_ptr<BoundWhileStatement> While(const SyntaxNode* syntax, shared_ptr<BoundExpression> condition,
									  shared_ptr<BoundStatement> body, BoundLabel breakLabel, BoundLabel continueLabel)
{
	return make_shared<BoundWhileStatement>(syntax, std::move(condition), std::move(body),
											std::move(breakLabel), std::move(continueLabel));
}

shared_ptr<BoundGotoStatement> Goto(const SyntaxNode* syntax, BoundLabel label)
{
	return make_shared<BoundGotoStatement>(syntax, std::move(label));
}

shared_ptr<BoundConditionalGotoStatement> GotoTrue(const SyntaxNode* syntax,
												   BoundLabel label,
												   shared_ptr<BoundExpression> condition)
{
	return make_shared<BoundConditionalGotoStatement>(syntax, std::move(label),
													  std::move(condition), true);
}

shared_ptr<BoundConditionalGotoStatement> GotoFalse(const SyntaxNode* syntax,
													BoundLabel label,
													shared_ptr<BoundExpression> condition)
{
	return make_shared<BoundConditionalGotoStatement>(syntax, std::move(label),
													  std::move(condition), false);
}

shared_ptr<BoundLabelStatement> Label(const SyntaxNode* syntax, BoundLabel label)
{
	return make_shared<BoundLabelStatement>(syntax, std::move(label));
}

shared_ptr<BoundNopStatement> Nop(const SyntaxNode* syntax)
{
	return make_shared<BoundNopStatement>(syntax);
}

shared_ptr<BoundBinaryExpression> Binary(const SyntaxNode* syntax,
										 shared_ptr<BoundExpression> left,
										 SyntaxKind kind,
										 shared_ptr<BoundExpression> right)
{
	auto op = BoundBinaryOperator::Bind(kind, left->Type(), right->Type());
	return make_shared<BoundBinaryExpression>(syntax, std::move(left),
											  std::move(op), std::move(right));
}

shared_ptr<BoundBinaryExpression> Add(const SyntaxNode* syntax,
									  shared_ptr<BoundExpression> left,
									  shared_ptr<BoundExpression> right)
{
	return Binary(syntax, std::move(left), SyntaxKind::PlusToken, std::move(right));
}

shared_ptr<BoundBinaryExpression> LessOrEqual(const SyntaxNode* syntax,
											  shared_ptr<BoundExpression> left,
											  shared_ptr<BoundExpression> right)
{
	return Binary(syntax, std::move(left), SyntaxKind::LessOrEqualsToken, std::move(right));
}

shared_ptr<BoundLiteralExpression> Literal(const SyntaxNode* syntax, ValueType literal)
{
	assert(literal.Type() == TYPE_BOOL
		   || literal.Type() == TYPE_INT
		   || literal.Type() == TYPE_STRING);

	return make_shared<BoundLiteralExpression>(syntax, std::move(literal));
}

shared_ptr<BoundExpressionStatement> Increment(const SyntaxNode* syntax,
											   shared_ptr<BoundVariableExpression> variable)
{
	auto increment = Add(syntax, variable, Literal(syntax, 1));
	auto incrementAssign = make_shared<BoundAssignmentExpression>(syntax,
																  variable->Variable(),
																  std::move(increment));
	return make_shared<BoundExpressionStatement>(syntax, std::move(incrementAssign));
}

[[maybe_unused]] shared_ptr<BoundUnaryExpression> Not(const SyntaxNode* syntax,
													  shared_ptr<BoundExpression> condition)
{
	assert(condition->Type() == TYPE_BOOL);
	auto op = BoundUnaryOperator::Bind(SyntaxKind::BangToken, TYPE_BOOL);
	assert(op.IsUseful());
	return make_shared<BoundUnaryExpression>(syntax, std::move(op), std::move(condition));
}

shared_ptr<BoundVariableExpression> Variable(const SyntaxNode* syntax,
											 const shared_ptr<BoundVariableDeclaration>& variable)
{
	return make_shared<BoundVariableExpression>(syntax, variable->Variable());
}

} // namespace

class BoundTreeRewriter
{
protected:
	virtual shared_ptr<BoundStatement> RewriteBlockStatement(shared_ptr<BoundBlockStatement> node);
	virtual shared_ptr<BoundStatement> RewriteNopStatement(shared_ptr<BoundNopStatement> node);
	virtual shared_ptr<BoundStatement> RewriteVariableDeclaration(shared_ptr<BoundVariableDeclaration> node);
	virtual shared_ptr<BoundStatement> RewriteIfStatement(shared_ptr<BoundIfStatement> node);
	virtual shared_ptr<BoundStatement> RewriteWhileStatement(shared_ptr<BoundWhileStatement> node);
	virtual shared_ptr<BoundStatement> RewriteDoWhileStatement(shared_ptr<BoundDoWhileStatement> node);
	virtual shared_ptr<BoundStatement> RewriteForStatement(shared_ptr<BoundForStatement> node);
	virtual shared_ptr<BoundStatement> RewriteLabelStatement(shared_ptr<BoundLabelStatement> node);
	virtual shared_ptr<BoundStatement> RewriteGotoStatement(shared_ptr<BoundGotoStatement> node);
	virtual shared_ptr<BoundStatement> RewriteConditionalGotoStatement(shared_ptr<BoundConditionalGotoStatement> node);
	virtual shared_ptr<BoundStatement> RewriteReturnStatement(shared_ptr<BoundReturnStatement> node);
	virtual shared_ptr<BoundStatement> RewriteExpressionStatement(shared_ptr<BoundExpressionStatement> node);

	virtual shared_ptr<BoundExpression> RewriteErrorExpression(shared_ptr<BoundErrorExpression> node);
	virtual shared_ptr<BoundExpression> RewriteLiteralExpression(shared_ptr<BoundLiteralExpression> node);
	virtual shared_ptr<BoundExpression> RewriteVariableExpression(shared_ptr<BoundVariableExpression> node);
	virtual shared_ptr<BoundExpression> RewriteAssignmentExpression(shared_ptr<BoundAssignmentExpression> node);
	virtual shared_ptr<BoundExpression> RewriteUnaryExpression(shared_ptr<BoundUnaryExpression> node);
	virtual shared_ptr<BoundExpression> RewriteBinaryExpression(shared_ptr<BoundBinaryExpression> node);
	virtual shared_ptr<BoundExpression> RewriteCallExpression(shared_ptr<BoundCallExpression> node);
	virtual shared_ptr<BoundExpression> RewriteConversionExpression(shared_ptr<BoundConversionExpression> node);
	virtual shared_ptr<BoundExpression> RewritePostfixExpression(shared_ptr<BoundPostfixExpression> node);

public:
	virtual ~BoundTreeRewriter() = default;

	virtual shared_ptr<BoundStatement> RewriteStatement(shared_ptr<BoundStatement> node);
	virtual shared_ptr<BoundExpression> RewriteExpression(shared_ptr<BoundExpression> node);
};

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteStatement(shared_ptr<BoundStatement> node)
{
#define REWRITE_STMT(kind) \
case BoundNodeKind::kind:                                  \
{                                                          \
	auto p = std::dynamic_pointer_cast<Bound##kind>(node); \
	if (p) return Rewrite##kind(std::move(p));             \
	else break;                                            \
}

	switch (node->Kind())
	{
		case BoundNodeKind::NopStatement:
		case BoundNodeKind::LabelStatement:
		case BoundNodeKind::GotoStatement:
			return node;

			REWRITE_STMT(BlockStatement);
			REWRITE_STMT(VariableDeclaration);
			REWRITE_STMT(IfStatement);
			REWRITE_STMT(WhileStatement);
			REWRITE_STMT(DoWhileStatement);
			REWRITE_STMT(ForStatement);
			REWRITE_STMT(ConditionalGotoStatement);
			REWRITE_STMT(ReturnStatement);
			REWRITE_STMT(ExpressionStatement);

		default:
			break;
	}
	throw std::invalid_argument(BuildStringFrom("Unexpected node: ", nameof(node->Kind())));

#undef REWRITE_STMT
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteBlockStatement(shared_ptr<BoundBlockStatement> node)
{
	auto result = vector<shared_ptr<BoundStatement>>();
	auto& statements = node->Statements();
	for (size_t i = 0; i < statements.size(); ++i)
	{
		auto& oldStmt = statements.at(i);
		auto newStmt = RewriteStatement(oldStmt);
		if (newStmt == oldStmt)
		{
			if (!result.empty())
				result.push_back(std::move(newStmt));
		} else
		{
			if (result.empty())
				for (size_t j = 0; j < i; ++j)
					result.push_back(statements.at(j));
			result.push_back(std::move(newStmt));
		}
	}
	if (result.empty())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundBlockStatement>(s, std::move(result));
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteNopStatement(shared_ptr<BoundNopStatement> node)
{
	return node;
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteVariableDeclaration(shared_ptr<BoundVariableDeclaration> node)
{
	auto initializer = RewriteExpression(node->Initializer());
	if (initializer == node->Initializer())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundVariableDeclaration>(s, node->Variable(), std::move(initializer));
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteIfStatement(shared_ptr<BoundIfStatement> node)
{
	auto condition = RewriteExpression(node->Condition());
	auto thenStatement = RewriteStatement(node->ThenStatement());
	auto elseStatement =
		node->ElseStatement() == nullptr ?
		nullptr : RewriteStatement(node->ElseStatement());
	if (condition == node->Condition()
		&& thenStatement == node->ThenStatement()
		&& elseStatement == node->ElseStatement())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundIfStatement>(s, std::move(condition),
										 std::move(thenStatement), std::move(elseStatement));
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteWhileStatement(shared_ptr<BoundWhileStatement> node)
{
	auto condition = RewriteExpression(node->Condition());
	auto body = RewriteStatement(node->Body());
	if (condition == node->Condition() && body == node->Body())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundWhileStatement>(s, std::move(condition), std::move(body),
											node->BreakLabel(), node->ContinueLabel());
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteDoWhileStatement(shared_ptr<BoundDoWhileStatement> node)
{
	auto body = RewriteStatement(node->Body());
	auto condition = RewriteExpression(node->Condition());
	if (body == node->Body() && condition == node->Condition())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundDoWhileStatement>(s, std::move(body), std::move(condition),
											  node->BreakLabel(), node->ContinueLabel());
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteForStatement(shared_ptr<BoundForStatement> node)
{
	auto lowerBound = RewriteExpression(node->LowerBound());
	auto upperBound = RewriteExpression(node->UpperBound());
	auto body = RewriteStatement(node->Body());
	if (lowerBound == node->LowerBound()
		&& upperBound == node->UpperBound()
		&& body == node->Body())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundForStatement>(s, node->Variable(), std::move(lowerBound),
										  std::move(upperBound), body, node->BreakLabel(), node->ContinueLabel());
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteLabelStatement(shared_ptr<BoundLabelStatement> node)
{
	return node;
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteGotoStatement(shared_ptr<BoundGotoStatement> node)
{
	return node;
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteConditionalGotoStatement(shared_ptr<BoundConditionalGotoStatement> node)
{
	auto condition = RewriteExpression(node->Condition());
	if (condition == node->Condition())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundConditionalGotoStatement>(s, node->Label(),
													  std::move(condition), node->JumpIfTrue());
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteReturnStatement(shared_ptr<BoundReturnStatement> node)
{
	auto expression = node->Expression() == nullptr ?
		nullptr : RewriteExpression(node->Expression());

	if (expression == node->Expression())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundReturnStatement>(s, std::move(expression));
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteExpressionStatement(shared_ptr<BoundExpressionStatement> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundExpressionStatement>(s, std::move(expression));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteExpression(shared_ptr<BoundExpression> node)
{
#define REWRITE_EXPR(kind) \
case BoundNodeKind::kind:                                  \
{                                                          \
	auto p = std::dynamic_pointer_cast<Bound##kind>(node); \
	if (p) return Rewrite##kind(std::move(p));             \
	else break;                                            \
}

	switch (node->Kind())
	{
		case BoundNodeKind::ErrorExpression:
		case BoundNodeKind::LiteralExpression:
		case BoundNodeKind::VariableExpression:
			return node;

			REWRITE_EXPR(AssignmentExpression);
			REWRITE_EXPR(UnaryExpression);
			REWRITE_EXPR(BinaryExpression);
			REWRITE_EXPR(CallExpression);
			REWRITE_EXPR(ConversionExpression);
			REWRITE_EXPR(PostfixExpression);

		default:
			break;
	}
	throw std::invalid_argument(BuildStringFrom("Unexpected node: ", nameof(node->Kind())));

#undef REWRITE_EXPR
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteErrorExpression(shared_ptr<BoundErrorExpression> node)
{
	return node;
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteLiteralExpression(shared_ptr<BoundLiteralExpression> node)
{
	return node;
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteVariableExpression(shared_ptr<BoundVariableExpression> node)
{
	return node;
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteAssignmentExpression(shared_ptr<BoundAssignmentExpression> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundAssignmentExpression>(s, node->Variable(), std::move(expression));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteUnaryExpression(shared_ptr<BoundUnaryExpression> node)
{
	auto operand = RewriteExpression(node->Operand());
	if (operand == node->Operand())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundUnaryExpression>(s, node->Op(), std::move(operand));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteBinaryExpression(shared_ptr<BoundBinaryExpression> node)
{
	auto left = RewriteExpression(node->Left());
	auto right = RewriteExpression(node->Right());
	if (left == node->Left() && right == node->Right())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundBinaryExpression>(s, std::move(left), node->Op(), std::move(right));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteCallExpression(shared_ptr<BoundCallExpression> node)
{
	auto result = vector<shared_ptr<BoundExpression>>();
	for (size_t i = 0; i < node->Arguments().size(); ++i)
	{
		auto& oldArg = node->Arguments().at(i);
		auto newArg = RewriteExpression(oldArg);
		if (newArg == oldArg)
		{
			if (!result.empty())
				result.push_back(std::move(newArg));
		} else
		{
			if (result.empty())
				for (size_t j = 0; j < i; ++j)
					result.push_back(node->Arguments().at(j));
			result.push_back(std::move(newArg));
		}
	}
	if (result.empty())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundCallExpression>(s, node->Function(), std::move(result));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteConversionExpression(shared_ptr<BoundConversionExpression> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundConversionExpression>(s, node->Type(), std::move(expression));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewritePostfixExpression(shared_ptr<BoundPostfixExpression> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	auto s = node->Syntax();
	return make_shared<BoundPostfixExpression>(s, node->Variable(),
											   node->OperatorKind(), std::move(expression));
}

class Lowerer final :public BoundTreeRewriter
{
private:
	size_t _labelCount{ 0 };

	[[nodiscard]] BoundLabel GenerateLabel();

protected:
	shared_ptr<BoundStatement> RewriteIfStatement(shared_ptr<BoundIfStatement> node)override;
	shared_ptr<BoundStatement> RewriteWhileStatement(shared_ptr<BoundWhileStatement> node)override;
	shared_ptr<BoundStatement> RewriteDoWhileStatement(shared_ptr<BoundDoWhileStatement> node)override;
	shared_ptr<BoundStatement> RewriteForStatement(shared_ptr<BoundForStatement> node)override;
	shared_ptr<BoundStatement> RewriteConditionalGotoStatement(shared_ptr<BoundConditionalGotoStatement> node)override;

	shared_ptr<BoundExpression> RewriteBinaryExpression(shared_ptr<BoundBinaryExpression> node)override;

public:
	shared_ptr<BoundExpression> RewriteExpression(shared_ptr<BoundExpression> node)override;

};

BoundLabel Lowerer::GenerateLabel()
{
	++_labelCount;
	string name("Label" + std::to_string(_labelCount));
	return BoundLabel(std::move(name));
}


shared_ptr<BoundStatement> Lowerer::RewriteIfStatement(shared_ptr<BoundIfStatement> node)
{
	if (node->ElseStatement() == nullptr)
	{
		auto endLabel = GenerateLabel();
		auto result = Block(
			node->Syntax(),
			{
				GotoFalse(node->Syntax(), endLabel, node->Condition()),
				node->ThenStatement(),
				Label(node->Syntax(),std::move(endLabel))
			}
		);
		return RewriteStatement(std::move(result));
	} else
	{
		auto elseLabel = GenerateLabel();
		auto endLabel = GenerateLabel();
		auto result = Block(
			node->Syntax(),
			{
				GotoFalse(node->Syntax(), elseLabel, node->Condition()),
				node->ThenStatement(),
				Goto(node->Syntax(), endLabel),
				Label(node->Syntax(), elseLabel),
				node->ElseStatement(),
				Label(node->Syntax(), endLabel)
			}
		);

		return RewriteStatement(std::move(result));
	}
}

shared_ptr<BoundStatement> Lowerer::RewriteWhileStatement(shared_ptr<BoundWhileStatement> node)
{
	auto bodyLabel = GenerateLabel();
	auto result = Block(
		node->Syntax(),
		{
			Goto(node->Syntax(), node->ContinueLabel()),
			Label(node->Syntax(), bodyLabel),
			node->Body(),
			Label(node->Syntax(), node->ContinueLabel()),
			GotoTrue(node->Syntax(), std::move(bodyLabel), node->Condition()),
			Label(node->Syntax(), node->BreakLabel())
		}
	);

	return RewriteStatement(std::move(result));
}

shared_ptr<BoundStatement> Lowerer::RewriteDoWhileStatement(shared_ptr<BoundDoWhileStatement> node)
{
	auto bodyLabel = GenerateLabel();
	auto result = Block(
		node->Syntax(),
		{
			Label(node->Syntax(), bodyLabel),
			node->Body(),
			Label(node->Syntax(), node->ContinueLabel()),
			GotoTrue(node->Syntax(), std::move(bodyLabel), node->Condition()),
			Label(node->Syntax(), node->BreakLabel())
		}
	);

	return RewriteStatement(std::move(result));
}

shared_ptr<BoundStatement> Lowerer::RewriteForStatement(shared_ptr<BoundForStatement> node)
{
	auto lowerBound = VariableDeclaration(node->Syntax(), node->Variable(), node->LowerBound());
	auto upperBound = ConstantDeclaration(node->Syntax(), "upperBound", node->UpperBound());
	auto result = Block(
		node->Syntax(),
		{
			lowerBound,
			upperBound,
			While(node->Syntax(),
				  LessOrEqual(
					  node->Syntax(),
					  Variable(node->Syntax(), lowerBound),
					  Variable(node->Syntax(),std::move(upperBound))
				  ),
				  Block(
					  node->Syntax(),
					  {
						  node->Body(),
						  Label(node->Syntax(), node->ContinueLabel()),
						  Increment(
							  node->Syntax(),
							  Variable(node->Syntax(), std::move(lowerBound))
						  )
					  }
				  ),
				  node->BreakLabel(),
				  GenerateLabel())
		}
	);

	return RewriteStatement(std::move(result));
}

shared_ptr<BoundStatement> Lowerer::RewriteConditionalGotoStatement(shared_ptr<BoundConditionalGotoStatement> node)
{
	if (node->Condition()->ConstantValue() != NULL_VALUE)
	{
		bool condition = node->Condition()->ConstantValue().GetValue<bool>();
		condition = node->JumpIfTrue() ? condition : !condition;
		if (condition)
			return RewriteStatement(Goto(node->Syntax(), node->Label()));
		else
			return RewriteStatement(Nop(node->Syntax()));
	}

	return BoundTreeRewriter::RewriteConditionalGotoStatement(std::move(node));
}

shared_ptr<BoundExpression> Lowerer::RewriteExpression(shared_ptr<BoundExpression> node)
{
	// swap out exprssion with constant value with literal expression
	if (node->ConstantValue().HasValue())
	{
		return Literal(node->Syntax(), node->ConstantValue());
	}

	return BoundTreeRewriter::RewriteExpression(std::move(node));
}

namespace {

[[nodiscard]] vector<shared_ptr<BoundExpression>> FlattenStrNodes(shared_ptr<BoundExpression> node)
{
	auto result = vector<shared_ptr<BoundExpression>>();
	if (node->Kind() == BoundNodeKind::BinaryExpression)
	{
		auto b = static_cast<const BoundBinaryExpression*>(node.get());
		if (b->Op().Kind() == BoundBinaryOperatorKind::Addition
			&& b->Left()->Type() == TYPE_STRING
			&& b->Right()->Type() == TYPE_STRING)
		{
			auto rest = FlattenStrNodes(b->Left());
			result.insert(result.end(), std::make_move_iterator(rest.begin()),
						  std::make_move_iterator(rest.end()));

			rest = FlattenStrNodes(b->Right());
			result.insert(result.end(), std::make_move_iterator(rest.begin()),
						  std::make_move_iterator(rest.end()));
		}
	} else
	{
		if (node->Type() != TYPE_STRING)
			throw std::invalid_argument(
				BuildStringFrom("Unexpected node type in string concatenation: '",
								node->Type().Name(), "'.")
			);

		result.push_back(std::move(node));
	}
	return result;
}

[[nodiscard]] vector<shared_ptr<BoundExpression>> FoldStrConstants(const SyntaxNode* syntax,
																   vector<shared_ptr<BoundExpression>> nodes)
{
	auto result = vector<shared_ptr<BoundExpression>>();
	auto builder = string();

	for (auto& node : nodes)
	{
		if (node->ConstantValue() == NULL_VALUE)
		{
			if (!builder.empty())
			{
				result.push_back(Literal(syntax, std::move(builder)));
				builder.clear();
			}
			result.push_back(std::move(node));
		} else
		{
			auto value = node->ConstantValue().GetValue<string>();
			if (value.empty())
				continue;

			builder += value;
		}
	}
	if (!builder.empty())
		result.push_back(Literal(syntax, std::move(builder)));

	return result;
}

[[nodiscard]] shared_ptr<BoundExpression> ConstructStrConcatExpr(const SyntaxNode* syntax,
																 vector<shared_ptr<BoundExpression>> nodes)
{
	if (nodes.empty())
	{
		throw std::invalid_argument("Cannot construct string concat expr: no nodes presented.");
	} else if (nodes.size() == 1)
	{
		return std::move(nodes.back());
	} else
	{
		auto right = std::move(nodes.back());
		nodes.pop_back();
		auto left = ConstructStrConcatExpr(syntax, std::move(nodes));

		return Binary(syntax, std::move(left),
					  SyntaxKind::PlusToken, std::move(right));
	}
}

} //namespace

shared_ptr<BoundExpression> Lowerer::RewriteBinaryExpression(shared_ptr<BoundBinaryExpression> node)
{
	if (node->Op().Kind() == BoundBinaryOperatorKind::Addition)
	{
		if (node->Left()->Type() == TYPE_STRING
			&& node->Right()->Type() == TYPE_STRING)
		{
			auto s = node->Syntax();
			return ConstructStrConcatExpr(s, FoldStrConstants(s, FlattenStrNodes(node)));
		}
	}
	return BoundTreeRewriter::RewriteBinaryExpression(std::move(node));
}

[[nodiscard]] unique_ptr<BoundBlockStatement> Flatten(const FunctionSymbol& func, shared_ptr<BoundStatement> statement)
{
	auto result = vector<shared_ptr<BoundStatement>>();
	auto stack = std::stack<shared_ptr<BoundStatement>>();
	auto s = statement->Syntax();
	stack.push(std::move(statement));

	while (!stack.empty())
	{
		auto current = std::move(stack.top());
		stack.pop();

		if (current->Kind() == BoundNodeKind::BlockStatement)
		{
			auto p = static_cast<BoundBlockStatement*>(current.get());
			auto& statements = p->Statements();
			for (auto it = statements.rbegin(); it != statements.rend(); ++it)
				stack.push(*it);
		} else
		{
			result.push_back(std::move(current));
		}
	}

	auto canFallThrough = [](const BoundStatement& s)
	{
		return s.Kind() != BoundNodeKind::ReturnStatement
			&& s.Kind() != BoundNodeKind::GotoStatement;
	};
	if (func.Type() == TYPE_VOID)
	{
		if (result.empty() || canFallThrough(*result.back()))
			result.push_back(make_shared<BoundReturnStatement>(s, nullptr));
	}
	return make_unique<BoundBlockStatement>(s, std::move(result));
}

[[nodiscard]] unique_ptr<BoundBlockStatement> RemoveDeadCode(unique_ptr<BoundBlockStatement> node)
{
	auto cfg = ControlFlowGraph::Create(node.get());
	auto reachableStmts = std::unordered_set<const BoundStatement*>();
	for (const auto& block : cfg.Blocks())
	{
		for (const auto& p : block->Statements())
			reachableStmts.emplace(p);
	}

	auto result = node->Statements();
	auto it = result.end();
	while (it > result.begin())
	{
		--it;
		if (reachableStmts.find(it->get()) == reachableStmts.end())
		{
			it = result.erase(it);
		}
	}
	return make_unique<BoundBlockStatement>(node->Syntax(), std::move(result));
}

unique_ptr<BoundBlockStatement> Lower(const FunctionSymbol& func, shared_ptr<BoundStatement> statement)
{
	auto lowerer = Lowerer();
	auto result = lowerer.RewriteStatement(std::move(statement));
	return RemoveDeadCode(Flatten(func, std::move(result)));
}

}//MCF
