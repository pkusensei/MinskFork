#include "Lowering.h"

#include <algorithm>
#include <stack>
#include <unordered_set>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "ControlFlowGraph.h"
#include "helpers.h"
#include "SyntaxKind.h"

namespace MCF {

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
	switch (node->Kind())
	{
		case BoundNodeKind::BlockStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundBlockStatement>(node);
			if (p) return RewriteBlockStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::NopStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundNopStatement>(node);
			if (p) return RewriteNopStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::VariableDeclaration:
		{
			auto p = std::dynamic_pointer_cast<BoundVariableDeclaration> (node);
			if (p) return RewriteVariableDeclaration(std::move(p));
			else break;
		}
		case BoundNodeKind::IfStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundIfStatement> (node);
			if (p) return RewriteIfStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::WhileStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundWhileStatement> (node);
			if (p) return RewriteWhileStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::DoWhileStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundDoWhileStatement> (node);
			if (p) return RewriteDoWhileStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::ForStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundForStatement> (node);
			if (p) return RewriteForStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::LabelStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundLabelStatement> (node);
			if (p) return RewriteLabelStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::GotoStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundGotoStatement> (node);
			if (p) return RewriteGotoStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::ConditionalGotoStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundConditionalGotoStatement> (node);
			if (p) return RewriteConditionalGotoStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::ReturnStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundReturnStatement>(node);
			if (p) return RewriteReturnStatement(std::move(p));
			else break;
		}
		case BoundNodeKind::ExpressionStatement:
		{
			auto p = std::dynamic_pointer_cast<BoundExpressionStatement>(node);
			if (p) return RewriteExpressionStatement(std::move(p));
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument(BuildStringFrom("Unexpected node: ", nameof(node->Kind())));
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
	return make_shared<BoundBlockStatement>(std::move(result));
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
	return make_shared<BoundVariableDeclaration>(node->Variable(), std::move(initializer));
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
	return make_shared<BoundIfStatement>(std::move(condition),
		std::move(thenStatement), std::move(elseStatement));
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteWhileStatement(shared_ptr<BoundWhileStatement> node)
{
	auto condition = RewriteExpression(node->Condition());
	auto body = RewriteStatement(node->Body());
	if (condition == node->Condition() && body == node->Body())
		return node;
	return make_shared<BoundWhileStatement>(std::move(condition), std::move(body),
		node->BreakLabel(), node->ContinueLabel());
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteDoWhileStatement(shared_ptr<BoundDoWhileStatement> node)
{
	auto body = RewriteStatement(node->Body());
	auto condition = RewriteExpression(node->Condition());
	if (body == node->Body() && condition == node->Condition())
		return node;
	return make_shared<BoundDoWhileStatement>(std::move(body), std::move(condition),
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
	return make_shared<BoundForStatement>(node->Variable(), std::move(lowerBound),
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
	return make_shared<BoundConditionalGotoStatement>(node->Label(),
		std::move(condition), node->JumpIfTrue());
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteReturnStatement(shared_ptr<BoundReturnStatement> node)
{
	auto expression = node->Expression() == nullptr ?
		nullptr : RewriteExpression(node->Expression());

	if (expression == node->Expression())
		return node;
	return make_shared<BoundReturnStatement>(std::move(expression));
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteExpressionStatement(shared_ptr<BoundExpressionStatement> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundExpressionStatement>(std::move(expression));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteExpression(shared_ptr<BoundExpression> node)
{
	switch (node->Kind())
	{
		case BoundNodeKind::ErrorExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundErrorExpression>(node);
			if (p) return RewriteErrorExpression(std::move(p));
			else break;
		}
		case BoundNodeKind::LiteralExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundLiteralExpression>(node);
			if (p) return RewriteLiteralExpression(std::move(p));
			else break;
		}
		case BoundNodeKind::VariableExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundVariableExpression>(node);
			if (p) return RewriteVariableExpression(std::move(p));
			else break;
		}
		case BoundNodeKind::AssignmentExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundAssignmentExpression>(node);
			if (p) return RewriteAssignmentExpression(std::move(p));
			else break;
		}
		case BoundNodeKind::UnaryExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundUnaryExpression>(node);
			if (p) return RewriteUnaryExpression(std::move(p));
			else break;
		}
		case BoundNodeKind::BinaryExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundBinaryExpression>(node);
			if (p) return RewriteBinaryExpression(std::move(p));
			else break;
		}
		case BoundNodeKind::CallExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundCallExpression>(node);
			if (p) return RewriteCallExpression(std::move(p));
			else break;
		}
		case BoundNodeKind::ConversionExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundConversionExpression>(node);
			if (p) return RewriteConversionExpression(std::move(p));
			else break;
		}
		case BoundNodeKind::PostfixExpression:
		{
			auto p = std::dynamic_pointer_cast<BoundPostfixExpression>(node);
			if (p) return RewritePostfixExpression(std::move(p));
			else break;
		}
		default:
			break;
	}
	throw std::invalid_argument(BuildStringFrom("Unexpected node: ", nameof(node->Kind())));
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
	return make_shared<BoundAssignmentExpression>(node->Variable(), std::move(expression));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteUnaryExpression(shared_ptr<BoundUnaryExpression> node)
{
	auto operand = RewriteExpression(node->Operand());
	if (operand == node->Operand())
		return node;
	return make_shared<BoundUnaryExpression>(node->Op(), std::move(operand));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteBinaryExpression(shared_ptr<BoundBinaryExpression> node)
{
	auto left = RewriteExpression(node->Left());
	auto right = RewriteExpression(node->Right());
	if (left == node->Left() && right == node->Right())
		return node;
	return make_shared<BoundBinaryExpression>(std::move(left), node->Op(), std::move(right));
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
	return make_shared<BoundCallExpression>(node->Function(), std::move(result));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteConversionExpression(shared_ptr<BoundConversionExpression> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundConversionExpression>(node->Type(), std::move(expression));
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewritePostfixExpression(shared_ptr<BoundPostfixExpression> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundPostfixExpression>(node->Variable(),
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

public:
	[[nodiscard]] static unique_ptr<BoundBlockStatement> Flatten(const FunctionSymbol& func, shared_ptr<BoundStatement> statement);
	[[nodiscard]] static unique_ptr<BoundBlockStatement> RemoveDeadCode(unique_ptr<BoundBlockStatement> node);

};

BoundLabel Lowerer::GenerateLabel()
{
	++_labelCount;
	string name("Label" + std::to_string(_labelCount));
	return BoundLabel(std::move(name));
}

unique_ptr<BoundBlockStatement> Lower(const FunctionSymbol& func, shared_ptr<BoundStatement> statement)
{
	auto lowerer = Lowerer();
	auto result = lowerer.RewriteStatement(std::move(statement));
	return Lowerer::RemoveDeadCode(Lowerer::Flatten(func, std::move(result)));
}

unique_ptr<BoundBlockStatement> Lowerer::Flatten(const FunctionSymbol& func, shared_ptr<BoundStatement> statement)
{
	auto result = vector<shared_ptr<BoundStatement>>();
	auto stack = std::stack<shared_ptr<BoundStatement>>();
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
			result.push_back(make_shared<BoundReturnStatement>(nullptr));
	}
	return make_unique<BoundBlockStatement>(std::move(result));
}

unique_ptr<BoundBlockStatement> Lowerer::RemoveDeadCode(unique_ptr<BoundBlockStatement> node)
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
	return make_unique<BoundBlockStatement>(std::move(result));
}

shared_ptr<BoundStatement> Lowerer::RewriteIfStatement(shared_ptr<BoundIfStatement> node)
{
	if (node->ElseStatement() == nullptr)
	{
		auto endLabel = GenerateLabel();
		auto endLabelStatement = make_shared<BoundLabelStatement>(endLabel);
		auto gotoFalse = make_shared<BoundConditionalGotoStatement>(
			endLabel, node->Condition(), false
			);

		auto statements = vector<shared_ptr<BoundStatement>>{
			gotoFalse, node->ThenStatement(), endLabelStatement
		};
		auto result = make_shared<BoundBlockStatement>(statements);
		return RewriteStatement(std::move(result));
	} else
	{
		auto elseLabel = GenerateLabel();
		auto endLabel = GenerateLabel();
		auto gotoFalse = make_shared<BoundConditionalGotoStatement>(
			elseLabel, node->Condition(), false
			);
		auto gotoEndStatement = make_shared<BoundGotoStatement>(endLabel);
		auto elseLabelStatement = make_shared<BoundLabelStatement>(elseLabel);
		auto endLabelStatement = make_shared<BoundLabelStatement>(endLabel);

		auto statements = vector<shared_ptr<BoundStatement>>{
			gotoFalse, node->ThenStatement(), gotoEndStatement,
			elseLabelStatement, node->ElseStatement(), endLabelStatement
		};
		auto result = make_shared<BoundBlockStatement>(std::move(statements));
		return RewriteStatement(std::move(result));
	}
}

shared_ptr<BoundStatement> Lowerer::RewriteWhileStatement(shared_ptr<BoundWhileStatement> node)
{
	auto bodyLabel = GenerateLabel();

	auto gotoContinue = make_shared<BoundGotoStatement>(node->ContinueLabel());
	auto bodyLabelStatement = make_shared<BoundLabelStatement>(bodyLabel);
	auto continueLabelStatement = make_shared<BoundLabelStatement>(node->ContinueLabel());
	auto gotoTrue = make_shared<BoundConditionalGotoStatement>(bodyLabel,
		node->Condition());
	auto breakLabelStatement = make_shared<BoundLabelStatement>(node->BreakLabel());

	auto statements = vector<shared_ptr<BoundStatement>>{
		gotoContinue, bodyLabelStatement, node->Body(),
		continueLabelStatement, gotoTrue, breakLabelStatement
	};

	auto result = make_shared<BoundBlockStatement>(std::move(statements));
	return RewriteStatement(std::move(result));
}

shared_ptr<BoundStatement> Lowerer::RewriteDoWhileStatement(shared_ptr<BoundDoWhileStatement> node)
{
	auto bodyLabel = GenerateLabel();

	auto bodyLabelStatement = make_shared<BoundLabelStatement>(bodyLabel);
	auto continueLabelStatement = make_shared<BoundLabelStatement>(node->ContinueLabel());
	auto gotoTrue = make_shared<BoundConditionalGotoStatement>(bodyLabel,
		node->Condition());
	auto breakLabelStatement = make_shared<BoundLabelStatement>(node->BreakLabel());

	auto statements = vector<shared_ptr<BoundStatement>>{
		bodyLabelStatement, node->Body(), continueLabelStatement,
		gotoTrue, breakLabelStatement
	};
	auto result = make_shared<BoundBlockStatement>(std::move(statements));
	return RewriteStatement(std::move(result));
}

shared_ptr<BoundStatement> Lowerer::RewriteForStatement(shared_ptr<BoundForStatement> node)
{
	auto variableDeclaration = make_shared<BoundVariableDeclaration>(node->Variable(),
		node->LowerBound());
	auto variableExpression = make_shared<BoundVariableExpression>(node->Variable());
	auto upperBoundSymbol = make_shared<LocalVariableSymbol>(
		"upperBound", true, TYPE_INT, node->UpperBound()->ConstantValue()
		);
	auto upperBoundDeclaration = make_shared<BoundVariableDeclaration>(upperBoundSymbol,
		node->UpperBound());

	auto condition = make_shared<BoundBinaryExpression>(
		variableExpression,
		BoundBinaryOperator::Bind(SyntaxKind::LessOrEqualsToken,
			TYPE_INT,
			TYPE_INT),
		make_shared<BoundVariableExpression>(upperBoundSymbol)
		);
	auto continueLabelStatement = make_shared<BoundLabelStatement>(node->ContinueLabel());
	auto increment = make_shared<BoundExpressionStatement>(
		make_shared<BoundAssignmentExpression>(
			node->Variable(),
			make_shared<BoundBinaryExpression>(
				variableExpression,
				BoundBinaryOperator::Bind(SyntaxKind::PlusToken,
					TYPE_INT,
					TYPE_INT),
				make_shared<BoundLiteralExpression>(1)
				)
			)
		);

	auto statements = vector<shared_ptr<BoundStatement>>{
		node->Body(),continueLabelStatement, increment };
	auto whileBody = make_shared<BoundBlockStatement>(std::move(statements));
	auto whileStatement = make_shared<BoundWhileStatement>(condition, whileBody,
		node->BreakLabel(), GenerateLabel());

	statements = { variableDeclaration, upperBoundDeclaration, whileStatement };
	auto result = make_shared<BoundBlockStatement>(std::move(statements));
	return RewriteStatement(std::move(result));
}

shared_ptr<BoundStatement> Lowerer::RewriteConditionalGotoStatement(shared_ptr<BoundConditionalGotoStatement> node)
{
	if (node->Condition()->ConstantValue() != NULL_VALUE)
	{
		bool condition = node->Condition()->ConstantValue().GetValue<bool>();
		condition = node->JumpIfTrue() ? condition : !condition;
		if (condition)
			return make_shared<BoundGotoStatement>(node->Label());
		else
			return make_shared<BoundNopStatement>();
	}

	return BoundTreeRewriter::RewriteConditionalGotoStatement(std::move(node));
}

}//MCF
