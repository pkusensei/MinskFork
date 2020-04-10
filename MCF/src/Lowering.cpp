#include "Lowering.h"

#include <stack>
#include <stdexcept>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "helpers.h"
#include "SyntaxKind.h"

namespace MCF {

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
		auto oldStatement = statements[i];
		auto newStatement = RewriteStatement(std::move(oldStatement));
		result.push_back(std::move(newStatement));

		//The block of code below and similarly in RewriteCallExpression
		//produces malformed rewritten tree which fails to evaluate. :(
		//if (newStatement != oldStatement)
		//{
		//	for (auto j = 0; j < i; ++j)
		//		result.emplace_back(statements[j]);
		//}
		//if (!result.empty())
		//	result.emplace_back(newStatement);
	}
	if (result.empty())
		return node;
	return make_shared<BoundBlockStatement>(result);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteVariableDeclaration(shared_ptr<BoundVariableDeclaration> node)
{
	auto initializer = RewriteExpression(node->Initializer());
	if (initializer == node->Initializer())
		return node;
	return make_shared<BoundVariableDeclaration>(node->Variable(), initializer);
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
	return make_shared<BoundIfStatement>(condition, thenStatement, elseStatement);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteWhileStatement(shared_ptr<BoundWhileStatement> node)
{
	auto condition = RewriteExpression(node->Condition());
	auto body = RewriteStatement(node->Body());
	if (condition == node->Condition() && body == node->Body())
		return node;
	return make_shared<BoundWhileStatement>(condition, body,
		node->BreakLabel(), node->ContinueLabel());
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteDoWhileStatement(shared_ptr<BoundDoWhileStatement> node)
{
	auto body = RewriteStatement(node->Body());
	auto condition = RewriteExpression(node->Condition());
	if (body == node->Body() && condition == node->Condition())
		return node;
	return make_shared<BoundDoWhileStatement>(body, condition,
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
	return make_shared<BoundForStatement>(node->Variable(), lowerBound,
		upperBound, body, node->BreakLabel(), node->ContinueLabel());
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
	return make_shared<BoundConditionalGotoStatement>(node->Label(), condition, node->JumpIfTrue());
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteReturnStatement(shared_ptr<BoundReturnStatement> node)
{
	auto expression = node->Expression() == nullptr ?
		nullptr : RewriteExpression(node->Expression());

	if (expression == node->Expression())
		return node;
	return make_shared<BoundReturnStatement>(expression);
}

shared_ptr<BoundStatement> BoundTreeRewriter::RewriteExpressionStatement(shared_ptr<BoundExpressionStatement> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundExpressionStatement>(expression);
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
	return make_shared<BoundAssignmentExpression>(node->Variable(), expression);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteUnaryExpression(shared_ptr<BoundUnaryExpression> node)
{
	auto operand = RewriteExpression(node->Operand());
	if (operand == node->Operand())
		return node;
	return make_shared<BoundUnaryExpression>(node->Op(), operand);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteBinaryExpression(shared_ptr<BoundBinaryExpression> node)
{
	auto left = RewriteExpression(node->Left());
	auto right = RewriteExpression(node->Right());
	if (left == node->Left() && right == node->Right())
		return node;
	return make_shared<BoundBinaryExpression>(left, node->Op(), right);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteCallExpression(shared_ptr<BoundCallExpression> node)
{
	auto result = vector<shared_ptr<BoundExpression>>();
	for (size_t i = 0; i < node->Arguments().size(); ++i)
	{
		auto oldArg = node->Arguments()[i];
		auto newArg = RewriteExpression(std::move(oldArg));
		result.push_back(std::move(newArg));
		//if (newArg != oldArg)
		//{
		//	if (result.empty())
		//		for (auto j = 0; j < i; ++j)
		//			result.emplace_back(node->Arguments()[i]);
		//}
		//if (!result.empty())
		//	result.emplace_back(newArg);
	}
	if (result.empty())
		return node;
	return make_shared<BoundCallExpression>(node->Function(), result);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewriteConversionExpression(shared_ptr<BoundConversionExpression> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundConversionExpression>(node->Type(), expression);
}

shared_ptr<BoundExpression> BoundTreeRewriter::RewritePostfixExpression(shared_ptr<BoundPostfixExpression> node)
{
	auto expression = RewriteExpression(node->Expression());
	if (expression == node->Expression())
		return node;
	return make_shared<BoundPostfixExpression>(node->Variable(), node->OperatorKind(), expression);
}

BoundLabel Lowerer::GenerateLabel()
{
	++_labelCount;
	string name("Label" + std::to_string(_labelCount));
	return BoundLabel(std::move(name));
}

unique_ptr<BoundBlockStatement> Lowerer::Lower(shared_ptr<BoundStatement> statement)
{
	auto lowerer = Lowerer();
	auto result = lowerer.RewriteStatement(std::move(statement));
	return Flatten(result);
}

unique_ptr<BoundBlockStatement> Lowerer::Flatten(shared_ptr<BoundStatement> statement)
{
	auto result = vector<shared_ptr<BoundStatement>>();
	auto stack = std::stack<shared_ptr<BoundStatement>>();
	stack.push(std::move(statement));

	while (!stack.empty())
	{
		auto current = std::move(stack.top());
		stack.pop();

		auto p = std::dynamic_pointer_cast<BoundBlockStatement>(current);
		if (p)
		{
			auto& statements = p->Statements();
			for (auto it = statements.rbegin(); it != statements.rend(); ++it)
				stack.push(*it);
		} else
		{
			result.push_back(std::move(current));
		}
	}
	return make_unique<BoundBlockStatement>(result);
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
		return RewriteStatement(result);
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
		auto result = make_shared<BoundBlockStatement>(statements);
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

	auto result = make_shared<BoundBlockStatement>(statements);
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
	auto result = make_shared<BoundBlockStatement>(statements);
	return RewriteStatement(std::move(result));
}

shared_ptr<BoundStatement> Lowerer::RewriteForStatement(shared_ptr<BoundForStatement> node)
{
	auto variableDeclaration = make_shared<BoundVariableDeclaration>(node->Variable(),
		node->LowerBound());
	auto variableExpression = make_shared<BoundVariableExpression>(node->Variable());
	auto upperBoundSymbol = make_shared<LocalVariableSymbol>(
		"upperBound", true, TypeSymbol(TypeEnum::Int)
		);
	auto upperBoundDeclaration = make_shared<BoundVariableDeclaration>(upperBoundSymbol,
		node->UpperBound());

	auto condition = make_shared<BoundBinaryExpression>(
		variableExpression,
		BoundBinaryOperator::Bind(SyntaxKind::LessOrEqualsToken,
			TypeSymbol(TypeEnum::Int),
			TypeSymbol(TypeEnum::Int)),
		make_shared<BoundVariableExpression>(upperBoundSymbol)
		);
	auto continueLabelStatement = make_shared<BoundLabelStatement>(node->ContinueLabel());
	auto increment = make_shared<BoundExpressionStatement>(
		make_shared<BoundAssignmentExpression>(
			node->Variable(),
			make_shared<BoundBinaryExpression>(
				variableExpression,
				BoundBinaryOperator::Bind(SyntaxKind::PlusToken,
					TypeSymbol(TypeEnum::Int),
					TypeSymbol(TypeEnum::Int)),
				make_shared<BoundLiteralExpression>(1)
				)
			)
		);

	auto statements = vector<shared_ptr<BoundStatement>>{
		node->Body(),continueLabelStatement, increment };
	auto whileBody = make_shared<BoundBlockStatement>(statements);
	auto whileStatement = make_shared<BoundWhileStatement>(condition, whileBody,
		node->BreakLabel(), GenerateLabel());

	statements = { variableDeclaration, upperBoundDeclaration, whileStatement };
	auto result = make_shared<BoundBlockStatement>(statements);
	return RewriteStatement(std::move(result));
}

}//MCF
