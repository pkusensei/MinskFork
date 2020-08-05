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

template<typename Elem, typename T, typename... Args>
[[nodiscard]] auto MakeVec(T&& t, Args&&... args)
{
	using Base = typename Elem::element_type;
	using Derived = typename std::decay_t<T>::element_type;
	static_assert(std::is_base_of_v<Base, Derived>);

	auto result = vector<Elem>();
	result.reserve(1 + sizeof...(Args));
	result.push_back(std::forward<T>(t));

	if constexpr (sizeof...(Args) > 0)
	{
		auto r = MakeVec<Elem>(std::forward<Args>(args)...);
		result.insert(result.end(),
					  std::make_move_iterator(r.begin()), std::make_move_iterator(r.end()));
	}
	return result;
}

template<typename... Args>
unique_ptr<BoundBlockStatement> Block(const SyntaxNode& syntax,
									  Args&&... statements)
{
	auto vec = MakeVec<unique_ptr<BoundStatement>>(std::forward<Args>(statements)...);
	return make_unique<BoundBlockStatement>(syntax, std::move(vec));
}

unique_ptr<BoundVariableDeclaration> VariableDeclaration(const SyntaxNode& syntax,
														 unique_ptr<VariableSymbol> symbol,
														 unique_ptr<BoundExpression> initializer)
{
	return make_unique<BoundVariableDeclaration>(syntax, std::move(symbol), std::move(initializer));
}

unique_ptr<BoundVariableDeclaration> VariableDeclarationInternal(const SyntaxNode& syntax,
																 string name,
																 unique_ptr<BoundExpression> initializer,
																 bool isReadOnly)
{
	auto local = make_unique<LocalVariableSymbol>(name, isReadOnly,
												  initializer->Type(),
												  initializer->ConstantValue());
	return make_unique<BoundVariableDeclaration>(syntax, std::move(local), std::move(initializer));
}

[[maybe_unused]] unique_ptr<BoundVariableDeclaration> VariableDeclaration(const SyntaxNode& syntax, string name,
																		  unique_ptr<BoundExpression> initializer)
{
	return VariableDeclarationInternal(syntax, std::move(name), std::move(initializer), false);
}

unique_ptr<BoundVariableDeclaration> ConstantDeclaration(const SyntaxNode& syntax,
														 string name,
														 unique_ptr<BoundExpression> initializer)
{
	return VariableDeclarationInternal(syntax, std::move(name), std::move(initializer), true);
}

unique_ptr<BoundWhileStatement> While(const SyntaxNode& syntax, unique_ptr<BoundExpression> condition,
									  unique_ptr<BoundStatement> body, BoundLabel breakLabel, BoundLabel continueLabel)
{
	return make_unique<BoundWhileStatement>(syntax, std::move(condition), std::move(body),
											std::move(breakLabel), std::move(continueLabel));
}

unique_ptr<BoundGotoStatement> Goto(const SyntaxNode& syntax, BoundLabel label)
{
	return make_unique<BoundGotoStatement>(syntax, std::move(label));
}

unique_ptr<BoundConditionalGotoStatement> GotoTrue(const SyntaxNode& syntax,
												   BoundLabel label,
												   unique_ptr<BoundExpression> condition)
{
	return make_unique<BoundConditionalGotoStatement>(syntax, std::move(label),
													  std::move(condition), true);
}

unique_ptr<BoundConditionalGotoStatement> GotoFalse(const SyntaxNode& syntax,
													BoundLabel label,
													unique_ptr<BoundExpression> condition)
{
	return make_unique<BoundConditionalGotoStatement>(syntax, std::move(label),
													  std::move(condition), false);
}

unique_ptr<BoundLabelStatement> Label(const SyntaxNode& syntax, BoundLabel label)
{
	return make_unique<BoundLabelStatement>(syntax, std::move(label));
}

unique_ptr<BoundNopStatement> Nop(const SyntaxNode& syntax)
{
	return make_unique<BoundNopStatement>(syntax);
}

unique_ptr<BoundBinaryExpression> Binary(const SyntaxNode& syntax,
										 unique_ptr<BoundExpression> left,
										 SyntaxKind kind,
										 unique_ptr<BoundExpression> right)
{
	auto op = BoundBinaryOperator::Bind(kind, left->Type(), right->Type());
	assert(op.IsUseful);
	return make_unique<BoundBinaryExpression>(syntax, std::move(left),
											  std::move(op), std::move(right));
}

unique_ptr<BoundBinaryExpression> Add(const SyntaxNode& syntax,
									  unique_ptr<BoundExpression> left,
									  unique_ptr<BoundExpression> right)
{
	return Binary(syntax, std::move(left), SyntaxKind::PlusToken, std::move(right));
}

unique_ptr<BoundBinaryExpression> LessOrEqual(const SyntaxNode& syntax,
											  unique_ptr<BoundExpression> left,
											  unique_ptr<BoundExpression> right)
{
	return Binary(syntax, std::move(left), SyntaxKind::LessOrEqualsToken, std::move(right));
}

unique_ptr<BoundLiteralExpression> Literal(const SyntaxNode& syntax, ValueType literal)
{
	assert(literal.Type() == TYPE_BOOL
		   || literal.Type() == TYPE_INT
		   || literal.Type() == TYPE_STRING);

	return make_unique<BoundLiteralExpression>(syntax, std::move(literal));
}

unique_ptr<BoundExpressionStatement> Increment(const SyntaxNode& syntax,
											   unique_ptr<BoundVariableExpression> variable)
{
	auto increment = Add(syntax, variable->Clone(), Literal(syntax, 1));
	auto incrementAssign = make_unique<BoundAssignmentExpression>(syntax,
																  variable->Variable->UniqueCloneAs<VariableSymbol>(),
																  std::move(increment));
	return make_unique<BoundExpressionStatement>(syntax, std::move(incrementAssign));
}

[[maybe_unused]] unique_ptr<BoundUnaryExpression> Not(const SyntaxNode& syntax,
													  unique_ptr<BoundExpression> condition)
{
	assert(condition->Type() == TYPE_BOOL);
	auto op = BoundUnaryOperator::Bind(SyntaxKind::BangToken, TYPE_BOOL);
	assert(op.IsUseful);
	return make_unique<BoundUnaryExpression>(syntax, std::move(op), std::move(condition));
}

unique_ptr<BoundVariableExpression> Variable(const SyntaxNode& syntax,
											 const BoundVariableDeclaration& variable)
{
	return make_unique<BoundVariableExpression>(syntax,
												variable.Variable->UniqueCloneAs<VariableSymbol>());
}

} // namespace

class BoundTreeRewriter
{
protected:
	virtual unique_ptr<BoundStatement> RewriteBlockStatement(unique_ptr<BoundBlockStatement> node);
	virtual unique_ptr<BoundStatement> RewriteNopStatement(unique_ptr<BoundNopStatement> node);
	virtual unique_ptr<BoundStatement> RewriteVariableDeclaration(unique_ptr<BoundVariableDeclaration> node);
	virtual unique_ptr<BoundStatement> RewriteIfStatement(unique_ptr<BoundIfStatement> node);
	virtual unique_ptr<BoundStatement> RewriteWhileStatement(unique_ptr<BoundWhileStatement> node);
	virtual unique_ptr<BoundStatement> RewriteDoWhileStatement(unique_ptr<BoundDoWhileStatement> node);
	virtual unique_ptr<BoundStatement> RewriteForStatement(unique_ptr<BoundForStatement> node);
	virtual unique_ptr<BoundStatement> RewriteLabelStatement(unique_ptr<BoundLabelStatement> node);
	virtual unique_ptr<BoundStatement> RewriteGotoStatement(unique_ptr<BoundGotoStatement> node);
	virtual unique_ptr<BoundStatement> RewriteConditionalGotoStatement(unique_ptr<BoundConditionalGotoStatement> node);
	virtual unique_ptr<BoundStatement> RewriteReturnStatement(unique_ptr<BoundReturnStatement> node);
	virtual unique_ptr<BoundStatement> RewriteExpressionStatement(unique_ptr<BoundExpressionStatement> node);

	virtual unique_ptr<BoundExpression> RewriteErrorExpression(unique_ptr<BoundErrorExpression> node);
	virtual unique_ptr<BoundExpression> RewriteLiteralExpression(unique_ptr<BoundLiteralExpression> node);
	virtual unique_ptr<BoundExpression> RewriteVariableExpression(unique_ptr<BoundVariableExpression> node);
	virtual unique_ptr<BoundExpression> RewriteAssignmentExpression(unique_ptr<BoundAssignmentExpression> node);
	virtual unique_ptr<BoundExpression> RewriteCompoundAssignmentExpression(unique_ptr<BoundCompoundAssignmentExpression> node);
	virtual unique_ptr<BoundExpression> RewriteUnaryExpression(unique_ptr<BoundUnaryExpression> node);
	virtual unique_ptr<BoundExpression> RewriteBinaryExpression(unique_ptr<BoundBinaryExpression> node);
	virtual unique_ptr<BoundExpression> RewriteCallExpression(unique_ptr<BoundCallExpression> node);
	virtual unique_ptr<BoundExpression> RewriteConversionExpression(unique_ptr<BoundConversionExpression> node);
	virtual unique_ptr<BoundExpression> RewritePostfixExpression(unique_ptr<BoundPostfixExpression> node);

public:
	virtual ~BoundTreeRewriter() = default;

	virtual unique_ptr<BoundStatement> RewriteStatement(unique_ptr<BoundStatement> node);
	virtual unique_ptr<BoundExpression> RewriteExpression(unique_ptr<BoundExpression> node);
};

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteStatement(unique_ptr<BoundStatement> node)
{
#define REWRITE_STMT(kind) \
case BoundNodeKind::kind:                                       \
{                                                               \
	auto p = StaticUniquePtrCast<Bound##kind>(std::move(node)); \
	return Rewrite##kind(std::move(p));                         \
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

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteBlockStatement(unique_ptr<BoundBlockStatement> node)
{
	auto result = vector<unique_ptr<BoundStatement>>();
	auto& statements = node->Statements;
	for (size_t i = 0; i < statements.size(); ++i)
	{
		auto& oldStmt = statements.at(i);
		auto newStmt = RewriteStatement(oldStmt->Clone());
		if (*newStmt == *oldStmt)
		{
			if (!result.empty())
				result.push_back(std::move(newStmt));
		} else
		{
			if (result.empty())
				for (size_t j = 0; j < i; ++j)
					result.push_back(std::move(statements.at(j)));
			result.push_back(std::move(newStmt));
		}
	}
	if (result.empty())
		return node;
	auto& s = node->Syntax();

	return make_unique<BoundBlockStatement>(s, std::move(result));
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteNopStatement(unique_ptr<BoundNopStatement> node)
{
	return node;
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteVariableDeclaration(unique_ptr<BoundVariableDeclaration> node)
{
	auto initializer = RewriteExpression(node->Initializer->Clone());
	if (*initializer == *node->Initializer)
		return node;
	auto& s = node->Syntax();
	return VariableDeclaration(s, node->Variable->UniqueCloneAs<VariableSymbol>(),
							   std::move(initializer));
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteIfStatement(unique_ptr<BoundIfStatement> node)
{
	auto condition = RewriteExpression(node->Condition->Clone());
	auto thenStatement = RewriteStatement(node->ThenStatement->Clone());
	auto elseStatement = node->ElseStatement == nullptr ?
		nullptr : RewriteStatement(node->ElseStatement->Clone());
	if (*condition == *node->Condition
		&& *thenStatement == *node->ThenStatement)
	{
		if (elseStatement && *elseStatement == *node->ElseStatement)
			return node;
	}
	auto& s = node->Syntax();
	return make_unique<BoundIfStatement>(s, std::move(condition),
										 std::move(thenStatement), std::move(elseStatement));
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteWhileStatement(unique_ptr<BoundWhileStatement> node)
{
	auto condition = RewriteExpression(node->Condition->Clone());
	auto body = RewriteStatement(node->Body->Clone());
	if (*condition == *node->Condition && *body == *node->Body)
		return node;
	auto& s = node->Syntax();
	return While(s, std::move(condition), std::move(body),
				 node->BreakLabel, node->ContinueLabel);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteDoWhileStatement(unique_ptr<BoundDoWhileStatement> node)
{
	auto body = RewriteStatement(node->Body->Clone());
	auto condition = RewriteExpression(node->Condition->Clone());
	if (*body == *node->Body && *condition == *node->Condition)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundDoWhileStatement>(s, std::move(body), std::move(condition),
											  node->BreakLabel, node->ContinueLabel);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteForStatement(unique_ptr<BoundForStatement> node)
{
	auto lowerBound = RewriteExpression(node->LowerBound->Clone());
	auto upperBound = RewriteExpression(node->UpperBound->Clone());
	auto body = RewriteStatement(node->Body->Clone());
	if (*lowerBound == *node->LowerBound
		&& *upperBound == *node->UpperBound
		&& *body == *node->Body)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundForStatement>(s, node->Variable->UniqueCloneAs<VariableSymbol>(),
										  std::move(lowerBound), std::move(upperBound),
										  std::move(body),
										  node->BreakLabel, node->ContinueLabel);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteLabelStatement(unique_ptr<BoundLabelStatement> node)
{
	return node;
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteGotoStatement(unique_ptr<BoundGotoStatement> node)
{
	return node;
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteConditionalGotoStatement(unique_ptr<BoundConditionalGotoStatement> node)
{
	auto condition = RewriteExpression(node->Condition->Clone());
	if (*condition == *node->Condition)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundConditionalGotoStatement>(s, node->Label,
													  std::move(condition), node->JumpIfTrue);
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteReturnStatement(unique_ptr<BoundReturnStatement> node)
{
	auto expression = node->Expression == nullptr ?
		nullptr : RewriteExpression(node->Expression->Clone());

	if (expression && *expression == *node->Expression)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundReturnStatement>(s, std::move(expression));
}

unique_ptr<BoundStatement> BoundTreeRewriter::RewriteExpressionStatement(unique_ptr<BoundExpressionStatement> node)
{
	auto expression = RewriteExpression(node->Expression->Clone());
	if (*expression == *node->Expression)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundExpressionStatement>(s, std::move(expression));
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteExpression(unique_ptr<BoundExpression> node)
{
#define REWRITE_EXPR(kind) \
case BoundNodeKind::kind:                                       \
{                                                               \
	auto p = StaticUniquePtrCast<Bound##kind>(std::move(node)); \
	return Rewrite##kind(std::move(p));                         \
}

	switch (node->Kind())
	{
		case BoundNodeKind::ErrorExpression:
		case BoundNodeKind::LiteralExpression:
		case BoundNodeKind::VariableExpression:
			return node;

			REWRITE_EXPR(AssignmentExpression);
			REWRITE_EXPR(CompoundAssignmentExpression);
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

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteErrorExpression(unique_ptr<BoundErrorExpression> node)
{
	return node;
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteLiteralExpression(unique_ptr<BoundLiteralExpression> node)
{
	return node;
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteVariableExpression(unique_ptr<BoundVariableExpression> node)
{
	return node;
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteAssignmentExpression(unique_ptr<BoundAssignmentExpression> node)
{
	auto expression = RewriteExpression(node->Expression->Clone());
	if (*expression == *node->Expression)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundAssignmentExpression>(s, node->Variable->UniqueCloneAs<VariableSymbol>(),
												  std::move(expression));
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteCompoundAssignmentExpression(unique_ptr<BoundCompoundAssignmentExpression> node)
{
	auto& s = node->Syntax();

	auto variable = make_unique<BoundVariableExpression>(s, node->Variable->UniqueCloneAs<VariableSymbol>());
	auto binary = make_unique<BoundBinaryExpression>(s, std::move(variable),
													 node->Op,
													 std::move(node->Expression));
	auto assignment = make_unique<BoundAssignmentExpression>(s, node->Variable->UniqueCloneAs<VariableSymbol>(),
															 std::move(binary));

	return RewriteAssignmentExpression(std::move(assignment));
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteUnaryExpression(unique_ptr<BoundUnaryExpression> node)
{
	auto operand = RewriteExpression(node->Operand->Clone());
	if (*operand == *node->Operand)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundUnaryExpression>(s, node->Op, std::move(operand));
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteBinaryExpression(unique_ptr<BoundBinaryExpression> node)
{
	auto left = RewriteExpression(node->Left->Clone());
	auto right = RewriteExpression(node->Right->Clone());
	if (*left == *node->Left && *right == *node->Right)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundBinaryExpression>(s, std::move(left), node->Op, std::move(right));
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteCallExpression(unique_ptr<BoundCallExpression> node)
{
	auto result = vector<unique_ptr<BoundExpression>>();
	for (size_t i = 0; i < node->Arguments.size(); ++i)
	{
		auto& oldArg = node->Arguments.at(i);
		auto newArg = RewriteExpression(oldArg->Clone());
		if (*newArg == *oldArg)
		{
			if (!result.empty())
				result.push_back(std::move(newArg));
		} else
		{
			if (result.empty())
				for (size_t j = 0; j < i; ++j)
					result.push_back(std::move(node->Arguments.at(j)));
			result.push_back(std::move(newArg));
		}
	}
	if (result.empty())
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundCallExpression>(s, node->Function->UniqueCloneAs<FunctionSymbol>(),
											std::move(result));
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewriteConversionExpression(unique_ptr<BoundConversionExpression> node)
{
	auto expression = RewriteExpression(node->Expression->Clone());
	if (*expression == *node->Expression)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundConversionExpression>(s, node->Type(), std::move(expression));
}

unique_ptr<BoundExpression> BoundTreeRewriter::RewritePostfixExpression(unique_ptr<BoundPostfixExpression> node)
{
	auto expression = RewriteExpression(node->Expression->Clone());
	if (*expression == *node->Expression)
		return node;
	auto& s = node->Syntax();
	return make_unique<BoundPostfixExpression>(s, node->Variable->UniqueCloneAs<VariableSymbol>(),
											   node->OperatorKind, std::move(expression));
}

class Lowerer final :public BoundTreeRewriter
{
protected:
	unique_ptr<BoundStatement> RewriteIfStatement(unique_ptr<BoundIfStatement> node)override;
	unique_ptr<BoundStatement> RewriteWhileStatement(unique_ptr<BoundWhileStatement> node)override;
	unique_ptr<BoundStatement> RewriteDoWhileStatement(unique_ptr<BoundDoWhileStatement> node)override;
	unique_ptr<BoundStatement> RewriteForStatement(unique_ptr<BoundForStatement> node)override;
	unique_ptr<BoundStatement> RewriteConditionalGotoStatement(unique_ptr<BoundConditionalGotoStatement> node)override;

	unique_ptr<BoundExpression> RewriteBinaryExpression(unique_ptr<BoundBinaryExpression> node)override;

public:
	unique_ptr<BoundExpression> RewriteExpression(unique_ptr<BoundExpression> node)override;

};

unique_ptr<BoundStatement> Lowerer::RewriteIfStatement(unique_ptr<BoundIfStatement> node)
{
	auto& s = node->Syntax();

	if (node->ElseStatement == nullptr)
	{
		auto endLabel = BoundLabel("end");

		// NOTE the evaluation order of function arguments is unspecified
		//      A std::move here might invalidate any BoundLabel
		auto result = Block(
			s,
			GotoFalse(s, endLabel, std::move(node->Condition)),
			std::move(node->ThenStatement),
			Label(s, endLabel)
		);
		return RewriteStatement(std::move(result));
	} else
	{
		auto elseLabel = BoundLabel("else");
		auto endLabel = BoundLabel("end");
		auto result = Block(
			s,
			GotoFalse(s, elseLabel, std::move(node->Condition)),
			std::move(node->ThenStatement),
			Goto(s, endLabel),
			Label(s, elseLabel),
			std::move(node->ElseStatement),
			Label(s, endLabel)
		);

		return RewriteStatement(std::move(result));
	}
}

unique_ptr<BoundStatement> Lowerer::RewriteWhileStatement(unique_ptr<BoundWhileStatement> node)
{
	auto& s = node->Syntax();

	auto bodyLabel = BoundLabel("body");
	auto result = Block(
		s,
		Goto(s, node->ContinueLabel),
		Label(s, bodyLabel),
		std::move(node->Body),
		Label(s, node->ContinueLabel),
		GotoTrue(s, bodyLabel, std::move(node->Condition)),
		Label(s, node->BreakLabel)
	);

	return RewriteStatement(std::move(result));
}

unique_ptr<BoundStatement> Lowerer::RewriteDoWhileStatement(unique_ptr<BoundDoWhileStatement> node)
{
	auto& s = node->Syntax();

	auto bodyLabel = BoundLabel("body");
	auto result = Block(
		s,
		Label(s, bodyLabel),
		std::move(node->Body),
		Label(s, node->ContinueLabel),
		GotoTrue(s, bodyLabel, std::move(node->Condition)),
		Label(s, node->BreakLabel)
	);

	return RewriteStatement(std::move(result));
}

unique_ptr<BoundStatement> Lowerer::RewriteForStatement(unique_ptr<BoundForStatement> node)
{
	auto& s = node->Syntax();

	auto lowerBound = VariableDeclaration(s, node->Variable->UniqueCloneAs<VariableSymbol>(),
										  std::move(node->LowerBound));
	auto upperBound = ConstantDeclaration(s, "upperBound", std::move(node->UpperBound));
	auto& lowerRef = *lowerBound;
	auto& upperRef = *upperBound;
	auto result = Block(
		s,
		std::move(lowerBound),
		std::move(upperBound),
		While(s,
			  LessOrEqual(
				  s,
				  Variable(s, lowerRef),
				  Variable(s, upperRef)
			  ),
			  Block(
				  s,
				  std::move(node->Body),
				  Label(s, node->ContinueLabel),
				  Increment(
					  s,
					  Variable(s, lowerRef)
				  )
			  ),
			  node->BreakLabel,
			  BoundLabel("while"))
	);

	return RewriteStatement(std::move(result));
}

unique_ptr<BoundStatement> Lowerer::RewriteConditionalGotoStatement(unique_ptr<BoundConditionalGotoStatement> node)
{
	auto& s = node->Syntax();

	if (node->Condition->ConstantValue() != NULL_VALUE)
	{
		bool condition = node->Condition->ConstantValue().GetValue<bool>();
		condition = node->JumpIfTrue ? condition : !condition;
		if (condition)
			return RewriteStatement(Goto(s, node->Label));
		else
			return RewriteStatement(Nop(s));
	}

	return BoundTreeRewriter::RewriteConditionalGotoStatement(std::move(node));
}

unique_ptr<BoundExpression> Lowerer::RewriteExpression(unique_ptr<BoundExpression> node)
{
	auto& s = node->Syntax();

	// swap out exprssion with constant value with literal expression
	if (node->ConstantValue().HasValue())
	{
		return Literal(s, node->ConstantValue());
	}

	return BoundTreeRewriter::RewriteExpression(std::move(node));
}

namespace {

[[nodiscard]] vector<unique_ptr<BoundExpression>> FlattenStrNodes(unique_ptr<BoundExpression> node)
{
	auto result = vector<unique_ptr<BoundExpression>>{};
	if (node->Kind() == BoundNodeKind::BinaryExpression)
	{
		auto& b = static_cast<BoundBinaryExpression&>(*node);
		if (b.Op.Kind == BoundBinaryOperatorKind::Addition
			&& b.Left->Type() == TYPE_STRING
			&& b.Right->Type() == TYPE_STRING)
		{
			auto rest = FlattenStrNodes(std::move(b.Left));
			result.insert(result.end(), std::make_move_iterator(rest.begin()),
						  std::make_move_iterator(rest.end()));

			rest = FlattenStrNodes(std::move(b.Right));
			result.insert(result.end(), std::make_move_iterator(rest.begin()),
						  std::make_move_iterator(rest.end()));
		}
	} else
	{
		if (node->Type() != TYPE_STRING)
			throw std::invalid_argument(
				BuildStringFrom("Unexpected node type in string concatenation: '",
								node->Type().Name, "'.")
			);

		result.push_back(std::move(node));
	}
	return result;
}

[[nodiscard]] vector<unique_ptr<BoundExpression>> FoldStrConstants(const SyntaxNode& syntax,
																   vector<unique_ptr<BoundExpression>> nodes)
{
	auto result = vector<unique_ptr<BoundExpression>>{};
	auto builder = string{};

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
			auto& value = node->ConstantValue().GetValue<string>();
			if (value.empty())
				continue;

			builder += value;
		}
	}
	if (!builder.empty())
		result.push_back(Literal(syntax, std::move(builder)));

	return result;
}

[[nodiscard]] unique_ptr<BoundExpression> ConstructStrConcatExpr(const SyntaxNode& syntax,
																 vector<unique_ptr<BoundExpression>> nodes)
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

unique_ptr<BoundExpression> Lowerer::RewriteBinaryExpression(unique_ptr<BoundBinaryExpression> node)
{
	if (node->Op.Kind == BoundBinaryOperatorKind::Addition)
	{
		if (node->Left->Type() == TYPE_STRING
			&& node->Right->Type() == TYPE_STRING)
		{
			auto& s = node->Syntax();
			return ConstructStrConcatExpr(s,
										  FoldStrConstants(s,
														   FlattenStrNodes(std::move(node))));
		}
	}
	return BoundTreeRewriter::RewriteBinaryExpression(std::move(node));
}

namespace {

[[nodiscard]] unique_ptr<BoundBlockStatement> Flatten(const FunctionSymbol& func,
													  unique_ptr<BoundStatement> statement)
{
	auto result = vector<unique_ptr<BoundStatement>>{};
	auto stack = std::stack<unique_ptr<BoundStatement>>{};
	auto& s = statement->Syntax();
	stack.push(std::move(statement));

	while (!stack.empty())
	{
		auto current = std::move(stack.top());
		stack.pop();

		if (current->Kind() == BoundNodeKind::BlockStatement)
		{
			auto& p = static_cast<BoundBlockStatement&>(*current);
			auto& statements = p.Statements;
			for (auto it = statements.rbegin(); it != statements.rend(); ++it)
				stack.push(std::move(*it));
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
	if (func.Type == TYPE_VOID)
	{
		if (result.empty() || canFallThrough(*result.back()))
			result.push_back(make_unique<BoundReturnStatement>(s, nullptr));
	}
	return make_unique<BoundBlockStatement>(s, std::move(result));
}

[[nodiscard]] unique_ptr<BoundBlockStatement> RemoveDeadCode(unique_ptr<BoundBlockStatement> node)
{
	assert(node && "Invalid unique_ptr<BoundBlockStatement>.");
	auto cfg = ControlFlowGraph::Create(*node);
	auto reachableStmts = std::unordered_set<const BoundStatement*>{};
	for (const auto& block : cfg.Blocks)
	{
		for (const auto& p : block->Statements)
			reachableStmts.emplace(p);
	}

	auto result = std::move(node->Statements);
	auto it = result.end();
	while (it > result.begin())
	{
		--it;
		if (!reachableStmts.contains(it->get()))
		{
			it = result.erase(it);
		}
	}
	auto& s = node->Syntax();

	return make_unique<BoundBlockStatement>(s, std::move(result));
}

} // namespace

unique_ptr<BoundBlockStatement> Lower(const FunctionSymbol& func,
									  unique_ptr<BoundStatement> statement)
{
	auto lowerer = Lowerer();
	auto result = lowerer.RewriteStatement(std::move(statement));
	return RemoveDeadCode(Flatten(func, std::move(result)));
}

}//MCF
