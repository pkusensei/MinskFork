#pragma once

#include "common.h"

namespace MCF {

class BoundLabel;

class BoundExpression;
class BoundErrorExpression;
class BoundLiteralExpression;
class BoundVariableExpression;
class BoundAssignmentExpression;
class BoundUnaryExpression;
class BoundBinaryExpression;
class BoundCallExpression;
class BoundConversionExpression;
class BoundPostfixExpression;

class BoundStatement;
class BoundBlockStatement;
class BoundVariableDeclaration;
class BoundIfStatement;
class BoundWhileStatement;
class BoundDoWhileStatement;
class BoundForStatement;
class BoundLabelStatement;
class BoundGotoStatement;
class BoundConditionalGotoStatement;
class BoundReturnStatement;
class BoundExpressionStatement;

class BoundTreeRewriter
{
protected:
	virtual shared_ptr<BoundStatement> RewriteBlockStatement(shared_ptr<BoundBlockStatement> node);
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

class Lowerer final :public BoundTreeRewriter
{
private:
	size_t _labelCount{ 0 };

	Lowerer() = default;
	[[nodiscard]] BoundLabel GenerateLabel();
	[[nodiscard]] static unique_ptr<BoundBlockStatement> Flatten(shared_ptr<BoundStatement> statement);

protected:
	shared_ptr<BoundStatement> RewriteIfStatement(shared_ptr<BoundIfStatement> node)override;
	shared_ptr<BoundStatement> RewriteWhileStatement(shared_ptr<BoundWhileStatement> node)override;
	shared_ptr<BoundStatement> RewriteDoWhileStatement(shared_ptr<BoundDoWhileStatement> node)override;
	shared_ptr<BoundStatement> RewriteForStatement(shared_ptr<BoundForStatement> node)override;

public:
	static unique_ptr<BoundBlockStatement> Lower(shared_ptr<BoundStatement> statement);
};

}//MCF
