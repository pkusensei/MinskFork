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
class BoundExpressionStatement;
class BoundGotoStatement;
class BoundConditionalGotoStatement;

class BoundTreeRewriter
{
protected:
	virtual shared_ptr<BoundStatement> RewriteBlockStatement(const shared_ptr<BoundBlockStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteVariableDeclaration(const shared_ptr<BoundVariableDeclaration>& node);
	virtual shared_ptr<BoundStatement> RewriteIfStatement(const shared_ptr<BoundIfStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteWhileStatement(const shared_ptr<BoundWhileStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteDoWhileStatement(const shared_ptr<BoundDoWhileStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteForStatement(const shared_ptr<BoundForStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteLabelStatement(const shared_ptr<BoundLabelStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteGotoStatement(const shared_ptr<BoundGotoStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteConditionalGotoStatement(const shared_ptr<BoundConditionalGotoStatement>& node);
	virtual shared_ptr<BoundStatement> RewriteExpressionStatement(const shared_ptr<BoundExpressionStatement>& node);

	virtual shared_ptr<BoundExpression> RewriteErrorExpression(const shared_ptr<BoundErrorExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteLiteralExpression(const shared_ptr<BoundLiteralExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteVariableExpression(const shared_ptr<BoundVariableExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteAssignmentExpression(const shared_ptr<BoundAssignmentExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteUnaryExpression(const shared_ptr<BoundUnaryExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteBinaryExpression(const shared_ptr<BoundBinaryExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteCallExpression(const shared_ptr<BoundCallExpression>& node);
	virtual shared_ptr<BoundExpression> RewriteConversionExpression(const shared_ptr<BoundConversionExpression>& node);
	virtual shared_ptr<BoundExpression> RewritePostfixExpression(const shared_ptr<BoundPostfixExpression>& node);

public:
	virtual ~BoundTreeRewriter() = default;

	virtual shared_ptr<BoundStatement> RewriteStatement(const shared_ptr<BoundStatement>& node);
	virtual shared_ptr<BoundExpression> RewriteExpression(const shared_ptr<BoundExpression>& node);
};

class Lowerer final :public BoundTreeRewriter
{
private:
	size_t _labelCount{ 0 };

	Lowerer() = default;
	BoundLabel GenerateLabel();
	static unique_ptr<BoundBlockStatement> Flatten(const shared_ptr<BoundStatement>& statement);

protected:
	shared_ptr<BoundStatement> RewriteIfStatement(const shared_ptr<BoundIfStatement>& node)override;
	shared_ptr<BoundStatement> RewriteWhileStatement(const shared_ptr<BoundWhileStatement>& node)override;
	shared_ptr<BoundStatement> RewriteDoWhileStatement(const shared_ptr<BoundDoWhileStatement>& node)override;
	shared_ptr<BoundStatement> RewriteForStatement(const shared_ptr<BoundForStatement>& node)override;

public:
	static unique_ptr<BoundBlockStatement> Lower(const shared_ptr<BoundStatement>& statement);
};

}//MCF
