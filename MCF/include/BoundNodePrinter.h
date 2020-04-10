#pragma once

#include "IO.h"

namespace MCF {

class BoundNode;

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
class BoundReturnStatement;
class BoundConditionalGotoStatement;

class BoundNodePrinter final
{
private:
	IndentedTextWriter _writer;

	void WriteNestedStatement(const BoundStatement& node);
	void WriteNestedExpression(int parentPrecedence, const BoundExpression& node);
	void WriteNestedExpression(int parentPrecedence, int currentPrecedence,
		const BoundExpression& node);

	void WriteBlockStatement(const BoundBlockStatement& node);
	void WriteVariableDeclaration(const BoundVariableDeclaration& node);
	void WriteIfStatement(const BoundIfStatement& node);
	void WriteWhileStatement(const BoundWhileStatement& node);
	void WriteDoWhileStatement(const BoundDoWhileStatement& node);
	void WriteForStatement(const BoundForStatement& node);
	void WriteLabelStatement(const BoundLabelStatement& node);
	void WriteGotoStatement(const BoundGotoStatement& node);
	void WriteConditionalGotoStatement(const BoundConditionalGotoStatement& node);
	void WriteReturnStatement(const BoundReturnStatement& node);
	void WriteExpressionStatement(const BoundExpressionStatement& node);
	void WriteErrorExpression();
	void WriteLiteralExpression(const BoundLiteralExpression& node);
	void WriteVariableExpression(const BoundVariableExpression& node);
	void WriteAssignmentExpression(const BoundAssignmentExpression& node);
	void WriteUnaryExpression(const BoundUnaryExpression& node);
	void WriteBinaryExpression(const BoundBinaryExpression& node);
	void WriteCallExpression(const BoundCallExpression& node);
	void WriteConversionExpression(const BoundConversionExpression& node);
	void WritePosifixExpression(const BoundPostfixExpression& node);

public:
	constexpr explicit BoundNodePrinter(std::ostream& out)noexcept
		:_writer(out)
	{
	}

	void Write(const BoundNode& node);
};

}//MCF
