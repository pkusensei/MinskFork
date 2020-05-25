#include "BoundNode.h"

#include <iostream>
#include <sstream>

#include "BoundNodePrinter.h"

namespace MCF {

string_view nameof(BoundNodeKind  kind)
{
#define NAME(kind) \
case BoundNodeKind::kind: return #kind;

	switch (kind)
	{
		NAME(BlockStatement);
		NAME(NopStatement);
		NAME(VariableDeclaration);
		NAME(IfStatement);
		NAME(WhileStatement);
		NAME(DoWhileStatement);
		NAME(ForStatement);
		NAME(LabelStatement);
		NAME(GotoStatement);
		NAME(ConditionalGotoStatement);
		NAME(ReturnStatement);
		NAME(ExpressionStatement);

		NAME(ErrorExpression);
		NAME(LiteralExpression);
		NAME(VariableExpression);
		NAME(AssignmentExpression);
		NAME(UnaryExpression);
		NAME(BinaryExpression);
		NAME(CallExpression);
		NAME(ConversionExpression);
		NAME(PostfixExpression);

		default:
			return string_view();
	}

#undef NAME
}

void BoundNode::WriteTo(std::ostream& out) const
{
	Write(*this, out);
}

string BoundNode::ToString() const
{
	std::ostringstream ss;
	WriteTo(ss);
	return ss.str();
}

}//MCF
