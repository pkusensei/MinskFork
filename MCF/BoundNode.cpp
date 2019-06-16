#include "stdafx.h"
#include "BoundNode.h"

#include <iostream>
#include <sstream>

#include "BoundNodePrinter.h"

namespace MCF {

string GetEnumText(const BoundNodeKind & kind)
{
	switch (kind)
	{
		case BoundNodeKind::BlockStatement:
			return "BlockStatement";
		case BoundNodeKind::VariableDeclaration:
			return "VariableDeclaration";
		case BoundNodeKind::IfStatement:
			return "IfStatement";
		case BoundNodeKind::WhileStatement:
			return "WhileStatement";
		case BoundNodeKind::DoWhileStatement:
			return "DoWhileStatement";
		case BoundNodeKind::ForStatement:
			return "ForStatement";
		case BoundNodeKind::LabelStatement:
			return "LabelStatement";
		case BoundNodeKind::GotoStatement:
			return "GotoStatement";
		case BoundNodeKind::ConditionalGotoStatement:
			return "ConditionalGotoStatement";
		case BoundNodeKind::ReturnStatement:
			return "ReturnStatement";
		case BoundNodeKind::ExpressionStatement:
			return "ExpressionStatement";

		case BoundNodeKind::ErrorExpression:
			return "ErrorExpression";
		case BoundNodeKind::LiteralExpression:
			return "LiteralExpression";
		case BoundNodeKind::VariableExpression:
			return "VariableExpression";
		case BoundNodeKind::AssignmentExpression:
			return "AssignmentExpression";
		case BoundNodeKind::UnaryExpression:
			return "UnaryExpression";
		case BoundNodeKind::BinaryExpression:
			return "BinaryExpression";
		case BoundNodeKind::CallExpression:
			return "CallExpression";
		case BoundNodeKind::ConversionExpression:
			return "ConversionExpression";
		case BoundNodeKind::PostfixExpression:
			return "PostfixExpression";

		default:
			return string();
	}
}

void BoundNode::WriteTo(std::ostream & out) const
{
	auto printer = BoundNodePrinter(out);
	printer.Write(this);
}

string BoundNode::ToString() const
{
	std::ostringstream ss;
	WriteTo(ss);
	return ss.str();
}

}//MCF
