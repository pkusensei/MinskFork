#include "stdafx.h"
#include "BoundNode.h"

#include <iostream>
#include <sstream>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "helpers.h"

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
		case BoundNodeKind::ForStatement:
			return "ForStatement";
		case BoundNodeKind::LabelStatement:
			return "LabelStatement";
		case BoundNodeKind::GotoStatement:
			return "GotoStatement";
		case BoundNodeKind::ConditionalGotoStatement:
			return "ConditionalGotoStatement";
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

ConsoleColor BoundNode::GetColor(const BoundNode * node)
{
	auto e = dynamic_cast<const BoundExpression*>(node);
	auto s = dynamic_cast<const BoundStatement*>(node);
	if (e != nullptr)
		return ConsoleColor::Blue;
	else if (s != nullptr)
		return ConsoleColor::Cyan;
	else return ConsoleColor::Yellow;
}

string BoundNode::GetText(const BoundNode * node)
{
	auto b = dynamic_cast<const BoundBinaryExpression*>(node);
	auto u = dynamic_cast<const BoundUnaryExpression*>(node);
	if (b != nullptr)
		return GetEnumText(b->Op()->Kind()) + "Expression";
	else if (u != nullptr)
		return GetEnumText(u->Op()->Kind()) + "Expression";
	else return GetEnumText(node->Kind());
}

void BoundNode::PrettyPrint(std::ostream & out, const BoundNode * node, string indent, bool isLast)
{
	auto isToConsole = out.rdbuf() == std::cout.rdbuf();
	string marker = isLast ? "+--" : "---";//"©¸©¤©¤" : "©À©¤©¤";

	if (isToConsole)
		SetConsoleColor(ConsoleColor::Grey);
	out << indent << marker;

	if (isToConsole)
		SetConsoleColor(GetColor(node));
	out << GetText(node);

	auto isFirstProperty = true;
	auto properties = node->GetProperties();
	if (!properties.empty())
	{
		for (const auto p : properties)
		{
			if (isFirstProperty)
				isFirstProperty = false;
			else
			{
				if (isToConsole)
					SetConsoleColor(ConsoleColor::Grey);
				out << ",";
			}
			out << ' ';

			if (isToConsole)
				SetConsoleColor(ConsoleColor::Yellow);
			out << p.first;

			if (isToConsole)
				SetConsoleColor(ConsoleColor::Grey);
			out << " = ";

			if (isToConsole)
				SetConsoleColor(ConsoleColor::DarkYellow);
			out << p.second;
		}
	}

	if (isToConsole)
		ResetConsoleColor();

	out << "\n";
	indent += isLast ? "   " : "|  ";
	auto children = node->GetChildren();
	if (!children.empty())
	{
		auto lastChild = children.back();
		for (const auto& child : children)
			PrettyPrint(out, child, indent, lastChild == child);
	}
}

string BoundNode::ToString() const
{
	std::stringstream ss;
	WriteTo(ss);
	return ss.str();
}

}//MCF
