#include "stdafx.h"
#include "BoundNode.h"

#include <iostream>
#include <sstream>

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "helpers.h"
#include "SyntaxKind.h"

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
	std::stringstream ss;
	WriteTo(ss);
	return ss.str();
}

BoundNodePrinter::BoundNodePrinter(std::ostream & out)
	:_writer(out)
{
}

void BoundNodePrinter::Write(const BoundNode* node)
{
	switch (node->Kind())
	{
		case BoundNodeKind::BlockStatement:
		{
			auto p = dynamic_cast<const BoundBlockStatement*>(node);
			if (p) WriteBlockStatement(p);
			break;
		}
		case BoundNodeKind::VariableDeclaration:
		{
			auto p = dynamic_cast<const BoundVariableDeclaration*>(node);
			if (p) WriteVariableDeclaration(p);
			break;
		}
		case BoundNodeKind::IfStatement:
		{
			auto p = dynamic_cast<const BoundIfStatement*>(node);
			if (p) WriteIfStatement(p);
			break;
		}
		case BoundNodeKind::WhileStatement:
		{
			auto p = dynamic_cast<const BoundWhileStatement*>(node);
			if (p) WriteWhileStatement(p);
			break;
		}
		case BoundNodeKind::DoWhileStatement:
		{
			auto p = dynamic_cast<const BoundDoWhileStatement*>(node);
			if (p) WriteDoWhileStatement(p);
			break;
		}
		case BoundNodeKind::ForStatement:
		{
			auto p = dynamic_cast<const BoundForStatement*>(node);
			if (p) WriteForStatement(p);
			break;
		}
		case BoundNodeKind::LabelStatement:
		{
			auto p = dynamic_cast<const BoundLabelStatement*>(node);
			if (p) WriteLabelStatement(p);
			break;
		}
		case BoundNodeKind::GotoStatement:
		{
			auto p = dynamic_cast<const BoundGotoStatement*>(node);
			if (p) WriteGotoStatement(p);
			break;
		}
		case BoundNodeKind::ConditionalGotoStatement:
		{
			auto p = dynamic_cast<const BoundConditionalGotoStatement*>(node);
			if (p) WriteConditionalGotoStatement(p);
			break;
		}
		case BoundNodeKind::ExpressionStatement:
		{
			auto p = dynamic_cast<const BoundExpressionStatement*>(node);
			if (p) WriteExpressionStatement(p);
			break;
		}
		case BoundNodeKind::ErrorExpression:
		{
			auto p = dynamic_cast<const BoundErrorExpression*>(node);
			if (p) WriteErrorExpression(p);
			break;
		}
		case BoundNodeKind::LiteralExpression:
		{
			auto p = dynamic_cast<const BoundLiteralExpression*>(node);
			if (p) WriteLiteralExpression(p);
			break;
		}
		case BoundNodeKind::VariableExpression:
		{
			auto p = dynamic_cast<const BoundVariableExpression*>(node);
			if (p) WriteVariableExpression(p);
			break;
		}
		case BoundNodeKind::AssignmentExpression:
		{
			auto p = dynamic_cast<const BoundAssignmentExpression*>(node);
			if (p) WriteAssignmentExpression(p);
			break;
		}
		case BoundNodeKind::UnaryExpression:
		{
			auto p = dynamic_cast<const BoundUnaryExpression*>(node);
			if (p) WriteUnaryExpression(p);
			break;
		}
		case BoundNodeKind::BinaryExpression:
		{
			auto p = dynamic_cast<const BoundBinaryExpression*>(node);
			if (p) WriteBinaryExpression(p);
			break;
		}
		case BoundNodeKind::CallExpression:
		{
			auto p = dynamic_cast<const BoundCallExpression*>(node);
			if (p) WriteCallExpression(p);
			break;
		}
		case BoundNodeKind::ConversionExpression:
		{
			auto p = dynamic_cast<const BoundConversionExpression*>(node);
			if (p) WriteConversionExpression(p);
			break;
		}
		case BoundNodeKind::PostfixExpression:
		{
			auto p = dynamic_cast<const BoundPostfixExpression*>(node);
			if (p) WritePosifixExpression(p);
			break;
		}
		default:
			throw std::invalid_argument("Unexpected node " + GetEnumText(node->Kind()));
	}
}

void BoundNodePrinter::WriteNestedStatement(const BoundStatement * node)
{
	auto p = dynamic_cast<const BoundBlockStatement*>(node);
	auto needsIndentation = p == nullptr;
	if (needsIndentation) _writer.Indent();
	Write(node);
	if (needsIndentation) _writer.Dedent();
}

void BoundNodePrinter::WriteNestedExpression(int parentPrecedence,
	const BoundExpression * node)
{
	auto u = dynamic_cast<const BoundUnaryExpression*>(node);
	auto b = dynamic_cast<const BoundBinaryExpression*>(node);
	if (u)
		WriteNestedExpression(parentPrecedence,
			GetUnaryOperatorPrecedence(u->Op()->SyntaxKind()), u);
	else if (b)
		WriteNestedExpression(parentPrecedence,
			GetBinaryOperatorPrecedence(b->Op()->SyntaxKind()), b);
	else
		Write(node);
}

void BoundNodePrinter::WriteNestedExpression(int parentPrecedence,
	int currentPrecedence, const BoundExpression * node)
{
	auto needsParenthesis = parentPrecedence >= currentPrecedence;
	if (needsParenthesis)_writer.WritePunctuation("(");
	Write(node);
	if (needsParenthesis)_writer.WritePunctuation(")");
}

void BoundNodePrinter::WriteBlockStatement(const BoundBlockStatement * node)
{
	_writer.WritePunctuation("{");
	_writer.Indent();
	_writer.WriteLine();
	for (const auto& it : node->Statements())
		Write(it.get());
	_writer.Dedent();
	_writer.WritePunctuation("}");
	_writer.WriteLine();
}

void BoundNodePrinter::WriteVariableDeclaration(const BoundVariableDeclaration * node)
{
	_writer.WriteKeyword(node->Variable()->IsReadOnly() ? "let " : "var ");
	_writer.WriteIdentifier(node->Variable()->Name());
	_writer.WritePunctuation(" = ");
	Write(node->Initializer().get());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteIfStatement(const BoundIfStatement * node)
{
	_writer.WriteKeyword("if ");
	Write(node->Condition().get());
	_writer.WriteLine();
	WriteNestedStatement(node->ThenStatement().get());

	if (node->ElseStatement() != nullptr)
	{
		_writer.WriteKeyword("else");
		_writer.WriteLine();
		WriteNestedStatement(node->ElseStatement().get());
	}
}

void BoundNodePrinter::WriteWhileStatement(const BoundWhileStatement * node)
{
	_writer.WriteKeyword("while ");
	Write(node->Condition().get());
	_writer.WriteLine();
	WriteNestedStatement(node->Body().get());
}

void BoundNodePrinter::WriteDoWhileStatement(const BoundDoWhileStatement * node)
{
	_writer.WriteKeyword("do");
	_writer.WriteLine();
	WriteNestedStatement(node->Body().get());
	_writer.WriteKeyword("while ");
	Write(node->Condition().get());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteForStatement(const BoundForStatement * node)
{
	_writer.WriteKeyword("for ");
	_writer.WriteIdentifier(node->Variable()->Name());
	_writer.WritePunctuation(" = ");
	Write(node->LowerBound().get());
	_writer.WriteKeyword(" to ");
	Write(node->UpperBound().get());
	_writer.WriteLine();
	WriteNestedStatement(node->Body().get());
}

void BoundNodePrinter::WriteLabelStatement(const BoundLabelStatement * node)
{
	_writer.Dedent();
	_writer.WritePunctuation(node->Label().Name());
	_writer.WritePunctuation(":");
	_writer.WriteLine();
	_writer.Indent();
}

void BoundNodePrinter::WriteGotoStatement(const BoundGotoStatement * node)
{
	_writer.WriteKeyword("goto ");
	_writer.WriteIdentifier(node->Label().Name());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteConditionalGotoStatement(const BoundConditionalGotoStatement * node)
{
	_writer.WriteKeyword("goto ");
	_writer.WriteIdentifier(node->Label().Name());
	_writer.WriteKeyword(node->JumpIfTrue() ? " if " : " unless ");
	Write(node->Condition().get());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteExpressionStatement(const BoundExpressionStatement * node)
{
	Write(node->Expression().get());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteErrorExpression(const BoundErrorExpression * node)
{
	_writer.WriteKeyword("?");
}

void BoundNodePrinter::WriteLiteralExpression(const BoundLiteralExpression * node)
{
	auto value = node->Value().ToString();
	if (node->Type() == TypeSymbol::GetType(TypeEnum::Bool))
		_writer.WriteKeyword(value);
	else if (node->Type() == TypeSymbol::GetType(TypeEnum::Int))
		_writer.WriteNumber(value);
	else if (node->Type() == TypeSymbol::GetType(TypeEnum::String))
		_writer.WriteKeyword(value);
	else
		throw std::invalid_argument("Unexpected type " + node->Type().Name());
}

void BoundNodePrinter::WriteVariableExpression(const BoundVariableExpression * node)
{
	_writer.WriteIdentifier(node->Variable()->Name());
}

void BoundNodePrinter::WriteAssignmentExpression(const BoundAssignmentExpression * node)
{
	_writer.WriteIdentifier(node->Variable()->Name());
	_writer.WritePunctuation(" = ");
	Write(node->Expression().get());
}

void BoundNodePrinter::WriteUnaryExpression(const BoundUnaryExpression * node)
{
	auto op = GetText(node->Op()->SyntaxKind());
	auto precedence = GetUnaryOperatorPrecedence(node->Op()->SyntaxKind());
	_writer.WritePunctuation(op);
	WriteNestedExpression(precedence, node->Operand().get());
}

void BoundNodePrinter::WriteBinaryExpression(const BoundBinaryExpression * node)
{
	auto op = GetText(node->Op()->SyntaxKind());
	auto precedence = GetBinaryOperatorPrecedence(node->Op()->SyntaxKind());
	WriteNestedExpression(precedence, node->Left().get());
	_writer.Write(" ");
	_writer.WritePunctuation(op);
	_writer.Write(" ");
	WriteNestedExpression(precedence, node->Right().get());
}

void BoundNodePrinter::WriteCallExpression(const BoundCallExpression * node)
{
	_writer.WriteIdentifier(node->Function()->Name());
	_writer.WritePunctuation("(");

	auto isFirst = true;
	for (const auto& it : node->Arguments())
	{
		if (isFirst)
			isFirst = false;
		else
			_writer.WritePunctuation(", ");

		Write(it.get());
	}
	_writer.WritePunctuation(")");
}

void BoundNodePrinter::WriteConversionExpression(const BoundConversionExpression * node)
{
	_writer.WriteIdentifier(node->Type().Name());
	_writer.WritePunctuation("(");
	Write(node->Expression().get());
	_writer.WritePunctuation(")");
}

void BoundNodePrinter::WritePosifixExpression(const BoundPostfixExpression * node)
{
}

}//MCF
