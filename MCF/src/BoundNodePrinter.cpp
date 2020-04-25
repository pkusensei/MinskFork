#include "BoundNodePrinter.h"

#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "helpers.h"
#include "SyntaxKind.h"

namespace MCF {

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

void Write(const BoundNode& node, std::ostream& out)
{
	auto w = BoundNodePrinter(out);
	w.Write(node);
}

void BoundNodePrinter::Write(const BoundNode& node)
{
	switch (node.Kind())
	{
		case BoundNodeKind::BlockStatement:
		{
			auto& p = static_cast<const BoundBlockStatement&>(node);
			WriteBlockStatement(p);
			break;
		}
		case BoundNodeKind::VariableDeclaration:
		{
			auto& p = static_cast<const BoundVariableDeclaration&>(node);
			WriteVariableDeclaration(p);
			break;
		}
		case BoundNodeKind::IfStatement:
		{
			auto& p = static_cast<const BoundIfStatement&>(node);
			WriteIfStatement(p);
			break;
		}
		case BoundNodeKind::WhileStatement:
		{
			auto& p = static_cast<const BoundWhileStatement&>(node);
			WriteWhileStatement(p);
			break;
		}
		case BoundNodeKind::DoWhileStatement:
		{
			auto& p = static_cast<const BoundDoWhileStatement&>(node);
			WriteDoWhileStatement(p);
			break;
		}
		case BoundNodeKind::ForStatement:
		{
			auto& p = static_cast<const BoundForStatement&>(node);
			WriteForStatement(p);
			break;
		}
		case BoundNodeKind::LabelStatement:
		{
			auto& p = static_cast<const BoundLabelStatement&>(node);
			WriteLabelStatement(p);
			break;
		}
		case BoundNodeKind::GotoStatement:
		{
			auto& p = static_cast<const BoundGotoStatement&>(node);
			WriteGotoStatement(p);
			break;
		}
		case BoundNodeKind::ConditionalGotoStatement:
		{
			auto& p = static_cast<const BoundConditionalGotoStatement&>(node);
			WriteConditionalGotoStatement(p);
			break;
		}
		case BoundNodeKind::ReturnStatement:
		{
			auto& p = static_cast<const BoundReturnStatement&>(node);
			WriteReturnStatement(p);
			break;
		}
		case BoundNodeKind::ExpressionStatement:
		{
			auto& p = static_cast<const BoundExpressionStatement&>(node);
			WriteExpressionStatement(p);
			break;
		}
		case BoundNodeKind::ErrorExpression:
		{
			auto& p = static_cast<const BoundErrorExpression&>(node);
			static_cast<void>(p);
			WriteErrorExpression();
			break;
		}
		case BoundNodeKind::LiteralExpression:
		{
			auto& p = static_cast<const BoundLiteralExpression&>(node);
			WriteLiteralExpression(p);
			break;
		}
		case BoundNodeKind::VariableExpression:
		{
			auto& p = static_cast<const BoundVariableExpression&>(node);
			WriteVariableExpression(p);
			break;
		}
		case BoundNodeKind::AssignmentExpression:
		{
			auto& p = static_cast<const BoundAssignmentExpression&>(node);
			WriteAssignmentExpression(p);
			break;
		}
		case BoundNodeKind::UnaryExpression:
		{
			auto& p = static_cast<const BoundUnaryExpression&>(node);
			WriteUnaryExpression(p);
			break;
		}
		case BoundNodeKind::BinaryExpression:
		{
			auto& p = static_cast<const BoundBinaryExpression&>(node);
			WriteBinaryExpression(p);
			break;
		}
		case BoundNodeKind::CallExpression:
		{
			auto& p = static_cast<const BoundCallExpression&>(node);
			WriteCallExpression(p);
			break;
		}
		case BoundNodeKind::ConversionExpression:
		{
			auto& p = static_cast<const BoundConversionExpression&>(node);
			WriteConversionExpression(p);
			break;
		}
		case BoundNodeKind::PostfixExpression:
		{
			auto& p = static_cast<const BoundPostfixExpression&>(node);
			WritePosifixExpression(p);
			break;
		}
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected node: ", nameof(node.Kind())));
	}
}

void BoundNodePrinter::WriteNestedStatement(const BoundStatement& node)
{
	auto needsIndentation = node.Kind() == BoundNodeKind::BlockStatement;
	if (needsIndentation) _writer.Indent();
	Write(node);
	if (needsIndentation) _writer.Dedent();
}

void BoundNodePrinter::WriteNestedExpression(int parentPrecedence,
	const BoundExpression& node)
{
	if (node.Kind() == BoundNodeKind::UnaryExpression)
	{
		auto u = static_cast<const BoundUnaryExpression&>(node);
		WriteNestedExpression(parentPrecedence,
			GetUnaryOperatorPrecedence(u.Op().SynKind()), u);
	} else if (node.Kind() == BoundNodeKind::BinaryExpression)
	{
		auto b = static_cast<const BoundBinaryExpression&>(node);
		WriteNestedExpression(parentPrecedence,
			GetBinaryOperatorPrecedence(b.Op().SynKind()), b);
	} else
		Write(node);
}

void BoundNodePrinter::WriteNestedExpression(int parentPrecedence,
	int currentPrecedence, const BoundExpression& node)
{
	auto needsParenthesis = parentPrecedence >= currentPrecedence;
	if (needsParenthesis)
		_writer.WritePunctuation(SyntaxKind::OpenParenthesisToken);

	Write(node);

	if (needsParenthesis)
		_writer.WritePunctuation(SyntaxKind::CloseParenthesisToken);
}

void BoundNodePrinter::WriteBlockStatement(const BoundBlockStatement& node)
{
	_writer.WritePunctuation(SyntaxKind::OpenBraceToken);
	_writer.WriteLine();
	_writer.Indent();
	for (const auto& it : node.Statements())
		Write(*it);
	_writer.Dedent();
	_writer.WritePunctuation(SyntaxKind::CloseBraceToken);
	_writer.WriteLine();
}

void BoundNodePrinter::WriteVariableDeclaration(const BoundVariableDeclaration& node)
{
	_writer.WriteKeyword(node.Variable()->IsReadOnly() ?
		SyntaxKind::LetKeyword : SyntaxKind::VarKeyword);
	_writer.WriteSpace();
	_writer.WriteIdentifier(node.Variable()->Name());
	_writer.WriteSpace();
	_writer.WritePunctuation(SyntaxKind::EqualsToken);
	_writer.WriteSpace();
	Write(*node.Initializer());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteIfStatement(const BoundIfStatement& node)
{
	_writer.WriteKeyword(SyntaxKind::IfKeyword);
	_writer.WriteSpace();
	Write(*node.Condition());
	_writer.WriteLine();
	WriteNestedStatement(*node.ThenStatement());

	if (node.ElseStatement() != nullptr)
	{
		_writer.WriteKeyword(SyntaxKind::ElseKeyword);
		_writer.WriteLine();
		WriteNestedStatement(*node.ElseStatement());
	}
}

void BoundNodePrinter::WriteWhileStatement(const BoundWhileStatement& node)
{
	_writer.WriteKeyword(SyntaxKind::WhileKeyword);
	_writer.WriteSpace();
	Write(*node.Condition());
	_writer.WriteLine();
	WriteNestedStatement(*node.Body());
}

void BoundNodePrinter::WriteDoWhileStatement(const BoundDoWhileStatement& node)
{
	_writer.WriteKeyword(SyntaxKind::DoKeyword);
	_writer.WriteLine();
	WriteNestedStatement(*node.Body());
	_writer.WriteKeyword(SyntaxKind::WhileKeyword);
	_writer.WriteSpace();
	Write(*node.Condition());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteForStatement(const BoundForStatement& node)
{
	_writer.WriteKeyword(SyntaxKind::ForKeyword);
	_writer.WriteSpace();
	_writer.WriteIdentifier(node.Variable()->Name());
	_writer.WriteSpace();
	_writer.WritePunctuation(SyntaxKind::EqualsToken);
	_writer.WriteSpace();
	Write(*node.LowerBound());
	_writer.WriteSpace();
	_writer.WriteKeyword(SyntaxKind::ToKeyword);
	_writer.WriteSpace();
	Write(*node.UpperBound());
	_writer.WriteLine();
	WriteNestedStatement(*node.Body());
}

void BoundNodePrinter::WriteLabelStatement(const BoundLabelStatement& node)
{
	_writer.Dedent();
	_writer.WritePunctuation(node.Label().Name());
	_writer.WritePunctuation(SyntaxKind::ColonToken);
	_writer.WriteLine();
	_writer.Indent();
}

void BoundNodePrinter::WriteGotoStatement(const BoundGotoStatement& node)
{
	_writer.WriteKeyword("goto");
	_writer.WriteSpace();
	_writer.WriteIdentifier(node.Label().Name());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteConditionalGotoStatement(const BoundConditionalGotoStatement& node)
{
	_writer.WriteKeyword("goto");
	_writer.WriteSpace();
	_writer.WriteIdentifier(node.Label().Name());
	_writer.WriteSpace();
	_writer.WriteKeyword(node.JumpIfTrue() ? "if" : "unless");
	_writer.WriteSpace();
	Write(*node.Condition());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteReturnStatement(const BoundReturnStatement& node)
{
	_writer.WriteKeyword(SyntaxKind::ReturnKeyword);
	if (node.Expression() != nullptr)
	{
		_writer.WriteSpace();
		Write(*node.Expression());
	}
	_writer.WriteLine();
}

void BoundNodePrinter::WriteExpressionStatement(const BoundExpressionStatement& node)
{
	Write(*node.Expression());
	_writer.WriteLine();
}

void BoundNodePrinter::WriteErrorExpression()
{
	_writer.WriteKeyword("?");
}

void BoundNodePrinter::WriteLiteralExpression(const BoundLiteralExpression& node)
{
	auto value = node.Value().ToString();
	if (node.Type() == TypeSymbol(TypeEnum::Bool))
		_writer.WriteKeyword(value);
	else if (node.Type() == TypeSymbol(TypeEnum::Int))
		_writer.WriteNumber(value);
	else if (node.Type() == TypeSymbol(TypeEnum::String))
	{
		StringReplaceAll(value, "\"", "\"\"");
		value = '"' + value + '"';
		_writer.WriteString(value);
	} else
		throw std::invalid_argument(BuildStringFrom("Unexpected type: ", node.Type().Name()));
}

void BoundNodePrinter::WriteVariableExpression(const BoundVariableExpression& node)
{
	_writer.WriteIdentifier(node.Variable()->Name());
}

void BoundNodePrinter::WriteAssignmentExpression(const BoundAssignmentExpression& node)
{
	_writer.WriteIdentifier(node.Variable()->Name());
	_writer.WriteSpace();
	_writer.WritePunctuation(SyntaxKind::EqualsToken);
	_writer.WriteSpace();
	Write(*node.Expression());
}

void BoundNodePrinter::WriteUnaryExpression(const BoundUnaryExpression& node)
{
	auto precedence = GetUnaryOperatorPrecedence(node.Op().SynKind());
	_writer.WritePunctuation(node.Op().SynKind());
	WriteNestedExpression(precedence, *node.Operand());
}

void BoundNodePrinter::WriteBinaryExpression(const BoundBinaryExpression& node)
{
	auto precedence = GetBinaryOperatorPrecedence(node.Op().SynKind());
	WriteNestedExpression(precedence, *node.Left());
	_writer.WriteSpace();
	_writer.WritePunctuation(node.Op().SynKind());
	_writer.WriteSpace();
	WriteNestedExpression(precedence, *node.Right());
}

void BoundNodePrinter::WriteCallExpression(const BoundCallExpression& node)
{
	_writer.WriteIdentifier(node.Function()->Name());
	_writer.WritePunctuation(SyntaxKind::OpenParenthesisToken);

	auto isFirst = true;
	for (const auto& it : node.Arguments())
	{
		if (isFirst)
			isFirst = false;
		else
		{
			_writer.WritePunctuation(SyntaxKind::CommaToken);
			_writer.WriteSpace();
		}

		Write(*it);
	}
	_writer.WritePunctuation(SyntaxKind::CloseParenthesisToken);
}

void BoundNodePrinter::WriteConversionExpression(const BoundConversionExpression& node)
{
	_writer.WriteIdentifier(node.Type().Name());
	_writer.WritePunctuation(SyntaxKind::OpenParenthesisToken);
	Write(*node.Expression());
	_writer.WritePunctuation(SyntaxKind::CloseParenthesisToken);
}

void BoundNodePrinter::WritePosifixExpression(const BoundPostfixExpression& node)
{
	_writer.WriteIdentifier(node.Variable()->Name());
	_writer.WritePunctuation(node.OperatorKind() == BoundPostfixOperatorEnum::Increment ?
		SyntaxKind::PlusPlusToken : SyntaxKind::MinusMinusToken);
}

}//MCF
