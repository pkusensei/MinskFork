#include "stdafx.h"
#include "Parser.h"
#include "SyntaxNode.h"
#include "Diagnostic.h"

namespace MCF {

Parser::Parser(const std::string& text)
{
	_tokens = std::vector<std::unique_ptr<SyntaxToken>>();
	Lexer lexer(text);
	std::unique_ptr<SyntaxToken> pToken;
	SyntaxKind kind;
	do
	{
		pToken = std::make_unique<SyntaxToken>(lexer.Lex());
		if (pToken->Kind() != SyntaxKind::WhitespaceToken &&
			pToken->Kind() != SyntaxKind::BadToken)
		{
			kind = pToken->Kind();
			_tokens.emplace_back(std::move(pToken));
		}
	} while (kind != SyntaxKind::EndOfFileToken);
	// TODO
	//_diagnostics->AddRange(*lexer.Diagnostics());
}

SyntaxToken* Parser::Peek(int offset) const
{
	auto idx = _position + offset;
	if (idx >= _tokens.size())
		return _tokens.end()->get();
	return _tokens[idx].get();
}

SyntaxToken* Parser::Current() const
{
	return Peek(0);
}

SyntaxToken* Parser::NextToken()
{
	auto current = Current();
	_position++;
	return current;
}

SyntaxToken* Parser::MatchToken(SyntaxKind kind)
{
	auto current = Current();
	if (current->Kind() == kind)
		return NextToken();
	_diagnostics->ReportUnexpectedToken(current->Span(), current->Kind(), kind);
	// HACK
	return current;
}

std::unique_ptr<ExpressionSyntax> Parser::ParseExpression()
{
	return ParseAssignmentExpression();
}

std::unique_ptr<ExpressionSyntax> Parser::ParseAssignmentExpression()
{
	if (Peek(0)->Kind() == SyntaxKind::IdentifierToken &&
		Peek(1)->Kind() == SyntaxKind::EqualsToken)
	{
		auto identifierToken = NextToken();
		auto operatorToken = NextToken();
		auto right = ParseAssignmentExpression();
		return std::make_unique<AssignmentExpressionSyntax>(*identifierToken, *operatorToken, right);
	}
	return ParseBinaryExpression();
}

std::unique_ptr<ExpressionSyntax> Parser::ParseBinaryExpression(int parentPrecedence)
{
	auto left = std::make_unique<ExpressionSyntax>();
	auto unaryOperatorPrecedence = GetUnaryOperatorPrecedence(Current()->Kind());
	if (unaryOperatorPrecedence != 0 &&
		unaryOperatorPrecedence >= parentPrecedence)
	{
		auto operatorToken = NextToken();
		auto operand = ParseBinaryExpression(unaryOperatorPrecedence);
		left = std::make_unique<UnaryExpressionSyntax>(*operatorToken, operand);
	} else
	{
		left = ParsePrimaryExpression();
	}

	while (true)
	{
		auto precedence = GetBinaryOperatorPrecedence(Current()->Kind());
		if (precedence == 0 || precedence <= parentPrecedence)
			break;
		auto operatorToken = NextToken();
		auto right = ParseBinaryExpression(precedence);
		left = std::make_unique<BinaryExpressionSyntax>(left, *operatorToken, right);
	}
	return left;
}

std::unique_ptr<ExpressionSyntax> Parser::ParsePrimaryExpression()
{
	switch (Current()->Kind())
	{
		case SyntaxKind::OpenParenthesisToken:
		{
			auto left = NextToken();
			auto expression = ParseExpression();
			auto right = MatchToken(SyntaxKind::CloseParenthesisToken);
			return std::make_unique<ParenthesizedExpressionSyntax>(*left, expression, *right);
		}
		case SyntaxKind::TrueKeyword:
		case SyntaxKind::FalseKeyword:
		{
			auto keywordToken = NextToken();
			auto value = ValueType(keywordToken->Kind() == SyntaxKind::TrueKeyword);
			return std::make_unique<LiteralExpressionSyntax>(*keywordToken, value);
		}
		case SyntaxKind::IdentifierToken:
		{
			auto identifier = NextToken();
			return std::make_unique<NameExpressionSyntax>(*identifier);
		}
		default:
		{
			auto numberToken = NextToken();
			return std::make_unique<LiteralExpressionSyntax>(*numberToken);
		}
	}
}

SyntaxTree Parser::Parse()
{
	auto expression = ParseExpression();
	auto endOfFileToken = MatchToken(SyntaxKind::EndOfFileToken);
	return SyntaxTree(_diagnostics, expression, *endOfFileToken);
}

int Parser::GetUnaryOperatorPrecedence(SyntaxKind kind)
{
	switch (kind)
	{

		case MCF::SyntaxKind::PlusToken:
		case MCF::SyntaxKind::MinusToken:
		case MCF::SyntaxKind::BangToken:
			return 6;
		default:
			return 0;
	}
}

int Parser::GetBinaryOperatorPrecedence(SyntaxKind kind)
{
	switch (kind)
	{
		case MCF::SyntaxKind::StarToken:
		case MCF::SyntaxKind::SlashToken:
			return 5;
		case MCF::SyntaxKind::PlusToken:
		case MCF::SyntaxKind::MinusToken:
			return 4;
		case MCF::SyntaxKind::EqualsEqualsToken:
		case MCF::SyntaxKind::BangEqualsToken:
			return 3;
		case MCF::SyntaxKind::AmpersandAmpersandToken:
			return 2;
		case MCF::SyntaxKind::PipePipeToken:
			return 1;
		default:
			return 0;
	}
}

SyntaxTree::SyntaxTree(std::unique_ptr<DiagnosticBag>& diagnostics, std::unique_ptr<ExpressionSyntax>& root,
					   SyntaxToken& endOfFileToken)
{
	_diagnostics.swap(diagnostics);
	_root.swap(root);
	_endOfFileToken = std::make_unique<SyntaxToken>(endOfFileToken);
}

SyntaxTree::SyntaxTree(SyntaxTree && other)
{
	_diagnostics.swap(other._diagnostics);
	_root.swap(other._root);
	_endOfFileToken.swap(other._endOfFileToken);
}

SyntaxTree SyntaxTree::Parse(const std::string & text)
{
	Parser parser(text);
	return parser.Parse();
}

}//MCF