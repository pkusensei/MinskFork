#include "stdafx.h"
#include "Parser.h"
#include "SyntaxNode.h"
#include "Diagnostic.h"

namespace MCF {

Parser::Parser(const string& text)
	:_position(0), _diagnostics(std::make_unique<DiagnosticBag>())
{
	_tokens = vector<unique_ptr<SyntaxToken>>();
	Lexer lexer(text);
	unique_ptr<SyntaxToken> pToken;
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
		return _tokens.back().get();
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

unique_ptr<ExpressionSyntax> Parser::ParseExpression()
{
	return ParseAssignmentExpression();
}

unique_ptr<ExpressionSyntax> Parser::ParseAssignmentExpression()
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

unique_ptr<ExpressionSyntax> Parser::ParseBinaryExpression(int parentPrecedence)
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

unique_ptr<ExpressionSyntax> Parser::ParsePrimaryExpression()
{
	switch (Current()->Kind())
	{
		case SyntaxKind::OpenParenthesisToken:
			return ParseParenthesizedExpression();
		case SyntaxKind::TrueKeyword:
		case SyntaxKind::FalseKeyword:
			return ParseBooleanLiteral();
		case SyntaxKind::NumberToken:
			return ParseNumberLiteral();
		case SyntaxKind::IdentifierToken:
		default:		
			return ParseNameExpression();
	}
}

unique_ptr<ExpressionSyntax> Parser::ParseParenthesizedExpression()
{
	auto left = MatchToken(SyntaxKind::OpenParenthesisToken);
	auto expression = ParseExpression();
	auto right = MatchToken(SyntaxKind::CloseParenthesisToken);
	return std::make_unique<ParenthesizedExpressionSyntax>(*left, expression, *right);
}

unique_ptr<ExpressionSyntax> Parser::ParseBooleanLiteral()
{
	auto isTrue = Current()->Kind() == SyntaxKind::TrueKeyword;
	auto keywordToken = isTrue ? MatchToken(SyntaxKind::TrueKeyword) : MatchToken(SyntaxKind::FalseKeyword);
	return std::make_unique<LiteralExpressionSyntax>(*keywordToken, isTrue);
}

unique_ptr<ExpressionSyntax> Parser::ParseNumberLiteral()
{
	auto numberToken = MatchToken(SyntaxKind::NumberToken);
	return std::make_unique<LiteralExpressionSyntax>(*numberToken);
}

unique_ptr<ExpressionSyntax> Parser::ParseNameExpression()
{
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	return std::make_unique<NameExpressionSyntax>(*identifier);
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

SyntaxTree::SyntaxTree(unique_ptr<DiagnosticBag>& diagnostics, unique_ptr<ExpressionSyntax>& root,
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

SyntaxTree SyntaxTree::Parse(const string & text)
{
	Parser parser(text);
	return parser.Parse();
}

}//MCF