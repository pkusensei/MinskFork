#include "stdafx.h"
#include "Parser.h"

#include "SyntaxNode.h"
#include "Diagnostic.h"
#include "SourceText.h"

namespace MCF {

Parser::Parser(const SourceText& text)
	:_text(text), _position(0), _diagnostics(std::make_unique<DiagnosticBag>())
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
	_diagnostics->AddRange(*lexer.Diagnostics());
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

unique_ptr<StatementSyntax> Parser::ParseStatement()
{
	switch (Current()->Kind())
	{
		case SyntaxKind::OpenBraceToken:
			return ParseBlockStatement();
		case SyntaxKind::LetKeyword:
		case SyntaxKind::VarKeyword:
			return ParseVariableDeclaration();
		default:
			return ParseExpressionStatement();
	}
}

unique_ptr<StatementSyntax> Parser::ParseBlockStatement()
{
	auto statements = vector<unique_ptr<StatementSyntax>>();
	auto openBraceToken = MatchToken(SyntaxKind::OpenBraceToken);

	while (Current()->Kind() != SyntaxKind::EndOfFileToken &&
		   Current()->Kind() != SyntaxKind::CloseBraceToken)
	{
		statements.emplace_back(ParseStatement());
	}
	auto closeBraceToken = MatchToken(SyntaxKind::CloseBraceToken);
	return std::make_unique<BlockStatementSyntax>(*openBraceToken, statements, *closeBraceToken);
}

unique_ptr<StatementSyntax> Parser::ParseVariableDeclaration()
{
	auto expected = Current()->Kind() == SyntaxKind::LetKeyword ? SyntaxKind::LetKeyword
		: SyntaxKind::VarKeyword;
	auto keyword = MatchToken(expected);
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto equals = MatchToken(SyntaxKind::EqualsToken);
	auto initializer = ParseExpression();

	return std::make_unique<VariableDeclarationSyntax>(*keyword, *identifier, *equals, initializer);
}

unique_ptr<StatementSyntax> Parser::ParseExpressionStatement()
{
	auto expression = ParseExpression();
	return std::make_unique<ExpressionStatementSyntax>(expression);
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

unique_ptr<CompilationUnitSyntax> Parser::ParseCompilationUnit()
{
	auto statement = ParseStatement();
	auto endOfFileToken = MatchToken(SyntaxKind::EndOfFileToken);
	return std::make_unique<CompilationUnitSyntax>(statement, *endOfFileToken);
}

SyntaxTree::SyntaxTree(const SourceText& text)
	:_text(std::make_unique<SourceText>(text)),_diagnostics(std::make_unique<DiagnosticBag>())
{
	Parser parser(text);
	_root = parser.ParseCompilationUnit();
	_diagnostics->AddRange(*parser.Diagnostics());
}

SyntaxTree::SyntaxTree(SyntaxTree && other)
{
	_text.swap(other._text);
	_diagnostics.swap(other._diagnostics);
	_root.swap(other._root);
}

SyntaxTree SyntaxTree::Parse(const string & text)
{
	auto sourceText = SourceText::From(text);
	return Parse(sourceText);
}

SyntaxTree SyntaxTree::Parse(const SourceText & text)
{
	return SyntaxTree(text);
}

}//MCF