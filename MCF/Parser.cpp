#include "stdafx.h"
#include "Parser.h"
#include "SyntaxNode.h"
#include "Diagnostic.h"

namespace MCF {

Parser::Parser(std::string text)
{
	_tokens = std::vector<SyntaxToken>();
	Lexer lexer(text);
	std::unique_ptr<SyntaxToken> pToken;
	do
	{
		pToken = std::make_unique<SyntaxToken>(lexer.Lex());
		if (pToken->Kind() != SyntaxKind::WhitespaceToken &&
			pToken->Kind() != SyntaxKind::BadToken)
			_tokens.emplace_back(*pToken);
	} while (pToken != nullptr &&
			 pToken->Kind() != SyntaxKind::EndOfFileToken);
	_diagnostics->AddRange(*lexer.Diagnostics());
}

Parser::~Parser()
{
}

SyntaxToken Parser::Peek(int offset) const
{
	auto idx = _position + offset;
	if (idx >= _tokens.size())
		return *_tokens.end();
	return _tokens[idx];
}

SyntaxToken Parser::Current() const
{
	return Peek(0);
}

SyntaxToken Parser::NextToken()
{
	auto current = Current();
	_position++;
	return current;
}

SyntaxToken Parser::MatchToken(SyntaxKind kind)
{
	auto current = Current();
	if (current.Kind() == kind)
		return NextToken();
	_diagnostics->ReportUnexpectedToken(current.Span(), current.Kind(), kind);
	return SyntaxToken(kind, current.Position(), "", NULL);
}

SyntaxTree Parser::Parse()
{
	// TODO
	auto expression = std::make_unique<ExpressionSyntax>(ParseExpression());
	auto endOfFileToken = MatchToken(SyntaxKind::EndOfFileToken);
	return SyntaxTree(_diagnostics, expression, endOfFileToken);
}

SyntaxTree::SyntaxTree(std::unique_ptr<DiagnosticBag>& diagnostics, std::unique_ptr<ExpressionSyntax>& root, 
					   SyntaxToken& endOfFileToken)
{
	_root.swap(root);
	_diagnostics.swap(diagnostics);
	_endOfFileToken = std::make_unique<SyntaxToken>(endOfFileToken);
}

SyntaxTree SyntaxTree::Parse(const std::string & text)
{
	Parser parser(text);
	return parser.Parse();
}

}//MCF