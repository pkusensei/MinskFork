#pragma once

#include "common.h"

namespace MCF {

class SyntaxToken;
class DiagnosticBag;
class ExpressionSyntax;
class StatementSyntax;
class CompilationUnitSyntax;
class SyntaxTree;
class SourceText;

class Parser final
{
private:
	const SourceText& _text;
	vector<unique_ptr<SyntaxToken>> _tokens;
	size_t _position;
	unique_ptr<DiagnosticBag> _diagnostics;

	SyntaxToken* Peek(int offset) const;
	SyntaxToken* Current() const;
	SyntaxToken* NextToken();
	SyntaxToken* MatchToken(SyntaxKind kind);

	unique_ptr<StatementSyntax> ParseStatement();
	unique_ptr<StatementSyntax> ParseBlockStatement();
	unique_ptr<StatementSyntax> ParseVariableDeclaration();
	unique_ptr<StatementSyntax> ParseExpressionStatement();

	unique_ptr<ExpressionSyntax> ParseExpression();
	unique_ptr<ExpressionSyntax> ParseAssignmentExpression();
	unique_ptr<ExpressionSyntax> ParseBinaryExpression(int parentPrecedence = 0);
	unique_ptr<ExpressionSyntax> ParsePrimaryExpression();

	unique_ptr<ExpressionSyntax> ParseParenthesizedExpression();
	unique_ptr<ExpressionSyntax> ParseBooleanLiteral();
	unique_ptr<ExpressionSyntax> ParseNumberLiteral();
	unique_ptr<ExpressionSyntax> ParseNameExpression();

public:
	//Parser() :Parser("") {}
	explicit Parser(const SourceText& text);
	~Parser() = default;

	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
	unique_ptr<CompilationUnitSyntax> ParseCompilationUnit();
};

class MCF_API SyntaxTree final
{
private:
	unique_ptr<SourceText> _text;
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<CompilationUnitSyntax> _root;

	SyntaxTree(const SourceText& text);

public:
	~SyntaxTree() = default;
	SyntaxTree(SyntaxTree&& other);

	const SourceText& Text() const { return *_text; }
	const CompilationUnitSyntax* Root()const { return _root.get(); }
	DiagnosticBag* Diagnostics() const { return _diagnostics.get(); }

	static SyntaxTree Parse(const string& text);
	static SyntaxTree Parse(const SourceText& text);
	static vector<unique_ptr<SyntaxToken>> ParseTokens(const string& text);
	static vector<unique_ptr<SyntaxToken>> ParseTokens(const SourceText& text);
};

}//MCF