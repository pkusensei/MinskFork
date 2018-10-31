#pragma once

#include <vector>
#include <memory>

#include "common.h"

namespace MCF {

class SyntaxNode;
class SyntaxToken;
class DiagnosticBag;
enum class SyntaxKind;
class ExpressionSyntax;
class SyntaxTree;

class Parser final
{
private:
	vector<unique_ptr<SyntaxToken>> _tokens;
	size_t _position;
	unique_ptr<DiagnosticBag> _diagnostics;

	SyntaxToken* Peek(int offset) const;
	SyntaxToken* Current() const;
	SyntaxToken* NextToken();
	SyntaxToken* MatchToken(SyntaxKind kind);

	unique_ptr<ExpressionSyntax> ParseExpression();
	unique_ptr<ExpressionSyntax> ParseAssignmentExpression();
	unique_ptr<ExpressionSyntax> ParseBinaryExpression(int parentPrecedence = 0);
	unique_ptr<ExpressionSyntax> ParsePrimaryExpression();
public:
	explicit Parser(const string& text);
	~Parser() = default;

	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
	SyntaxTree Parse();

	static int GetUnaryOperatorPrecedence(SyntaxKind kind);
	static int GetBinaryOperatorPrecedence(SyntaxKind kind);
};

class SyntaxTree final
{
private:
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<ExpressionSyntax> _root;
	unique_ptr<SyntaxToken> _endOfFileToken;
public:
	SyntaxTree(unique_ptr<DiagnosticBag>& diagnostics, unique_ptr<ExpressionSyntax>& root,
			   SyntaxToken& endOfFileToken);
	~SyntaxTree() = default;
	SyntaxTree(SyntaxTree&& other);

	const ExpressionSyntax* Root()const { return _root.get(); }
	const SyntaxToken* EndOfFileToken() const { return _endOfFileToken.get(); }

	MCF_API static SyntaxTree Parse(const string& text);
};

}//MCF