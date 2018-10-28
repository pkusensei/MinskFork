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
	std::vector<SyntaxToken> _tokens;
	int _position;
	std::unique_ptr<DiagnosticBag> _diagnostics;

	SyntaxToken Peek(int offset) const;
	SyntaxToken Current() const;
	SyntaxToken NextToken();
	SyntaxToken MatchToken(SyntaxKind kind);

	//ExpressionSyntax ParseExpression() const;
	//ExpressionSyntax ParseAssignmentExpression() const;
	//ExpressionSyntax ParseBinaryExpression(int parentPrecedence = 0) const;
	//ExpressionSyntax ParsePrimaryExpression() const;
public:
	MCF_API Parser(std::string text);
	MCF_API ~Parser();

	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
	SyntaxTree Parse();
};

class SyntaxTree
{
private:
	std::unique_ptr<DiagnosticBag> _diagnostics;
	std::unique_ptr<ExpressionSyntax> _root;
	std::unique_ptr<SyntaxToken> _endOfFileToken;
public:
	SyntaxTree(std::unique_ptr<DiagnosticBag>& diagnostics, std::unique_ptr<ExpressionSyntax>& root,
			   SyntaxToken& endOfFileToken);

	static SyntaxTree Parse(const std::string& text);
};

}//MCF