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
	std::vector<std::unique_ptr<SyntaxToken>> _tokens;
	size_t _position;
	std::unique_ptr<DiagnosticBag> _diagnostics;

	SyntaxToken* Peek(int offset) const;
	SyntaxToken* Current() const;
	SyntaxToken* NextToken();
	SyntaxToken* MatchToken(SyntaxKind kind);

	std::unique_ptr<ExpressionSyntax> ParseExpression();
	std::unique_ptr<ExpressionSyntax> ParseAssignmentExpression();
	std::unique_ptr<ExpressionSyntax> ParseBinaryExpression(int parentPrecedence = 0);
	std::unique_ptr<ExpressionSyntax> ParsePrimaryExpression();
public:
	Parser(const std::string& text);
	~Parser() = default;

	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
	SyntaxTree Parse();

	static int GetUnaryOperatorPrecedence(SyntaxKind kind);
	static int GetBinaryOperatorPrecedence(SyntaxKind kind);
};

class SyntaxTree final
{
private:
	std::unique_ptr<DiagnosticBag> _diagnostics;
	std::unique_ptr<ExpressionSyntax> _root;
	std::unique_ptr<SyntaxToken> _endOfFileToken;
public:
	SyntaxTree(std::unique_ptr<DiagnosticBag>& diagnostics, std::unique_ptr<ExpressionSyntax>& root,
			   SyntaxToken& endOfFileToken);
	~SyntaxTree() = default;
	SyntaxTree(SyntaxTree&& other);

	const ExpressionSyntax* Root()const { return _root.get(); }
	const SyntaxToken* EndOfFileToken() const { return _endOfFileToken.get(); }

	MCF_API static SyntaxTree Parse(const std::string& text);
};

}//MCF