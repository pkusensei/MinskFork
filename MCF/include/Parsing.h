#pragma once

#include <optional>

#include "SyntaxStatements.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

class DiagnosticBag;
class SourceText;

class MemberSyntax :public SyntaxNode
{
};

class ParameterSyntax final :public SyntaxNode
{
private:
	SyntaxToken _identifier;
	TypeClauseSyntax _type;

public:
	ParameterSyntax(const SyntaxToken& identifier, const TypeClauseSyntax& type)
		:_identifier(identifier), _type(type)
	{
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::Parameter; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& Identifier()const noexcept { return _identifier; }
	const TypeClauseSyntax& Type()const noexcept { return _type; }
};

class FunctionDeclarationSyntax :public MemberSyntax
{
private:
	SyntaxToken _funcKeyword;
	SyntaxToken _identifier;
	SyntaxToken _openParenthesisToken;
	SeparatedSyntaxList<ParameterSyntax> _parameters;
	SyntaxToken _closeParenthesisToken;
	std::optional<TypeClauseSyntax> _type;
	unique_ptr<BlockStatementSyntax> _body;

public:
	FunctionDeclarationSyntax(const SyntaxToken& funcKeyword, const SyntaxToken& identifier,
		const SyntaxToken& openParenthesisToken,
		SeparatedSyntaxList<ParameterSyntax>& params,
		const SyntaxToken& closeParenthesisToken,
		const std::optional<TypeClauseSyntax>& type,
		unique_ptr<BlockStatementSyntax>& body)
		:_funcKeyword(funcKeyword), _identifier(identifier), _openParenthesisToken(openParenthesisToken),
		_parameters(std::move(params)), _closeParenthesisToken(closeParenthesisToken),
		_type(type), _body(std::move(body))
	{
	}

	FunctionDeclarationSyntax(FunctionDeclarationSyntax&&) = default;
	FunctionDeclarationSyntax& operator=(FunctionDeclarationSyntax&&) = default;

	// Inherited via MemberSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::FunctionDeclaration; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const SyntaxToken& FunctionKeyword()const noexcept { return _funcKeyword; }
	const SyntaxToken& Identifier()const noexcept { return _identifier; }
	const SyntaxToken& OpenParenthesisToken()const noexcept { return _openParenthesisToken; }
	decltype(auto) Parameters()const noexcept { return &_parameters; }
	const SyntaxToken& CloseParenthesisToken()const noexcept { return _closeParenthesisToken; }
	const std::optional<TypeClauseSyntax>& Type()const noexcept { return _type; }
	const BlockStatementSyntax* Body()const noexcept { return _body.get(); }
};

class GlobalStatementSyntax final :public MemberSyntax
{
private:
	unique_ptr<StatementSyntax> _statement;

public:
	explicit GlobalStatementSyntax(unique_ptr<StatementSyntax>& statement)
		:_statement(std::move(statement))
	{
	}
	GlobalStatementSyntax(GlobalStatementSyntax&&) = default;
	GlobalStatementSyntax& operator=(GlobalStatementSyntax&&) = default;

	// Inherited via MemberSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::GlobalStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const StatementSyntax* Statement()const noexcept { return _statement.get(); }
};

class CompilationUnitSyntax final :public SyntaxNode
{
private:
	vector<unique_ptr<MemberSyntax>> _members;
	SyntaxToken _endOfFileToken;

public:
	CompilationUnitSyntax(vector<unique_ptr<MemberSyntax>>& members,
		const SyntaxToken& endOfFile)
		:_members(std::move(members)), _endOfFileToken(endOfFile)
	{
	}

	CompilationUnitSyntax(CompilationUnitSyntax&&) = default;
	CompilationUnitSyntax& operator=(CompilationUnitSyntax&&) = default;

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::CompilationUnit; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const vector<unique_ptr<MemberSyntax>>& Members()const noexcept { return _members; }
	const SyntaxToken& EndOfFileToken()const noexcept { return _endOfFileToken; }
};

class Parser final
{
private:
	const SourceText* _text;
	vector<SyntaxToken> _tokens;
	size_t _position;
	unique_ptr<DiagnosticBag> _diagnostics;

	const SyntaxToken& Peek(int offset = 0) const;
	const SyntaxToken& Current() const;
	const SyntaxToken& NextToken();
	[[nodiscard]] SyntaxToken MatchToken(const SyntaxKind& kind);

	vector<unique_ptr<MemberSyntax>> ParseMembers();
	unique_ptr<MemberSyntax> ParseMember();
	unique_ptr<MemberSyntax> ParseFunctionDeclaration();
	SeparatedSyntaxList<ParameterSyntax> ParseParameterList();
	unique_ptr<ParameterSyntax> ParseParameter();
	unique_ptr<MemberSyntax> ParseGlobalStatement();

	unique_ptr<StatementSyntax> ParseStatement();
	unique_ptr<BlockStatementSyntax> ParseBlockStatement();
	unique_ptr<StatementSyntax> ParseVariableDeclaration();
	std::optional<TypeClauseSyntax> ParseOptionalTypeClause();
	TypeClauseSyntax ParseTypeClause();
	unique_ptr<StatementSyntax> ParseIfStatement();
	unique_ptr<ElseClauseSyntax> ParseElseClause();
	unique_ptr<StatementSyntax> ParseWhileStatement();
	unique_ptr<StatementSyntax> ParseDoWhileStatement();
	unique_ptr<StatementSyntax> ParseForStatement();
	unique_ptr<StatementSyntax> ParseBreakStatement();
	unique_ptr<StatementSyntax> ParseContinueStatement();
	unique_ptr<StatementSyntax> ParseReturnStatement();
	unique_ptr<ExpressionStatementSyntax> ParseExpressionStatement();

	unique_ptr<ExpressionSyntax> ParseExpression();
	unique_ptr<ExpressionSyntax> ParseAssignmentExpression();
	unique_ptr<ExpressionSyntax> ParseBinaryExpression(int parentPrecedence = 0);
	unique_ptr<ExpressionSyntax> ParsePostfixExpression(unique_ptr<ExpressionSyntax>& expression);

	unique_ptr<ExpressionSyntax> ParsePrimaryExpression();
	unique_ptr<ExpressionSyntax> ParseParenthesizedExpression();
	unique_ptr<ExpressionSyntax> ParseBooleanLiteral();
	unique_ptr<ExpressionSyntax> ParseNumberLiteral();
	unique_ptr<ExpressionSyntax> ParseStringLiteral();
	unique_ptr<ExpressionSyntax> ParseNameOrCallExpression();
	unique_ptr<ExpressionSyntax> ParseCallExpression();
	unique_ptr<ExpressionSyntax> ParseNameExpression();

	SeparatedSyntaxList<ExpressionSyntax> ParseArguments();

public:
	explicit Parser(const SourceText& text);

	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }
	unique_ptr<CompilationUnitSyntax> ParseCompilationUnit();
};

class MCF_API SyntaxTree final
{
private:
	unique_ptr<SourceText> _text;
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<CompilationUnitSyntax> _root;

public:
	explicit SyntaxTree(const SourceText& text);

	SyntaxTree(SyntaxTree&& other);
	SyntaxTree& operator=(SyntaxTree&& other);
	~SyntaxTree();

	const SourceText& Text() const { return *_text; }
	const CompilationUnitSyntax* Root()const noexcept { return _root.get(); }
	DiagnosticBag* Diagnostics() const noexcept { return _diagnostics.get(); }

	static unique_ptr<SyntaxTree> Parse(string_view text);
	static unique_ptr<SyntaxTree> Parse(const SourceText& text);
	static vector<SyntaxToken> ParseTokens(string_view text);
	static vector<SyntaxToken> ParseTokens(string_view text, DiagnosticBag& diagnostics);
	static vector<SyntaxToken> ParseTokens(const SourceText& text);
	static vector<SyntaxToken> ParseTokens(const SourceText& text, DiagnosticBag& diagnostics);

};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
