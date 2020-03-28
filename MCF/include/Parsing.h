#pragma once

#include <filesystem>
#include <functional>
#include <optional>

#include "SyntaxStatements.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

namespace fs = std::filesystem;

class DiagnosticBag;
class SourceText;

class MemberSyntax :public SyntaxNode
{
protected:
	explicit MemberSyntax(const SyntaxTree& tree)
		:SyntaxNode(tree)
	{
	}
};

class UsingDirective final :public MemberSyntax
{
private:
	SyntaxToken _keyword;
	SyntaxToken _fileName;

public:
	UsingDirective(const SyntaxTree& tree, SyntaxToken keyword, SyntaxToken fileName)
		:MemberSyntax(tree),
		_keyword(std::move(keyword)), _fileName(std::move(fileName))
	{
	}

	// Inherited via MemberSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::UsingDirective; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& Keyword()const noexcept { return _keyword; }
	constexpr const SyntaxToken& FileName()const noexcept { return _fileName; }
};

class ParameterSyntax final :public SyntaxNode
{
private:
	SyntaxToken _identifier;
	TypeClauseSyntax _type;

public:
	ParameterSyntax(const SyntaxTree& tree,
		SyntaxToken identifier, TypeClauseSyntax type)
		:SyntaxNode(tree),
		_identifier(std::move(identifier)), _type(std::move(type))
	{
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::Parameter; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& Identifier()const noexcept { return _identifier; }
	constexpr const TypeClauseSyntax& Type()const noexcept { return _type; }
};

class FunctionDeclarationSyntax final :public MemberSyntax
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
	FunctionDeclarationSyntax(const SyntaxTree& tree, SyntaxToken funcKeyword, SyntaxToken identifier,
		SyntaxToken openParenthesisToken,
		SeparatedSyntaxList<ParameterSyntax> params,
		SyntaxToken closeParenthesisToken,
		std::optional<TypeClauseSyntax> type,
		unique_ptr<BlockStatementSyntax> body)
		:MemberSyntax(tree),
		_funcKeyword(std::move(funcKeyword)), _identifier(std::move(identifier)),
		_openParenthesisToken(std::move(openParenthesisToken)),
		_parameters(std::move(params)),
		_closeParenthesisToken(std::move(closeParenthesisToken)),
		_type(std::move(type)), _body(std::move(body))
	{
	}

	// Inherited via MemberSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::FunctionDeclaration; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const SyntaxToken& FunctionKeyword()const noexcept { return _funcKeyword; }
	constexpr const SyntaxToken& Identifier()const noexcept { return _identifier; }
	constexpr const SyntaxToken& OpenParenthesisToken()const noexcept { return _openParenthesisToken; }
	constexpr decltype(auto) Parameters()const noexcept { return &_parameters; }
	constexpr const SyntaxToken& CloseParenthesisToken()const noexcept { return _closeParenthesisToken; }
	constexpr const std::optional<TypeClauseSyntax>& Type()const noexcept { return _type; }
	const BlockStatementSyntax* Body()const noexcept { return _body.get(); }
};

class GlobalStatementSyntax final :public MemberSyntax
{
private:
	unique_ptr<StatementSyntax> _statement;

public:
	GlobalStatementSyntax(const SyntaxTree& tree, unique_ptr<StatementSyntax> statement)
		:MemberSyntax(tree),
		_statement(std::move(statement))
	{
	}

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
	CompilationUnitSyntax(const SyntaxTree& tree, vector<unique_ptr<MemberSyntax>> members,
		SyntaxToken endOfFile)
		:SyntaxNode(tree),
		_members(std::move(members)), _endOfFileToken(std::move(endOfFile))
	{
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::CompilationUnit; }
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr const vector<unique_ptr<MemberSyntax>>& Members()const noexcept { return _members; }
	constexpr const SyntaxToken& EndOfFileToken()const noexcept { return _endOfFileToken; }
};

class Parser final
{
private:
	const SyntaxTree& _tree;
	const SourceText& _text;
	vector<SyntaxToken> _tokens;
	size_t _position;
	DiagnosticBag& _diagnostics;

	vector<unique_ptr<SyntaxTree>> _usings;

	const SyntaxToken& Peek(int offset = 0) const;
	const SyntaxToken& Current() const;
	const SyntaxToken& NextToken();
	[[nodiscard]] SyntaxToken MatchToken(SyntaxKind kind);

	vector<unique_ptr<MemberSyntax>> ParseMembers();
	unique_ptr<MemberSyntax> ParseMember();
	unique_ptr<MemberSyntax> ParseFunctionDeclaration();
	SeparatedSyntaxList<ParameterSyntax> ParseParameterList();
	unique_ptr<ParameterSyntax> ParseParameter();
	unique_ptr<MemberSyntax> ParseGlobalStatement();
	unique_ptr<MemberSyntax> ParseUsingDirective();

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
	explicit Parser(const SyntaxTree& tree);

	const DiagnosticBag& Diagnostics()const noexcept { return _diagnostics; }
	unique_ptr<CompilationUnitSyntax> ParseCompilationUnit();
	vector<unique_ptr<SyntaxTree>> FetchUsings()noexcept { return std::move(_usings); };
};

class MCF_API SyntaxTree final
{
private:

	unique_ptr<SourceText> _text;
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<CompilationUnitSyntax> _root;

	vector<unique_ptr<SyntaxTree>> _usings;
	// HACK
	// this is very hacky
	template<typename ParseHandle,
		typename = std::enable_if_t<std::is_invocable_v<ParseHandle, const SyntaxTree&>>>
		SyntaxTree(unique_ptr<SourceText> text, ParseHandle handle)
		:_text(std::move(text)), _diagnostics(make_unique<DiagnosticBag>())
	{
		auto [root, usings] = handle(*this);
		_root = std::move(root);
		_usings = std::move(usings);
	}

	static auto ParseTree(const SyntaxTree&)->
		std::tuple<unique_ptr<CompilationUnitSyntax>, vector<unique_ptr<SyntaxTree>>>;
public:

	SyntaxTree(SyntaxTree&& other);
	SyntaxTree& operator=(SyntaxTree&& other);
	~SyntaxTree();

	constexpr const SourceText& Text() const { return *_text; }
	const CompilationUnitSyntax* Root()const noexcept { return _root.get(); }
	DiagnosticBag& Diagnostics() const noexcept { return *_diagnostics; }

	static unique_ptr<SyntaxTree> Load(fs::path path);

	static unique_ptr<SyntaxTree> Parse(string_view text);
	static unique_ptr<SyntaxTree> Parse(unique_ptr<SourceText> text);
	static std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
		ParseTokens(string_view text);
	static std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
		ParseTokens(string_view text, DiagnosticBag& diagnostics);
	static std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
		ParseTokens(unique_ptr<SourceText> text);
	static std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
		ParseTokens(unique_ptr<SourceText> text, DiagnosticBag& diagnostics);

	static vector<unique_ptr<SyntaxTree>> Flatten(unique_ptr<SyntaxTree> tree);

};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
