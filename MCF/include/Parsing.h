#pragma once

#include <filesystem>
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

class MCF_API SyntaxTree final
{
private:

	unique_ptr<SourceText> _text;
	unique_ptr<CompilationUnitSyntax> _root;
	unique_ptr<DiagnosticBag> _diagnostics;

	vector<SyntaxTree> _usings;

	template<typename ParseHandle,
		typename = std::enable_if_t<std::is_invocable_v<ParseHandle, const SyntaxTree&>>>
		SyntaxTree(unique_ptr<SourceText> text, ParseHandle handle)
		:_text(std::move(text))
	{
		auto [root, usings, bag] = handle(*this);
		_root = std::move(root);
		_usings = std::move(usings);
		_diagnostics = std::move(bag);
	}

	static auto ParseTree(const SyntaxTree&)->
		std::tuple<unique_ptr<CompilationUnitSyntax>, vector<SyntaxTree>, unique_ptr<DiagnosticBag>>;

public:

	SyntaxTree(SyntaxTree&& other);
	SyntaxTree& operator=(SyntaxTree&& other);
	SyntaxTree(const SyntaxTree&) = delete;
	SyntaxTree& operator=(const SyntaxTree&) = delete;
	~SyntaxTree();

	const SourceText& Text() const noexcept { return *_text; }
	const CompilationUnitSyntax* Root()const noexcept { return _root.get(); }
	const DiagnosticBag& Diagnostics() const& noexcept { return *_diagnostics; }
	[[nodiscard]] DiagnosticBag&& Diagnostics() const&& noexcept { return std::move(*_diagnostics); }

	[[nodiscard]] static SyntaxTree Load(const fs::path& path);

	[[nodiscard]] static SyntaxTree Parse(string_view text);
	[[nodiscard]] static SyntaxTree Parse(unique_ptr<SourceText> text);

	[[nodiscard]] static std::pair<vector<SyntaxToken>, SyntaxTree>
		ParseTokens(string_view text, bool includeEndOfFile = false);
	[[nodiscard]] static std::pair<vector<SyntaxToken>, SyntaxTree>
		ParseTokens(string_view text, DiagnosticBag& diagnostics,
			bool includeEndOfFile = false);
	[[nodiscard]] static std::pair<vector<SyntaxToken>, SyntaxTree>
		ParseTokens(unique_ptr<SourceText> text, bool includeEndOfFile = false);
	[[nodiscard]] static std::pair<vector<SyntaxToken>, SyntaxTree>
		ParseTokens(unique_ptr<SourceText> text, DiagnosticBag& diagnostics,
			bool includeEndOfFile = false);

	[[nodiscard]] static vector<unique_ptr<SyntaxTree>> Flatten(unique_ptr<SyntaxTree> tree);

};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
