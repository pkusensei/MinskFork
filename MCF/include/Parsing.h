#pragma once

#include <filesystem>
#include <optional>
#include <unordered_map>

#include "Diagnostic.h"
#include "SyntaxStatements.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

namespace fs = std::filesystem;

class DiagnosticBag;
struct SourceText;

struct MemberSyntax :public SyntaxNode
{
protected:
	explicit MemberSyntax(const SyntaxTree& tree)noexcept
		:SyntaxNode(tree)
	{
	}
};

struct UsingDirective final :public MemberSyntax
{
	SyntaxToken Keyword;
	SyntaxToken FileName;

public:
	explicit UsingDirective(const SyntaxTree& tree,
							SyntaxToken keyword,
							SyntaxToken fileName)noexcept
		:MemberSyntax(tree),
		Keyword(std::move(keyword)), FileName(std::move(fileName))
	{
	}

	// Inherited via MemberSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::UsingDirective; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct ParameterSyntax final :public SyntaxNode
{
	TypeClauseSyntax Type;
	SyntaxToken Identifier;

public:
	explicit ParameterSyntax(const SyntaxTree& tree,
							 SyntaxToken identifier,
							 TypeClauseSyntax type)noexcept
		:SyntaxNode(tree),
		Type(std::move(type)), Identifier(std::move(identifier))
	{
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::Parameter; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct FunctionDeclarationSyntax final :public MemberSyntax
{
	std::optional<TypeClauseSyntax> Type;
	SyntaxToken FuncKeyword;
	SyntaxToken Identifier;
	SyntaxToken OpenParenthesisToken;
	SyntaxToken CloseParenthesisToken;
	SeparatedSyntaxList<ParameterSyntax> Parameters;
	unique_ptr<BlockStatementSyntax> Body;

public:
	explicit FunctionDeclarationSyntax(const SyntaxTree& tree,
									   SyntaxToken funcKeyword,
									   SyntaxToken identifier,
									   SyntaxToken openParenthesisToken,
									   SeparatedSyntaxList<ParameterSyntax> params,
									   SyntaxToken closeParenthesisToken,
									   std::optional<TypeClauseSyntax> type,
									   unique_ptr<BlockStatementSyntax> body)noexcept
		:MemberSyntax(tree),
		Type(std::move(type)),
		FuncKeyword(std::move(funcKeyword)),
		Identifier(std::move(identifier)),
		OpenParenthesisToken(std::move(openParenthesisToken)),
		CloseParenthesisToken(std::move(closeParenthesisToken)),
		Parameters(std::move(params)),
		Body(std::move(body))
	{
	}

	// Inherited via MemberSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::FunctionDeclaration; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct GlobalStatementSyntax final :public MemberSyntax
{
	unique_ptr<StatementSyntax> Statement;

public:
	explicit GlobalStatementSyntax(const SyntaxTree& tree,
								   unique_ptr<StatementSyntax> statement)noexcept
		:MemberSyntax(tree),
		Statement(std::move(statement))
	{
	}

	// Inherited via MemberSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::GlobalStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

struct CompilationUnitSyntax final :public SyntaxNode
{
	SyntaxToken EndOfFileToken;
	vector<unique_ptr<MemberSyntax>> Members;

public:
	explicit CompilationUnitSyntax(const SyntaxTree& tree,
								   vector<unique_ptr<MemberSyntax>> members,
								   SyntaxToken endOfFile)noexcept
		:SyntaxNode(tree),
		EndOfFileToken(std::move(endOfFile)), Members(std::move(members))
	{
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::CompilationUnit; }
	const vector<const SyntaxNode*> GetChildren() const override;

};

class MCF_API [[nodiscard]] SyntaxTree final
{
private:

	// NOTE by the time _parents gets "filled in" in GetParent()
	//      AST is already constructed and should remain const
	//      _parents serves as a side-effect, not directly related to AST
	mutable std::unordered_map<const SyntaxNode*, const SyntaxNode*> _parents;

	vector<unique_ptr<SyntaxTree>> _usings;

	unique_ptr<SourceText> _text;
	unique_ptr<CompilationUnitSyntax> _root;
	unique_ptr<DiagnosticBag> _diagnostics;

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
		std::tuple<unique_ptr<CompilationUnitSyntax>, vector<unique_ptr<SyntaxTree>>, unique_ptr<DiagnosticBag>>;

public:

	// Pin the tree in memory 
	// so that SyntaxNodes keep a valid const SyntaxTree*
	SyntaxTree(SyntaxTree&& other) = delete;
	SyntaxTree& operator=(SyntaxTree&& other) = delete;
	SyntaxTree(const SyntaxTree&) = delete;
	SyntaxTree& operator=(const SyntaxTree&) = delete;
	~SyntaxTree();

	const SourceText& Text() const noexcept { return *_text; }
	const CompilationUnitSyntax* Root()const noexcept { return _root.get(); }
	const DiagnosticBag& Diagnostics() const& noexcept { return *_diagnostics; }
	DiagnosticBag Diagnostics() const&& noexcept { return std::move(*_diagnostics); }

	const SyntaxNode* GetParent(const SyntaxNode& node)const;

	[[nodiscard]] static unique_ptr<SyntaxTree> Load(const fs::path& path);

	[[nodiscard]] static unique_ptr<SyntaxTree> Parse(string_view text);
	[[nodiscard]] static unique_ptr<SyntaxTree> Parse(unique_ptr<SourceText> text);

	[[nodiscard]] static std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
		ParseTokens(string_view text, bool includeEndOfFile = false);
	[[nodiscard]] static std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
		ParseTokens(string_view text, DiagnosticBag& diagnostics,
					bool includeEndOfFile = false);
	[[nodiscard]] static std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
		ParseTokens(unique_ptr<SourceText> text, bool includeEndOfFile = false);
	[[nodiscard]] static std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
		ParseTokens(unique_ptr<SourceText> text, DiagnosticBag& diagnostics,
					bool includeEndOfFile = false);

	[[nodiscard]] static vector<unique_ptr<SyntaxTree>> Flatten(unique_ptr<SyntaxTree> tree);

};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
