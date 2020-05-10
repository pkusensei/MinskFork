#include <catch2/catch.hpp>

#include <set>

#include "Diagnostic.h"
#include "helpers.h"
#include "Parsing.h"
#include "SourceText.h"

auto GetTokens()->const std::vector<std::pair<MCF::SyntaxKind, std::string_view>>&;
auto GetSeparators()->const std::vector<std::pair<MCF::SyntaxKind, std::string_view>>&;
auto GetTokenData()->std::vector<std::pair<MCF::SyntaxKind, std::string_view>>;
auto GetTokenPairsData()->std::vector<
	std::tuple<MCF::SyntaxKind, std::string_view, MCF::SyntaxKind, std::string_view>>;
auto GetTokenPairsWithSeparator()->std::vector<std::tuple<
	MCF::SyntaxKind, std::string_view, MCF::SyntaxKind, std::string_view,
	MCF::SyntaxKind, std::string_view>>;
bool RequiresSeparators(MCF::SyntaxKind t1kind, MCF::SyntaxKind t2kind);

TEST_CASE("Lexer covers all tokens", "[Lexer]")
{
	auto tokenKinds = std::vector<MCF::SyntaxKind>();
	std::copy_if(MCF::AllSyntaxKinds.cbegin(), MCF::AllSyntaxKinds.cend(),
		std::back_inserter(tokenKinds),
		[](const auto k) { return MCF::IsToken(k); });

	std::set<MCF::SyntaxKind> untestedTokenKinds(tokenKinds.cbegin(), tokenKinds.cend());
	untestedTokenKinds.erase(MCF::SyntaxKind::BadTokenTrivia);
	untestedTokenKinds.erase(MCF::SyntaxKind::EndOfFileToken);

	auto tokens = GetTokenData();
	auto testedTokenKinds = std::set<MCF::SyntaxKind>();
	for (const auto& it : tokens)
		testedTokenKinds.emplace(it.first);
	for (const auto& it : testedTokenKinds)
	{
		if (untestedTokenKinds.find(it) != untestedTokenKinds.end())
			untestedTokenKinds.erase(it);
	}
	REQUIRE(untestedTokenKinds.empty());
}

TEST_CASE("Lexer lexes unterminated string", "[Lexer]")
{
	auto text = std::string("\"text");
	auto diagnostics = MCF::DiagnosticBag();
	auto [tokens, _] = MCF::SyntaxTree::ParseTokens(text, diagnostics);

	CHECK(tokens.size() == 1);
	CHECK(MCF::SyntaxKind::StringToken == tokens[0].Kind());
	CHECK(text == tokens[0].Text());

	CHECK(diagnostics.size() == 1);
	CHECK(MCF::TextSpan(0, 1) == diagnostics[0].Location().Span());
	CHECK("Unterminated string literal." == diagnostics[0].Message());
}

TEST_CASE("Lexer lexes token", "[Lexer]")
{
	auto data = GetTokenData();
	for (const auto& it : data)
	{
		auto [tokens, _] = MCF::SyntaxTree::ParseTokens(it.second);
		REQUIRE(tokens.size() == 1);
		REQUIRE(it.first == tokens[0].Kind());
		REQUIRE(it.second == tokens[0].Text());
	}
}

TEST_CASE("Lexer lexes token pairs", "[Lexer]")
{
	auto data = GetTokenPairsData();
	for (const auto& it : data)
	{
		auto [t1kind, t1text, t2kind, t2text] = it;
		auto text = MCF::BuildStringFrom(t1text, t2text);
		auto [tokens, _] = MCF::SyntaxTree::ParseTokens(text);
		REQUIRE(2 == tokens.size());

		REQUIRE(t1kind == tokens[0].Kind());
		REQUIRE(t1text == tokens[0].Text());
		REQUIRE(t2kind == tokens[1].Kind());
		REQUIRE(t2text == tokens[1].Text());
	}
}

TEST_CASE("Lexer lexes token pairs with separators", "[Lexer]")
{
	auto data = GetTokenPairsWithSeparator();
	for (const auto& it : data)
	{
		auto [t1kind, t1text, skind, stext, t2kind, t2text] = it;
		auto text = MCF::BuildStringFrom(t1text, stext, t2text);
		auto [tokens, _] = MCF::SyntaxTree::ParseTokens(text);
		REQUIRE(3 == tokens.size());

		REQUIRE(t1kind == tokens[0].Kind());
		REQUIRE(t1text == tokens[0].Text());
		REQUIRE(skind == tokens[1].Kind());
		REQUIRE(stext == tokens[1].Text());
		REQUIRE(t2kind == tokens[2].Kind());
		REQUIRE(t2text == tokens[2].Text());
	}
}

TEST_CASE("GetText_RoundTrip")
{
	for (const auto& kind : MCF::AllSyntaxKinds)
	{
		auto text = MCF::GetText(kind);
		if (!text.empty())
		{
			auto [tokens, _] = MCF::SyntaxTree::ParseTokens(text);
			REQUIRE(1 == tokens.size());

			REQUIRE(kind == tokens[0].Kind());
			REQUIRE(text == tokens[0].Text());
		}
	}
}

auto GetTokens()->const std::vector<std::pair<MCF::SyntaxKind, std::string_view>>&
{
	auto build = []()
	{
		auto result = std::vector<std::pair<MCF::SyntaxKind, std::string_view>>();
		result.reserve(100);
		// fixed tokens
		for (const auto& kind : MCF::AllSyntaxKinds)
		{
			auto text = MCF::GetText(kind);
			if (!text.empty())
				result.emplace_back(kind, text);
		}

		// dynamic tokens
		result.emplace_back(MCF::SyntaxKind::NumberToken, "1");
		result.emplace_back(MCF::SyntaxKind::NumberToken, "123");
		result.emplace_back(MCF::SyntaxKind::IdentifierToken, "a");
		result.emplace_back(MCF::SyntaxKind::IdentifierToken, "abc");
		result.emplace_back(MCF::SyntaxKind::StringToken, "\"Test\"");
		result.emplace_back(MCF::SyntaxKind::StringToken, "\"Te\"\"st\"");
		result.shrink_to_fit();
		return result;
	};
	static const auto result = build();
	return result;
}

auto GetSeparators()->const std::vector<std::pair<MCF::SyntaxKind, std::string_view>>&
{
	static const auto result = std::vector<std::pair<MCF::SyntaxKind, std::string_view>>{
		{MCF::SyntaxKind::WhitespaceTrivia, " "},
		{MCF::SyntaxKind::WhitespaceTrivia, "  "},
		{MCF::SyntaxKind::WhitespaceTrivia, "\r"},
		{MCF::SyntaxKind::WhitespaceTrivia, "\n"},
		{MCF::SyntaxKind::WhitespaceTrivia, "\r\n"},
		{MCF::SyntaxKind::MultiLineCommentTriva, "/**/"}
	};
	return result;
}

auto GetTokenData()->std::vector<std::pair<MCF::SyntaxKind, std::string_view>>
{
	auto tokens = GetTokens();
	auto& separators = GetSeparators();
	tokens.insert(tokens.end(), separators.begin(), separators.end());
	tokens.shrink_to_fit();
	return tokens;
}

auto GetTokenPairsData()->std::vector<std::tuple<
	MCF::SyntaxKind, std::string_view, MCF::SyntaxKind, std::string_view>>
{
	auto& tokens = GetTokens();
	auto result = std::vector<std::tuple<MCF::SyntaxKind, std::string_view,
		MCF::SyntaxKind, std::string_view>>();
	for (const auto& [t1kind, t1text] : tokens)
		for (const auto& [t2kind, t2text] : tokens)
		{
			if (!RequiresSeparators(t1kind, t2kind))
				result.emplace_back(t1kind, t1text, t2kind, t2text);
		}
	return result;
}

auto GetTokenPairsWithSeparator()->std::vector<std::tuple<
	MCF::SyntaxKind, std::string_view, MCF::SyntaxKind, std::string_view,
	MCF::SyntaxKind, std::string_view>>
{
	auto& tokens = GetTokens();
	auto result = std::vector<std::tuple<MCF::SyntaxKind, std::string_view,
		MCF::SyntaxKind, std::string_view, MCF::SyntaxKind, std::string_view>>();
	for (const auto& [t1kind, t1text] : tokens)
		for (const auto& [t2kind, t2text] : tokens)
		{
			auto& separators = GetSeparators();
			for (const auto& [skind, stext] : separators)
				if (!RequiresSeparators(t1kind, skind)
					&& !RequiresSeparators(skind, t2kind))
				{
					result.emplace_back(t1kind, t1text, skind, stext, t2kind, t2text);
				}
		}
	return result;
}

bool RequiresSeparators(MCF::SyntaxKind t1kind, MCF::SyntaxKind t2kind)
{
	auto t1IsKeyword = MCF::IsKeyword(t1kind);
	auto t2IsKeyword = MCF::IsKeyword(t2kind);

	if (t1kind == MCF::SyntaxKind::IdentifierToken && t2kind == MCF::SyntaxKind::IdentifierToken)
		return true;
	if (t1IsKeyword && t2IsKeyword)
		return true;
	if (t1IsKeyword && t2kind == MCF::SyntaxKind::IdentifierToken)
		return true;
	if (t1kind == MCF::SyntaxKind::IdentifierToken && t2IsKeyword)
		return true;
	if (t1kind == MCF::SyntaxKind::NumberToken && t2kind == MCF::SyntaxKind::NumberToken)
		return true;
	if (t1kind == MCF::SyntaxKind::StringToken && t2kind == MCF::SyntaxKind::StringToken)
		return true;

	if (t1kind == MCF::SyntaxKind::BangToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::BangToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::EqualsToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::EqualsToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;

	if (t1kind == MCF::SyntaxKind::LessToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::LessToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::GreaterToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::GreaterToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;

	if (t1kind == MCF::SyntaxKind::PlusToken && t2kind == MCF::SyntaxKind::PlusToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PlusToken && t2kind == MCF::SyntaxKind::PlusPlusToken)
		return true;
	if (t1kind == MCF::SyntaxKind::MinusToken && t2kind == MCF::SyntaxKind::MinusToken)
		return true;
	if (t1kind == MCF::SyntaxKind::MinusToken && t2kind == MCF::SyntaxKind::MinusMinusToken)
		return true;

	if (t1kind == MCF::SyntaxKind::AmpersandToken && t2kind == MCF::SyntaxKind::AmpersandToken)
		return true;
	if (t1kind == MCF::SyntaxKind::AmpersandToken && t2kind == MCF::SyntaxKind::AmpersandAmpersandToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PipeToken && t2kind == MCF::SyntaxKind::PipeToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PipeToken && t2kind == MCF::SyntaxKind::PipePipeToken)
		return true;

	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::SlashToken)
		return true;
	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::StarToken)
		return true;
	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::SingleLineCommentTrivia)
		return true;
	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::MultiLineCommentTriva)
		return true;

	return false;
}