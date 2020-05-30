#include <catch2/catch.hpp>

#include <set>

#include "Diagnostic.h"
#include "Parsing.h"
#include "SourceText.h"
#include "StringHelper.h"

namespace {
auto GetTokens()->const std::vector<std::pair<MCF::SyntaxKind, std::string_view>>&;
auto GetSeparators()->const std::vector<std::pair<MCF::SyntaxKind, std::string_view>>&;
auto GetTokenData()->std::vector<std::pair<MCF::SyntaxKind, std::string_view>>;
auto GetTokenPairsData()->std::vector<
	std::tuple<MCF::SyntaxKind, std::string_view, MCF::SyntaxKind, std::string_view>>;
auto GetTokenPairsWithSeparator()->std::vector<std::tuple<
	MCF::SyntaxKind, std::string_view, MCF::SyntaxKind, std::string_view,
	MCF::SyntaxKind, std::string_view>>;
bool RequiresSeparators(MCF::SyntaxKind t1kind, MCF::SyntaxKind t2kind);
} //namespace

TEST_CASE("Lexer covers all tokens", "[Lexer]")
{
	auto tokenKinds = std::vector<MCF::SyntaxKind>();
	std::copy_if(MCF::AllSyntaxKinds.cbegin(), MCF::AllSyntaxKinds.cend(),
				 std::back_inserter(tokenKinds),
				 [](const auto k) { return MCF::IsToken(k); });

	std::set<MCF::SyntaxKind> untestedTokenKinds(tokenKinds.cbegin(), tokenKinds.cend());
	untestedTokenKinds.erase(MCF::SyntaxKind::BadToken);
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

	REQUIRE(tokens.size() == 1);
	CHECK(MCF::SyntaxKind::StringToken == tokens[0].Kind());
	CHECK(text == tokens[0].Text());

	REQUIRE(diagnostics.size() == 1);
	CHECK(MCF::TextSpan(0, 1) == diagnostics[0].Location().Span());
	CHECK("Unterminated string literal." == diagnostics[0].Message());
}

TEST_CASE("Lexer lexes token", "[Lexer]")
{
	auto& data = GetTokens();
	for (const auto& [kind, text] : data)
	{
		auto [tokens, _] = MCF::SyntaxTree::ParseTokens(text);
		REQUIRE(tokens.size() == 1);
		CHECK(kind == tokens[0].Kind());
		CHECK(text == tokens[0].Text());
	}
}

TEST_CASE("Lexer lexes sparator", "[Lexer]")
{
	auto& data = GetSeparators();
	for (const auto& [kind, text] : data)
	{
		auto [tokens, _] = MCF::SyntaxTree::ParseTokens(text, true);

		REQUIRE(tokens.size() == 1);
		REQUIRE(tokens.at(0).LeadingTrivia().size() == 1);
		CHECK(kind == tokens.at(0).LeadingTrivia().at(0).Kind);
		CHECK(text == tokens.at(0).LeadingTrivia().at(0).Text);
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

		CHECK(t1kind == tokens[0].Kind());
		CHECK(t1text == tokens[0].Text());
		CHECK(t2kind == tokens[1].Kind());
		CHECK(t2text == tokens[1].Text());
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
		REQUIRE(2 == tokens.size());

		CHECK(t1kind == tokens[0].Kind());
		CHECK(t1text == tokens[0].Text());

		REQUIRE(tokens.at(0).TrailingTrivia().size() == 1);
		auto& separator = tokens.at(0).TrailingTrivia().at(0);
		CHECK(skind == separator.Kind);
		CHECK(stext == separator.Text);

		CHECK(t2kind == tokens[1].Kind());
		CHECK(t2text == tokens[1].Text());
	}
}

TEST_CASE("Lexer lexes identifiers", "[Lexer]")
{
	auto name = GENERATE(as<std::string_view>{},
						 "foo", "foo42", "foo_42", "_foo");

	auto [tokens, _] = MCF::SyntaxTree::ParseTokens(name);

	REQUIRE(tokens.size() == 1);
	CHECK(tokens.at(0).Kind() == MCF::SyntaxKind::IdentifierToken);
	CHECK(tokens.at(0).Text() == name);
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

			CHECK(kind == tokens[0].Kind());
			CHECK(text == tokens[0].Text());
		}
	}
}

namespace {

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
		{MCF::SyntaxKind::LineBreakTrivia, "\r"},
		{MCF::SyntaxKind::LineBreakTrivia, "\n"},
		{MCF::SyntaxKind::LineBreakTrivia, "\r\n"},
		{MCF::SyntaxKind::MultiLineCommentTrivia, "/**/"}
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

	if (t1kind == MCF::SyntaxKind::IdentifierToken && t2kind == MCF::SyntaxKind::NumberToken)
		return true;
	if (t1IsKeyword && t2kind == MCF::SyntaxKind::NumberToken)
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

	if (t1kind == MCF::SyntaxKind::PlusToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PlusToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PlusToken && t2kind == MCF::SyntaxKind::PlusEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::MinusToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::MinusToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::MinusToken && t2kind == MCF::SyntaxKind::MinusEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::StarToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::StarToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PercentToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PercentToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
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
	if (t1kind == MCF::SyntaxKind::PlusToken && t2kind == MCF::SyntaxKind::PlusEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PlusToken && t2kind == MCF::SyntaxKind::PlusPlusToken)
		return true;
	if (t1kind == MCF::SyntaxKind::MinusToken && t2kind == MCF::SyntaxKind::MinusToken)
		return true;
	if (t1kind == MCF::SyntaxKind::MinusToken && t2kind == MCF::SyntaxKind::MinusEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::MinusToken && t2kind == MCF::SyntaxKind::MinusMinusToken)
		return true;

	if (t1kind == MCF::SyntaxKind::AmpersandToken && t2kind == MCF::SyntaxKind::AmpersandToken)
		return true;
	if (t1kind == MCF::SyntaxKind::AmpersandToken && t2kind == MCF::SyntaxKind::AmpersandAmpersandToken)
		return true;

	if (t1kind == MCF::SyntaxKind::AmpersandToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::AmpersandToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::AmpersandToken && t2kind == MCF::SyntaxKind::AmpersandEqualsToken)
		return true;

	if (t1kind == MCF::SyntaxKind::PipeToken && t2kind == MCF::SyntaxKind::PipeToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PipeToken && t2kind == MCF::SyntaxKind::PipePipeToken)
		return true;

	if (t1kind == MCF::SyntaxKind::PipeToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PipeToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::PipeToken && t2kind == MCF::SyntaxKind::PipeEqualsToken)
		return true;

	if (t1kind == MCF::SyntaxKind::HatToken && t2kind == MCF::SyntaxKind::EqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::HatToken && t2kind == MCF::SyntaxKind::EqualsEqualsToken)
		return true;

	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::SlashToken)
		return true;
	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::StarToken)
		return true;

	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::SlashEqualsToken)
		return true;
	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::StarEqualsToken)
		return true;

	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::SingleLineCommentTrivia)
		return true;
	if (t1kind == MCF::SyntaxKind::SlashToken && t2kind == MCF::SyntaxKind::MultiLineCommentTrivia)
		return true;

	return false;
}

} //namespace