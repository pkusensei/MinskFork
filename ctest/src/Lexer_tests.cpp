#include <catch.hpp>

#include <set>

#include "Diagnostic.h"
#include "helpers.h"
#include "Parsing.h"
#include "SourceText.h"

auto GetTokens()->const std::vector<std::pair<MCF::SyntaxKind, std::string>> &;
auto GetSeperators()->const std::vector<std::pair<MCF::SyntaxKind, std::string>> &;
auto GetTokenData()->std::vector<std::pair<MCF::SyntaxKind, std::string>>;
auto GetTokenPairsData()->std::vector<
	std::tuple<MCF::SyntaxKind, std::string, MCF::SyntaxKind, std::string>>;
auto GetTokenPairsWithSeperator()->std::vector<std::tuple<
	MCF::SyntaxKind, std::string, MCF::SyntaxKind, std::string,
	MCF::SyntaxKind, std::string>>;
bool RequiresSeperators(MCF::SyntaxKind t1kind, MCF::SyntaxKind t2kind);

TEST_CASE("Lexer covers all tokens", "[Lexer]")
{
	auto tokenKinds = std::vector<MCF::SyntaxKind>();
	for (const auto& kind : MCF::AllSyntaxKinds)
	{
		auto text = MCF::GetSyntaxKindName(kind);
		if (MCF::StringEndsWith(text, "Keyword") || MCF::StringEndsWith(text, "Token"))
			tokenKinds.emplace_back(kind);
	}
	std::set<MCF::SyntaxKind> untestedTokenKinds(tokenKinds.begin(), tokenKinds.end());
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
	auto tokens = MCF::SyntaxTree::ParseTokens(text, diagnostics);

	CHECK(tokens.size() == 1);
	CHECK(MCF::SyntaxKind::StringToken == tokens[0].Kind());
	CHECK(text == tokens[0].Text());

	CHECK(diagnostics.size() == 1);
	CHECK(MCF::TextSpan(0, 1) == diagnostics[0].Span());
	CHECK("Unterminated string literal." == diagnostics[0].Message());
}

TEST_CASE("Lexer lexes token", "[Lexer]")
{
	auto data = GetTokenData();
	for (const auto& it : data)
	{
		auto tokens = MCF::SyntaxTree::ParseTokens(it.second);
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
		auto text = t1text + t2text;
		auto tokens = MCF::SyntaxTree::ParseTokens(text);
		REQUIRE(2 == tokens.size());

		REQUIRE(t1kind == tokens[0].Kind());
		REQUIRE(t1text == tokens[0].Text());
		REQUIRE(t2kind == tokens[1].Kind());
		REQUIRE(t2text == tokens[1].Text());
	}
}

TEST_CASE("Lexer lexes token pairs with seperators", "[Lexer]")
{
	auto data = GetTokenPairsWithSeperator();
	for (const auto& it : data)
	{
		auto [t1kind, t1text, skind, stext, t2kind, t2text] = it;
		auto text = t1text + stext + t2text;
		auto tokens = MCF::SyntaxTree::ParseTokens(text);
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
			auto tokens = MCF::SyntaxTree::ParseTokens(text);
			REQUIRE(1 == tokens.size());

			REQUIRE(kind == tokens[0].Kind());
			REQUIRE(text == tokens[0].Text());
		}
	}
}

auto GetTokens()->const std::vector<std::pair<MCF::SyntaxKind, std::string>> &
{
	auto build = []()
	{
		auto result = std::vector<std::pair<MCF::SyntaxKind, std::string>>();
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

auto GetSeperators()->const std::vector<std::pair<MCF::SyntaxKind, std::string>> &
{
	static const auto result = std::vector<std::pair<MCF::SyntaxKind, std::string>>{
		{MCF::SyntaxKind::WhitespaceToken, " "},
		{MCF::SyntaxKind::WhitespaceToken, "  "},
		{MCF::SyntaxKind::WhitespaceToken, "\r"},
		{MCF::SyntaxKind::WhitespaceToken, "\n"},
		{MCF::SyntaxKind::WhitespaceToken, "\r\n"},
	};
	return result;
}

auto GetTokenData()->std::vector<std::pair<MCF::SyntaxKind, std::string>>
{
	auto tokens = GetTokens();
	auto seperators = GetSeperators();
	tokens.insert(tokens.end(), seperators.begin(), seperators.end());
	tokens.shrink_to_fit();
	return tokens;
}

auto GetTokenPairsData()->std::vector<std::tuple<
	MCF::SyntaxKind, std::string, MCF::SyntaxKind, std::string>>
{
	auto tokens = GetTokens();
	auto result = std::vector<std::tuple<MCF::SyntaxKind, std::string,
		MCF::SyntaxKind, std::string>>();
	for (const auto& t1 : tokens)
		for (const auto& t2 : tokens)
		{
			if (!RequiresSeperators(t1.first, t2.first))
				result.emplace_back(t1.first, t1.second, t2.first, t2.second);
		}
	return result;
}

auto GetTokenPairsWithSeperator()->std::vector<std::tuple<
	MCF::SyntaxKind, std::string, MCF::SyntaxKind, std::string,
	MCF::SyntaxKind, std::string>>
{
	auto tokens = GetTokens();
	auto result = std::vector<std::tuple<MCF::SyntaxKind, std::string,
		MCF::SyntaxKind, std::string, MCF::SyntaxKind, std::string>>();
	for (const auto& t1 : tokens)
		for (const auto& t2 : tokens)
		{
			if (RequiresSeperators(t1.first, t2.first))
			{
				auto seperators = GetSeperators();
				for (const auto& s : seperators)
					result.emplace_back(t1.first, t1.second, s.first, s.second, t2.first, t2.second);
			}
		}
	return result;
}

bool RequiresSeperators(MCF::SyntaxKind t1kind, MCF::SyntaxKind t2kind)
{
	auto t1IsKeyword = MCF::StringEndsWith(MCF::GetSyntaxKindName(t1kind), "Keyword");
	auto t2IsKeyword = MCF::StringEndsWith(MCF::GetSyntaxKindName(t2kind), "Keyword");
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

	return false;
}