#include "stdafx.h"
#include "CppUnitTest.h"

#include <set>

#include "..\MCF\Syntax.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace Tests {

TEST_CLASS(LexerTests)
{
public:

	TEST_METHOD(Lexer_Tests_AllTokens)
	{
		auto tokenKinds = std::vector<MCF::SyntaxKind>();
		auto kinds = MCF::GetAllSyntaxKinds();
		for (const auto& kind : kinds)
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
		Assert::IsTrue(untestedTokenKinds.empty());
	}

	TEST_METHOD(Lexer_Lexes_Token)
	{
		auto data = GetTokenData();
		for (const auto& it : data)
		{
			auto tokens = MCF::SyntaxTree::ParseTokens(it.second);
			Assert::IsTrue(tokens.size() == 1);
			Assert::IsTrue(it.first == tokens[0]->Kind());
			Assert::AreEqual(it.second, tokens[0]->Text());
		}
	}

	TEST_METHOD(Lexer_Lexes_TokenPairs)
	{
		auto data = GetTokenPairsData();
		for (const auto& it : data)
		{
			auto[t1kind, t1text, t2kind, t2text] = it;
			auto text = t1text+t2text;
			auto tokens = MCF::SyntaxTree::ParseTokens(text);
			Assert::IsTrue(2 == tokens.size());

			Assert::IsTrue(t1kind == tokens[0]->Kind());
			Assert::AreEqual(t1text, tokens[0]->Text());
			Assert::IsTrue(t2kind == tokens[1]->Kind());
			Assert::AreEqual(t2text, tokens[1]->Text());
		}
	}

	TEST_METHOD(Lexer_Lexes_TokenPairs_WithSeperators)
	{
		auto data = GetTokenPairsWithSeperator();
		for (const auto& it : data)
		{
			auto[t1kind, t1text, skind, stext, t2kind, t2text] = it;
			auto text = t1text + stext + t2text;
			auto tokens = MCF::SyntaxTree::ParseTokens(text);
			Assert::IsTrue(3 == tokens.size());

			Assert::IsTrue(t1kind == tokens[0]->Kind());
			Assert::AreEqual(t1text, tokens[0]->Text());
			Assert::IsTrue(skind == tokens[1]->Kind());
			Assert::AreEqual(stext, tokens[1]->Text());
			Assert::IsTrue(t2kind == tokens[2]->Kind());
			Assert::AreEqual(t2text, tokens[2]->Text());
		}
	}

	TEST_METHOD(GetText_RoundTrip)
	{
		auto kinds = MCF::GetAllSyntaxKinds();
		for (const auto& kind : kinds)
		{
			auto text = MCF::GetText(kind);
			if (text.length() > 0)
			{
				auto tokens = MCF::SyntaxTree::ParseTokens(text);
				Assert::IsTrue(1 == tokens.size());

				Assert::IsTrue(kind == tokens[0]->Kind());
				Assert::AreEqual(text, tokens[0]->Text());
			}
		}
	}

private:

	static std::vector<std::pair<MCF::SyntaxKind, std::string>> GetTokens()
	{
		auto result = std::vector<std::pair<MCF::SyntaxKind, std::string>>();

		// fixed tokens
		auto kinds = MCF::GetAllSyntaxKinds();
		for (const auto& kind : kinds)
		{
			auto text = MCF::GetText(kind);
			if (text.length() > 0)
				result.emplace_back(kind, text);
		}

		// dynamic tokens
		result.emplace_back(MCF::SyntaxKind::NumberToken, "1");
		result.emplace_back(MCF::SyntaxKind::NumberToken, "123");
		result.emplace_back(MCF::SyntaxKind::IdentifierToken, "a");
		result.emplace_back(MCF::SyntaxKind::IdentifierToken, "abc");
		return result;
	}

	static std::vector<std::pair<MCF::SyntaxKind, std::string>> GetSeperators()
	{
		auto result = std::vector<std::pair<MCF::SyntaxKind, std::string>>();
		result.emplace_back(MCF::SyntaxKind::WhitespaceToken, " ");
		result.emplace_back(MCF::SyntaxKind::WhitespaceToken, "  ");
		result.emplace_back(MCF::SyntaxKind::WhitespaceToken, "\r");
		result.emplace_back(MCF::SyntaxKind::WhitespaceToken, "\n");
		result.emplace_back(MCF::SyntaxKind::WhitespaceToken, "\r\n");
		return result;
	}

	static std::vector<std::pair<MCF::SyntaxKind, std::string>> GetTokenData()
	{
		auto tokens = GetTokens();
		auto seperators = GetSeperators();
		tokens.insert(tokens.end(), seperators.begin(), seperators.end());
		tokens.shrink_to_fit();
		return tokens;
	}

	static std::vector<std::tuple<MCF::SyntaxKind, std::string,
		MCF::SyntaxKind, std::string>> GetTokenPairsData()
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

	static std::vector<std::tuple<MCF::SyntaxKind, std::string,
		MCF::SyntaxKind, std::string, MCF::SyntaxKind, std::string>> GetTokenPairsWithSeperator()
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

	static bool RequiresSeperators(MCF::SyntaxKind t1kind, MCF::SyntaxKind t2kind)
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

		return false;
	}
};

}//Tests