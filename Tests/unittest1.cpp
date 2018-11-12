#include "stdafx.h"
#include "CppUnitTest.h"

#include <set>

#include "..\MCF\SyntaxNode.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace Tests {

TEST_CLASS(LexerTests)
{
public:

	TEST_METHOD(Lexer_Lexes_AllTokens)
	{
		auto tokenKinds = std::vector<MCF::SyntaxKind>();
		for (auto kind = MCF::SyntaxKind::BadToken;
			 kind != MCF::SyntaxKind::AssignmentExpression; kind++)
		{
			auto text = MCF::GetSyntaxKindName(kind);
			if (MCF::StringEndsWith(text, "Keyword") || MCF::StringEndsWith(text, "Token"))
				tokenKinds.emplace_back(kind);
		}
		std::set<MCF::SyntaxKind> untestedTokenKinds(tokenKinds.begin(), tokenKinds.end());
		untestedTokenKinds.erase(MCF::SyntaxKind::BadToken);
		untestedTokenKinds.erase(MCF::SyntaxKind::EndOfFileToken);

		auto tokens = GetTokens();
		auto seperators = GetSeperators();
		tokens.insert(tokens.end(), seperators.begin(), seperators.end());
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

private:

	static std::vector<std::pair<MCF::SyntaxKind, std::string>> GetTokens()
	{
		auto result = std::vector<std::pair<MCF::SyntaxKind, std::string>>();
		
		for (auto kind = MCF::SyntaxKind::BadToken;
			 kind != MCF::SyntaxKind::AssignmentExpression; ++kind)
		{
			auto text = MCF::GetText(kind);
			result.emplace_back(kind, text);
		}

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
};

}// Tests