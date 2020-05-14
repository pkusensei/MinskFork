#include <catch2/catch.hpp>

#include "Parsing.h"
#include "StringHelper.h"

#include "AssertingHelper.h"

const MCF::ExpressionSyntax* ParseExpression(const MCF::SyntaxTree* tree);
std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetBinaryOperatorPairsData();
std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetUnaryOperatorPairsData();

TEST_CASE("Parser honors precedences in BinaryExpression", "[Parser]")
{
	auto data = GetBinaryOperatorPairsData();
	for (const auto& it : data)
	{
		auto op1Precedence = MCF::GetBinaryOperatorPrecedence(it.first);
		auto op2Precedence = MCF::GetBinaryOperatorPrecedence(it.second);
		auto op1Text = MCF::GetText(it.first);
		auto op2Text = MCF::GetText(it.second);
		auto text = MCF::BuildStringFrom("a ", op1Text, " b ", op2Text, " c");
		auto tree = MCF::SyntaxTree::Parse(text);
		auto expression = ParseExpression(tree.get());
		auto e = AssertingHelper(expression);

		if (op1Precedence >= op2Precedence)
		{
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
			e.AssertToken(it.first, op1Text);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "b");
			e.AssertToken(it.second, op2Text);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "c");
		} else
		{
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
			e.AssertToken(it.first, op1Text);
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "b");
			e.AssertToken(it.second, op2Text);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "c");
		}
	}
}

TEST_CASE("Parser honors precedences in UinaryExpression", "[Parser]")
{
	auto data = GetUnaryOperatorPairsData();
	for (const auto& it : data)
	{
		auto unaryPrecedence = MCF::GetUnaryOperatorPrecedence(it.first);
		auto binaryPrecedence = MCF::GetBinaryOperatorPrecedence(it.second);
		auto unaryText = MCF::GetText(it.first);
		auto binaryText = MCF::GetText(it.second);
		auto text = MCF::BuildStringFrom(unaryText, " a ", binaryText, " b");
		auto tree = MCF::SyntaxTree::Parse(text);
		auto expression = ParseExpression(tree.get());
		auto e = AssertingHelper(expression);

		if (unaryPrecedence >= binaryPrecedence)
		{
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::UnaryExpression);
			e.AssertToken(it.first, unaryText);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
			e.AssertToken(it.second, binaryText);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "b");

		} else
		{
			e.AssertNode(MCF::SyntaxKind::UnaryExpression);
			e.AssertToken(it.first, unaryText);
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
			e.AssertToken(it.second, binaryText);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "b");
		}
	}
}

TEST_CASE("Parser honors precedences in PostfixExpression", "[Parser]")
{
	std::string text = "a---b";
	auto tree = MCF::SyntaxTree::Parse(text);
	auto expression = ParseExpression(tree.get());
	auto e = AssertingHelper(expression);

	e.AssertNode(MCF::SyntaxKind::BinaryExpression);
	e.AssertNode(MCF::SyntaxKind::PostfixExpression);
	e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
	e.AssertToken(MCF::SyntaxKind::MinusMinusToken, "--");
	e.AssertNode(MCF::SyntaxKind::NameExpression);
	e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
	e.AssertToken(MCF::SyntaxKind::MinusToken, "-");
	e.AssertNode(MCF::SyntaxKind::NameExpression);
	e.AssertToken(MCF::SyntaxKind::IdentifierToken, "b");
}

const MCF::ExpressionSyntax* ParseExpression(const MCF::SyntaxTree* tree)
{
	auto root = tree->Root();
	auto& members = root->Members();
	CHECK(1 == members.size());
	auto gs = dynamic_cast<MCF::GlobalStatementSyntax*>(members[0].get());
	REQUIRE_FALSE(nullptr == gs);
	auto s = gs->Statement();
	auto es = dynamic_cast<const MCF::ExpressionStatementSyntax*>(s);
	REQUIRE_FALSE(nullptr == es);
	return es->Expression();
}

std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetBinaryOperatorPairsData()
{
	auto data = MCF::GetBinaryOperatorKinds();
	auto result = std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>>();
	for (const auto& op1 : data)
		for (const auto& op2 : data)
			result.emplace_back(op1, op2);
	return result;
}

std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetUnaryOperatorPairsData()
{
	auto unaryData = MCF::GetUnaryOperatorKinds();
	auto binaryData = MCF::GetBinaryOperatorKinds();
	auto result = std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>>();
	for (const auto& unary : unaryData)
		for (const auto& binary : binaryData)
			result.emplace_back(unary, binary);
	return result;
}
