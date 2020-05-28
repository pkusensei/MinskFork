#include <catch2/catch.hpp>

#include "Parsing.h"
#include "StringHelper.h"

#include "AssertingHelper.h"

namespace {
const MCF::ExpressionSyntax* ParseExpression(const MCF::SyntaxTree& tree);
std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetBinaryOperatorPairsData();
std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetUnaryOperatorPairsData();
} //namespace

TEST_CASE("Parser honors precedences in BinaryExpression", "[Parser]")
{
	auto data = GetBinaryOperatorPairsData();
	for (const auto& [k1, k2] : data)
	{
		auto op1Precedence = MCF::GetBinaryOperatorPrecedence(k1);
		auto op2Precedence = MCF::GetBinaryOperatorPrecedence(k2);
		auto op1Text = MCF::GetText(k1);
		auto op2Text = MCF::GetText(k2);
		auto text = MCF::BuildStringFrom("a ", op1Text, " b ", op2Text, " c");
		auto tree = MCF::SyntaxTree::Parse(text);
		auto expression = ParseExpression(tree);
		auto e = AssertingHelper(expression);

		if (op1Precedence >= op2Precedence)
		{
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
			e.AssertToken(k1, op1Text);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "b");
			e.AssertToken(k2, op2Text);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "c");
		} else
		{
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
			e.AssertToken(k1, op1Text);
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "b");
			e.AssertToken(k2, op2Text);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "c");
		}
	}
}

TEST_CASE("Parser honors precedences in UinaryExpression", "[Parser]")
{
	auto data = GetUnaryOperatorPairsData();
	for (const auto& [k1, k2] : data)
	{
		auto unaryPrecedence = MCF::GetUnaryOperatorPrecedence(k1);
		auto binaryPrecedence = MCF::GetBinaryOperatorPrecedence(k2);
		auto unaryText = MCF::GetText(k1);
		auto binaryText = MCF::GetText(k2);
		auto text = MCF::BuildStringFrom(unaryText, " a ", binaryText, " b");
		auto tree = MCF::SyntaxTree::Parse(text);
		auto expression = ParseExpression(tree);
		auto e = AssertingHelper(expression);

		if (unaryPrecedence >= binaryPrecedence)
		{
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::UnaryExpression);
			e.AssertToken(k1, unaryText);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
			e.AssertToken(k2, binaryText);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "b");

		} else
		{
			e.AssertNode(MCF::SyntaxKind::UnaryExpression);
			e.AssertToken(k1, unaryText);
			e.AssertNode(MCF::SyntaxKind::BinaryExpression);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "a");
			e.AssertToken(k2, binaryText);
			e.AssertNode(MCF::SyntaxKind::NameExpression);
			e.AssertToken(MCF::SyntaxKind::IdentifierToken, "b");
		}
	}
}

TEST_CASE("Parser honors precedences in PostfixExpression", "[Parser]")
{
	std::string text = "a---b";
	auto tree = MCF::SyntaxTree::Parse(text);
	auto expression = ParseExpression(tree);
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

namespace {

const MCF::ExpressionSyntax* ParseExpression(const MCF::SyntaxTree& tree)
{
	auto root = tree.Root();
	auto& members = root->Members();
	REQUIRE(1 == members.size());
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

} //namespace