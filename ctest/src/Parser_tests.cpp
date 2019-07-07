#include <catch.hpp>

#include <stack>

#include "Diagnostic.h"
#include "Parsing.h"
#include "SourceText.h"

class AssertingHelper final
{
private:
	std::vector<const MCF::SyntaxNode*> _nodes;
	bool _hasErrors;
	size_t _position;

	bool MarkFailed()
	{
		_hasErrors = true;
		return false;
	}

public:
	explicit AssertingHelper(const MCF::SyntaxNode* node);
	~AssertingHelper();
	void AssertNode(MCF::SyntaxKind kind);
	void AssertToken(MCF::SyntaxKind kind, const std::string& text);
};

const MCF::ExpressionSyntax* ParseExpression(const MCF::SyntaxTree* tree);
std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetBinaryOperatorPairsData();
std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetUnaryOperatorPairsData();

TEST_CASE("Parser honors precedences in binary expression", "[Parser]")
{
	auto data = GetBinaryOperatorPairsData();
	for (const auto& it : data)
	{
		auto op1Precedence = MCF::GetBinaryOperatorPrecedence(it.first);
		auto op2Precedence = MCF::GetBinaryOperatorPrecedence(it.second);
		auto op1Text = MCF::GetText(it.first);
		auto op2Text = MCF::GetText(it.second);
		auto text = "a " + op1Text + " b " + op2Text + " c";
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

TEST_CASE("Parser honors precedences in uinary expression", "[Parser]")
{
	auto data = GetUnaryOperatorPairsData();
	for (const auto& it : data)
	{
		auto unaryPrecedence = MCF::GetUnaryOperatorPrecedence(it.first);
		auto binaryPrecedence = MCF::GetBinaryOperatorPrecedence(it.second);
		auto unaryText = MCF::GetText(it.first);
		auto binaryText = MCF::GetText(it.second);
		auto text = unaryText + " a " + binaryText + " b";
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

TEST_CASE("Parser honors precedences in postfix expression", "[Parser]")
{
	auto text = "a---b";
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

AssertingHelper::AssertingHelper(const MCF::SyntaxNode* node)
	:_nodes({}), _hasErrors(false), _position(0)
{
	auto stack = std::stack<const MCF::SyntaxNode*>();
	stack.emplace(node);
	while (!stack.empty())
	{
		auto n = stack.top();
		_nodes.emplace_back(n);
		stack.pop();
		auto children = n->GetChildren();
		for (auto it = children.rbegin(); it != children.rend(); ++it)
			stack.emplace(*it);
	}
}

AssertingHelper::~AssertingHelper()
{
	if (!_hasErrors)
		CHECK(_position == _nodes.size());
}

void AssertingHelper::AssertNode(MCF::SyntaxKind kind)
{
	try
	{
		REQUIRE(_position < _nodes.size());
		REQUIRE(kind == _nodes[_position]->Kind());
		REQUIRE_FALSE(typeid(MCF::SyntaxToken) == typeid(decltype(*_nodes[_position])));
		++_position;
	} catch (std::exception& e)
	{
		if (MarkFailed())
			throw e;
	}
}

void AssertingHelper::AssertToken(MCF::SyntaxKind kind, const std::string& text)
{
	try
	{
		REQUIRE(_position < _nodes.size());
		REQUIRE(kind == _nodes[_position]->Kind());
		auto& r = *_nodes[_position];
		REQUIRE(typeid(MCF::SyntaxToken) == typeid(r));
		auto p = dynamic_cast<const MCF::SyntaxToken*>(_nodes[_position]);
		REQUIRE(text == p->Text());
		++_position;
	} catch (std::exception& e)
	{
		if (MarkFailed())
			throw e;
	}
}
