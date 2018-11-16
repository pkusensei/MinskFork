#include "stdafx.h"
#include "CppUnitTest.h"

#include <stack>

#include "..\MCF\Diagnostic.h"
#include "..\MCF\SourceText.h"
#include "..\MCF\Syntax.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace Tests {

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
	AssertingHelper(const MCF::SyntaxNode* node)
		:_hasErrors(false), _nodes({}), _position(0)
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

	~AssertingHelper()
	{
		if (!_hasErrors)
			Assert::AreEqual(_position, _nodes.size());
	}

	void AssertNode(MCF::SyntaxKind kind)
	{
		try
		{
			Assert::IsTrue(_position < _nodes.size());
			Assert::IsTrue(kind == _nodes[_position]->Kind());
			Assert::IsFalse(typeid(MCF::SyntaxToken) == typeid(*_nodes[_position]));
			++_position;
		} catch (std::exception& e)
		{
			if (MarkFailed())
				throw e;
		}
	}

	void AssertToken(MCF::SyntaxKind kind, const std::string& text)
	{
		try
		{
			Assert::IsTrue(_position < _nodes.size());
			Assert::IsTrue(kind == _nodes[_position]->Kind());
			Assert::IsTrue(typeid(MCF::SyntaxToken) == typeid(*_nodes[_position]));
			auto p = dynamic_cast<const MCF::SyntaxToken*>(_nodes[_position]);
			Assert::AreEqual(text, p->Text());
			++_position;
		} catch (std::exception& e)
		{
			if (MarkFailed())
				throw e;
		}
	}
};

TEST_CLASS(ParserTests)
{
private:
	std::unique_ptr<MCF::SyntaxTree> _tree;

public:

	TEST_METHOD(Parser_BinaryExpression_HonorsPrecedences)
	{
		auto data = GetBinaryOperatorPairsData();
		for (const auto& it : data)
		{
			auto op1Precedence = MCF::GetBinaryOperatorPrecedence(it.first);
			auto op2Precedence = MCF::GetBinaryOperatorPrecedence(it.second);
			auto op1Text = MCF::GetText(it.first);
			auto op2Text = MCF::GetText(it.second);
			auto text = "a " + op1Text + " b " + op2Text + " c";
			auto expression = ParseExpression(text);
			AssertingHelper e(expression);

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

	TEST_METHOD(Parser_UnaryExpression_HonorsPrecedences)
	{
		auto data = GetUnaryOperatorPairsData();
		for (const auto& it : data)
		{
			auto unaryPrecedence = MCF::GetUnaryOperatorPrecedence(it.first);
			auto binaryPrecedence = MCF::GetBinaryOperatorPrecedence(it.second);
			auto unaryText = MCF::GetText(it.first);
			auto binaryText = MCF::GetText(it.second);
			auto text = unaryText + " a " + binaryText + " b";
			auto expression = ParseExpression(text);
			AssertingHelper e(expression);

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

private:
	const MCF::ExpressionSyntax* ParseExpression(const std::string& text)
	{
		_tree = std::make_unique<MCF::SyntaxTree>(MCF::SyntaxTree::Parse(text));
		auto root = _tree->Root();
		auto statement = root->Statement();
		Assert::IsTrue(typeid(MCF::ExpressionStatementSyntax) == typeid(*statement));
		auto p = dynamic_cast<const MCF::ExpressionStatementSyntax*>(statement);
		return p->Expression();
	}

	static std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetBinaryOperatorPairsData()
	{
		auto data = MCF::GetBinaryOperatorKinds();
		auto result = std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>>();
		for (const auto& op1 : data)
			for (const auto& op2 : data)
				result.emplace_back(op1, op2);
		return result;
	}

	static std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>> GetUnaryOperatorPairsData()
	{
		auto unaryData = MCF::GetUnaryOperatorKinds();
		auto binaryData = MCF::GetBinaryOperatorKinds();
		auto result = std::vector<std::pair<MCF::SyntaxKind, MCF::SyntaxKind>>();
		for (const auto& unary : unaryData)
			for (const auto& binary : binaryData)
				result.emplace_back(unary, binary);
		return result;
	}
};

TEST_CLASS(SourceTextTests)
{
public:
	TEST_METHOD(SourceText_IncludesLastLine)
	{
		auto data = std::vector<std::pair<std::string, size_t>>{
			std::pair<std::string, size_t>(".", 1),
			std::pair<std::string, size_t>("\r\n", 2),
			std::pair<std::string, size_t>(".\r\n\r\n", 3),
		};
		for (const auto& it : data)
		{
			auto sourceText = MCF::SourceText::From(it.first);
			auto lines = sourceText.Lines();
			Assert::AreEqual(it.second, lines.size());
		}
	}
};

}//Tests