#include "AssertingHelper.h"

#include <catch2/catch.hpp>

#include <stack>

#include "Diagnostic.h"
#include "SourceText.h"
#include "SyntaxToken.h"

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
		REQUIRE(_position == _nodes.size());
}

void AssertingHelper::AssertNode(MCF::SyntaxKind kind)
{
	// Catch2 catches exceptions internally
	REQUIRE(_position < _nodes.size());
	REQUIRE(kind == _nodes[_position]->Kind());
	REQUIRE_FALSE(MCF::IsToken(_nodes[_position]->Kind()));
	++_position;
}

void AssertingHelper::AssertToken(MCF::SyntaxKind kind, std::string_view text)
{
	REQUIRE(_position < _nodes.size());
	REQUIRE(kind == _nodes[_position]->Kind());
	REQUIRE(MCF::IsToken(kind));
	auto p = static_cast<const MCF::SyntaxToken*>(_nodes[_position]);
	REQUIRE(text == p->Text());
	++_position;
}
