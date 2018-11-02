// mcfc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>

#include "SyntaxNode.h"
#include "Parser.h"
#include "Diagnostic.h"

void PrintTree(const MCF::SyntaxNode* node, std::string indent = "", bool isLast = true)
{
	std::string marker = isLast ? "+--" : "---";//"└──" : "├──";
	std::cout << indent << marker;
	auto token = dynamic_cast<const MCF::SyntaxToken*>(node);
	if (token != nullptr && token->Value().HasValue())
	{
		std::cout << " " << token->Value().GetValue<long>();
	}
	std::cout << std::endl;
	indent += isLast ? "   " : "|  ";
	auto children = node->GetChildren();
	if (children.size() > 0)
	{
		auto lastChild = children.back();
		for (const auto& child : children)
			PrintTree(child, indent, lastChild == child);
	}
}

int main()
{
	std::cout << "Enter expression. Q to quit.\n" << "> ";
	std::string input;
	while (std::getline(std::cin, input) && input != "Q")
	{
		auto tree = MCF::SyntaxTree::Parse(input);
		PrintTree(tree.Root());
		std::cout << "> ";
	}
	return 0;
}
