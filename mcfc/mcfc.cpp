// mcfc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>

#include "SyntaxNode.h"
#include "Parser.h"
#include "Diagnostic.h"
#include "Compilation.h"

void PrintTree(const MCF::SyntaxNode* node, std::string indent = "", bool isLast = true)
{
	std::string marker = isLast ? "+--" : "---";//"└──" : "├──";
	std::cout << indent << marker << MCF::GetSyntaxKindName(node->Kind());
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

void PrintValue(const MCF::ValueType& value)
{
	auto id = MCF::GetValueTypeId(value.Type());
	std::cout << "Result value is ";
	switch (id)
	{
		case 1:
			std::cout << MCF::GetTypeName(value.Type()) << " " << value.GetValue<long>() << std::endl;
			break;
		case 2:
			std::cout << MCF::GetTypeName(value.Type()) << " " << static_cast<bool>(value.GetValue<bool>()) << std::endl;
			break;
		default:
			std::cout << "Not valid value or type";
			break;
	}
}

int main()
{
	std::cout << "Enter expression. Q to quit.\n" << "> ";
	std::string input;
	std::unordered_map<MCF::VariableSymbol, MCF::ValueType, MCF::VariableHash> variables;
	while (std::getline(std::cin, input) && input != "Q")
	{
		auto tree = std::make_unique<MCF::SyntaxTree>(MCF::SyntaxTree::Parse(input));
		auto diagnostics = tree->Diagnostics();
		if (diagnostics->size() > 0)
		{
			for (const auto& it : *diagnostics)
				std::cout << it.Message() << "\n";
			std::cout << "> ";
			continue;
		} else
		{
			MCF::Compilation compilation(tree);
			auto result = compilation.Evaluate(variables);
			diagnostics = result.Diagnostics();
			if (diagnostics != nullptr)
				for (const auto& it : *diagnostics)
					std::cout << it.Message() << "\n";
			else
			{
				auto value = result.Value();
				PrintValue(value);
			}
			PrintTree(compilation.Syntax()->Root());
			std::cout << "> ";
		}
	}
	return 0;
}
