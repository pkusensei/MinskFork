// mcfc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <cctype>
#include <iostream>

#include "Compilation.h"
#include "Diagnostic.h"
#include "Parser.h"
#include "SyntaxNode.h"

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

bool IsStringBlank(const std::string& s)
{
	bool result = true;
	for (const auto& c : s)
		if (!std::isspace(c))
			result = false;
	return result;
}

int main()
{
	std::cout << "Enter expression.\n";
	std::string text, input;
	std::unordered_map<MCF::VariableSymbol, MCF::ValueType, MCF::VariableHash> variables;
	while (true)
	{
		if (text.length() == 0)
			std::cout << "> ";
		else std::cout << "| ";

		std::getline(std::cin, input);
		auto isBlank = IsStringBlank(input);
		if (text.length() == 0 && isBlank)
			break;
		
		text += input+"\r"; // HACK "\r" works, "\n" doesn't
		auto tree = std::make_unique<MCF::SyntaxTree>(MCF::SyntaxTree::Parse(text));
		if (!isBlank && tree->Diagnostics()->size() > 0)
			continue;

		MCF::Compilation compilation(tree);
		auto result = compilation.Evaluate(variables);
		auto diagnostics = result.Diagnostics();
		if (diagnostics == nullptr)
		{
			auto value = result.Value();
			PrintValue(value);
		} else
		{
			for (const auto& diag : *diagnostics)
			{
				auto lineIndex = tree->Text().GetLineIndex(diag.Span().Start());
				auto lineNumber = lineIndex + 1;
				auto line = tree->Text().Lines()[lineIndex];
				auto character = diag.Span().Start() - line.Start() + 1;

				std::cout << "\n" << "(" << lineNumber << ", " << character << ")";
				std::cout << diag.Message() << "\n";

				auto prefixSpan = MCF::TextSpan::FromBounds(line.Start(), diag.Span().Start());
				auto suffixSpan = MCF::TextSpan::FromBounds(diag.Span().End(), line.End());

				auto prefix = tree->Text().ToString(prefixSpan);
				auto error = tree->Text().ToString(diag.Span());
				auto suffix = tree->Text().ToString(suffixSpan);
				std::cout << "    " << prefix << error << suffix << "\n";
			}
			std::cout << std::endl;
		}
		tree->Root()->WriteTo(std::cout);
		text = "";
	}
	return 0;
}
