// mcfc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <cctype>
#include <iostream>

#include "Compilation.h"
#include "Diagnostic.h"
#include "Syntax.h"

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
	std::unique_ptr<MCF::Compilation> previous{nullptr};
	while (true)
	{
		if (text.length() == 0)
			std::cout << "> ";
		else std::cout << "| ";

		std::getline(std::cin, input);
		auto isBlank = IsStringBlank(input);
		if (text.length() == 0 && isBlank)
			break;

		if (!std::cin.eof() && !std::cin.fail())
			text += input + '\r'; // HACK Windows does "\r\n" together
		auto tree = std::make_unique<MCF::SyntaxTree>(MCF::SyntaxTree::Parse(text));
		if (!isBlank && tree->Diagnostics()->size() > 0)
			continue;

		auto compilation = previous == nullptr ? std::make_unique<MCF::Compilation>(tree) 
			:MCF::Compilation::ContinueWith(previous,tree);
		auto result = compilation->Evaluate(variables);
		auto diagnostics = result.Diagnostics();
		if (diagnostics->size() == 0)
		{
			auto value = result.Value();
			value.WriteTo(std::cout);
			tree->Root()->WriteTo(std::cout);
			previous = std::move(compilation);
		} else
		{
			for (const auto& diag : *diagnostics)
			{
				auto lineIndex = tree->Text().GetLineIndex(diag.Span().Start());
				auto lineNumber = lineIndex + 1;
				auto line = tree->Text().Lines()[lineIndex];
				auto character = diag.Span().Start() - line.Start() + 1;

				std::cout << "\n" << "(" << lineNumber << ", " << character << ") ";
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
		std::cout << "\n";
		text = "";
	}
	return 0;
}
