// mcfc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>
#include <string>

#include "SyntaxNode.h"
#include "Diagnostic.h"

int main()
{
	std::cout << "Enter expression. Q to quit.\n" << "> ";
	std::string input;
	while (std::getline(std::cin, input) && input!="Q")
	{	
		MCF::Lexer lexer(input);
		auto token =std::make_unique<MCF::SyntaxToken>(lexer.Lex());
		while (token->Kind() != MCF::SyntaxKind::EndOfFileToken)
		{
			auto diagnostics = lexer.Diagnostics();			
			if (diagnostics->size() > 0)
				for (const auto& it : *diagnostics)
					std::cout << it.Message();
			std::cout << token->Text() <<" "<<
				static_cast<std::underlying_type<MCF::SyntaxKind>::type>(token->Kind());
			token = std::make_unique<MCF::SyntaxToken>(lexer.Lex());
		}
		std::cout << "\n> ";
	}
	return 0;
}
