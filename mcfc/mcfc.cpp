// mcfc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>
#include <string>

#include "SyntaxNode.h"

int main()
{
	std::cout << "Enter expression.\n" << "> ";
	std::string input;
	while (std::getline(std::cin, input))
	{
		auto lexer = std::make_unique<MCF::Lexer>(input);
		auto token = std::make_unique<MCF::SyntaxToken>( lexer->Lex());
		while (token->Kind() != MCF::SyntaxKind::EndOfFileToken)
		{
			std::cout << token->Text();
			token = std::make_unique<MCF::SyntaxToken>(lexer->Lex());
		}
	}
	return 0;
}
