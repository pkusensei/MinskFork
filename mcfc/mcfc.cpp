// mcfc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>
#include <string>

#include "Parser.h"
#include "Diagnostic.h"

int main()
{
	std::cout << "Enter expression. Q to quit.\n" << "> ";
	std::string input;
	while (std::getline(std::cin, input) && input!="Q")
	{	
		MCF::Parser parser(input);
		std::cout << "\n> ";
	}
	return 0;
}
