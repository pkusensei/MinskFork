// mcfc.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"

#include "Repl.h"

int main()
{
	auto repl = McfRepl();
	repl.Run();
	return 0;
}
