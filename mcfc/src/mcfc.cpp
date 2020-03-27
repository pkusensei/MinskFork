#include <fstream>
#include <iostream>
#include <sstream>

#include "Compilation.h"
#include "Diagnostic.h"
#include "IO.h"
#include "Parsing.h"

int main(int argc, char** argv)
{
	if (argc == 1)
	{
		std::cerr << "usage: mcfc <source-paths>" << '\n';
		return 0;
	}
	if (argc > 2)
	{
		std::cout << "error: only one path supported right now" << '\n';
		return 0;
	}

	auto path = argv[1];
	auto file = std::ifstream(path);
	auto ss = std::stringstream();
	ss << file.rdbuf();

	auto tree = MCF::SyntaxTree::Parse(ss.str());
	auto compilation = MCF::Compilation(tree);
	MCF::VarMap variables;
	auto result = compilation.Evaluate(variables);

	if (result.Diagnostics()->empty())
	{
		if (result.Value().HasValue())
			std::cout << result.Value() << '\n';
	} else
	{
		auto writer = MCF::IndentedTextWriter(std::cerr);
		writer.WriteDiagnostics(*result.Diagnostics());
	}

}