#include <fstream>
#include <iostream>
#include <sstream>

#include "Compilation.h"
#include "Diagnostic.h"
#include "IO.h"
#include "Parsing.h"

namespace fs = std::filesystem;

int main(int argc, char** argv)
{
	if (argc == 1)
	{
		std::cerr << "usage: mcfc <source-paths>" << '\n';
		return 0;
	}

	auto trees = std::vector<std::unique_ptr<MCF::SyntaxTree>>();
	auto hasError = false;

	for (int i = 1; i < argc; ++i)
	{
		auto file = argv[1];
		auto path = fs::path(file);
		if (!fs::exists(path))
		{
			std::cerr << "error: file '" << path << "' doesn't exist.\n";
			hasError = true;
			continue;
		} else
		{
			auto tree = MCF::SyntaxTree::Load(path);
			trees.push_back(std::move(tree));
		}
	}

	auto compilation = MCF::Compilation(std::move(trees));
	MCF::VarMap variables;
	auto result = compilation.Evaluate(variables);

	if (result.Diagnostics().empty())
	{
		if (result.Value().HasValue())
			std::cout << result.Value() << '\n';
	} else
	{
		auto writer = MCF::IndentedTextWriter(std::cerr);
		writer.WriteDiagnostics(result.Diagnostics());
	}

}