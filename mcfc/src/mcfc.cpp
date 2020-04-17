#include <iostream>

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
		return 1;
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

	if (hasError)
		return 1;

	auto compilation = MCF::Compilation::Create(std::move(trees));
	auto result = compilation->Emit("output.o");

	if (!result.empty())
	{
		auto writer = MCF::IndentedTextWriter(std::cerr);
		writer.WriteDiagnostics(result);
		return 1;
	}

	return 0;
}