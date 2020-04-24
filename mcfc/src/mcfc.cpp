#include <iostream>

#include <cxxopts.hpp>

#include "Compilation.h"
#include "Diagnostic.h"
#include "IO.h"
#include "Parsing.h"

namespace fs = std::filesystem;

int main(int argc, char** argv)
{
	if (argc == 1)
	{
		std::cout << "usage: mcfc [OPTIONS...]" << '\n';
		return 0;
	}

	auto options = cxxopts::Options("mcfc", "A C++ fork of Minsk Compiler");
	options.add_options()
		("h,help", "Prints help", cxxopts::value<bool>()->default_value("false"))
		("m,module", "The name of module", cxxopts::value<std::string>())
		("o,output", "The output path to create the binary", cxxopts::value<fs::path>())
		("p,path", "Source path", cxxopts::value<fs::path>())
		;

	auto optResult = options.parse(argc, argv);

	if (optResult.count("help") > 0)
	{
		std::cout << options.help() << '\n';
		return 0;
	}

	auto sourcePath = optResult["path"].count() > 0 ?
		optResult["path"].as<fs::path>()
		: fs::path();
	if (sourcePath.empty())
	{
		std::cerr << "error: need one source file" << '\n';
		return 1;
	}

	auto outputPath = optResult["output"].count() > 0 ?
		optResult["output"].as<fs::path>()
		: fs::path();
	if (outputPath.empty())
	{
		outputPath = sourcePath;
		outputPath.replace_extension("obj");
	}
	auto moduleName = optResult["module"].count() > 0 ?
		optResult["module"].as<std::string>()
		: std::string();
	if (moduleName.empty())
		moduleName = sourcePath.filename().string();

	auto trees = std::vector<std::unique_ptr<MCF::SyntaxTree>>();
	auto hasError = false;

	for (int i = 1; i < argc; ++i)
	{
		if (!fs::exists(sourcePath))
		{
			std::cerr << "error: file '" << sourcePath << "' doesn't exist.\n";
			hasError = true;
			continue;
		} else
		{
			auto tree = MCF::SyntaxTree::Load(sourcePath);
			trees.push_back(std::move(tree));
		}
	}

	if (hasError)
		return 1;

	auto compilation = MCF::Compilation::Create(std::move(trees));
	auto result = compilation->Emit(moduleName, outputPath);

	if (!result.empty())
	{
		auto writer = MCF::IndentedTextWriter(std::cerr);
		writer.WriteDiagnostics(result);
		return 1;
	}

	return 0;
}