#pragma once

#include <filesystem>

namespace MCF {

namespace fs = std::filesystem;

struct BoundProgram;
class DiagnosticBag;

DiagnosticBag Emit(const BoundProgram& program,
				   const std::string& moduleName,
				   const fs::path& srcPath,
				   const fs::path& outPath);

} //MCF