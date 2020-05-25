#pragma once

#include <filesystem>

namespace MCF {

namespace fs = std::filesystem;

class BoundProgram;
class DiagnosticBag;

[[nodiscard]] DiagnosticBag Emit(const BoundProgram& program, const std::string& moduleName,
	const fs::path& outPath);

} //MCF