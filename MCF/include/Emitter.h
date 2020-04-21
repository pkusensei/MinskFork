#pragma once

#include <filesystem>

namespace MCF {

namespace fs = std::filesystem;

class DiagnosticBag;

DiagnosticBag Emit(const std::string& moduleName, const fs::path& outPath);

} //MCF