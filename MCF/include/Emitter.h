#pragma once

#include <string>

namespace MCF {

class DiagnosticBag;

DiagnosticBag Emit(const std::string& outPath);

} //MCF