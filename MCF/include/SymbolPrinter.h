#pragma once

#include "IO.h"

namespace MCF {

struct Symbol;

void Write(const Symbol& symbol, std::ostream& out);

}//MCF