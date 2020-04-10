#pragma once

#include "Symbols.h"

namespace MCF {

enum class ConversionEnum
{
	None, Identity, Implicit, Explicit
};

ConversionEnum Classify(const TypeSymbol& from, const TypeSymbol& to);

}//MCF
