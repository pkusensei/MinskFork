#pragma once

#include "Symbols.h"

namespace MCF {

enum class ConversionEnum
{
	None, Identity, Implicit, Explicit
};

ConversionEnum Classify(ConstTypeRef from, ConstTypeRef to);

}//MCF
