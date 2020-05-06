#include "Conversion.h"

#include <stdexcept>

namespace MCF {

ConversionEnum Classify(const TypeSymbol& from, const TypeSymbol& to)
{
	if (from == to)
		return ConversionEnum::Identity;
	if (from != TYPE_VOID && to == TYPE_ANY)
	{
		return ConversionEnum::Implicit;
	}
	if (from == TYPE_ANY && to != TYPE_VOID)
	{
		return ConversionEnum::Explicit;
	}
	if (from == TYPE_INT || from == TYPE_BOOL)
	{
		if (to == TYPE_STRING)
			return ConversionEnum::Explicit;
	}
	if (from == TYPE_STRING)
	{
		if (to == TYPE_INT || to == TYPE_BOOL)
			return ConversionEnum::Explicit;
	}
	return ConversionEnum::None;
}

}//MCF
