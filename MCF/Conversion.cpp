#include "stdafx.h"
#include "Conversion.h"

#include "Symbols.h"

namespace MCF {

Conversion Conversion::GetConversion(const ConversionEnum & kind)
{
	switch (kind)
	{
		case ConversionEnum::None:
			return Conversion(false, false, false);
		case ConversionEnum::Identity:
			return Conversion(true, true, true);
		case ConversionEnum::Implicit:
			return Conversion(true, false, true);
		case ConversionEnum::Explicit:
			return Conversion(true, false, false);
		default:
			throw std::invalid_argument("Unexpected ConversionEnum value");
	}
}

Conversion Conversion::Classify(const TypeSymbol & from, const TypeSymbol & to)
{
	if (from == to)
		return GetConversion(ConversionEnum::Identity);
	if (from == TypeSymbol::GetType(TypeEnum::Int)
		|| from == TypeSymbol::GetType(TypeEnum::Bool))
	{
		if (to == TypeSymbol::GetType(TypeEnum::String))
			return GetConversion(ConversionEnum::Explicit);
	}
	if (from == TypeSymbol::GetType(TypeEnum::String))
	{
		if (to == TypeSymbol::GetType(TypeEnum::Int)
			|| to == TypeSymbol::GetType(TypeEnum::Bool))
			return GetConversion(ConversionEnum::Explicit);
	}
	return GetConversion(ConversionEnum::None);
}

}//MCF
