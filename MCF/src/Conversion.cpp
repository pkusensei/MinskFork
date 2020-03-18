#include "Conversion.h"

#include <stdexcept>

#include "Symbols.h"

namespace MCF {

Conversion Conversion::GetConversion(ConversionEnum  kind)
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

Conversion Conversion::Classify(const TypeSymbol& from, const TypeSymbol& to)
{
	if (from == to)
		return GetConversion(ConversionEnum::Identity);
	if (from == GetTypeSymbol(TypeEnum::Int)
		|| from == GetTypeSymbol(TypeEnum::Bool))
	{
		if (to == GetTypeSymbol(TypeEnum::String))
			return GetConversion(ConversionEnum::Explicit);
	}
	if (from == GetTypeSymbol(TypeEnum::String))
	{
		if (to == GetTypeSymbol(TypeEnum::Int)
			|| to == GetTypeSymbol(TypeEnum::Bool))
			return GetConversion(ConversionEnum::Explicit);
	}
	return GetConversion(ConversionEnum::None);
}

}//MCF
