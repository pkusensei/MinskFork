#include "Conversion.h"

#include <stdexcept>

namespace MCF {

Conversion Conversion::GetConversion(ConversionEnum kind)
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

Conversion Conversion::Classify(ConstTypeRef from, ConstTypeRef to)
{
	if (from.get() == to)
		return GetConversion(ConversionEnum::Identity);
	if (from.get() != TypeSymbol::Get(TypeEnum::Void)
		&& to.get() == TypeSymbol::Get(TypeEnum::Any))
	{
		return GetConversion(ConversionEnum::Implicit);
	}
	if (from.get() == TypeSymbol::Get(TypeEnum::Any)
		&& to.get() != TypeSymbol::Get(TypeEnum::Void))
	{
		return GetConversion(ConversionEnum::Explicit);
	}
	if (from.get() == TypeSymbol::Get(TypeEnum::Int)
		|| from.get() == TypeSymbol::Get(TypeEnum::Bool))
	{
		if (to.get() == TypeSymbol::Get(TypeEnum::String))
			return GetConversion(ConversionEnum::Explicit);
	}
	if (from.get() == TypeSymbol::Get(TypeEnum::String))
	{
		if (to.get() == TypeSymbol::Get(TypeEnum::Int)
			|| to.get() == TypeSymbol::Get(TypeEnum::Bool))
			return GetConversion(ConversionEnum::Explicit);
	}
	return GetConversion(ConversionEnum::None);
}

}//MCF
