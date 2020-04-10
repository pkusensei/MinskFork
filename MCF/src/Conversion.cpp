#include "Conversion.h"

#include <stdexcept>

namespace MCF {

ConversionEnum Classify(const TypeSymbol& from, const TypeSymbol& to)
{
	if (from == to)
		return ConversionEnum::Identity;
	if (from != TypeSymbol(TypeEnum::Void)
		&& to == TypeSymbol(TypeEnum::Any))
	{
		return ConversionEnum::Implicit;
	}
	if (from == TypeSymbol(TypeEnum::Any)
		&& to != TypeSymbol(TypeEnum::Void))
	{
		return ConversionEnum::Explicit;
	}
	if (from == TypeSymbol(TypeEnum::Int)
		|| from == TypeSymbol(TypeEnum::Bool))
	{
		if (to == TypeSymbol(TypeEnum::String))
			return ConversionEnum::Explicit;
	}
	if (from == TypeSymbol(TypeEnum::String))
	{
		if (to == TypeSymbol(TypeEnum::Int)
			|| to == TypeSymbol(TypeEnum::Bool))
			return ConversionEnum::Explicit;
	}
	return ConversionEnum::None;
}

}//MCF
