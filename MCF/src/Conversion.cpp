#include "Conversion.h"

#include <stdexcept>

namespace MCF {

ConversionEnum Classify(ConstTypeRef from, ConstTypeRef to)
{
	if (from.get() == to)
		return ConversionEnum::Identity;
	if (from.get() != TypeSymbol::Get(TypeEnum::Void)
		&& to.get() == TypeSymbol::Get(TypeEnum::Any))
	{
		return ConversionEnum::Implicit;
	}
	if (from.get() == TypeSymbol::Get(TypeEnum::Any)
		&& to.get() != TypeSymbol::Get(TypeEnum::Void))
	{
		return ConversionEnum::Explicit;
	}
	if (from.get() == TypeSymbol::Get(TypeEnum::Int)
		|| from.get() == TypeSymbol::Get(TypeEnum::Bool))
	{
		if (to.get() == TypeSymbol::Get(TypeEnum::String))
			return ConversionEnum::Explicit;
	}
	if (from.get() == TypeSymbol::Get(TypeEnum::String))
	{
		if (to.get() == TypeSymbol::Get(TypeEnum::Int)
			|| to.get() == TypeSymbol::Get(TypeEnum::Bool))
			return ConversionEnum::Explicit;
	}
	return ConversionEnum::None;
}

}//MCF
