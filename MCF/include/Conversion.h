#pragma once

namespace MCF {

class TypeSymbol;

enum class ConversionEnum
{
	None, Identity, Implicit, Explicit
};

class Conversion
{
private:
	bool _exists;
	bool _isIdentity;
	bool _isImplicit;

	Conversion(bool exists, bool isIdentity, bool isImplicit)noexcept
		:_exists(exists), _isIdentity(isIdentity), _isImplicit(isImplicit)
	{
	}

	static Conversion GetConversion(ConversionEnum kind);

public:

	constexpr bool Exists()const noexcept { return _exists; }
	constexpr bool IsIdentity()const noexcept { return _isIdentity; }
	constexpr bool IsImplicit()const noexcept { return _isImplicit; }
	constexpr bool IsExplicit()const noexcept { return _exists && !_isImplicit; }

	static Conversion Classify(const TypeSymbol& from, const TypeSymbol& to);
};

}//MCF
