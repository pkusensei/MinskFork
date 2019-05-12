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

	Conversion(bool exists, bool isIdentity, bool isImplicit)
		:_exists(exists), _isIdentity(isIdentity), _isImplicit(isImplicit)
	{
	}

	static Conversion GetConversion(const ConversionEnum& kind);

public:

	bool Exists()const noexcept { return _exists; }
	bool IsIdentity()const noexcept { return _isIdentity; }
	bool IsImplicit()const noexcept { return _isImplicit; }
	bool IsExplicit()const noexcept { return _exists && !_isImplicit; }

	static Conversion Classify(const TypeSymbol& from, const TypeSymbol& to);
};

}//MCF
