#include "stdafx.h"
#include "SourceText.h"

namespace MCF {

TextSpan::TextSpan(size_t start, size_t length)
	:_span(start, length)
{
}

TextSpan & TextSpan::operator=(const TextSpan & other)
{
	_span = other._span;
	return *this;
}

TextSpan TextSpan::FromBounds(size_t start, size_t end)
{
	return TextSpan(start, end - start);
}

}//MCF