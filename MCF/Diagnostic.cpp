#include "stdafx.h"
#include "Diagnostic.h"

namespace MCF {

Diagnostic::Diagnostic(TextSpan span, std::string& message)
	:_span(span), _message(message)
{
}

Diagnostic::~Diagnostic() = default;

void DiagnosticBag::Report(TextSpan span, std::string& message)
{
	_diagnostics.emplace_back(std::make_unique<Diagnostic>(span, message));
}

DiagnosticBag::~DiagnosticBag() = default;

Diagnostic * DiagnosticBag::GetOneDiagnostic(int idx) const
{
	return _diagnostics[idx].get();
}

DiagnosticBag::iterator DiagnosticBag::begin()
{
	return iterator(0, *this);
}

DiagnosticBag::iterator DiagnosticBag::end()
{
	return iterator(_diagnostics.size(), *this);
}

void DiagnosticBag::AddRange(DiagnosticBag& other)
{
	_diagnostics.reserve(_diagnostics.size() + other._diagnostics.size());
	_diagnostics.insert(_diagnostics.end(), 
						std::make_move_iterator(other._diagnostics.begin()), 
						std::make_move_iterator(other._diagnostics.end()));
}

void DiagnosticBag::ReportInvalidNumber(TextSpan span, const std::string & text, const std::type_index & type)
{
	auto message = "The number " + text + " is not valid " + type.name();
	Report(span, message);
}

DiagnosticBag::iterator::iterator(int pos, DiagnosticBag & bag)
	:_position(pos), _bag(&bag)
{
}

Diagnostic * DiagnosticBag::iterator::operator*() const
{
	return _bag->GetOneDiagnostic(_position);
}

DiagnosticBag::iterator & DiagnosticBag::iterator::operator++(int)
{
	_position += 1;
	return *this;
}

DiagnosticBag::iterator & DiagnosticBag::iterator::operator++()
{
	_position += 1;
	return *this;
}

}//MCF