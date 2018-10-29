#include "stdafx.h"
#include "Diagnostic.h"
#include <iostream>

namespace MCF {

Diagnostic::Diagnostic(TextSpan span, const std::string& message)
	:_span(span), _message(message)
{
}

Diagnostic::Diagnostic(Diagnostic && other)
	:_span(other._span), _message(other._message)
{
}

Diagnostic& Diagnostic::operator=(Diagnostic && other)
{
	_span = std::move(other._span);
	_message = std::move(other._message);
	return *this;
}

DiagnosticBag::DiagnosticBag()
	:_diagnostics(std::vector<Diagnostic>())
{
}

DiagnosticBag::DiagnosticBag(DiagnosticBag && other)
{
	for (auto& d : other._diagnostics)
	{
		_diagnostics.emplace_back(d.Span(), d.Message());
	}
}

void DiagnosticBag::Report(TextSpan span, const std::string& message)
{
	//if(_diagnostics==nullptr)
	_diagnostics.emplace_back(span, message);
}

const Diagnostic& DiagnosticBag::GetOneDiagnostic(int idx) const
{
	return _diagnostics[idx];
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
						std::make_move_iterator(other._diagnostics.begin()), std::make_move_iterator(other._diagnostics.end()));
}

void DiagnosticBag::ReportInvalidNumber(TextSpan span, const std::string & text, const std::type_info & type)
{
	auto message = "The number " + text + " is not valid " + type.name();
	Report(span, message);
}

void DiagnosticBag::ReportBadCharacter(int position, char character)
{
	std::string message{"Bad character in input: "};
	message.append(&character);
	Report(TextSpan(position, 1, position + 1), message);
}

void DiagnosticBag::ReportUnexpectedToken(TextSpan span, SyntaxKind actualKind, SyntaxKind expectedKind)
{
	std::string message{"Unexpected token"};
	Report(span, message);
}

DiagnosticBag::iterator::iterator(int pos, DiagnosticBag & bag)
	:_position(pos), _bag(&bag)
{
}

const Diagnostic& DiagnosticBag::iterator::operator*() const
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