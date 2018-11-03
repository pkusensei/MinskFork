#include "stdafx.h"
#include "Diagnostic.h"
#include <iostream>

namespace MCF {

Diagnostic::Diagnostic(const TextSpan& span, const string& message)
	:_span(span), _message(message)
{
}

Diagnostic::Diagnostic(Diagnostic && other)
	:_span(std::move(other._span)), _message(std::move(other._message))
{
}

Diagnostic& Diagnostic::operator=(Diagnostic && other)
{
	_span = std::move(other._span);
	_message = std::move(other._message);
	return *this;
}

DiagnosticBag::DiagnosticBag()
	:_diagnostics(vector<Diagnostic>())
{
}

DiagnosticBag::DiagnosticBag(DiagnosticBag && other)
{
	for (auto& d : other._diagnostics)
	{
		_diagnostics.emplace_back(d.Span(), d.Message());
	}
	other._diagnostics.clear();
}

void DiagnosticBag::Report(const TextSpan& span, const string& message)
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
						make_move_iterator(other._diagnostics.begin()), make_move_iterator(other._diagnostics.end()));
}

void DiagnosticBag::ReportInvalidNumber(const TextSpan& span, const string & text, const type_index & type)
{
	auto message = "The number " + text + " is not valid " + type.name();
	Report(span, message);
}

void DiagnosticBag::ReportBadCharacter(int position, char character)
{
	string message{"Bad character in input: "};
	message.append(&character);
	Report(TextSpan(position, 1), message);
}

void DiagnosticBag::ReportUnexpectedToken(const TextSpan& span, SyntaxKind actualKind, SyntaxKind expectedKind)
{
	string message{"Unexpected token <"};
	message += GetSyntaxKindName(actualKind) + ">, expected <" + GetSyntaxKindName(expectedKind) + ">.";
	Report(span, message);
}

void DiagnosticBag::ReportUndefinedName(const TextSpan & span, const string & name)
{
	string message{"Variable "};
	message += name + " doesn't exist.";
	Report(span, message);
}

void DiagnosticBag::ReportUndefinedUnaryOperator(const TextSpan & span, const string & operatorText, const type_index & operandType)
{
	string message{"Unary operator '"};
	message += operatorText + "' is not defined for type " + GetTypeName(operandType);
	Report(span, message);
}

void DiagnosticBag::ReportUndefinedBinaryOperator(const TextSpan & span, const string & operatorText, const type_index& leftType, const type_index& rightType)
{
	string message{"Binary operator '"};
	message += "' is not defined for types " + GetTypeName(leftType) + " and " + GetTypeName(rightType);
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