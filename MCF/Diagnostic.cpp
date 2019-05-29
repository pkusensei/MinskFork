#include "stdafx.h"
#include "Diagnostic.h"

#include <algorithm>

#include "SourceText.h"
#include "SyntaxKind.h"

namespace MCF {

Diagnostic::Diagnostic(const TextSpan& span, const string& message)
	:_span(make_unique<TextSpan>(span.Start(), span.Length())), _message(message)
{
}

Diagnostic::~Diagnostic() = default;
Diagnostic::Diagnostic(Diagnostic &&) = default;
Diagnostic & Diagnostic::operator=(Diagnostic &&) = default;

TextSpan Diagnostic::Span() const
{
	return *_span;
}

DiagnosticBag::DiagnosticBag()
	:_diagnostics(std::deque<Diagnostic>())
{
}

void DiagnosticBag::Report(const TextSpan& span, const string& message)
{
	//if(_diagnostics==nullptr)
	_diagnostics.emplace_back(span, message);
}

const DiagnosticBag& DiagnosticBag::SortBySpanAscending()
{
	std::sort(_diagnostics.begin(), _diagnostics.end(),
		[](const auto& a, const auto& b)
	{
		if (a.Span().Start() == b.Span().Start())
			return a.Span().End() < b.Span().End();
		else
			return a.Span().Start() < b.Span().Start();
	});
	return *this;
}

const Diagnostic & DiagnosticBag::operator[](size_t idx) const
{
	if (idx >= size())
		throw std::out_of_range("Index out of range of DiagnosticBag.");
	return _diagnostics.at(idx);
}

DiagnosticBag::iterator DiagnosticBag::begin()const
{
	return iterator(0, *this);
}

DiagnosticBag::iterator DiagnosticBag::end()const
{
	return iterator(_diagnostics.size(), *this);
}

void DiagnosticBag::AddRangeFront(DiagnosticBag & other)
{
	_diagnostics.insert(_diagnostics.begin(),
		make_move_iterator(other._diagnostics.begin()),
		make_move_iterator(other._diagnostics.end()));
}

void DiagnosticBag::AddRange(DiagnosticBag& other)
{
	_diagnostics.insert(_diagnostics.end(),
		make_move_iterator(other._diagnostics.begin()),
		make_move_iterator(other._diagnostics.end()));
}

void DiagnosticBag::ReportInvalidNumber(const TextSpan& span,
	const string & text, const TypeSymbol & type)
{
	auto message = "The number " + text + " is not valid " + type.ToString();
	Report(span, message);
}

void DiagnosticBag::ReportBadCharacter(size_t position, char character)
{
	string message{ "Bad character in input: " };
	message.append(&character);
	Report(TextSpan(position, 1), message);
}

void DiagnosticBag::ReportUnterminatedString(const TextSpan & span)
{
	string message{ "Unterminated string literal." };
	Report(span, message);
}

void DiagnosticBag::ReportUnexpectedToken(const TextSpan& span,
	const SyntaxKind& actualKind, const SyntaxKind& expectedKind)
{
	string message{ "Unexpected token <" };
	message += GetSyntaxKindName(actualKind) + ">, expected <" + GetSyntaxKindName(expectedKind) + ">.";
	Report(span, message);
}

void DiagnosticBag::ReportUndefinedUnaryOperator(const TextSpan & span,
	const string & operatorText, const TypeSymbol & operandType)
{
	string message{ "Unary operator '" };
	message += operatorText + "' is not defined for type '"
		+ operandType.ToString() + "'.";
	Report(span, message);
}

void DiagnosticBag::ReportUndefinedBinaryOperator(const TextSpan & span,
	const string & operatorText,
	const TypeSymbol& leftType, const TypeSymbol& rightType)
{
	string message{ "Binary operator '" };
	message += operatorText + "' is not defined for types '"
		+ leftType.ToString() + "' and '" + rightType.ToString() + "'.";
	Report(span, message);
}

void DiagnosticBag::ReportUndefinedName(const TextSpan & span, const string & name)
{
	string message{ "Variable '" };
	message += name + "' doesn't exist.";
	Report(span, message);
}

void DiagnosticBag::ReportUndefinedType(const TextSpan & span, const string & name)
{
	string message{ "Type '" };
	message += name + "' doesn't exist.";
	Report(span, message);
}

void DiagnosticBag::ReportCannotConvert(const TextSpan & span,
	const TypeSymbol & fromType, const TypeSymbol & toType)
{
	string message("Cannot convert type '");
	message += fromType.ToString() + "' to '" + toType.ToString() + "'.";
	Report(span, message);
}

void DiagnosticBag::ReportCannotConvertImplicitly(const TextSpan & span,
	const TypeSymbol & fromType, const TypeSymbol & toType)
{
	string message("Cannot convert type '");
	message += fromType.ToString() + "' to '" + toType.ToString() + "' implicitly.";
	Report(span, message);
}

void DiagnosticBag::ReportSymbolAlreadyDeclared(const TextSpan & span,
	const string & name)
{
	string message("Variable '");
	message += name + "' is already declared.";
	Report(span, message);
}

void DiagnosticBag::ReportCannotAssign(const TextSpan & span, const string & name)
{
	string message("Variable '");
	message += name + "' is read-only; cannot be assigned to.";
	Report(span, message);
}

void DiagnosticBag::ReportUndefinedFunction(const TextSpan & span,
	const string & name)
{
	string message("Function '");
	message += name + "' doesn't exist.";
	Report(span, message);
}

void DiagnosticBag::ReportWrongArgumentCount(const TextSpan & span,
	const string & name, size_t expectedCount, size_t actualCount)
{
	string message("Function '");
	message += name + "' requires " + std::to_string(expectedCount)
		+ " arguments but was given " + std::to_string(actualCount) + ".";
	Report(span, message);
}

void DiagnosticBag::ReportWrongArgumentType(const TextSpan & span, const string & name,
	const TypeSymbol & expectedType, const TypeSymbol & actualType)
{
	string message("Parameter '");
	message += name + "' requires a value of type '" + expectedType.ToString()
		+ "' but was given '" + actualType.ToString() + "'.";
	Report(span, message);
}

void DiagnosticBag::ReportExpressionMustHaveValue(const TextSpan & span)
{
	string message("Expression must have a value.");
	Report(span, message);
}

void DiagnosticBag::ReportInvalidBreakOrContinue(const TextSpan & span,
	const string & text)
{
	string message("The keyword '");
	message += text + "' can only be used inside loops.";
	Report(span, message);
}

void DiagnosticBag::ReportExpressionNotSupportPostfixOperator(const TextSpan & span,
	const string & operatorText, const SyntaxKind & kind)
{
	string message{ "Operator '" };
	message += operatorText + "' is not defined for expression '" + GetSyntaxKindName(kind) + "'.";
	Report(span, message);
}

void DiagnosticBag::ReportAllPathsMustReturn(const TextSpan & span)
{
	string message = "Not all code paths return a value.";
	Report(span, message);
}

void DiagnosticBag::ReportInvalidReturn(const TextSpan & span)
{
	string message="The 'return' keyword can only be used inside of function";
	Report(span, message);
}

void DiagnosticBag::ReportInvalidReturnExpression(const TextSpan & span, const string & funcName)
{
	string message{ "Function '" };
	message += funcName + "' does not return a value";
	Report(span, message);
}

void DiagnosticBag::ReportMissingReturnExpression(const TextSpan & span, const TypeSymbol & returnType)
{
	string message{ "An expression of Type '" };
	message += returnType.ToString() + "' expected";
	Report(span, message);
}

void DiagnosticBag::ReportVariableNotSupportPostfixOperator(const TextSpan & span,
	const string & operatorText, const TypeSymbol & variableType)
{
	string message{ "Operator '" };
	message += operatorText + "' is not defined for type '" + variableType.ToString() + "'.";
	Report(span, message);
}

DiagnosticBag::iterator::iterator(size_t pos, const DiagnosticBag & bag)
	:_position(pos), _bag(&bag)
{
}

const Diagnostic & DiagnosticBag::iterator::operator*() const
{
	return (*_bag)[_position];
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

bool DiagnosticBag::iterator::operator==(const iterator & other) const noexcept
{
	return _position == other._position && _bag == other._bag;
}

}//MCF