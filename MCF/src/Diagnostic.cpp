#include "Diagnostic.h"

#include <stdexcept>

#include "helpers.h"
#include "Symbols.h"
#include "SyntaxKind.h"

namespace MCF {

void DiagnosticBag::Report(TextLocation location, string message)
{
	_diagnostics.emplace_back(std::move(location), std::move(message));
}

const Diagnostic& DiagnosticBag::operator[](size_t idx) const
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

void DiagnosticBag::AddRangeFront(DiagnosticBag& other)
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

void DiagnosticBag::ReportInvalidNumber(TextLocation location,
	string_view text, const TypeSymbol& type)
{
	auto message = BuildStringFrom("The number ", text, " is not valid ", type.ToString());
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportBadCharacter(TextLocation location, char character)
{
	string message{ "Bad character in input: " };
	message.append(&character);
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUnterminatedString(TextLocation location)
{
	string message{ "Unterminated string literal." };
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUnexpectedToken(TextLocation location,
	SyntaxKind actualKind, SyntaxKind expectedKind)
{
	auto message = BuildStringFrom("Unexpected token <", GetSyntaxKindName(actualKind),
		">, expected <", GetSyntaxKindName(expectedKind), ">.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedUnaryOperator(TextLocation location,
	string_view operatorText, const TypeSymbol& operandType)
{
	auto message = BuildStringFrom("Unary operator '", operatorText, "' is not defined for type '"
		, operandType.ToString(), "'.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedBinaryOperator(TextLocation location,
	string_view operatorText, const TypeSymbol& leftType, const TypeSymbol& rightType)
{
	auto message = BuildStringFrom("Binary operator '", operatorText, "' is not defined for types '"
		, leftType.ToString(), "' and '", rightType.ToString(), "'.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedVariable(TextLocation location, string_view name)
{
	auto message = BuildStringFrom("Variable '", name, "' doesn't exist.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportNotAVariable(TextLocation location, string_view name)
{
	auto message = BuildStringFrom('\'', name, "' is not a variable.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedType(TextLocation location, string_view name)
{
	auto message = BuildStringFrom("Type '", name, "' doesn't exist.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportCannotConvert(TextLocation location,
	const TypeSymbol& fromType, const TypeSymbol& toType)
{
	string message("Cannot convert type '");
	message += fromType.ToString() + "' to '" + toType.ToString() + "'.";
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportCannotConvertImplicitly(TextLocation location,
	const TypeSymbol& fromType, const TypeSymbol& toType)
{
	string message("Cannot convert type '");
	message += fromType.ToString() + "' to '" + toType.ToString() + "' implicitly.";
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportSymbolAlreadyDeclared(TextLocation location,
	string_view name)
{
	auto message = BuildStringFrom("Variable '", name, "' is already declared.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportCannotAssign(TextLocation location, string_view name)
{
	auto message = BuildStringFrom("Variable '", name, "' is read-only; cannot be assigned to.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedFunction(TextLocation location,
	string_view name)
{
	auto message = BuildStringFrom("Function '", name, "' doesn't exist.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportNotAFunction(TextLocation location, string_view name)
{
	auto message = BuildStringFrom('\'', name, "' is not a function.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportParameterAlreadyDeclared(TextLocation location, string_view name)
{
	auto message = BuildStringFrom("A parameter with name '", name, "' already exists.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportWrongArgumentCount(TextLocation location,
	string_view name, size_t expectedCount, size_t actualCount)
{
	auto message = BuildStringFrom("Function '", name, "' requires ", std::to_string(expectedCount)
		, " arguments but was given ", actualCount, ".");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportWrongArgumentType(TextLocation location, string_view name,
	const TypeSymbol& expectedType, const TypeSymbol& actualType)
{
	auto message = BuildStringFrom("Parameter '", name, "' requires a value of type '", expectedType.ToString()
		, "' but was given '", actualType.ToString(), "'.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportExpressionMustHaveValue(TextLocation location)
{
	string message("Expression must have a value.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportInvalidBreakOrContinue(TextLocation location,
	string_view text)
{
	auto message = BuildStringFrom("The keyword '", text, "' can only be used inside of loops.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportExpressionNotSupportPostfixOperator(TextLocation location,
	string_view operatorText, SyntaxKind kind)
{
	auto message = BuildStringFrom("Operator '", operatorText, 
		"' is not defined for expression '", GetSyntaxKindName(kind), "'.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportAllPathsMustReturn(TextLocation location)
{
	string message = "Not all code paths return a value.";
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportInvalidReturn(TextLocation location)
{
	string message = "The keyword 'return' can only be used inside of functions.";
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportInvalidReturnExpression(TextLocation location, string_view funcName)
{
	auto message = BuildStringFrom("Function '", funcName, "' does not return a value.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportMissingReturnExpression(TextLocation location, const TypeSymbol& returnType)
{
	auto message = BuildStringFrom("An expression of type '", returnType.ToString(), "' is expected.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportVariableNotSupportPostfixOperator(TextLocation location,
	string_view operatorText, const TypeSymbol& variableType)
{
	auto message = BuildStringFrom("Operator '", operatorText, "' is not defined for type '", variableType.ToString(), "'.");
	Report(std::move(location), std::move(message));
}

DiagnosticBag::iterator::iterator(size_t pos, const DiagnosticBag& bag)
	:_position(pos), _bag(&bag)
{
}

const Diagnostic& DiagnosticBag::iterator::operator*() const
{
	return (*_bag)[_position];
}

DiagnosticBag::iterator& DiagnosticBag::iterator::operator++(int)
{
	_position += 1;
	return *this;
}

DiagnosticBag::iterator& DiagnosticBag::iterator::operator++()
{
	_position += 1;
	return *this;
}

bool DiagnosticBag::iterator::operator==(const iterator& other) const noexcept
{
	return _position == other._position && _bag == other._bag;
}

}//MCF