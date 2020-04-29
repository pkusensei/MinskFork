#include "Diagnostic.h"

#include <stdexcept>

#include "helpers.h"
#include "Symbols.h"
#include "SyntaxKind.h"

namespace MCF {

void DiagnosticBag::Report(std::optional<TextLocation> location, string message)
{
	_diagnostics.emplace_back(std::move(location), std::move(message));
}

const Diagnostic& DiagnosticBag::operator[](size_t idx) const
{
	if (idx >= size())
		throw std::out_of_range("Index out of range of DiagnosticBag.");
	return _diagnostics.at(idx);
}

void DiagnosticBag::AddRangeFront(DiagnosticBag& other)
{
	_diagnostics.insert(_diagnostics.begin(),
		std::make_move_iterator(other._diagnostics.begin()),
		std::make_move_iterator(other._diagnostics.end()));
}

void DiagnosticBag::AddRange(DiagnosticBag& other)
{
	_diagnostics.insert(_diagnostics.end(),
		std::make_move_iterator(other._diagnostics.begin()),
		std::make_move_iterator(other._diagnostics.end()));
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
	message.push_back(character);
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUnterminatedString(TextLocation location)
{
	auto message = "Unterminated string literal.";
	Report(std::move(location), message);
}

void DiagnosticBag::ReportUnexpectedToken(TextLocation location,
	SyntaxKind actualKind, SyntaxKind expectedKind)
{
	auto message = BuildStringFrom("Unexpected token <", nameof(actualKind),
		">, expected <", nameof(expectedKind), ">.");
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
	auto message = BuildStringFrom("Cannot convert type '", fromType.ToString(),
		"' to '", toType.ToString(), "'.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportCannotConvertImplicitly(TextLocation location,
	const TypeSymbol& fromType, const TypeSymbol& toType)
{
	auto message = BuildStringFrom("Cannot convert type '", fromType.ToString(), "' to '", toType.ToString(), "'.",
		" An explicit conversion exists (are you missing a cast?)");
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

void DiagnosticBag::ReportExpressionMustHaveValue(TextLocation location)
{
	auto message = "Expression must have a value.";
	Report(std::move(location), message);
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
		"' is not defined for expression '", nameof(kind), "'.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportAllPathsMustReturn(TextLocation location)
{
	auto message = "Not all code paths return a value.";
	Report(std::move(location), message);
}

void DiagnosticBag::ReportInvalidReturnExpression(TextLocation location, string_view funcName)
{
	auto message = BuildStringFrom("Function '", funcName, "' does not return a value.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportInvalidReturnWithValueInGlobalStatements(TextLocation location)
{
	auto message = "The 'return' keyword cannot be followed by an expression in global statements.";
	Report(std::move(location), message);
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

void DiagnosticBag::ReportSourceFileNotExist(TextLocation location, string_view fileName)
{
	auto message = BuildStringFrom("File '", fileName, ", doesn't exist.");
	Report(std::move(location), std::move(message));
}

void DiagnosticBag::ReportInvalidExpressionStatement(TextLocation location)
{
	auto message = "Only assignment, call, and postfix expressions can be used as a statement.";
	Report(std::move(location), message);
}

void DiagnosticBag::ReportMainMustHaveCorrectSignature(TextLocation location)
{
	auto message = "main must not take arguments and not return anything.";
	Report(std::move(location), message);
}

void DiagnosticBag::ReportCannotMixMainAndGlobalStatements(TextLocation location)
{
	auto message = "Cannot declare main function when global statements are used.";
	Report(std::move(location), message);
}

void DiagnosticBag::ReportRequestedTargetNotFound(string_view error)
{
	auto message = BuildStringFrom("Cannot find requested target. ", error);
	Report(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportCannotOpenOutputFile(string_view error)
{
	auto message = BuildStringFrom("Cannot open output file: ", error);
	Report(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportCannotEmitFileType()
{
	// Note Not so sure about this
	Report(std::nullopt, "Target machine cannot emit object file.");
}

void DiagnosticBag::ReportFunctionDeclarationNotEmitted(string_view name)
{
	auto message = BuildStringFrom("Declaration of function '", name, "' is not found.");
	Report(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportFunctionViolateODR(string_view name)
{
	auto message = BuildStringFrom("Cannot define function '", name, "' twice.");
	Report(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportCannotEmitFunctionBody(string_view error)
{
	auto message = BuildStringFrom("Cannot create function body: ", error);
	Report(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportWrongArgumentCountEmitted(string_view name,
	size_t expectedCount, size_t actualCount)
{
	auto message = BuildStringFrom("Function '", name, "' requires ", std::to_string(expectedCount)
		, " argument(s) but only ", actualCount, " were emitted.");
	Report(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportVariableNotEmitted(string_view name)
{
	auto message = BuildStringFrom("Variable '", name, "' was not emitted.");
	Report(std::nullopt, std::move(message));
}

}//MCF