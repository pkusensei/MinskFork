#include "Diagnostic.h"

#include <stdexcept>

#include "StringHelper.h"
#include "Symbols.h"
#include "SyntaxKind.h"
#include "SyntaxStatements.h"

namespace MCF {

void DiagnosticBag::ReportError(std::optional<TextLocation> location, string message)
{
	_diagnostics.push_back(Diagnostic::Error(std::move(location), std::move(message)));
}

void DiagnosticBag::ReportWarning(std::optional<TextLocation> location, string message)
{
	_diagnostics.push_back(Diagnostic::Warning(std::move(location), std::move(message)));
}

const Diagnostic& DiagnosticBag::operator[](size_t idx) const
{
	if (idx >= size())
		throw std::out_of_range("Index out of range of DiagnosticBag.");
	return _diagnostics.at(idx);
}

vector<const Diagnostic*> DiagnosticBag::All()const
{
	auto result = vector<const Diagnostic*>();
	std::transform(_diagnostics.cbegin(), _diagnostics.cend(),
		std::back_inserter(result),
		[](const auto& d) { return &d; });

	return result;
}

vector<const Diagnostic*> DiagnosticBag::Errors()const
{
	auto result = vector<const Diagnostic*>();

	for (const auto& d : _diagnostics)
	{
		if (d.IsError())
			result.push_back(&d);
	}
	return result;
}

vector<const Diagnostic*> DiagnosticBag::Warnings()const
{
	auto result = vector<const Diagnostic*>();

	for (const auto& d : _diagnostics)
	{
		if (d.IsWarning())
			result.push_back(&d);
	}
	return result;
}

void DiagnosticBag::AddRangeFront(DiagnosticBag other)
{
	_diagnostics.insert(_diagnostics.begin(),
		std::make_move_iterator(other._diagnostics.begin()),
		std::make_move_iterator(other._diagnostics.end()));
}

void DiagnosticBag::AddRange(DiagnosticBag other)
{
	_diagnostics.insert(_diagnostics.end(),
		std::make_move_iterator(other._diagnostics.begin()),
		std::make_move_iterator(other._diagnostics.end()));
}

void DiagnosticBag::ReportInvalidNumber(TextLocation location,
	string_view text, const TypeSymbol& type)
{
	auto message = BuildStringFrom("The number ", text, " is not valid ", type.ToString());
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportBadCharacter(TextLocation location, char character)
{
	string message{ "Bad character in input: " };
	message.push_back(character);
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUnterminatedString(TextLocation location)
{
	auto message = "Unterminated string literal.";
	ReportError(std::move(location), message);
}

void DiagnosticBag::ReportUnterminatedMultiLineComment(TextLocation location)
{
	auto message = "Unterminated multi-line comment.";
	ReportError(std::move(location), message);
}

void DiagnosticBag::ReportUnexpectedToken(TextLocation location,
	SyntaxKind actualKind, SyntaxKind expectedKind)
{
	auto message = BuildStringFrom("Unexpected token <", nameof(actualKind),
		">, expected <", nameof(expectedKind), ">.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedUnaryOperator(TextLocation location,
	string_view operatorText, const TypeSymbol& operandType)
{
	auto message = BuildStringFrom("Unary operator '", operatorText, "' is not defined for type '"
		, operandType.ToString(), "'.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedBinaryOperator(TextLocation location,
	string_view operatorText, const TypeSymbol& leftType, const TypeSymbol& rightType)
{
	auto message = BuildStringFrom("Binary operator '", operatorText, "' is not defined for types '"
		, leftType.ToString(), "' and '", rightType.ToString(), "'.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedVariable(TextLocation location, string_view name)
{
	auto message = BuildStringFrom("Variable '", name, "' doesn't exist.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportNotAVariable(TextLocation location, string_view name)
{
	auto message = BuildStringFrom('\'', name, "' is not a variable.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedType(TextLocation location, string_view name)
{
	auto message = BuildStringFrom("Type '", name, "' doesn't exist.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportCannotConvert(TextLocation location,
	const TypeSymbol& fromType, const TypeSymbol& toType)
{
	auto message = BuildStringFrom("Cannot convert type '", fromType.ToString(),
		"' to '", toType.ToString(), "'.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportCannotConvertImplicitly(TextLocation location,
	const TypeSymbol& fromType, const TypeSymbol& toType)
{
	auto message = BuildStringFrom("Cannot convert type '", fromType.ToString(), "' to '", toType.ToString(), "'.",
		" An explicit conversion exists (are you missing a cast?)");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportSymbolAlreadyDeclared(TextLocation location,
	string_view name)
{
	auto message = BuildStringFrom("Variable '", name, "' is already declared.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportCannotAssign(TextLocation location, string_view name)
{
	auto message = BuildStringFrom("Variable '", name, "' is read-only; cannot be assigned to.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportUndefinedFunction(TextLocation location,
	string_view name)
{
	auto message = BuildStringFrom("Function '", name, "' doesn't exist.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportNotAFunction(TextLocation location, string_view name)
{
	auto message = BuildStringFrom('\'', name, "' is not a function.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportParameterAlreadyDeclared(TextLocation location, string_view name)
{
	auto message = BuildStringFrom("A parameter with name '", name, "' already exists.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportWrongArgumentCount(TextLocation location,
	string_view name, size_t expectedCount, size_t actualCount)
{
	auto message = BuildStringFrom("Function '", name, "' requires ", std::to_string(expectedCount)
		, " arguments but was given ", actualCount, ".");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportExpressionMustHaveValue(TextLocation location)
{
	auto message = "Expression must have a value.";
	ReportError(std::move(location), message);
}

void DiagnosticBag::ReportInvalidBreakOrContinue(TextLocation location,
	string_view text)
{
	auto message = BuildStringFrom("The keyword '", text, "' can only be used inside of loops.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportExpressionNotSupportPostfixOperator(TextLocation location,
	string_view operatorText, SyntaxKind kind)
{
	auto message = BuildStringFrom("Operator '", operatorText,
		"' is not defined for expression '", nameof(kind), "'.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportAllPathsMustReturn(TextLocation location)
{
	auto message = "Not all code paths return a value.";
	ReportError(std::move(location), message);
}

void DiagnosticBag::ReportInvalidReturnExpression(TextLocation location, string_view funcName)
{
	auto message = BuildStringFrom("Function '", funcName, "' does not return a value.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportInvalidReturnWithValueInGlobalStatements(TextLocation location)
{
	auto message = "The 'return' keyword cannot be followed by an expression in global statements.";
	ReportError(std::move(location), message);
}

void DiagnosticBag::ReportMissingReturnExpression(TextLocation location, const TypeSymbol& retType)
{
	auto message = BuildStringFrom("An expression of type '", retType.ToString(), "' is expected.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportVariableNotSupportPostfixOperator(TextLocation location,
	string_view operatorText, const TypeSymbol& variableType)
{
	auto message = BuildStringFrom("Operator '", operatorText, "' is not defined for type '", variableType.ToString(), "'.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportSourceFileNotExist(TextLocation location, string_view fileName)
{
	auto message = BuildStringFrom("File '", fileName, ", doesn't exist.");
	ReportError(std::move(location), std::move(message));
}

void DiagnosticBag::ReportInvalidExpressionStatement(TextLocation location)
{
	auto message = "Only assignment, call, and postfix expressions can be used as a statement.";
	ReportError(std::move(location), message);
}

void DiagnosticBag::ReportMainMustHaveCorrectSignature(TextLocation location)
{
	auto message = "main must not take arguments and not return anything.";
	ReportError(std::move(location), message);
}

void DiagnosticBag::ReportCannotMixMainAndGlobalStatements(TextLocation location)
{
	auto message = "Cannot declare main function when global statements are used.";
	ReportError(std::move(location), message);
}

void DiagnosticBag::ReportRequestedTargetNotFound(string_view error)
{
	auto message = BuildStringFrom("Cannot find requested target. ", error);
	ReportError(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportCannotOpenOutputFile(string_view error)
{
	auto message = BuildStringFrom("Cannot open output file: ", error);
	ReportError(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportCannotEmitFileType()
{
	// Note Not so sure about this
	ReportError(std::nullopt, "Target machine cannot emit object file.");
}

void DiagnosticBag::ReportFunctionDeclarationNotEmitted(string_view name)
{
	auto message = BuildStringFrom("Declaration of function '", name, "' is not found.");
	ReportError(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportFunctionViolateODR(string_view name)
{
	auto message = BuildStringFrom("Cannot define function '", name, "' twice.");
	ReportError(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportCannotEmitFunctionBody(string_view error)
{
	auto message = BuildStringFrom("Cannot create function body: ", error);
	ReportError(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportWrongArgumentCountEmitted(string_view name,
	size_t expectedCount, size_t actualCount)
{
	auto message = BuildStringFrom("Function '", name, "' requires ",
		std::to_string(expectedCount)
		, " argument(s) but only ", actualCount, " were emitted.");
	ReportError(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportVariableNotEmitted(string_view name)
{
	auto message = BuildStringFrom("Variable '", name, "' was not emitted.");
	ReportError(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportBasicBlockNotCreatedFromLabel(string_view labelName)
{
	auto message = BuildStringFrom("No Basic Block created from label '",
		labelName, "'.");
	ReportError(std::nullopt, std::move(message));
}

void DiagnosticBag::ReportUnreachableCode(TextLocation location)
{
	auto message = "Unreachable code detected.";
	ReportWarning(std::move(location), message);
}

void DiagnosticBag::ReportUnreachableCode(const SyntaxNode& node)
{
	switch (node.Kind())
	{
		case SyntaxKind::BlockStatement:
		{
			auto& b = static_cast<const BlockStatementSyntax&>(node);
			if (b.Statements().empty())
				break;
			ReportUnreachableCode(*b.Statements().front());
			break;
		}
		case SyntaxKind::VariableDeclaration:
		{
			auto& v = static_cast<const VariableDeclarationSyntax&>(node);
			ReportUnreachableCode(v.Keyword().Location());
			break;
		}
		case SyntaxKind::IfStatement:
		{
			auto& i = static_cast<const IfStatementSyntax&>(node);
			ReportUnreachableCode(i.IfKeyword().Location());
			break;
		}
		case SyntaxKind::WhileStatement:
		{
			auto& w = static_cast<const WhileStatementSyntax&>(node);
			ReportUnreachableCode(w.WhileKeyword().Location());
			break;
		}
		case SyntaxKind::DoWhileStatement:
		{
			auto& d = static_cast<const DoWhileStatementSyntax&>(node);
			ReportUnreachableCode(d.DoKeyword().Location());
			break;
		}
		case SyntaxKind::ForStatement:
		{
			auto& f = static_cast<const ForStatementSyntax&>(node);
			ReportUnreachableCode(f.Keyword().Location());
			break;
		}
		case SyntaxKind::BreakStatement:
		{
			auto& b = static_cast<const BreakStatementSyntax&>(node);
			ReportUnreachableCode(b.Keyword().Location());
			break;
		}
		case SyntaxKind::ContinueStatement:
		{
			auto& c = static_cast<const ContinueStatementSyntax&>(node);
			ReportUnreachableCode(c.Keyword().Location());
			break;
		}
		case SyntaxKind::ReturnStatement:
		{
			auto& r = static_cast<const ReturnStatementSyntax&>(node);
			ReportUnreachableCode(r.Keyword().Location());
			break;
		}
		case SyntaxKind::ExpressionStatement:
		{
			auto e = static_cast<const ExpressionStatementSyntax&>(node).Expression();
			ReportUnreachableCode(*e);
			break;
		}
		case SyntaxKind::CallExpression:
		{
			auto& c = static_cast<const CallExpressionSyntax&>(node);
			ReportUnreachableCode(c.Identifier().Location());
			break;
		}
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected syntax: '",
				nameof(node.Kind()), "'."));
	}
}


}//MCF