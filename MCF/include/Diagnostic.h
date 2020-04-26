#pragma once

#include <algorithm>
#include <deque>
#include <optional>

#include "SourceText.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

enum class SyntaxKind;
class TypeSymbol;

class Diagnostic final
{
private:
	std::optional<TextLocation> _location;
	string _message;

public:
	explicit Diagnostic(string message)
		:_location(std::nullopt), _message(std::move(message))
	{
	}

	Diagnostic(std::optional<TextLocation> location, string message)
		:_location(std::move(location)), _message(std::move(message))
	{
	}

	constexpr bool HasLocation()const noexcept { return _location.has_value(); }

	constexpr const TextLocation& Location() const
	{
		return _location.value(); // HACK this throws
	}
	string_view Message() const { return _message; }

	string_view ToString()const { return Message(); }
};

class MCF_API DiagnosticBag final
{
private:
	std::deque<Diagnostic> _diagnostics;
	void Report(std::optional<TextLocation> location, string message);

public:
	DiagnosticBag()
		:_diagnostics(std::deque<Diagnostic>())
	{
	}

	void clear()noexcept { _diagnostics.clear(); }
	size_t size() const noexcept { return _diagnostics.size(); }
	bool empty()const noexcept { return _diagnostics.empty(); }

	const Diagnostic& operator[](size_t idx) const;
	auto cbegin() const noexcept { return _diagnostics.cbegin(); }
	auto cend() const noexcept { return _diagnostics.cend(); }
	auto begin() noexcept { return _diagnostics.begin(); }
	auto end() noexcept { return _diagnostics.end(); }

	void AddRangeFront(DiagnosticBag& other);
	void AddRange(DiagnosticBag& other);
	void ReportInvalidNumber(TextLocation location, string_view text, const TypeSymbol& type);
	void ReportBadCharacter(TextLocation location, char character);
	void ReportUnterminatedString(TextLocation location);
	void ReportUnexpectedToken(TextLocation location, SyntaxKind actualKind,
		SyntaxKind expectedKind);
	void ReportUndefinedUnaryOperator(TextLocation location, string_view operatorText,
		const TypeSymbol& operandType);
	void ReportUndefinedBinaryOperator(TextLocation location, string_view operatorText,
		const TypeSymbol& leftType, const TypeSymbol& rightType);
	void ReportUndefinedVariable(std::optional<TextLocation> location, string_view name);
	void ReportNotAVariable(TextLocation location, string_view name);
	void ReportUndefinedType(TextLocation location, string_view name);
	void ReportCannotConvert(TextLocation location, const TypeSymbol& fromType,
		const TypeSymbol& toType);
	void ReportCannotConvertImplicitly(TextLocation location, const TypeSymbol& fromType,
		const TypeSymbol& toType);
	void ReportSymbolAlreadyDeclared(TextLocation location, string_view name);
	void ReportCannotAssign(TextLocation location, string_view name);

	void ReportUndefinedFunction(TextLocation location, string_view name);
	void ReportNotAFunction(TextLocation location, string_view name);
	void ReportParameterAlreadyDeclared(TextLocation location, string_view name);
	void ReportWrongArgumentCount(TextLocation location, string_view name,
		size_t expectedCount, size_t actualCount);
	void ReportExpressionMustHaveValue(TextLocation location);
	void ReportInvalidBreakOrContinue(TextLocation location, string_view text);

	void ReportExpressionNotSupportPostfixOperator(TextLocation location,
		string_view operatorText, SyntaxKind kind);

	void ReportAllPathsMustReturn(TextLocation location);
	void ReportInvalidReturnExpression(TextLocation location, string_view funcName);
	void ReportInvalidReturnWithValueInGlobalStatements(TextLocation location);
	void ReportMissingReturnExpression(TextLocation location,
		const TypeSymbol& returnType);

	void ReportVariableNotSupportPostfixOperator(TextLocation location,
		string_view operatorText, const TypeSymbol& variableType);
	void ReportSourceFileNotExist(TextLocation, string_view fileName);

	void ReportInvalidExpressionStatement(TextLocation location);
	void ReportMainMustHaveCorrectSignature(TextLocation location);
	void ReportCannotMixMainAndGlobalStatements(TextLocation location);

	void ReportRequestedTargetNotFound(string_view error);
	void ReportCannotOpenOutputFile(string_view error);
	void ReportCannotEmitFileType();
	void ReportFunctionDeclarationNotEmitted(string_view name);
	void ReportFunctionViolateODR(string_view name);
	void ReportCannotEmitFunctionBody(string_view error);
	void ReportWrongArgumentCountEmitted(string_view name,
		size_t expectedCount, size_t actualCount);
};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
