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
class SyntaxNode;

class [[nodiscard]] Diagnostic final
{
private:
	string _message;
	std::optional<TextLocation> _location;
	bool _isError;

	explicit Diagnostic(bool isError, std::optional<TextLocation> location, string message)noexcept
		:_message(std::move(message)), _location(std::move(location)), _isError(isError)
	{
	}

public:
	constexpr bool HasLocation()const noexcept { return _location.has_value(); }

	MCF_API const TextLocation& Location() const;

	constexpr bool IsError()const noexcept { return _isError; }
	constexpr bool IsWarning()const noexcept { return !_isError; }
	string_view Message() const noexcept { return _message; }
	string_view ToString()const noexcept { return Message(); }

	static Diagnostic Error(std::optional<TextLocation> location, string message)noexcept
	{
		return Diagnostic(true, std::move(location), std::move(message));
	}

	static Diagnostic Warning(std::optional<TextLocation> location, string message)noexcept
	{
		return Diagnostic(false, std::move(location), std::move(message));
	}

};

class MCF_API [[nodiscard]] DiagnosticBag final
{
private:
	std::deque<Diagnostic> _diagnostics;
	void ReportError(std::optional<TextLocation> location, string message);
	void ReportWarning(std::optional<TextLocation> location, string message);

public:

	void clear()noexcept { _diagnostics.clear(); }
	size_t size() const noexcept { return _diagnostics.size(); }
	bool empty()const noexcept { return _diagnostics.empty(); }

	const Diagnostic& operator[](size_t idx) const;
	auto cbegin() const noexcept { return _diagnostics.cbegin(); }
	auto cend() const noexcept { return _diagnostics.cend(); }
	auto begin() noexcept { return _diagnostics.begin(); }
	auto end() noexcept { return _diagnostics.end(); }
	auto begin()const noexcept { return _diagnostics.begin(); }
	auto end()const noexcept { return _diagnostics.end(); }

	bool HasErrors()const;

	void AddRangeFront(DiagnosticBag other);
	void AddRange(DiagnosticBag other);

	void ReportInvalidNumber(TextLocation location, string_view text,
							 const TypeSymbol& type);
	void ReportBadCharacter(TextLocation location, char character);
	void ReportUnterminatedString(TextLocation location);
	void ReportUnterminatedMultiLineComment(TextLocation location);
	void ReportUnexpectedToken(TextLocation location, SyntaxKind actualKind,
							   SyntaxKind expectedKind);
	void ReportUndefinedUnaryOperator(TextLocation location,
									  string_view operatorText,
									  const TypeSymbol& operandType);
	void ReportUndefinedBinaryOperator(TextLocation location,
									   string_view operatorText,
									   const TypeSymbol& leftType,
									   const TypeSymbol& rightType);
	void ReportUndefinedVariable(TextLocation location, string_view name);
	void ReportNotAVariable(TextLocation location, string_view name);
	void ReportUndefinedType(TextLocation location, string_view name);
	void ReportCannotConvert(TextLocation location, const TypeSymbol& fromType,
							 const TypeSymbol& toType);
	void ReportCannotConvertImplicitly(TextLocation location,
									   const TypeSymbol& fromType,
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
												   string_view operatorText,
												   SyntaxKind kind);

	void ReportAllPathsMustReturn(TextLocation location);
	void ReportInvalidReturnExpression(TextLocation location, string_view funcName);
	void ReportInvalidReturnWithValueInGlobalStatements(TextLocation location);
	void ReportMissingReturnExpression(TextLocation location,
									   const TypeSymbol& returnType);

	void ReportVariableNotSupportPostfixOperator(TextLocation location,
												 string_view operatorText,
												 const TypeSymbol& variableType);
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
										 size_t expectedCount,
										 size_t actualCount);
	void ReportVariableNotEmitted(string_view name);
	void ReportBasicBlockNotCreatedFromLabel(string_view labelName);

	void ReportUnreachableCode(TextLocation location);
	void ReportUnreachableCode(const SyntaxNode& node);
};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
