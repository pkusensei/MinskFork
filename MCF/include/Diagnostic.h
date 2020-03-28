#pragma once

#include <algorithm>
#include <deque>

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
	TextLocation _location;
	string _message;

public:
	Diagnostic(TextLocation location, string message)
		:_location(std::move(location)), _message(std::move(message))
	{
	}

	constexpr const TextLocation& Location() const noexcept { return _location; }
	constexpr string_view Message() const { return _message; }

	constexpr string_view ToString()const { return Message(); }
};

class MCF_API DiagnosticBag final
{
private:
	std::deque<Diagnostic> _diagnostics;
	void Report(TextLocation location, string message);

public:
	DiagnosticBag()
		:_diagnostics(std::deque<Diagnostic>())
	{
	}

	size_t size() const noexcept { return _diagnostics.size(); }
	bool empty()const noexcept { return _diagnostics.empty(); }

	class iterator;
	const Diagnostic& operator[](size_t idx) const;
	iterator begin()const;
	iterator end()const;

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
	void ReportUndefinedVariable(TextLocation location, string_view name);
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
	void ReportWrongArgumentType(TextLocation location, string_view name,
		const TypeSymbol& expectedType, const TypeSymbol& actualType);
	void ReportExpressionMustHaveValue(TextLocation location);
	void ReportInvalidBreakOrContinue(TextLocation location, string_view text);

	void ReportExpressionNotSupportPostfixOperator(TextLocation location,
		string_view operatorText, SyntaxKind kind);

	void ReportAllPathsMustReturn(TextLocation location);
	void ReportInvalidReturn(TextLocation location);
	void ReportInvalidReturnExpression(TextLocation location, string_view funcName);
	void ReportMissingReturnExpression(TextLocation location,
		const TypeSymbol& returnType);

	void ReportVariableNotSupportPostfixOperator(TextLocation location,
		string_view operatorText, const TypeSymbol& variableType);
	void ReportSourceFileNotExist(TextLocation, string_view fileName);

	template<typename Cond,
		typename = std::enable_if_t<std::is_invocable_v<Cond, const Diagnostic&, const Diagnostic&>>>
		const DiagnosticBag& SortBy(Cond&& cond)
	{
		std::sort(_diagnostics.begin(), _diagnostics.end(), std::forward<Cond>(cond));
		return *this;
	}
};

class MCF_API DiagnosticBag::iterator
{
private:
	size_t _position = 0;
	const DiagnosticBag* _bag;
public:
	iterator(size_t pos, const DiagnosticBag& bag);

	const Diagnostic& operator*() const;
	iterator& operator++(int); //i++
	iterator& operator++(); //++i
	bool operator==(const iterator& other)const noexcept;
	bool operator!=(const iterator& other)const noexcept { return !(*this == other); }
};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
