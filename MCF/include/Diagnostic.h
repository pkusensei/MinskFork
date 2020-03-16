#pragma once

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

class MCF_API Diagnostic final
{
private:
	TextSpan _span;
	string _message;

public:
	Diagnostic(const TextSpan& span, const string& message)
		:_span(span), _message(message)
	{
	}

	const TextSpan& Span() const noexcept { return _span; }
	const string& Message() const noexcept { return _message; }
};

class MCF_API DiagnosticBag final
{
private:
	std::deque<Diagnostic> _diagnostics;
	void Report(const TextSpan& span, const string& message);

public:
	DiagnosticBag()
		:_diagnostics(std::deque<Diagnostic>())
	{
	}
	DiagnosticBag(DiagnosticBag&&) = default;
	DiagnosticBag& operator=(DiagnosticBag&&) = default;

	size_t size() const noexcept { return _diagnostics.size(); }
	bool empty()const noexcept { return _diagnostics.empty(); }
	const DiagnosticBag& SortBySpanAscending();

	class iterator;
	const Diagnostic& operator[](size_t idx) const;
	iterator begin()const;
	iterator end()const;

	void AddRangeFront(DiagnosticBag& other);
	void AddRange(DiagnosticBag& other);
	void ReportInvalidNumber(const TextSpan& span, const string& text, const TypeSymbol& type);
	void ReportBadCharacter(size_t position, char character);
	void ReportUnterminatedString(const TextSpan& span);
	void ReportUnexpectedToken(const TextSpan& span, const SyntaxKind& actualKind,
		const SyntaxKind& expectedKind);
	void ReportUndefinedUnaryOperator(const TextSpan& span, const string& operatorText,
		const TypeSymbol& operandType);
	void ReportUndefinedBinaryOperator(const TextSpan& span, const string& operatorText,
		const TypeSymbol& leftType, const TypeSymbol& rightType);
	void ReportUndefinedName(const TextSpan& span, const string& name);
	void ReportUndefinedType(const TextSpan& span, const string& name);
	void ReportCannotConvert(const TextSpan& span, const TypeSymbol& fromType,
		const TypeSymbol& toType);
	void ReportCannotConvertImplicitly(const TextSpan& span, const TypeSymbol& fromType,
		const TypeSymbol& toType);
	void ReportSymbolAlreadyDeclared(const TextSpan& span, string_view name);
	void ReportCannotAssign(const TextSpan& span, const string& name);

	void ReportUndefinedFunction(const TextSpan& span, const string& name);
	void ReportWrongArgumentCount(const TextSpan& span, string_view name,
		size_t expectedCount, size_t actualCount);
	void ReportWrongArgumentType(const TextSpan& span, string_view name,
		const TypeSymbol& expectedType, const TypeSymbol& actualType);
	void ReportExpressionMustHaveValue(const TextSpan& span);
	void ReportInvalidBreakOrContinue(const TextSpan& span, const string& text);

	void ReportExpressionNotSupportPostfixOperator(const TextSpan& span,
		const string& operatorText, const SyntaxKind& kind);

	void ReportAllPathsMustReturn(const TextSpan& span);
	void ReportInvalidReturn(const TextSpan& span);
	void ReportInvalidReturnExpression(const TextSpan& span, string_view funcName);
	void ReportMissingReturnExpression(const TextSpan& span,
		const TypeSymbol& returnType);

	void ReportVariableNotSupportPostfixOperator(const TextSpan& span,
		const string& operatorText, const TypeSymbol& variableType);
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
