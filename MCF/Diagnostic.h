#pragma once

#include <deque>

#include "Symbols.h"

namespace MCF {

enum class SyntaxKind;
class TextSpan;

class MCF_API Diagnostic final
{
private:
	unique_ptr<TextSpan> _span;
	string _message;

public:
	Diagnostic(const TextSpan& span, const string& message);
	~Diagnostic();
	Diagnostic(const Diagnostic&) = default;
	Diagnostic(Diagnostic&&);
	Diagnostic& operator=(const Diagnostic& other) = default;
	Diagnostic& operator=(Diagnostic&&);

	TextSpan Span() const;
	string Message() const { return _message; }
};

class MCF_API DiagnosticBag final
{
private:
	std::deque<Diagnostic> _diagnostics;
	void Report(const TextSpan& span, const string& message);

public:
	DiagnosticBag();
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
	void ReportSymbolAlreadyDeclared(const TextSpan& span, const string& name);
	void ReportCannotAssign(const TextSpan& span, const string& name);

	void ReportUndefinedFunction(const TextSpan& span, const string& name);
	void ReportWrongArgumentCount(const TextSpan& span, const string& name,
								  size_t expectedCount, size_t actualCount);
	void ReportWrongArgumentType(const TextSpan& span, const string& name,
								 const TypeSymbol& expectedType, const TypeSymbol& actualType);
	void ReportExpressionMustHaveValue(const TextSpan& span);

	void ReportExpressionNotSupportPostfixOperator(const TextSpan & span, const string & operatorText, const SyntaxKind& kind);

	void ReportVariableNotSupportPostfixOperator(const TextSpan & span, const string & operatorText, const TypeSymbol& variableType);
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
