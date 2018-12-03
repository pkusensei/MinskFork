#pragma once

#include <deque>

#include "common.h"


namespace MCF {

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
	bool empty()const noexcept { return size() == 0; }

	class iterator;
	const Diagnostic& operator[](size_t idx) const;
	iterator begin()const;
	iterator end()const;

	void AddRangeFront(DiagnosticBag& other);
	void AddRange(DiagnosticBag& other);
	void ReportInvalidNumber(const TextSpan& span, const string& text, const type_index& type);
	void ReportBadCharacter(size_t position, char character);
	void ReportUnexpectedToken(const TextSpan& span, const SyntaxKind& actualKind, const SyntaxKind& expectedKind);
	void ReportUndefinedName(const TextSpan& span, const string& name);
	void ReportCannotConvert(const TextSpan& span, const type_index& fromType, const type_index& toType);
	void ReportUndefinedUnaryOperator(const TextSpan& span, const string& operatorText, const type_index& operandType);
	void ReportUndefinedBinaryOperator(const TextSpan& span, const string& operatorText, const type_index& leftType, const type_index& rightType);
	void ReportVariableAlreadyDeclared(const TextSpan& span, const string& name);
	void ReportCannotAssign(const TextSpan& span, const string& name);

	void ReportExpressionNotSupportPostfixOperator(const TextSpan & span, const string & operatorText, const SyntaxKind& kind);
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
