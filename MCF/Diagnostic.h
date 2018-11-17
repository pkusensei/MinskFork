#pragma once

#include <deque>

#include "common.h"
#include "SourceText.h" // HACK


namespace MCF {

class TextSpan;

class MCF_API Diagnostic final
{
private:
	TextSpan _span;
	string _message;
public:
	Diagnostic(const TextSpan& span, const string& message);
	~Diagnostic();
	Diagnostic(const Diagnostic&) = default;
	Diagnostic(Diagnostic&& other);
	Diagnostic& operator=(const Diagnostic& other) = default;
	Diagnostic& operator=(Diagnostic&& other);

	TextSpan Span() const { return _span; }
	string Message() const {return _message; }
};

class MCF_API DiagnosticBag final
{
private:
	std::deque<Diagnostic> _diagnostics;
	void Report(const TextSpan& span, const string& message);
public:
	DiagnosticBag();
	~DiagnosticBag() = default;
	DiagnosticBag(DiagnosticBag&& other);
	size_t size() const { return _diagnostics.size(); }

	class iterator;
	const Diagnostic& operator[](size_t idx) const;
	iterator begin();
	iterator end();

	void AddRangeFront(DiagnosticBag& other);
	void AddRange(DiagnosticBag& other);
	void ReportInvalidNumber(const TextSpan& span, const string& text, const type_index& type);
	void ReportBadCharacter(int position, char character);
	void ReportUnexpectedToken(const TextSpan& span, SyntaxKind actualKind, SyntaxKind expectedKind);
	void ReportUndefinedName(const TextSpan& span, const string& name);
	void ReportCannotConvert(const TextSpan& span, const type_index& fromType, const type_index& toType);
	void ReportUndefinedUnaryOperator(const TextSpan& span, const string& operatorText, const type_index& operandType);
	void ReportUndefinedBinaryOperator(const TextSpan& span, const string& operatorText, const type_index& leftType, const type_index& rightType);
	void ReportVariableAlreadyDeclared(const TextSpan& span, const string& name);
	void ReportCannotAssign(const TextSpan& span, const string& name);
};

class MCF_API DiagnosticBag::iterator
{
private:
	size_t _position;
	DiagnosticBag* _bag;
public:
	iterator(size_t pos, DiagnosticBag& bag);

	const Diagnostic& operator*() const;
	iterator& operator++(int); //i++
	iterator& operator++(); //++i
	bool operator!=(const iterator& other)const { return _position != other._position; }
};

}//MCF
