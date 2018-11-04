#pragma once

#include "common.h"

namespace MCF {

class MCF_API Diagnostic final
{
private:
	TextSpan _span;
	string _message;
public:
	Diagnostic(const TextSpan& span, const string& message);
	~Diagnostic() = default;
	Diagnostic(Diagnostic&& other);
	Diagnostic& operator=(Diagnostic&& other);

	TextSpan Span() const { return _span; }
	string Message() const {return _message; }
};

class MCF_API DiagnosticBag final
{
private:
	vector<Diagnostic> _diagnostics;
	void Report(const TextSpan& span, const string& message);
public:
	DiagnosticBag();
	~DiagnosticBag() = default;
	DiagnosticBag(DiagnosticBag&& other);
	size_t size() const { return _diagnostics.size(); }

	class iterator;
	const Diagnostic& GetOneDiagnostic (int idx) const;
	iterator begin();
	iterator end();

	void AddRange(DiagnosticBag& other);
	void ReportInvalidNumber(const TextSpan& span, const string& text, const type_index& type);
	void ReportBadCharacter(int position, char character);
	void ReportUnexpectedToken(const TextSpan& span, SyntaxKind actualKind, SyntaxKind expectedKind);
	void ReportUndefinedName(const TextSpan& span, const string& name);
	void ReportUndefinedUnaryOperator(const TextSpan& span, const string& operatorText, const type_index& operandType);
	void ReportUndefinedBinaryOperator(const TextSpan& span, const string& operatorText, const type_index& leftType, const type_index& rightType);
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
