#pragma once
#include <string>
#include <vector>
#include <typeindex>
#include "common.h"

namespace MCF {

enum class SyntaxKind;

class Diagnostic final
{
private:
	TextSpan _span;
	std::string _message;
public:
	MCF_API Diagnostic(TextSpan span, const std::string& message);
	MCF_API ~Diagnostic() = default;
	Diagnostic(Diagnostic&& other);
	Diagnostic& operator=(Diagnostic&& other);

	TextSpan Span() const { return _span; }
	std::string Message() const {return _message; }
};

class DiagnosticBag final
{
private:
	std::vector<Diagnostic> _diagnostics;
	void Report(TextSpan span, const std::string& message);
public:
	DiagnosticBag();
	~DiagnosticBag() = default;
	DiagnosticBag(DiagnosticBag&& other);
	size_t size() const { return _diagnostics.size(); }

	class iterator;
	const Diagnostic& GetOneDiagnostic (int idx) const;
	MCF_API iterator begin();
	MCF_API iterator end();

	void AddRange(DiagnosticBag& other);
	void ReportInvalidNumber(TextSpan span, const std::string& text, const std::type_info& type);
	void ReportBadCharacter(int position, char character);
	void ReportUnexpectedToken(TextSpan span, SyntaxKind actualKind, SyntaxKind expectedKind);
};

class DiagnosticBag::iterator
{
private:
	int _position;
	DiagnosticBag* _bag;
public:
	iterator(int pos, DiagnosticBag& bag);

	MCF_API const Diagnostic& operator*() const;
	MCF_API iterator& operator++(int); //i++
	MCF_API iterator& operator++(); //++i
	bool operator!=(const iterator& other)const { return _position != other._position; }
};

}//MCF
