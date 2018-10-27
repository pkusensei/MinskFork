#pragma once
#include <string>
#include <vector>
#include <typeindex>
#include "common.h"

namespace MCF {

class Diagnostic final
{
private:
	TextSpan _span;
	std::string _message;
public:
	Diagnostic(TextSpan span, std::string& message);
	~Diagnostic();

	TextSpan Span() const { return _span; }
	std::string Message() const {return _message; }
};

class DiagnosticBag final
{
private:
	std::vector<std::unique_ptr<Diagnostic>> _diagnostics;
	void Report(TextSpan span, std::string& message);
public:
	~DiagnosticBag();
	class iterator;
	Diagnostic* GetOneDiagnostic (int idx) const;
	iterator begin();
	iterator end();
	void AddRange(DiagnosticBag& other);

	void ReportInvalidNumber(TextSpan span, const std::string& text, const std::type_index& type);
};

class DiagnosticBag::iterator
{
private:
	int _position;
	DiagnosticBag* _bag;
public:
	iterator(int pos, DiagnosticBag& bag);

	Diagnostic* operator*() const;
	iterator& operator++(int); //i++
	iterator& operator++(); //++i
	bool operator!=(const iterator& other)const { return _position != other._position; }
};

}//MCF
