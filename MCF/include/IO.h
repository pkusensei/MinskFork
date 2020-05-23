#pragma once

#include <iostream>

#include "common.h"

namespace MCF {

enum class ConsoleColor;
enum class SyntaxKind;

class Diagnostic;
class DiagnosticBag;
class SyntaxTree;

class MCF_API TextWriter
{
protected:
	std::ostream& _out;

	bool IsConsole() const
	{
		return _out.rdbuf() == std::cout.rdbuf() || _out.rdbuf() == std::cerr.rdbuf();
	}

	void SetForeground(const ConsoleColor& color);
	void ResetColor();

public:
	constexpr explicit TextWriter(std::ostream& out = std::cout)noexcept
		:_out(out)
	{
	}
	virtual ~TextWriter() = default;

	virtual void Write(string_view text);
	virtual void WriteLine();

	void WriteKeyword(const SyntaxKind& kind);
	void WriteKeyword(string_view text);
	void WriteIdentifier(string_view text);
	void WriteNumber(string_view text);
	void WriteString(string_view text);
	void WriteSpace();
	void WritePunctuation(const SyntaxKind& kind);
	void WritePunctuation(string_view text);
	void WriteDiagnostics(vector<const Diagnostic*> diagnostics);
};

class MCF_API IndentedTextWriter final :public TextWriter
{
private:
	static constexpr auto INDENT_UNIT = "    ";
	size_t _indentCount;
	bool _indentPending;

	void WriteIndent();

public:
	constexpr explicit IndentedTextWriter(std::ostream& out = std::cout, size_t count = 0)noexcept
		:TextWriter(out), _indentCount(count), _indentPending(false)
	{
	}

	constexpr void Indent()noexcept { ++_indentCount; }
	constexpr void Dedent()noexcept { if (_indentCount > 0) --_indentCount; }

	void Write(string_view text)override;
	void WriteLine() override;
};

}//MCF
