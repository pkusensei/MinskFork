#pragma once

#include <iostream>

#include "common.h"

namespace MCF {

enum class ConsoleColor;
enum class SyntaxKind;

class TextWriter
{
protected:
	std::ostream& _out;

	bool IsConsoleOutput() const { return _out.rdbuf() == std::cout.rdbuf(); }

	void SetForeground(const ConsoleColor& color);
	void ResetColor();

public:
	explicit TextWriter(std::ostream& out = std::cout)noexcept :_out(out) {}
	virtual ~TextWriter() = default;

	virtual void Write(const string& text);
	void WriteKeyword(const SyntaxKind& kind);
	void WriteKeyword(const string& text);
	void WriteIdentifier(const string& text);
	void WriteNumber(const string& text);
	void WriteString(const string& text);
	void WriteSpace();
	void WritePunctuation(const SyntaxKind& kind);
	void WritePunctuation(const string& text);
	virtual void WriteLine();
};

class IndentedTextWriter final :public TextWriter
{
private:
	static constexpr auto INDENT_UNIT = "    ";
	size_t _indentCount;
	bool _indentPending;

	void WriteIndent();

public:
	explicit IndentedTextWriter(std::ostream& out = std::cout, size_t count = 0)noexcept
		:TextWriter(out), _indentCount(count), _indentPending(false)
	{
	}

	void Indent()noexcept { ++_indentCount; }
	void Dedent()noexcept { if (_indentCount > 0) --_indentCount; }

	void Write(const string& text)override;
	void WriteLine() override;
};

}//MCF
