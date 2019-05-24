#pragma once

#include <iostream>

#include "common.h"

namespace MCF {

enum class ConsoleColor;

class TextWriter
{
protected:
	std::ostream& _out;

public:
	explicit TextWriter(std::ostream& out = std::cout)noexcept :_out(out) {}
	virtual ~TextWriter() = default;

	bool IsConsoleOutput() const { return _out.rdbuf() == std::cout.rdbuf(); }

	void SetForeground(const ConsoleColor& color);
	void ResetColor();

	virtual void Write(const string& text);
	void WriteKeyword(const string& text);
	void WriteIdentifier(const string& text);
	void WriteNumber(const string& text);
	void WriteString(const string& text);
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
