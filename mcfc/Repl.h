#pragma once

#include <iostream>
#include <string>
#include <unordered_map>

#include "common.h"

enum class ConsoleColor
{
	Red,
	Blue,
	Green,
	DarkRed,
	Magenta,
	White,
	Grey,
};

class Repl
{
protected:
	virtual void RenderLine(const std::string& line)const { std::cout << line; };
	virtual void EvaluateMetaCommand(const std::string& input);
	virtual bool IsCompleteSubmission(const std::string& text)const = 0;
	virtual void EvaluateSubmission(const std::string& text) = 0;

public:
	virtual ~Repl() = default;

	void Run();
};

namespace MCF {
class Compilation;
}

class McfRepl final :public Repl
{
private:
	std::unique_ptr<MCF::Compilation> _previous{nullptr};
	bool _showTree{false};
	bool _showProgram{false};
	std::unordered_map<MCF::VariableSymbol, MCF::ValueType, MCF::VariableHash> _variables;

protected:
	// Inherited via Repl
	void RenderLine(const std::string& line)const override;
	void EvaluateMetaCommand(const std::string& input) override;
	bool IsCompleteSubmission(const std::string & text) const override;
	void EvaluateSubmission(const std::string & text) override;
public:
	McfRepl();
	~McfRepl();
};