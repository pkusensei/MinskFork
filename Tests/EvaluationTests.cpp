#include "stdafx.h"
#include "CppUnitTest.h"

#include <algorithm>
#include <stack>

#include "..\MCF\Compilation.h"
#include "..\MCF\Diagnostic.h"
#include "..\MCF\Syntax.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace Tests {

class AnnotatedText final
{
private:
	std::string _text;
	std::vector<MCF::TextSpan> _spans;

	static std::string Dedent(const std::string& text)
	{
		auto lines = DedentLines(text);
		std::string result;
		for (const auto& it : lines)
			result += it + '\n';
		result.erase(--result.end());
		return result;
	}

public:
	AnnotatedText(const std::string& text, const std::vector<MCF::TextSpan>& spans)
		:_text(text), _spans(spans)
	{
	}

	std::string Text()const { return _text; }
	std::vector<MCF::TextSpan> Spans()const { return _spans; }

	static std::vector<std::string> DedentLines(const std::string& text)
	{
		auto lines = std::vector<std::string>();
		{
			std::stringstream ss(text);
			std::string line;
			while (std::getline(ss, line) && !line.empty())
				lines.emplace_back(line);
		}

		auto minIndentation = SIZE_MAX;
		for (size_t i = 0; i < lines.size(); ++i)
		{
			auto line = lines[i];
			if (MCF::TrimString(line).empty())
			{
				lines[i] = std::string();
				continue;
			}
			auto indentation = line.length() - MCF::TrimStringStart(line).length();
			minIndentation = std::min(indentation, minIndentation);
		}

		for (size_t i = 0; i < lines.size(); ++i)
		{
			if (lines[i].empty())
				continue;
			lines[i] = lines[i].substr(minIndentation);
		}
		while (!lines.empty() && lines.begin()->empty())
			lines.erase(lines.begin());
		while (!lines.empty() && lines.back().empty())
			lines.erase(--lines.end());
		lines.shrink_to_fit();
		return lines;
	}

	static AnnotatedText Parse(const std::string& input)
	{
		auto text = Dedent(input);
		std::string result;
		std::vector<MCF::TextSpan> spans;
		std::stack<size_t> startStack;

		size_t position = 0;
		for (const auto& c : text)
		{
			if (c == '[')
				startStack.emplace(position);
			else if (c == ']')
			{
				if (startStack.empty())
					throw std::invalid_argument("Too many ']' in input text");
				auto start = startStack.top();
				startStack.top();
				auto end = position;
				spans.emplace_back(MCF::TextSpan::FromBounds(start, end));
			} else
			{
				++position;
				result += c;
			}
		}
		if (!startStack.empty())
			throw std::invalid_argument("Missing ']' in input text");
		return AnnotatedText(result, spans);
	}
};

TEST_CLASS(EvaluationTests)
{
public:
	TEST_METHOD(Evaluator_Computes_CorrectValues)
	{
		auto data = std::vector<std::pair<std::string, MCF::ValueType>>{
					std::pair<std::string, MCF::ValueType>("1", 1),
					std::pair<std::string, MCF::ValueType>("+34", 34),
					std::pair<std::string, MCF::ValueType>("-42", -42),
					std::pair<std::string, MCF::ValueType>("3 + 1", 4),
					std::pair<std::string, MCF::ValueType>("2 * 4", 8),
					std::pair<std::string, MCF::ValueType>("9 / 3", 3),
					std::pair<std::string, MCF::ValueType>("(6 + 4)", 10),
					std::pair<std::string, MCF::ValueType>("12 == 5", false),
					std::pair<std::string, MCF::ValueType>("5 == 5", true),
					std::pair<std::string, MCF::ValueType>("12 != 5", true),
					std::pair<std::string, MCF::ValueType>("5 != 5", false),
					std::pair<std::string, MCF::ValueType>("3 < 4", true),
					std::pair<std::string, MCF::ValueType>("5 < 4", false),
					std::pair<std::string, MCF::ValueType>("4 <= 4", true),
					std::pair<std::string, MCF::ValueType>("4 <= 5", true),
					std::pair<std::string, MCF::ValueType>("5 <= 4", false),
					std::pair<std::string, MCF::ValueType>("3 > 4", false),
					std::pair<std::string, MCF::ValueType>("5 > 4", true),
					std::pair<std::string, MCF::ValueType>("4 >= 4", true),
					std::pair<std::string, MCF::ValueType>("4 >= 5", false),
					std::pair<std::string, MCF::ValueType>("5 >= 4", true),
					std::pair<std::string, MCF::ValueType>("true == false", false),
					std::pair<std::string, MCF::ValueType>("false == false", true),
					std::pair<std::string, MCF::ValueType>("true != false", true),
					std::pair<std::string, MCF::ValueType>("false != false", false),
					std::pair<std::string, MCF::ValueType>("true", true),
					std::pair<std::string, MCF::ValueType>("!true", false),
					std::pair<std::string, MCF::ValueType>("false", false),
					std::pair<std::string, MCF::ValueType>("!false", true),
					std::pair<std::string, MCF::ValueType>("{var a = 0 (a = 10) * a }", 100),
					std::pair<std::string, MCF::ValueType>("{var a = 0 if a == 0 a = 10 a }", 10),
					std::pair<std::string, MCF::ValueType>("{var a = 0 if a == 4 a = 10 a }", 0),
					std::pair<std::string, MCF::ValueType>("{var a = 0 if a == 0 a = 10 else a = 5 a }", 10),
					std::pair<std::string, MCF::ValueType>("{var a = 0 if a == 4 a = 10 else a = 5 a }", 5),
					std::pair<std::string, MCF::ValueType>("{ var i = 10 var result = 0 while i > 0 { result = result + i i = i - 1} result }", 55),
					std::pair<std::string, MCF::ValueType>("{ var result = 0 for i = 1 to 10 { result = result + i } result }", 55),
		};

		for (const auto& it : data)
		{
			AssertValue(it.first, it.second);
		}
	}

private:
	static void AssertValue(const std::string& text, const MCF::ValueType& value)
	{
		auto tree = MCF::SyntaxTree::Parse(text);
		MCF::Compilation compilation(tree);
		std::unordered_map<MCF::VariableSymbol, MCF::ValueType, MCF::VariableHash> variables;
		auto result = compilation.Evaluate(variables);

		Assert::IsTrue(result.Diagnostics()->size() == 0);
		Assert::IsTrue(value == result.Value());
	}
};

}