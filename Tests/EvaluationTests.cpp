#include "stdafx.h"
#include "CppUnitTest.h"

#include <algorithm>
#include <stack>

#include "..\MCF\helpers.h"
#include "..\MCF\Compilation.h"
#include "..\MCF\Diagnostic.h"
#include "..\MCF\Parsing.h"
#include "..\MCF\SourceText.h"

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
			while (std::getline(ss, line))
				if (!line.empty())
					lines.emplace_back(line);
		}

		auto minIndentation = SIZE_MAX;
		for (size_t i = 0; i < lines.size(); ++i)
		{
			auto line = lines[i];
			if ((line = MCF::TrimString(line)).empty())
			{
				lines[i] = std::string();
				continue;
			}
			auto indentation = line.length() - (line = MCF::TrimStringStart(line)).length();
			minIndentation = std::min(indentation, minIndentation);
			lines[i] = line;
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
				startStack.pop();
				auto end = position;
				spans.emplace_back(MCF::TextSpan::FromBounds(start, end));
			}
			else
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
			{"1", 1},
			{"+34", 34},
			{"-42", -42},
			{"~1", -2},
			{"3 + 1", 4},
			{"2 * 4", 8},
			{"9 / 3", 3},
			{"(6 + 4)", 10},
			{"12 == 5", false},
			{"5 == 5", true},
			{"12 != 5", true},
			{"5 != 5", false},
			{"3 < 4", true},
			{"5 < 4", false},
			{"4 <= 4", true},
			{"4 <= 5", true},
			{"5 <= 4", false},
			{"3 > 4", false},
			{"5 > 4", true},
			{"4 >= 4", true},
			{"4 >= 5", false},
			{"5 >= 4", true},
			{"1 | 2", 3},
			{"1 | 0", 1},
			{"1 & 3", 1},
			{"1 & 0", 0},
			{"1 ^ 0", 1},
			{"0 ^ 1", 1},
			{"1 ^ 3", 2},
			{"true == false", false},
			{"false == false", true},
			{"true != false", true},
			{"false != false", false},
			{"true && true", true},
			{"false || false", false},
			{"false | false", 0}, // false
			{"false | true", 1}, // true
			{"true | false", 1},
			{"true | true", 1},
			{"false & false", 0},
			{"false & true", 0},
			{"true & false", 0},
			{"true & true", 1},
			{"false ^ false", 0},
			{"false ^ true", 1},
			{"true ^ false", 1},
			{"true ^ true", 0},
			{"true", true},
			{"!true", false},
			{"false", false},
			{"!false", true},
			{"\"test\"", "test"},
			{"\"te\"\"st\"", "te\"st"},
			{"\"test\" == \"test\"", true},
			{"\"test\" != \"test\"", false},
			{"\"test\" == \"abc\"", false},
			{"\"test\" != \"abc\"", true},
			{"{var a = 10 a }", 10},
			{"{var a = 10 (a * a) }", 100},
			{"{var a = 0 (a = 10) * a }", 100},
			{"{var a = 0 if a == 0 a = 10 a }", 10},
			{"{var a = 0 if a == 4 a = 10 a }", 0},
			{"{var a = 0 if a == 0 a = 10 else a = 5 a }", 10},
			{"{var a = 0 if a == 4 a = 10 else a = 5 a }", 5},
			{"{ var i = 10 var result = 0 while i > 0 { result = result + i i = i - 1} result }", 55},
			{"{ var result = 0 for i = 1 to 10 { result = result + i } result }", 55},
			{"{ var a = 10 for i = 1 to (a = a - 1) { } a }", 9},
			{"{ var a = 0 do a = a + 1 while a < 10 a}", 10},

			{"{var x = 41 x++}", 42},
			{"{var x = 3 x---5}", -3},
		};

		for (const auto& it : data)
		{
			AssertValue(it.first, it.second);
		}
	}

	TEST_METHOD(Evaluator_VariableDeclaration_Reports_Redeclaration)
	{
		std::string text = R"({
					var x = 10
					var y = 100
					{
						var x = 10
					}
					var [x] = 5
				})";
		std::string diag = R"(
			Variable 'x' is already declared.
		)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_BlockStatement_NoInfiniteLoop)
	{
		std::string text = R"(
			{
			[)][]
			)";
		std::string diag = R"(
				Unexpected token <CloseParenthesisToken>, expected <IdentifierToken>.
				Unexpected token <EndOfFileToken>, expected <CloseBraceToken>.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_InvokeFunctionArguments_NoInfiniteLoop) {
		std::string text = R"(
			print("Hi"[[=]][)]
			)";

		std::string diagnostics = R"(
			Unexpected token <EqualsToken>, expected <CloseParenthesisToken>.
			Unexpected token <EqualsToken>, expected <IdentifierToken>.
			Unexpected token <CloseParenthesisToken>, expected <IdentifierToken>.
			)";

		AssertDiagnostics(text, diagnostics);
	}

	TEST_METHOD(Evaluator_FunctionParameters_NoInfiniteLoop)
	{
		std::string text = R"(
			function hi(name: string[[[=]]][)]
            {
                print("Hi " + name + "!" )
            }[]			
			)";

		std::string diagnostics = R"(
			Unexpected token <EqualsToken>, expected <CloseParenthesisToken>.
            Unexpected token <EqualsToken>, expected <OpenBraceToken>.
            Unexpected token <EqualsToken>, expected <IdentifierToken>.
            Unexpected token <CloseParenthesisToken>, expected <IdentifierToken>.
            Unexpected token <EndOfFileToken>, expected <CloseBraceToken>.
			)";

		AssertDiagnostics(text, diagnostics);
	}

	TEST_METHOD(Evaluator_IfStatement_Reports_CannotConvert)
	{
		std::string text = R"(
			{
                var x = 0
                if [10]
					x = 10              
            }
			)";
		std::string diag = R"(
                Cannot convert type 'int' to 'bool'.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_WhileStatement_Reports_CannotConvert)
	{
		std::string text = R"(
			{
                var x = 0
                while [10]
                    x = 10              
            }
			)";
		std::string diag = R"(
                Cannot convert type 'int' to 'bool'.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_DoWhileStatement_Reports_CannotConvert)
	{
		std::string text = R"(
			{
                var x = 0
                do
                    x = 10
                while [10]
            }
			)";
		std::string diag = R"(
				Cannot convert type 'int' to 'bool'.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_ForStatement_Reports_CannotConvert_LowerBound)
	{
		std::string text = R"(
			{
                var result = 0
                for i = [false] to 10
                    result = result + i              
            }
			)";
		std::string diag = R"(
                Cannot convert type 'bool' to 'int'.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_ForStatement_Reports_CannotConvert_UpperBound)
	{
		std::string text = R"(
			{
                var result = 0
                for i = 1 to [true]
                    result = result + i              
            }
			)";
		std::string diag = R"(
                Cannot convert type 'bool' to 'int'.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_Name_Reports_Undefined)
	{
		std::string text = "[x] = 10";
		std::string diag = R"(
                Variable 'x' doesn't exist.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_Name_Reports_NoErrorForInsertedToken)
	{
		std::string text = "1 + []";
		std::string diag = R"(
                Unexpected token <EndOfFileToken>, expected <IdentifierToken>.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_Assignment_Reports_CannotAssign)
	{
		std::string text = R"(
			{
                let x = 10
                x [=] 0                
            }
			)";
		std::string diag = R"(
                Variable 'x' is read-only; cannot be assigned to.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_Assignment_Reports_CannotConvert)
	{
		std::string text = R"(
			{
                var x = 10
                x = [true]               
            }
			)";
		std::string diag = R"(
                Cannot convert type 'bool' to 'int'.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_Unary_Reports_Undefined)
	{
		std::string text = "[+] true";
		std::string diag = R"(
                Unary operator '+' is not defined for type 'bool'.
				)";
		AssertDiagnostics(text, diag);
	}

	TEST_METHOD(Evaluator_Binary_Reports_Undefined)
	{
		std::string text = "10 [*] false";
		std::string diag = R"(
                Binary operator '*' is not defined for types 'int' and 'bool'.
				)";
		AssertDiagnostics(text, diag);
	}

private:
	static void AssertValue(const std::string& text, const MCF::ValueType& value)
	{
		auto tree = MCF::SyntaxTree::Parse(text);
		MCF::Compilation compilation(tree);
		MCF::VarMap variables;
		auto result = compilation.Evaluate(variables);

		Assert::IsTrue(result.Diagnostics()->empty());
		Assert::IsTrue(value == result.Value());
	}

	void AssertDiagnostics(const std::string& text, const std::string& diagnosticText)
	{
		auto annotatedText = AnnotatedText::Parse(text);
		auto tree = MCF::SyntaxTree::Parse(annotatedText.Text());
		auto compilation = MCF::Compilation(tree);
		MCF::VarMap variables;
		auto result = compilation.Evaluate(variables);
		auto expectedDiagnostoics = AnnotatedText::DedentLines(diagnosticText);

		if (annotatedText.Spans().size() != expectedDiagnostoics.size())
			throw std::invalid_argument("ERROR: Must mark as many spans as there are expected diagnostics");
		Assert::AreEqual(expectedDiagnostoics.size(), result.Diagnostics()->size());

		for (size_t i = 0; i < expectedDiagnostoics.size(); ++i)
		{
			Assert::AreEqual(expectedDiagnostoics[i], (*result.Diagnostics())[i].Message());
			Assert::IsTrue(annotatedText.Spans()[i] == (*result.Diagnostics())[i].Span());
		}

	}
};

}