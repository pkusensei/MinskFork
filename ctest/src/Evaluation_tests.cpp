#include <catch.hpp>

#include "Compilation.h"
#include "Diagnostic.h"
#include "Parsing.h"

#include "AnnotatedText.h"

void AssertValue(const std::string& text, const MCF::ValueType& value);
void AssertDiagnostics(const std::string& text, const std::string& diagnosticText);

TEST_CASE("Evaluator computes correct values", "[Evaluator]")
{
	auto [input, expected] = GENERATE(
		table<std::string, MCF::ValueType>({
		{"1", 1},
		{ "+34", 34 },
		{ "-42", -42 },
		{ "~1", -2 },
		{ "3 + 1", 4 },
		{ "2 * 4", 8 },
		{ "9 / 3", 3 },
		{ "(6 + 4)", 10 },
		{ "12 == 5", false },
		{ "5 == 5", true },
		{ "12 != 5", true },
		{ "5 != 5", false },
		{ "3 < 4", true },
		{ "5 < 4", false },
		{ "4 <= 4", true },
		{ "4 <= 5", true },
		{ "5 <= 4", false },
		{ "3 > 4", false },
		{ "5 > 4", true },
		{ "4 >= 4", true },
		{ "4 >= 5", false },
		{ "5 >= 4", true },
		{ "1 | 2", 3 },
		{ "1 | 0", 1 },
		{ "1 & 3", 1 },
		{ "1 & 0", 0 },
		{ "1 ^ 0", 1 },
		{ "0 ^ 1", 1 },
		{ "1 ^ 3", 2 },
		{ "true == false", false },
		{ "false == false", true },
		{ "true != false", true },
		{ "false != false", false },
		{ "true && true", true },
		{ "false || false", false },
		{ "false | false", 0 }, // false
		{ "false | true", 1 }, // true
		{ "true | false", 1 },
		{ "true | true", 1 },
		{ "false & false", 0 },
		{ "false & true", 0 },
		{ "true & false", 0 },
		{ "true & true", 1 },
		{ "false ^ false", 0 },
		{ "false ^ true", 1 },
		{ "true ^ false", 1 },
		{ "true ^ true", 0 },
		{ "true", true },
		{ "!true", false },
		{ "false", false },
		{ "!false", true },
		{ "\"test\"", "test" },
		{ "\"te\"\"st\"", "te\"st" },
		{ "\"test\" == \"test\"", true },
		{ "\"test\" != \"test\"", false },
		{ "\"test\" == \"abc\"", false },
		{ "\"test\" != \"abc\"", true },
		{ "{var a = 10 a }", 10 },
		{ "{var a = 10 (a * a) }", 100 },
		{ "{var a = 0 (a = 10) * a }", 100 },
		{ "{var a = 0 if a == 0 a = 10 a }", 10 },
		{ "{var a = 0 if a == 4 a = 10 a }", 0 },
		{ "{var a = 0 if a == 0 a = 10 else a = 5 a }", 10 },
		{ "{var a = 0 if a == 4 a = 10 else a = 5 a }", 5 },
		{ "{var i = 10 var result = 0 while i > 0 { result = result + i i = i - 1} result }", 55 },
		{ "{var result = 0 for i = 1 to 10 { result = result + i } result }", 55 },
		{ "{var a = 10 for i = 1 to (a = a - 1) { } a }", 9 },
		{ "{var a = 0 do a = a + 1 while a < 10 a}", 10 },

		{ "{var x = 41 x++}", 42 },
		{ "{var x = 3 x---5}", -3 },
			}));
	SECTION("One evaluation")
	{
		AssertValue(input, expected);
	}
}

TEST_CASE("Evaluator reports redeclaration in VariableDeclaration", "[Evaluator]")
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

TEST_CASE("Evaluator stops infinite loop in BlockStatement", "[Evaluator][infinite loop]")
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

TEST_CASE("Evaluator reports function arguments missing", "[Evaluator][function]")
{
	std::string text = R"(
			print([)]
			)";

	std::string diag = R"(
			Function 'print' requires 1 arguments but was given 0.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports function arguments exceeding", "[Evaluator][function]")
{
	std::string text = R"(
			print("Hello"[, " ", ", world!"])
			)";

	std::string diag = R"(
			Function 'print' requires 1 arguments but was given 3.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator stops infinite loop in function arguments", "[Evaluator][function][infinite loop]")
{
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

TEST_CASE("Evaluator stops infinite loop in function parameters", "[Evaluator][function][infinite loop]")
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

TEST_CASE("Evaluator reports CannotConvert in IfStatement", "[Evaluator][if]")
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

TEST_CASE("Evaluator reports CannotConvert in WhileStatement", "[Evaluator][while]")
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

TEST_CASE("Evaluator reports CannotConvert in DoWhileStatement", "[Evaluator][do][while]")
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

TEST_CASE("Evaluator reports CannotConvert LowerBound in ForStatement", "[Evaluator][for]")
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

TEST_CASE("Evaluator reports CannotConvert UpperBound in ForStatement", "[Evaluator][for]")
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

TEST_CASE("Evaluator reports undefined name", "[Evaluator]")
{
	std::string text = "[x] = 10";
	std::string diag = R"(
                Variable 'x' doesn't exist.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports error for inserted token", "[Evaluator]")
{
	std::string text = "1 + []";
	std::string diag = R"(
                Unexpected token <EndOfFileToken>, expected <IdentifierToken>.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports CannotAssign in Assignment", "[Evaluator]")
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

TEST_CASE("Evaluator reports CannotConvert in Assignment", "[Evaluator]")
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

TEST_CASE("Evaluator reports undefined unary operation", "[Evaluator]")
{
	std::string text = "[+] true";
	std::string diag = R"(
                Unary operator '+' is not defined for type 'bool'.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports undefined binary operation", "[Evaluator]")
{
	std::string text = "10 [*] false";
	std::string diag = R"(
                Binary operator '*' is not defined for types 'int' and 'bool'.
				)";
	AssertDiagnostics(text, diag);
}

void AssertValue(const std::string& text, const MCF::ValueType& value)
{
	auto tree = MCF::SyntaxTree::Parse(text);
	MCF::Compilation compilation(tree);
	MCF::VarMap variables;
	auto result = compilation.Evaluate(variables);

	CHECK(result.Diagnostics()->empty());
	CHECK(value == result.Value());
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
	CHECK(expectedDiagnostoics.size() == result.Diagnostics()->size());

	for (size_t i = 0; i < expectedDiagnostoics.size(); ++i)
	{
		CHECK(expectedDiagnostoics[i] == (*result.Diagnostics())[i].Message());
		CHECK(annotatedText.Spans()[i] == (*result.Diagnostics())[i].Span());
	}

}