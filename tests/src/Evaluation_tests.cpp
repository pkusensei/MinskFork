#include <catch2/catch.hpp>

#include "Compilation.h"
#include "Parsing.h"

#include "AnnotatedText.h"

namespace {
void AssertValue(std::string_view text, const MCF::ValueType& value);
void AssertDiagnostics(std::string_view text, std::string_view diagnosticText);
}

TEST_CASE("Evaluator computes correct values", "[Evaluator]")
{
	auto [input, expected] = GENERATE(
		table<std::string_view, MCF::ValueType>({
		{ "1", 1 },
		{ "+34", 34 },
		{ "-42", -42 },
		{ "~1", -2 },
		{ "3 + 1", 4 },
		{ "12 - 3", 9},
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
		{ "false | true", 1 },  // true
		{ "true | false", 1 },  // NOTE (bool & bool) evaluates to int
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
		{ "\"test\" + \"abc\"", "testabc"},
		{ "{ var a : any = 0 var b : any = \"b\" return a == b }", false },
		{ "{ var a : any = 0 var b : any = \"b\" return a != b }", true },
		{ "{ var a : any = 0 var b : any = 0 return a == b }", true },
		{ "{ var a : any = 0 var b : any = 0 return a != b }", false },
		{ "{var a = 10 return a}", 10 },
		{ "{var a = 10 return (a * a) }", 100 },
		{ "{var a = 0 return (a = 10) * a }", 100 },
		{ "{var a = 0 if a == 0 a = 10 return a }", 10 },
		{ "{var a = 0 if a == 4 a = 10 return a }", 0 },
		{ "{var a = 0 if a == 0 a = 10 else a = 5 return a }", 10 },
		{ "{var a = 0 if a == 4 a = 10 else a = 5 return a }", 5 },
		{ "{var i = 10 var result = 0 while i > 0 { result = result + i i = i - 1} return result }", 55 },
		{ "{var result = 0 for i = 1 to 10 { result = result + i } return result }", 55 },
		{ "{var a = 10 for i = 1 to (a = a - 1) { } return a }", 9 },
		{ "{var a = 0 do a = a + 1 while a < 10 return a}", 10 },
		{ "{ var i = 0 while i < 5 { i = i + 1 if i == 5 continue } return i }", 5 },
		{ "{ var i = 0 do { i = i + 1 if i == 5 continue } while i < 5 return i }", 5 },
		{ "{ var a = 1 a += (2 + 3) return a }", 6 },
		{ "{ var a = 1 a -= (2 + 3) return a }", -4 },
		{ "{ var a = 1 a *= (2 + 3) return a }", 5 },
		{ "{ var a = 1 a /= (2 + 3) return a }", 0 },
		{ "{ var a = true a &= (false) return a }", 0 }, 
		{ "{ var a = true a |= (false) return a }", 1 },
		{ "{ var a = true a ^= (true) return a }", 0 },
		{ "{ var a = 1 a |= 0 return a }", 1 },
		{ "{ var a = 1 a &= 3 return a }", 1 },
		{ "{ var a = 1 a &= 0 return a }", 0 },
		{ "{ var a = 1 a ^= 0 return a }", 1 },
		{ "{ var a = 1 var b = 2 var c = 3 a += b += c return a }", 6 },
		{ "{ var a = 1 var b = 2 var c = 3 a += b += c return b }", 5 },

		{ "{var x = 41 x++ return x }", 42 },
		{ "{var x = 3 return x---5 }", -3 },

												}));

	SECTION("One evaluation")
	{
		AssertValue(input, expected);
	}
}

TEST_CASE("Evaluator reports script returns keyword", "[Evaluator][return]")
{
	auto text = R"(
            return
			)";
	AssertValue(text, "");
}

TEST_CASE("Evaluator reports redeclaration in VariableDeclaration", "[Evaluator]")
{
	auto text = R"(
			{
				var x = 10
				var y = 100
				{
					var x = 10
				}
				var [x] = 5
			})";
	auto diag = R"(
			Variable 'x' is already declared.
		)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator stops infinite loop in BlockStatement", "[Evaluator][infinite loop]")
{
	auto text = R"(
			{
			[)][]
			)";
	auto diag = R"(
				Unexpected token <CloseParenthesisToken>, expected <IdentifierToken>.
				Unexpected token <EndOfFileToken>, expected <CloseBraceToken>.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports function arguments missing", "[Evaluator][function][call]")
{
	auto text = R"(
			print([)]
			)";

	auto diag = R"(
			Function 'print' requires 1 arguments but was given 0.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports function arguments exceeding", "[Evaluator][function][call]")
{
	auto text = R"(
			print("Hello"[, " ", ", world!"])
			)";

	auto diag = R"(
			Function 'print' requires 1 arguments but was given 3.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports wrong argument type", "[Evaluator][function][call]")
{
	auto text = R"(
            function test(n: int): bool
            {
                return n > 10
            }
            let testValue = "string"
            test([testValue])
			)";

	auto diag = R"(
			Cannot convert type 'string' to 'int'. An explicit conversion exists (are you missing a cast?)		
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports parameter already declared", "[Evaluator][function]")
{
	auto text = R"(
            function sum(a: int, b: int, [a: int]): int
            {
                return a + b + c
            }
			)";

	auto diag = R"(
            A parameter with name 'a' already exists.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator stops infinite loop in function arguments", "[Evaluator][function][infinite loop][call]")
{
	auto text = R"(
			print("Hi"[[=]][)]
			)";

	auto diagnostics = R"(
			Unexpected token <EqualsToken>, expected <CloseParenthesisToken>.
			Unexpected token <EqualsToken>, expected <IdentifierToken>.
			Unexpected token <CloseParenthesisToken>, expected <IdentifierToken>.
			)";

	AssertDiagnostics(text, diagnostics);
}

TEST_CASE("Evaluator stops infinite loop in function parameters", "[Evaluator][function][infinite loop]")
{
	auto text = R"(
			function hi(name: string[[[=]]][)]
            {
                print("Hi " + name + "!" )
            }[]			
			)";

	auto diagnostics = R"(
			Unexpected token <EqualsToken>, expected <CloseParenthesisToken>.
            Unexpected token <EqualsToken>, expected <OpenBraceToken>.
            Unexpected token <EqualsToken>, expected <IdentifierToken>.
            Unexpected token <CloseParenthesisToken>, expected <IdentifierToken>.
            Unexpected token <EndOfFileToken>, expected <CloseBraceToken>.
			)";

	AssertDiagnostics(text, diagnostics);
}

TEST_CASE("Evaluator reports missing return in functions", "[Evaluator][function][return]")
{
	auto text = R"(
			function [add](a:int, b:int):int
			{			
			}
			)";
	auto diag = R"(
			Not all code paths return a value.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports function should not return", "[Evaluator][function][return]")
{
	auto text = R"(
            function foo()
            {
                return [1]
            }
			)";
	auto diag = R"(
			Function 'foo' does not return a value.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports function should not return void", "[Evaluator][function][return]")
{
	auto text = R"(
            function foo():int
            {
                [return]
            }
			)";
	auto diag = R"(
            An expression of type 'int' is expected.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports not all code paths return value", "[Evaluator][control flow][return]")
{
	auto text = R"(
            function [foo](n: int): bool
            {
				if (n > 42)
					return true
            }
			)";
	auto diag = R"(
            Not all code paths return a value.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports function must have a name", "[Evaluator][function]")
{
	auto text = R"(
            function [(]a: int, b: int): int
            {
                return a + b
            }
			)";

	auto diag = R"(
            Unexpected token <OpenParenthesisToken>, expected <IdentifierToken>.
			)";

	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports CannotConvert in IfStatement", "[Evaluator][if]")
{
	auto text = R"(
			{
                var x = 0
                if [10]
					x = 10              
            }
			)";
	auto diag = R"(
                Cannot convert type 'int' to 'bool'.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports CannotConvert in WhileStatement", "[Evaluator][while]")
{
	auto text = R"(
			{
                var x = 0
                while [10]
                    x = 10              
            }
			)";
	auto diag = R"(
                Cannot convert type 'int' to 'bool'.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports CannotConvert in DoWhileStatement", "[Evaluator][do][while]")
{
	auto text = R"(
			{
                var x = 0
                do
                    x = 10
                while [10]
            }
			)";
	auto diag = R"(
				Cannot convert type 'int' to 'bool'.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports CannotConvert LowerBound in ForStatement", "[Evaluator][for]")
{
	auto text = R"(
			{
                var result = 0
                for i = [false] to 10
                    result = result + i              
            }
			)";
	auto diag = R"(
                Cannot convert type 'bool' to 'int'.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports CannotConvert UpperBound in ForStatement", "[Evaluator][for]")
{
	auto text = R"(
			{
                var result = 0
                for i = 1 to [true]
                    result = result + i              
            }
			)";
	auto diag = R"(
                Cannot convert type 'bool' to 'int'.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator warns unreachable code in IfStatement", "[Evaluator][if][unreachable]")
{
	auto text = R"(
			    function test()
                {
                    let x = 4 * 3
                    if x > 12
                    {
                        [print]("x")
                    }
                    else
                    {
                        print("x")
                    }
                }
			)";
	auto diag = R"(
                Unreachable code detected.
				)";
	AssertDiagnostics(text, diag);
}


TEST_CASE("Evaluator warns unreachable code in ElseStatement", "[Evaluator][if][else][unreachable]")
{
	auto text = R"(
			    function test(): int
                {
                    if true
                    {
                        return 1
                    }
                    else
                    {
                        [return] 0
                    }
                }
			)";
	auto diag = R"(
                Unreachable code detected.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator warns unreachable code in WhileStatement", "[Evaluator][while][unreachable]")
{
	auto text = R"(
			    function test()
                {
                    while false
                    {
                        [continue]
                    }
                }
			)";
	auto diag = R"(
                Unreachable code detected.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports invalid break or continue", "[Evaluator][break][return][loop]")
{
	auto [text, keyword] = GENERATE(
		table<std::string_view, std::string_view>(
			{
				{ "[break]", "break" },
				{ "[continue]", "continue" }
			}));
	auto diag = "The keyword '" + std::string(keyword) + "' can only be used inside of loops.";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports error for inserted token", "[Evaluator]")
{
	auto text = "1 + []";
	auto diag = R"(
                Unexpected token <EndOfFileToken>, expected <IdentifierToken>.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports Undefined in Assignment", "[Evaluator][assignment]")
{
	auto text = "[x] = 10";
	auto diag = R"(
                Variable 'x' doesn't exist.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports CannotAssign in Assignment", "[Evaluator][assignment]")
{
	auto text = R"(
			{
                let x = 10
                x [=] 0                
            }
			)";
	auto diag = R"(
                Variable 'x' is read-only; cannot be assigned to.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports NotAVariable in Assignment", "[Evaluator][assignment]")
{
	auto text = R"([print] = 42)";
	auto diag = R"(
                'print' is not a variable.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports CannotConvert in Assignment", "[Evaluator][assignment]")
{
	auto text = R"(
			{
                var x = 10
                x = [true]               
            }
			)";
	auto diag = R"(
                Cannot convert type 'bool' to 'int'.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports Undefined in CallExpression", "[Evaluator][call]")
{
	auto text = R"([foo](42))";

	auto diag = R"(
		Function 'foo' doesn't exist.
		)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports NotAFunction in CallExpression", "[Evaluator][call]")
{
	auto text = R"(
			{
				let foo = 42
				[foo](42)
			}
			)";

	auto diag = R"(
		'foo' is not a function.
		)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports Variable can shadow function", "[Evaluator][call]")
{
	auto text = R"(
            {
                let print = 42
                [print]("test")
            }
			)";

	auto diag = R"(
			'print' is not a function.
			)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports Expression must have value", "[Evaluator]")
{
	auto text = R"(
            function foo(n: int)
            {
                return
            }
            let value = [foo(42)]
			)";
	auto diag = R"(
            Expression must have a value.
		)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports bad type", "[Evaluator]")
{
	auto text = R"(
            function test(n: [invalidtype])
            {
            }
			)";
	auto diag = R"(
            Type 'invalidtype' doesn't exist.
			)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports undefined unary operation", "[Evaluator]")
{
	auto text = "[+] true";
	auto diag = R"(
                Unary operator '+' is not defined for type 'bool'.
				)";
	AssertDiagnostics(text, diag);
}

TEST_CASE("Evaluator reports undefined binary operation", "[Evaluator]")
{
	auto text = "10 [*] false";
	auto diag = R"(
                Binary operator '*' is not defined for types 'int' and 'bool'.
				)";
	AssertDiagnostics(text, diag);
}

namespace {

void AssertValue(std::string_view text, const MCF::ValueType& value)
{
	auto tree = MCF::SyntaxTree::Parse(text);
	auto compilation = MCF::Compilation::CreateScript(nullptr, std::move(tree));
	MCF::VarMap variables;
	auto result = compilation.Evaluate(variables);

	REQUIRE_FALSE(result.Diagnostics().HasErrors());
	CHECK(value == result.Value());
}

void AssertDiagnostics(std::string_view text, std::string_view diagnosticText)
{
	auto annotatedText = AnnotatedText::Parse(text);
	auto tree = MCF::SyntaxTree::Parse(annotatedText.Text());
	auto compilation = MCF::Compilation::CreateScript(nullptr, std::move(tree));
	MCF::VarMap variables;
	auto result = compilation.Evaluate(variables);
	auto expectedDiagnostoics = AnnotatedText::DedentLines(diagnosticText);

	if (annotatedText.Spans().size() != expectedDiagnostoics.size())
		throw std::invalid_argument("ERROR: Must mark as many spans as there are expected diagnostics");

	auto& diags = result.Diagnostics();
	REQUIRE(expectedDiagnostoics.size() == diags.size());

	for (size_t i = 0; i < expectedDiagnostoics.size(); ++i)
	{
		CHECK(expectedDiagnostoics[i] == diags[i].Message());
		CHECK(annotatedText.Spans()[i] == diags[i].Location().Span);
	}

}
}