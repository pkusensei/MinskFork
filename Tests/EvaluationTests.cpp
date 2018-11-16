#include "stdafx.h"
#include "CppUnitTest.h"

#include "..\MCF\Compilation.h"
#include "..\MCF\Diagnostic.h"
#include "..\MCF\Syntax.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

namespace Tests {

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
			auto tree = MCF::SyntaxTree::Parse(it.first);
			MCF::Compilation compilation(tree);
			std::unordered_map<MCF::VariableSymbol, MCF::ValueType, MCF::VariableHash> variables;
			auto result = compilation.Evaluate(variables);

			Assert::IsTrue(result.Diagnostics()->size() == 0);
			Assert::IsTrue(it.second == result.Value());
		}
	}
};

}