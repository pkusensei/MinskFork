#include <catch.hpp>

#include "SourceText.h"

TEST_CASE("SourceText includes last line", "[SourceText]")
{
	auto data = std::vector<std::pair<std::string, size_t>>{
		{".", 1},
		{"\r\n", 2},
		{".\r\n\r\n", 3},
	};
	for (const auto& it : data)
	{
		auto sourceText = MCF::SourceText::From(it.first);
		auto& lines = sourceText.Lines();
		REQUIRE(it.second == lines.size());
	}
}