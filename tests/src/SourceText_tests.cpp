#include <catch2/catch.hpp>

#include "SourceText.h"

TEST_CASE("SourceText includes last line", "[SourceText]")
{
	auto [input, expected] = GENERATE(
		// or values<std::pair<std::string, size_t>>
		table<std::string, size_t>({
			{".", 1},
			{"\r\n", 2},
			{".\r\n\r\n", 3},
			})
			);

	SECTION("One pair")
	{
		auto text = MCF::SourceText::From(input);
		auto& lines = text.Lines();
		CHECK(lines.size() == expected);
	}
}