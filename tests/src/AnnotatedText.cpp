#include "AnnotatedText.h"

#include <algorithm>
#include <stack>

#include "helpers.h"

std::string AnnotatedText::Dedent(std::string_view text)
{
	auto lines = DedentLines(text);
	std::string result;
	for (const auto& it : lines)
		result += std::string(it) + '\n';
	result.erase(--result.end());
	return result;
}

std::vector<std::string_view> AnnotatedText::DedentLines(std::string_view text)
{
	auto lines = MCF::StringSplit(text.cbegin(), text.cend(), '\n');

	auto minIndentation = std::numeric_limits<size_t>::max();
	for (size_t i = 0; i < lines.size(); ++i)
	{
		std::string_view line = lines[i];
		if ((line = MCF::TrimString(line)).empty())
		{
			lines[i] = std::string_view();
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
	return lines;
}

AnnotatedText AnnotatedText::Parse(std::string_view input)
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