#include "pch.h"
#include "Repl.h"

#include <cctype>
#include <Windows.h>

#include "Compilation.h"
#include "Diagnostic.h"
#include "SourceText.h"
#include "Syntax.h"

void SetConsoleColor(const ConsoleColor& color = ConsoleColor::Grey)
{
	HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	//CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	//GetConsoleScreenBufferInfo(hStdout, &csbiInfo);
	//WORD wOldColorAttrs = csbiInfo.wAttributes;
	switch (color)
	{
		case ConsoleColor::Red:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::Blue:
			SetConsoleTextAttribute(hStdout, FOREGROUND_BLUE | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::Green:
			SetConsoleTextAttribute(hStdout, FOREGROUND_GREEN | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::Magenta:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_BLUE);
			break;
		case ConsoleColor::DarkRed:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED);
			break;
		case ConsoleColor::White:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_INTENSITY);
			break;
		case ConsoleColor::Grey:
		default:
			SetConsoleTextAttribute(hStdout, FOREGROUND_RED | FOREGROUND_BLUE | FOREGROUND_GREEN);
			break;
	}
}

void ResetConsoleColor()
{
	SetConsoleColor();
}

void ClearConsole(char fill = ' ')
{
	COORD t1 = {0, 0};
	HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
	GetConsoleScreenBufferInfo(hStdout, &csbiInfo);
	DWORD written;
	DWORD cells = csbiInfo.dwSize.X*csbiInfo.dwSize.Y;
	FillConsoleOutputCharacter(hStdout, fill, cells, t1, &written);
	FillConsoleOutputAttribute(hStdout, csbiInfo.wAttributes, cells, t1, &written);
	SetConsoleCursorPosition(hStdout, t1);
}

bool IsStringBlank(const std::string& s)
{
	for (const auto& c : s)
		if (!std::isspace(c))
			return false;
	return true;
}

void Repl::EvaluateMetaCommand(const std::string & input)
{
	SetConsoleColor(ConsoleColor::Red);
	std::cout << "Invalid command " << input;
	ResetConsoleColor();
}

void Repl::Run()
{
	std::string text, input;

	while (true)
	{
		SetConsoleColor(ConsoleColor::Green);
		if (text.empty())
			std::cout << "> ";
		else std::cout << "| ";
		ResetConsoleColor();

		std::getline(std::cin, input);
		auto isBlank = IsStringBlank(input);
		if (text.empty())
		{
			if (isBlank)
				break;
			else if (MCF::StringStartsWith(input, "#"))
			{
				EvaluateMetaCommand(input);
				continue;
			}
		}
		if (!std::cin.eof() && !std::cin.fail())
			text += input + '\r'; // HACK Windows does "\r\n" together

		if (!IsCompleteSubmission(text))
			continue;
		EvaluateSubmission(text);
		text.clear();
	}
}

void McfRepl::RenderLine(const std::string & line) const
{
	using namespace MCF;
	auto tokens = SyntaxTree::ParseTokens(line);
	for (const auto& it : tokens)
	{
		auto isKeyword = StringEndsWith(GetSyntaxKindName(it->Kind()), "Keyword");
		auto isNumber = it->Kind() == SyntaxKind::NumberToken;

		if (isKeyword)
			SetConsoleColor(ConsoleColor::Blue);
		else if (isNumber)
			SetConsoleColor(ConsoleColor::Grey);

		std::cout << it->Text();
		ResetConsoleColor();
	}
}

void McfRepl::EvaluateMetaCommand(const std::string & input)
{
	if (input == "#showTree")
	{
		_showTree = !_showTree;
		std::cout << (_showTree ? "Showing parse trees." : "Not showing parse trees.") << "\n";
	} else if (input == "#showProgram")
	{
		_showProgram = !_showProgram;
		std::cout << (_showProgram ? "Showing bound tree." : "Not showing bound tree.") << "\n";
	} else if (input == "#cls")
	{
		ClearConsole();
	} else if (input == "#reset")
	{
		_previous = nullptr;
		ClearConsole();
	} else
	{
		Repl::EvaluateMetaCommand(input);
	}
}

bool McfRepl::IsCompleteSubmission(const std::string & text) const
{
	if (IsStringBlank(text))
		return true;

	auto tree = MCF::SyntaxTree::Parse(text);
	if (!tree->Diagnostics()->empty())
		return false;
	return true;
}

void McfRepl::EvaluateSubmission(const std::string & text)
{
	using namespace MCF;

	auto tree = SyntaxTree::Parse(text);

	auto compilation = _previous == nullptr ? std::make_unique<Compilation>(tree)
		: Compilation::ContinueWith(_previous, tree);
	if (_showTree)
		tree->Root()->WriteTo(std::cout);
	if (_showProgram)
		compilation->EmitTree(std::cout);

	auto result = compilation->Evaluate(_variables);
	auto diagnostics = result.Diagnostics();
	if (diagnostics->empty())
	{
		SetConsoleColor(ConsoleColor::Magenta);
		auto value = result.Value();
		std::cout << value << "\n";
		ResetConsoleColor();

		_previous = std::move(compilation);
	} else
	{
		for (const auto& diag : *diagnostics)
		{
			auto lineIndex = tree->Text().GetLineIndex(diag.Span().Start());
			auto lineNumber = lineIndex + 1;
			auto line = tree->Text().Lines()[lineIndex];
			auto character = diag.Span().Start() - line.Start() + 1;
			std::cout << "\n";

			SetConsoleColor(ConsoleColor::DarkRed);
			std::cout << "(" << lineNumber << ", " << character << ") ";
			std::cout << diag.Message() << "\n";
			ResetConsoleColor();

			auto prefixSpan = TextSpan::FromBounds(line.Start(), diag.Span().Start());
			auto suffixSpan = TextSpan::FromBounds(diag.Span().End(), line.End());

			auto prefix = tree->Text().ToString(prefixSpan);
			auto error = tree->Text().ToString(diag.Span());
			auto suffix = tree->Text().ToString(suffixSpan);
			std::cout << "    " << prefix;

			SetConsoleColor(ConsoleColor::DarkRed);
			std::cout << error;
			ResetConsoleColor();

			std::cout << suffix << "\n";
		}
		std::cout << "\n";
	}
	std::cout << "\n";

}

McfRepl::McfRepl()
	:_variables({})
{
}

McfRepl::~McfRepl() = default;
