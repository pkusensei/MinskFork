#include "pch.h"
#include "Repl.h"

#include <iostream>

#include "helpers.h"
#include "Compilation.h"
#include "Diagnostic.h"
#include "SourceText.h"
#include "Syntax.h"

void Repl::Run()
{
	//while (true)
	//{
	//	auto text = EditSubmission();
	//	if (text.empty())
	//		return;
	//	if (text.find('\r') == text.npos && MCF::StringStartsWith(text, "#"))
	//		EvaluateMetaCommand(text);
	//	else EvaluateSubmission(text);

	//	_submissionHistory.emplace_back(text);
	//	_submissionHistoryIndex = 0;
	//}
	std::string text, input;
	while (true)
	{
		MCF::SetConsoleColor(MCF::ConsoleColor::Green);
		if (text.empty())
			std::cout << "> ";
		else std::cout << "| ";
		MCF::ResetConsoleColor();

		std::getline(std::cin, input);
		auto isBlank = MCF::IsStringBlank(input);
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

std::string Repl::EditSubmission()
{
	_done = false;
	auto document = ObservableCollection<std::string>{""};
	auto view = SubmissionView(std::bind(&Repl::RenderLine, this, std::placeholders::_1), document);

	while (!_done)
	{
		auto key = MCF::ReadKeyFromConsole();
		HandleKey(key, &document, &view);
	}
	view.CurrentLine(document.size() - 1);
	view.CurrentCharacter(document[view.CurrentLine()].length());
	std::cout << "\n";
	return MCF::StringJoin(document.Contents(), '\r');
}

void Repl::HandleKey(const char & key, ObservableCollection<std::string>* document, SubmissionView * view)
{
	auto kind = MCF::DecideKeyInputKind(key);
	if (kind != MCF::KeyInputKind::Control)
	{
		switch (kind)
		{
			case MCF::KeyInputKind::Escape:
				HandleEscape(document, view);
				break;
			case MCF::KeyInputKind::Enter:
				HandleEnter(document, view);
				break;
			default:
				HandleTyping(document, view, std::string(1, key));
				break;
		}
	}
}

void Repl::HandleEscape(ObservableCollection<std::string>* document, SubmissionView * view)
{
	document->SetAt(view->CurrentLine(), std::string());
	view->CurrentCharacter(0);
}

void Repl::HandleEnter(ObservableCollection<std::string>* document, SubmissionView * view)
{
	auto text = MCF::StringJoin(document->Contents(), '\r');
	if (MCF::StringStartsWith(text, "#") || IsCompleteSubmission(text))
	{
		_done = true;
		return;
	}
	InsertLine(document, view);
}

void Repl::InsertLine(ObservableCollection<std::string>* document, SubmissionView * view)
{
	auto remainder = (*document)[view->CurrentLine()].substr(view->CurrentCharacter());
	document->SetAt(view->CurrentLine(), (*document)[view->CurrentLine()].substr(0, view->CurrentCharacter()));

	auto lineIndex = view->CurrentLine() + 1;
	document->Insert(lineIndex, std::string());
	view->CurrentCharacter(0);
	view->CurrentLine(lineIndex);
}

void Repl::HandleTyping(ObservableCollection<std::string>* document, SubmissionView * view, const std::string& text)
{
	auto lineIndex = view->CurrentLine();
	auto start = view->CurrentCharacter();
	auto line = (*document)[lineIndex];
	document->SetAt(lineIndex, line.insert(start, text));
	view->CurrentCharacter(view->CurrentCharacter() + text.length());
}

void Repl::RenderLine(const std::string & line) const
{
	std::cout << line;
}

void Repl::EvaluateMetaCommand(const std::string & input)
{
	MCF::SetConsoleColor(MCF::ConsoleColor::Red);
	std::cout << "Invalid command " << input << '\n';
	MCF::ResetConsoleColor();
}

void Repl::SubmissionView::SubmissionDocumentChanged()
{
	Render();
}

void Repl::SubmissionView::Render()
{
	MCF::SetCursorVisibility(false);

	auto lineCount = 0;
	for (const auto& line : *_submissionDocument)
	{
		MCF::SetCursorPosition(0, _cursorTop + lineCount);

		MCF::SetConsoleColor(MCF::ConsoleColor::Green);
		if (lineCount == 0)
			std::cout << "> ";
		else std::cout << "| ";
		MCF::ResetConsoleColor();

		_lineRenderer(line);
		std::cout << std::string(MCF::GetConsoleWidth() - line.length(), ' ') << '\n';
		++lineCount;
	}
	auto numberOfBlankLines = _renderedLineCount - lineCount;
	if (numberOfBlankLines > 0)
	{
		auto blankLine = std::string(MCF::GetConsoleWidth(), ' ');
		for (size_t i = 0; i < numberOfBlankLines; ++i)
		{
			MCF::SetCursorPosition(0, _cursorTop + lineCount + i);
			std::cout << blankLine << '\n';
		}
	}
	_renderedLineCount = lineCount;
	MCF::SetCursorVisibility(true);
	UpdateCursorPosition();
}

void Repl::SubmissionView::UpdateCursorPosition()
{
	MCF::SetCursorPosition(2 + _currentCharacter, _cursorTop + _currentLine);
}

Repl::SubmissionView::SubmissionView(const std::function<void(std::string)>& lineRenderer, const ObservableCollection<std::string>& document)
	:_lineRenderer(lineRenderer), _submissionDocument(&document), _cursorTop(MCF::GetCursorTop())
{
	auto& d = std::remove_const_t<ObservableCollection<std::string>&>(document);
	d.SetAction(std::bind(&SubmissionView::SubmissionDocumentChanged, this));
}

void Repl::SubmissionView::CurrentLine(const int & value)
{
	if (_currentLine != value)
	{
		_currentLine = value;
		_currentCharacter = (*_submissionDocument)[_currentLine].length() < _currentCharacter ?
			(*_submissionDocument)[_currentLine].length() : _currentCharacter;
		UpdateCursorPosition();
	}
}

void Repl::SubmissionView::CurrentCharacter(const int & value)
{
	if (_currentCharacter != value)
	{
		_currentCharacter = value;
		UpdateCursorPosition();
	}
}

McfRepl::McfRepl()
	:_variables({})
{
}

McfRepl::~McfRepl() = default;

void McfRepl::RenderLine(const std::string & line) const
{
	auto tokens = MCF::SyntaxTree::ParseTokens(line);
	for (const auto& it : tokens)
	{
		auto isKeyword = MCF::StringEndsWith(MCF::GetSyntaxKindName(it->Kind()), "Keyword");
		auto isNumber = it->Kind() == MCF::SyntaxKind::NumberToken;

		if (isKeyword)
			MCF::SetConsoleColor(MCF::ConsoleColor::Blue);
		else if (isNumber)
			MCF::SetConsoleColor(MCF::ConsoleColor::Grey);

		std::cout << it->Text();
		MCF::ResetConsoleColor();
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
		MCF::ClearConsole();
	} else if (input == "#reset")
	{
		_previous = nullptr;
		MCF::ClearConsole();
	} else
	{
		Repl::EvaluateMetaCommand(input);
	}
}

bool McfRepl::IsCompleteSubmission(const std::string & text) const
{
	if (MCF::IsStringBlank(text))
		return true;

	auto tree = MCF::SyntaxTree::Parse(text);
	if (!tree->Diagnostics()->empty())
		return false;
	return true;
}

void McfRepl::EvaluateSubmission(const std::string & text)
{
	auto tree = MCF::SyntaxTree::Parse(text);

	auto compilation = _previous == nullptr ? std::make_unique<MCF::Compilation>(tree)
		: MCF::Compilation::ContinueWith(_previous, tree);
	if (_showTree)
		tree->Root()->WriteTo(std::cout);
	if (_showProgram)
		compilation->EmitTree(std::cout);

	auto result = compilation->Evaluate(_variables);
	auto diagnostics = result.Diagnostics();
	if (diagnostics->empty())
	{
		MCF::SetConsoleColor(MCF::ConsoleColor::Magenta);
		auto value = result.Value();
		std::cout << value << "\n";
		MCF::ResetConsoleColor();

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

			MCF::SetConsoleColor(MCF::ConsoleColor::DarkRed);
			std::cout << "(" << lineNumber << ", " << character << ") ";
			std::cout << diag.Message() << "\n";
			MCF::ResetConsoleColor();

			auto prefixSpan = MCF::TextSpan::FromBounds(line.Start(), diag.Span().Start());
			auto suffixSpan = MCF::TextSpan::FromBounds(diag.Span().End(), line.End());

			auto prefix = tree->Text().ToString(prefixSpan);
			auto error = tree->Text().ToString(diag.Span());
			auto suffix = tree->Text().ToString(suffixSpan);
			std::cout << "    " << prefix;

			MCF::SetConsoleColor(MCF::ConsoleColor::DarkRed);
			std::cout << error;
			MCF::ResetConsoleColor();

			std::cout << suffix << "\n";
		}
		std::cout << "\n";
	}
	std::cout << "\n";

}
