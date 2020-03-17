#include "Repl.h"

#include <iostream>

#include "helpers.h"
#include "Compilation.h"
#include "ConsoleHelper.h"
#include "Diagnostic.h"
#include "Parsing.h"
#include "SourceText.h"

constexpr auto NEW_LINE = '\r';

void Repl::Run()
{
	if (!MCF::EnableVTMode())
	{
		std::cout << "Unable to enter VT processing mode. Quitting.\n";
		return;
	}

	while (true)
	{
		auto text = EditSubmission();
		if (text.empty())
			return;

		auto& lastItem = _submissionHistory.emplace_back(std::move(text));
		if (lastItem.find(NEW_LINE) == lastItem.npos && MCF::StringStartsWith(lastItem, "#"))
			EvaluateMetaCommand(lastItem);
		else EvaluateSubmission(lastItem);

		_submissionHistoryIndex = 0;
	}
}

std::string Repl::EditSubmission()
{
	_done = false;
	auto document = ObservableCollection<std::string>(std::string());
	auto view = SubmissionView(
		[this](const std::string& line) { this->RenderLine(line); },
		document);

	while (!_done)
	{
		auto key = MCF::ReadKeyFromConsole();
		HandleKey(key, document, view);
	}
	view.CurrentLine(document.size() - 1);
	view.CurrentCharacter(document[view.CurrentLine()].length());
	std::cout << '\n';
	return MCF::StringJoin(document.Contents(), NEW_LINE);
}

void Repl::HandleKey(const MCF::KeyInfo& key,
	Document& document, SubmissionView& view)
{
	if (key.IsFunctionalKey)
	{
		if (key.Kind != MCF::KeyInputKind::Control)
		{
			switch (key.Kind)
			{
				case MCF::KeyInputKind::Escape:
					HandleEscape(document, view);
					break;
				case MCF::KeyInputKind::Enter:
					HandleEnter(document, view);
					break;
				case MCF::KeyInputKind::LeftArrow:
					HandleLeftArrow(view);
					break;
				case MCF::KeyInputKind::RightArrow:
					HandleRightArrow(document, view);
					break;
				case MCF::KeyInputKind::UpArrow:
					HandleUpArrow(view);
					break;
				case MCF::KeyInputKind::DownArrow:
					HandleDownArrow(document, view);
					break;
				case MCF::KeyInputKind::Backspace:
					HandleBackspace(document, view);
					break;
				case MCF::KeyInputKind::Delete:
					HandleDelete(document, view);
					break;
				case MCF::KeyInputKind::Home:
					HandleHome(view);
					break;
				case MCF::KeyInputKind::End:
					HandleEnd(document, view);
					break;
				case MCF::KeyInputKind::Tab:
					HandleTab(document, view);
					break;
				case MCF::KeyInputKind::PageUp:
					HandlePageUp(document, view);
					break;
				case MCF::KeyInputKind::PageDown:
					HandlePageDown(document, view);
					break;
				default:
					auto c = key.Key;
					if (c >= ' ')
						HandleTyping(document, view, std::string(1, c));
					break;
			}
		}
	} else
	{
		auto c = key.Key;
		if (c >= ' ')
			HandleTyping(document, view, std::string(1, c));
	}
}

void Repl::HandleEscape(Document& document, SubmissionView& view)
{
	document.Clear();
	document.Add(std::string());
	view.CurrentLine(0);
}

void Repl::HandleEnter(Document& document, SubmissionView& view)
{
	auto text = MCF::StringJoin(document.Contents(), NEW_LINE);
	if (MCF::StringStartsWith(text, "#") || IsCompleteSubmission(text))
	{
		_done = true;
		return;
	}
	InsertLine(document, view);
}

void Repl::HandleControlEnter(Document& document, SubmissionView& view)
{
	InsertLine(document, view);
}

void Repl::InsertLine(Document& document, SubmissionView& view)
{
	auto remainder = document[view.CurrentLine()].substr(view.CurrentCharacter());
	document.SetAt(view.CurrentLine(), document[view.CurrentLine()].substr(0, view.CurrentCharacter()));

	auto lineIndex = view.CurrentLine() + 1;
	document.Insert(lineIndex, std::string());
	view.CurrentCharacter(0);
	view.CurrentLine(lineIndex);
}

void Repl::HandleLeftArrow(SubmissionView& view)
{
	if (view.CurrentCharacter() > 0)
		view.CurrentCharacter(view.CurrentCharacter() - 1);
}

void Repl::HandleRightArrow(Document& document, SubmissionView& view)
{
	auto line = document[view.CurrentLine()];
	if (view.CurrentCharacter() <= line.length() - 1)
		view.CurrentCharacter(view.CurrentCharacter() + 1);
}

void Repl::HandleUpArrow(SubmissionView& view)
{
	if (view.CurrentLine() > 0)
		view.CurrentLine(view.CurrentLine() - 1);
}

void Repl::HandleDownArrow(Document& document, SubmissionView& view)
{
	if (view.CurrentLine() < document.size() - 1)
		view.CurrentLine(view.CurrentLine() + 1);
}

void Repl::HandleBackspace(Document& document, SubmissionView& view)
{
	auto start = view.CurrentCharacter();
	if (start == 0)
	{
		if (view.CurrentLine() == 0)
			return;
		auto currentLine = document[view.CurrentLine()];
		auto previousLine = document[view.CurrentLine() - 1];
		document.RemoveAt(view.CurrentLine());
		view.CurrentLine(view.CurrentLine() - 1);
		document.SetAt(view.CurrentLine(), previousLine + currentLine);
		view.CurrentCharacter(previousLine.length());
	} else
	{
		auto lineIndex = view.CurrentLine();
		auto line = document[lineIndex];
		auto before = line.substr(0, start - 1);
		auto after = line.substr(start);
		document.SetAt(lineIndex, before + after);
		view.CurrentCharacter(view.CurrentCharacter() - 1);
	}
}

void Repl::HandleDelete(Document& document, SubmissionView& view)
{
	auto lineIndex = view.CurrentLine();
	auto line = document[lineIndex];
	auto start = view.CurrentCharacter();
	if (start >= line.length())
	{
		if (view.CurrentLine() == document.size() - 1) return;

		auto nextLine = document[view.CurrentLine() + 1];
		document.SetAt(view.CurrentLine(),
			document[view.CurrentLine()] + nextLine);
		document.RemoveAt(view.CurrentLine() + 1);
		return;
	}
	auto before = line.substr(0, start);
	auto after = line.substr(start + 1);
	document.SetAt(lineIndex, before + after);
}

void Repl::HandleHome(SubmissionView& view)
{
	view.CurrentCharacter(0);
}

void Repl::HandleEnd(Document& document, SubmissionView& view)
{
	view.CurrentCharacter(document[view.CurrentLine()].length());
}

void Repl::HandleTab(Document& document, SubmissionView& view)
{
	constexpr auto tabWidth = 4;
	auto start = view.CurrentCharacter();
	auto remainingSpaces = tabWidth - start % tabWidth;
	auto line = document[view.CurrentLine()];
	document.SetAt(view.CurrentLine(),
		line.insert(start, std::string(remainingSpaces, ' ')));
	view.CurrentCharacter(view.CurrentCharacter() + remainingSpaces);
}

void Repl::HandlePageUp(Document& document, SubmissionView& view)
{
	if (_submissionHistoryIndex == 0)
		_submissionHistoryIndex = _submissionHistory.size() - 1;
	else
		--_submissionHistoryIndex;
	UpdateDocumentFromHistory(document, view);
}

void Repl::HandlePageDown(Document& document, SubmissionView& view)
{
	++_submissionHistoryIndex;
	if (_submissionHistoryIndex > _submissionHistory.size() - 1)
		_submissionHistoryIndex = 0;
	UpdateDocumentFromHistory(document, view);
}

void Repl::UpdateDocumentFromHistory(Document& document,
	SubmissionView& view)
{
	if (_submissionHistory.empty()) return;

	document.Clear();
	auto& historyItem = _submissionHistory[_submissionHistoryIndex];
	auto lines = MCF::StringSplit(historyItem.begin(), historyItem.end(), NEW_LINE);
	for (const auto& it : lines)
	{
		document.Add(std::string(it));
	}
	view.CurrentLine(document.size() - 1);
	view.CurrentCharacter(document[view.CurrentLine()].length());
}

void Repl::HandleTyping(Document& document, SubmissionView& view, const std::string& text)
{
	auto lineIndex = view.CurrentLine();
	auto start = view.CurrentCharacter();
	auto line = document[lineIndex];
	document.SetAt(lineIndex, line.insert(start, text));
	view.CurrentCharacter(view.CurrentCharacter() + text.length());
}

void Repl::RenderLine(const std::string& line) const
{
	std::cout << line;
}

Repl::SubmissionView::SubmissionView(const std::function<void(const std::string&)>& lineRenderer,
	ObservableCollection<std::string>& document)
	:_lineRenderer(lineRenderer),
	_submissionDocument(document), _cursorTop(MCF::GetCursorTop())
{
	document.SetAction([this]() { this->SubmissionDocumentChanged(); });
	Render();

	//_lineRenderer -- Repl::RenderLine & McfRepl::RenderLine
	//             used in SubmissionView::Render & thus SubmissionView::SubmissionDocumentChanged
	//
	//SubmissionView::SubmissionDocumentChanged -- ObservableCollection::_action
	//             the function called when _submissionDocument changed
}

void Repl::EvaluateMetaCommand(const std::string& input)
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
	for (const auto& line : _submissionDocument.Contents())
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
		for (int i = 0; i < numberOfBlankLines; ++i)
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

void Repl::SubmissionView::CurrentLine(const size_t value)
{
	if (_currentLine != value)
	{
		_currentLine = value;
		_currentCharacter = _submissionDocument[_currentLine].length() < _currentCharacter ?
			_submissionDocument[_currentLine].length() : _currentCharacter;
		UpdateCursorPosition();
	}
}

void Repl::SubmissionView::CurrentCharacter(const size_t value)
{
	if (_currentCharacter != value)
	{
		_currentCharacter = value;
		UpdateCursorPosition();
	}
}

McfRepl::McfRepl()
	:_variables()
{
}

McfRepl::~McfRepl() = default;

void McfRepl::RenderLine(const std::string & line) const
{
	auto tokens = MCF::SyntaxTree::ParseTokens(line);
	for (const auto& it : tokens)
	{
		auto isKeyword = MCF::StringEndsWith(MCF::GetSyntaxKindName(it.Kind()), "Keyword");
		auto isIdentifier = it.Kind() == MCF::SyntaxKind::IdentifierToken;
		auto isNumber = it.Kind() == MCF::SyntaxKind::NumberToken;
		auto isString = it.Kind() == MCF::SyntaxKind::StringToken;

		if (isKeyword)
			MCF::SetConsoleColor(MCF::ConsoleColor::Blue);
		else if (isIdentifier)
			MCF::SetConsoleColor(MCF::ConsoleColor::DarkYellow);
		else if (isNumber)
			MCF::SetConsoleColor(MCF::ConsoleColor::Cyan);
		else if (isString)
			MCF::SetConsoleColor(MCF::ConsoleColor::Magenta);
		else MCF::SetConsoleColor(MCF::ConsoleColor::DarkGray);

		std::cout << it.Text();
		MCF::ResetConsoleColor();
	}
}

void McfRepl::EvaluateMetaCommand(const std::string & input)
{
	if (input == "#showTree")
	{
		_showTree = !_showTree;
		std::cout << (_showTree ? "Showing parse trees." : "Not showing parse trees.") << '\n';
	} else if (input == "#showProgram")
	{
		_showProgram = !_showProgram;
		std::cout << (_showProgram ? "Showing bound tree." : "Not showing bound tree.") << '\n';
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
	if (text.empty())
		return true;

	auto lastTwoLinesAreBlank = [&text]()
	{
		auto v = MCF::StringSplit(text.begin(), text.end(), NEW_LINE);
		auto i = v.rbegin();
		if (v.size() > 1)
			return MCF::StringIsBlank(*i) && MCF::StringIsBlank(*++i);
		else return false;
	};

	if (lastTwoLinesAreBlank()) return true;

	auto tree = MCF::SyntaxTree::Parse(text);
	if (tree->Root()->Members().back()->GetLastToken().IsMissing())
		return false;
	return true;
}

void McfRepl::EvaluateSubmission(const std::string & text)
{
	/// creates a string_view referncing to the last item in _submissionHistory
	auto syntaxTree = MCF::SyntaxTree::Parse(text);

	auto compilation = _previous == nullptr ?
		std::make_unique<MCF::Compilation>(syntaxTree)
		: MCF::Compilation::ContinueWith(_previous, syntaxTree);
	if (_showTree)
		compilation->Syntax()->Root()->WriteTo(std::cout);
	if (_showProgram)
		compilation->EmitTree(std::cout);

	auto result = compilation->Evaluate(_variables);
	auto diagnostics = result.Diagnostics();
	if (diagnostics->empty())
	{
		auto value = result.Value();
		if (value.HasValue())
		{
			MCF::SetConsoleColor(MCF::ConsoleColor::White);
			std::cout << value << '\n';
			MCF::ResetConsoleColor();
		}

		_previous = std::move(compilation);
	} else
	{
		for (const auto& diag : diagnostics->SortBySpanAscending())
		{
			auto tree = compilation->Syntax();
			auto lineIndex = tree->Text().GetLineIndex(diag.Span().Start());
			auto lineNumber = lineIndex + 1;
			auto& line = tree->Text().Lines()[lineIndex];
			auto character = diag.Span().Start() - line.Start() + 1;
			std::cout << '\n';

			MCF::SetConsoleColor(MCF::ConsoleColor::DarkRed);
			std::cout << "(" << lineNumber << ", " << character << ") ";
			std::cout << diag.Message() << '\n';
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

			std::cout << suffix << '\n';
		}
		std::cout << '\n';
	}
	std::cout << '\n';
}
