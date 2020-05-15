#include "Repl.h"

#include <fstream>
#include <iostream>

#include "Diagnostic.h"
#include "IO.h"
#include "Parsing.h"
#include "StringHelper.h"

#include "Authoring.h"

namespace fs = std::filesystem;

constexpr auto NEW_LINE = '\n';

fs::path GetSubmissionDir()
{
	/// %USERPROFILE%/AppData/Local/Temp
	auto temp = fs::temp_directory_path();
	temp.append("mcf").append("Submissions");
	return temp;
}

std::vector<fs::path> GetFilesInDir(const fs::path& dir)
{
	auto files = std::vector<fs::path>();
	for (const auto& p : fs::directory_iterator(dir))
		files.push_back(p.path());
	return files;
}

std::string ReadTextFromFile(const fs::path& path)
{
	auto text = std::stringstream();
	text << std::ifstream(path).rdbuf();
	return text.str();
}

struct Repl::MetaCommand
{
	// HACK a big hack
	using MethodType = std::variant<std::function<void()>, std::function<void(std::string_view)>>;

	std::string_view Name;
	std::string_view Description;
	size_t Arity;
	MethodType Method;

	MetaCommand(std::string_view name, std::string_view description,
		MethodType method, size_t arity = 0)
		:Name(name), Description(description), Arity(arity), Method(std::move(method))
	{
	}
};

class Repl::SubmissionView final
{
private:
	LineRenderHandle _lineRenderer;
	const Document& _submissionDocument;
	size_t _cursorTop;
	int _renderedLineCount{ 0 };
	size_t _currentLine{ 0 };
	size_t _currentCharacter{ 0 };

	void SubmissionDocumentChanged();
	void Render();
	void UpdateCursorPosition();

public:
	SubmissionView(LineRenderHandle lineRenderer, Document& document);

	size_t CurrentLine()const { return _currentLine; }
	void CurrentLine(const size_t value);
	size_t CurrentCharacter()const { return _currentCharacter; }
	void CurrentCharacter(const size_t value);
};

Repl::SubmissionView::SubmissionView(LineRenderHandle lineRenderer, Document& document)
	:_lineRenderer(std::move(lineRenderer)),
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

void Repl::SubmissionView::SubmissionDocumentChanged()
{
	Render();
}

void Repl::SubmissionView::Render()
{
	MCF::SetCursorVisibility(false);

	auto lineCount = 0;
	bool resetState = true;

	for (const auto& line : _submissionDocument)
	{
		if (_cursorTop + lineCount >= static_cast<size_t>(MCF::GetConsoleHeight()))
		{
			MCF::SetCursorPosition(0, MCF::GetConsoleHeight() - 1);
			std::cout << '\n';
			if (_cursorTop > 0)
				--_cursorTop;
		}
		MCF::SetCursorPosition(0, _cursorTop + lineCount);

		MCF::SetConsoleColor(MCF::ConsoleColor::Green);
		if (lineCount == 0)
			std::cout << "> ";
		else std::cout << "| ";
		MCF::ResetConsoleColor();

		resetState = _lineRenderer(_submissionDocument, lineCount, resetState);
		std::cout << std::string(MCF::GetConsoleWidth() - line.length() - 2, ' ');
		++lineCount;
	}
	auto numberOfBlankLines = _renderedLineCount - lineCount;
	if (numberOfBlankLines > 0)
	{
		auto blankLine = std::string(MCF::GetConsoleWidth() - 1, ' ');
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

Repl::Repl()
{
	if (!MCF::EnableVTMode())
	{
		std::cerr << "Warning: Unable to enter VT processing mode.\n";
	}

	_metaCommands.emplace_back("help", "Shows help.",
		[this] { EvaluateHelp(); });
}

void Repl::Run()
{
	while (true)
	{
		std::cout << '\n'; // HACK prevents last line displayed from being eaten
		auto text = EditSubmission();
		if (text.empty())
			continue;

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
	auto document = Document(std::string());
	auto view = SubmissionView(
		[this](const Document& lines, size_t index, bool resetState)
		{
			return this->RenderLine(lines, index, resetState);
		},
		document);

	while (!_done)
	{
		auto key = MCF::ReadKeyFromConsole();
		HandleKey(key, document, view);
	}
	view.CurrentLine(document.size() - 1);
	view.CurrentCharacter(document[view.CurrentLine()].length());
	std::cout << '\n';
	return MCF::StringJoin(document.cbegin(), document.cend(), NEW_LINE);
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
						HandleTyping(document, view, c);
					break;
			}
		}
	} else
	{
		auto c = key.Key;
		if (c >= ' ')
			HandleTyping(document, view, c);
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
	auto text = MCF::StringJoin(document.cbegin(), document.cend(), NEW_LINE);

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

void Repl::HandleTyping(Document& document, SubmissionView& view, char c)
{
	auto lineIndex = view.CurrentLine();
	auto& line = document[lineIndex];
	line.push_back(c);
	document.SetAt(lineIndex, line);
	view.CurrentCharacter(view.CurrentCharacter() + 1);
}

bool Repl::RenderLine(const Document& lines, size_t index, bool resetState)const
{
	std::cout << lines[index];
	return resetState;
}

std::vector<std::string> ParseArgs(std::string_view input)
{
	auto args = std::vector<std::string>();
	auto inQuotes = false;
	size_t position = 1;

	auto arg = std::string();
	auto commitArg = [&args, &arg]()
	{
		if (!MCF::StringIsBlank(arg))
			args.push_back(std::move(arg));
		arg.clear();
	};

	while (position < input.length())
	{
		auto c = input.at(position);
		auto l = position + 1 >= input.length() ? '\0' : input.at(position + 1);

		if (std::isspace(c))
		{
			if (!inQuotes)
				commitArg();
			else
				arg.push_back(c);
		} else if (c == '\"')
		{
			if (!inQuotes)
				inQuotes = true;
			else if (l == '\"')
			{
				arg.push_back(c);
				++position;
			} else
				inQuotes = false;
		} else
		{
			arg.push_back(c);
		}
		++position;
	}
	commitArg();

	return args;
}

void Repl::EvaluateMetaCommand(std::string_view input)
{
	auto args = ParseArgs(input);
	const auto& name = args.front();
	auto command = std::find_if(_metaCommands.cbegin(), _metaCommands.cend(),
		[&name](const auto& it) { return name == it.Name; });
	if (command == _metaCommands.cend())
	{
		MCF::SetConsoleColor(MCF::ConsoleColor::Red);
		std::cout << "Invalid command " << input << ".\n";
		MCF::ResetConsoleColor();
		return;
	}
	if (args.size() - 1 != command->Arity)
	{
		MCF::SetConsoleColor(MCF::ConsoleColor::Red);
		std::cout << "ERROR: invalid number of arguments; expecting exactly "
			<< command->Arity << ".\n";
		MCF::ResetConsoleColor();
		return;
	}
	auto& method = command->Method;
	if (method.index() == 0)
		std::get_if<0>(&method)->operator()();
	else
		std::get_if<1>(&method)->operator()(args.at(1));
}

void Repl::EvaluateHelp()
{
	auto maxNameLength = std::max_element(_metaCommands.cbegin(), _metaCommands.cend(),
		[](const auto& a, const auto& b) { return a.Name.length() < b.Name.length(); })
		->Name.length();

	std::sort(_metaCommands.begin(), _metaCommands.end(),
		[](const auto& a, const auto& b) { return a.Name < b.Name; });

	auto writer = MCF::TextWriter(std::cout);
	for (const auto& it : _metaCommands)
	{
		if (it.Arity == 0)
		{
			auto paddedName = std::string(it.Name).append(maxNameLength - it.Name.length(), ' ');
			writer.WritePunctuation("#");
			writer.WriteIdentifier(paddedName);
			writer.WriteSpace();
			writer.WriteSpace();
		} else
		{
			writer.WritePunctuation("#");
			writer.WriteIdentifier(it.Name);
			writer.WriteSpace();
			writer.WritePunctuation("<");
			writer.WriteIdentifier("symbol");
			writer.WritePunctuation(">");
		}
		writer.WriteSpace();
		writer.WritePunctuation(it.Description);
		writer.WriteLine();
	}
}

const std::unique_ptr<MCF::Compilation> McfRepl::emptyCompilation
= MCF::Compilation::CreateScript(nullptr, nullptr);

McfRepl::McfRepl()
	:Repl()
{
	_metaCommands.emplace_back("exit", "Exits the REPL.",
		[this] { EvaluateExit(); });
	_metaCommands.emplace_back("cls", "Clears the screen.",
		[this] { EvaluateCls(); });
	_metaCommands.emplace_back("ls", "Lists all symbols",
		[this] { EvaluateLs(); });
	_metaCommands.emplace_back("showTree", "Shows the parse tree.",
		[this] { EvaluateShowTree(); });
	_metaCommands.emplace_back("showProgram", "Shows the bound tree.",
		[this] { EvaluateShowProgram(); });
	_metaCommands.emplace_back("reset", "Clears all previous submissions.",
		[this] { EvaluateReset(); });
	_metaCommands.emplace_back("dump", "Shows bound tree of a given function.",
		[this](std::string_view name) { EvaluateDump(name); }, 1);
	_metaCommands.emplace_back("load", "Loads a script file",
		[this](std::string_view path) { EvaluateLoad(path); }, 1);

	LoadSubmissions();
}

McfRepl::~McfRepl() = default;

bool McfRepl::RenderLine(const Document & lines, size_t index, bool resetState)const
{
	static std::unique_ptr<MCF::SyntaxTree> tree = nullptr;

	if (resetState)
	{
		auto text = MCF::StringJoin(lines.cbegin(), lines.cend(), NEW_LINE);
		tree = MCF::SyntaxTree::Parse(text);
	}

	auto lineSpan = tree->Text().Lines()[index].Span();
	auto classSpans = Classify(*tree, lineSpan);

	for (const auto& span : classSpans)
	{
		auto spanText = tree->Text().ToString(span.Span);
		switch (span.ClassEnum)
		{
			case Classification::Keyword:
				MCF::SetConsoleColor(MCF::ConsoleColor::Blue);
				break;
			case Classification::Identifier:
				MCF::SetConsoleColor(MCF::ConsoleColor::DarkYellow);
				break;
			case Classification::Number:
				MCF::SetConsoleColor(MCF::ConsoleColor::Cyan);
				break;
			case Classification::String:
				MCF::SetConsoleColor(MCF::ConsoleColor::Magenta);
				break;
			case Classification::Comment:
				MCF::SetConsoleColor(MCF::ConsoleColor::Green);
				break;
			case Classification::Text:
			default:
				MCF::SetConsoleColor(MCF::ConsoleColor::DarkGray);
				break;
		}

		std::cout << spanText;
		MCF::ResetConsoleColor();
	}

	return false;
}

void McfRepl::EvaluateExit()const
{
	std::exit(0);
}

void McfRepl::EvaluateCls()const
{
	MCF::ClearConsole();
}

void McfRepl::EvaluateLs()
{
	auto compilation = _previous ? _previous.get() : emptyCompilation.get();

	auto symbols = compilation->GetSymbols();
	std::sort(symbols.begin(), symbols.end(),
		[](const auto& a, const auto& b)
		{
			if (a->Kind() == b->Kind())
				return a->Name() < b->Name();
			else
				return a->Kind() < b->Kind();
		});
	std::for_each(symbols.cbegin(), symbols.cend(),
		[](const auto& it)
		{
			it->WriteTo(std::cout);
			std::cout << '\n';
		});
}

void McfRepl::EvaluateReset()
{
	_previous = nullptr;
	_variables.clear();
	ClearSubmissions();
}

void McfRepl::EvaluateShowTree()
{
	_showTree = !_showTree;
	std::cout << (_showTree ? "Showing parse trees." : "Not showing parse trees.") << '\n';
}

void McfRepl::EvaluateShowProgram()
{
	_showProgram = !_showProgram;
	std::cout << (_showProgram ? "Showing bound tree." : "Not showing bound tree.") << '\n';
}

void McfRepl::EvaluateDump(std::string_view name)const
{
	auto compilation = _previous ? _previous.get() : emptyCompilation.get();
	auto symbols = compilation->GetSymbols();
	auto func =
		std::find_if(symbols.cbegin(), symbols.cend(),
			[name](const auto& it)
			{
				return it->Kind() == MCF::SymbolKind::Function
					&& it->Name() == name;
			});
	if (func == symbols.cend())
	{
		MCF::SetConsoleColor(MCF::ConsoleColor::Red);
		std::cout << "ERROR: function '" << name << "' does not exist." << '\n';
		MCF::ResetConsoleColor();
		return;
	}
	compilation->EmitTree(static_cast<const MCF::FunctionSymbol*>(*func), std::cout);
}

void McfRepl::EvaluateLoad(std::string_view path)
{
	auto p = fs::absolute(path);
	if (!fs::exists(p))
	{
		MCF::SetConsoleColor(MCF::ConsoleColor::Red);
		std::cout << "ERROR: file does not exist '" << p << "'.\n";
		MCF::ResetConsoleColor();
		return;
	}
	auto text = ReadTextFromFile(p);
	EvaluateSubmission(text);
}

void McfRepl::ClearSubmissions()const
{
	fs::remove_all(GetSubmissionDir());
}

void McfRepl::LoadSubmissions()
{
	auto dir = GetSubmissionDir();
	if (!fs::exists(dir))
		return;

	auto files = GetFilesInDir(dir);
	if (files.empty())
		return;

	MCF::SetConsoleColor(MCF::ConsoleColor::DarkGray);
	std::cout << "Loaded " << files.size() << " submission(s).\n";
	MCF::ResetConsoleColor();

	_loadingSubmission = true;
	for (const auto& file : files)
	{
		auto text = ReadTextFromFile(file);
		EvaluateSubmission(text);
	}
	_loadingSubmission = false;
}

void McfRepl::SaveSubmission(std::string_view text)
{
	if (_loadingSubmission)
		return;
	auto dir = GetSubmissionDir();
	fs::create_directories(dir);
	auto count = GetFilesInDir(dir).size();
	auto ss = std::stringstream();
	ss << "submission" << std::setw(4) << std::setfill('0') << count;
	auto name = ss.str();
	auto path = dir.append(name);
	auto file = std::ofstream(path);
	file << text;
}

bool McfRepl::IsCompleteSubmission(std::string_view text) const
{
	if (text.empty())
		return true;

	auto lastTwoLinesAreBlank = [&text]()
	{
		auto v = MCF::StringSplit(text.begin(), text.end(), NEW_LINE);
		if (v.size() > 1)
		{
			auto i = v.crbegin();
			return MCF::StringIsBlank(*i) && MCF::StringIsBlank(*++i);
		} else return false;
	};

	if (lastTwoLinesAreBlank()) return true;

	auto tree = MCF::SyntaxTree::Parse(text);
	auto last = tree->Root()->Members().empty() ?
		nullptr : tree->Root()->Members().back().get();
	if (last == nullptr || last->GetLastToken().IsMissing())
		return false;
	return true;
}

void McfRepl::EvaluateSubmission(std::string_view text)
{
	// creates a string_view referncing to the last item in _submissionHistory
	// That was the original idea BUT
	// _submissionHistory as a vector<T> moves/copies its content when resizing
	// That invalidates string_view in SyntaxTree esp. when SSO kicks in
	// At the end of the day each syntax tree keeps its own copy of input string
	auto syntaxTree = MCF::SyntaxTree::Parse(text);

	auto compilation = MCF::Compilation::CreateScript(std::move(_previous), std::move(syntaxTree));

	if (_showTree)
		compilation->SyntaxTrees().back()->Root()->WriteTo(std::cout);
	if (_showProgram)
		compilation->EmitTree(std::cout);

	auto result = compilation->Evaluate(_variables);
	auto diagnostics = result.Diagnostics();
	if (diagnostics.empty())
	{
		auto value = result.Value();
		if (value.HasValue())
		{
			MCF::SetConsoleColor(MCF::ConsoleColor::White);
			std::cout << value;
			MCF::ResetConsoleColor();
		}

		_previous = std::move(compilation);
		SaveSubmission(text);
	} else
	{
		auto writer = MCF::IndentedTextWriter(std::cout);
		writer.WriteDiagnostics(diagnostics);
	}
	std::cout << '\n';
}
