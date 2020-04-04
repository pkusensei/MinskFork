#pragma once

#include <functional>
#include <string>

#include "Compilation.h"
#include "ConsoleHelper.h"

template<typename T>
class ObservableCollection final
{
private:
	std::vector<T> _collection;
	std::function<void()> _action;
	void CollectionChanged() { if (_action) _action(); }

public:
	template<typename U = T, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
	constexpr explicit ObservableCollection(U&& init)
		:_collection()
	{
		_collection.emplace_back(std::forward<U>(init));
	}

	void SetAction(std::function<void()> action) { _action = std::move(action); }

	size_t size()const noexcept { return _collection.size(); }
	const T& operator[](size_t index)const { return _collection.at(index); }
	const std::vector<T>& Contents()const noexcept { return _collection; }

	template<typename U = T, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
	void Add(U&& content)
	{
		_collection.emplace_back(std::forward<U>(content));
		CollectionChanged();
	}

	template<typename U = T, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
	void SetAt(size_t index, U&& content)
	{
		if (index < size())
			_collection.at(index) = std::forward<U>(content);
		else _collection.emplace_back(std::forward<U>(content));

		CollectionChanged();
	}

	template<typename U = T, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
	void Insert(size_t index, U&& content)
	{
		_collection.insert(_collection.begin() + index, std::forward<U>(content));
		CollectionChanged();
	}

	void RemoveAt(size_t index)
	{
		_collection.erase(_collection.begin() + index);
		CollectionChanged();
	}

	void Clear()
	{
		_collection.clear();
		CollectionChanged();
	}
};

class Repl
{

	using Document = ObservableCollection<std::string>;

private:
	struct MetaCommand;
	class SubmissionView;

	std::vector<std::string> _submissionHistory;
	size_t _submissionHistoryIndex{ 0 };
	bool _done{ false };

	std::string EditSubmission();
	void HandleKey(const MCF::KeyInfo& key, Document& document,
		SubmissionView& view);

	void HandleEscape(Document& document, SubmissionView& view);
	void HandleEnter(Document& document, SubmissionView& view);
	void HandleControlEnter(Document& document, SubmissionView& view);
	static void InsertLine(Document& document, SubmissionView& view);

	void HandleLeftArrow(SubmissionView& view);
	void HandleRightArrow(Document& document, SubmissionView& view);
	void HandleUpArrow(SubmissionView& view);
	void HandleDownArrow(Document& document, SubmissionView& view);
	void HandleBackspace(Document& document, SubmissionView& view);
	void HandleDelete(Document& document, SubmissionView& view);
	void HandleHome(SubmissionView& view);
	void HandleEnd(Document& document, SubmissionView& view);
	void HandleTab(Document& document, SubmissionView& view);

	void HandlePageUp(Document& document, SubmissionView& view);
	void HandlePageDown(Document& document, SubmissionView& view);
	void UpdateDocumentFromHistory(Document& document, SubmissionView& view);

	void HandleTyping(Document& document, SubmissionView& view,
		const std::string& text);

protected:

	std::vector<MetaCommand> _metaCommands;

	Repl();

	void EvaluateMetaCommand(std::string_view input);
	void EvaluateHelp();

	virtual void RenderLine(std::string_view line)const;
	virtual bool IsCompleteSubmission(std::string_view text)const = 0;
	virtual void EvaluateSubmission(std::string_view text) = 0;

	void ClearHistory()noexcept { _submissionHistory.clear(); }

public:
	virtual ~Repl() = default;
	void Run();
};

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
	std::function<void(std::string_view)> _lineRenderer;
	const ObservableCollection<std::string>& _submissionDocument;
	const size_t _cursorTop;
	int _renderedLineCount{ 0 };
	size_t _currentLine{ 0 };
	size_t _currentCharacter{ 0 };

	void SubmissionDocumentChanged();
	void Render();
	void UpdateCursorPosition();

public:
	SubmissionView(std::function<void(std::string_view)> lineRenderer,
		ObservableCollection<std::string>& document);

	size_t CurrentLine()const { return _currentLine; }
	void CurrentLine(const size_t value);
	size_t CurrentCharacter()const { return _currentCharacter; }
	void CurrentCharacter(const size_t value);
};

class McfRepl final :public Repl
{
private:
	inline static bool _loadingSubmission;

	std::unique_ptr<MCF::Compilation> _previous{ nullptr };
	bool _showTree{ false };
	bool _showProgram{ false };
	MCF::VarMap _variables;

	void EvaluateCls()const;
	void EvaluateLs();
	void EvaluateReset();
	void EvaluateShowTree();
	void EvaluateShowProgram();
	void EvaluateDump(std::string_view funcName)const;
	void EvaluateLoad(std::string_view);

	void ClearSubmissions()const;
	void LoadSubmissions();
	void SaveSubmission(std::string_view text);

protected:
	// Inherited via Repl
	void RenderLine(std::string_view line)const override;
	bool IsCompleteSubmission(std::string_view text) const override;
	void EvaluateSubmission(std::string_view text) override;

public:
	McfRepl();
};
