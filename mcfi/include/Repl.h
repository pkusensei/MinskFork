#pragma once

#include <functional>
#include <string>

#include "Symbols.h"

namespace MCF {
class Compilation;
struct KeyInfo;

using VarMap = std::unordered_map<shared_ptr<VariableSymbol>, ValueType,
	SymbolHash, SymbolEqual>; //HACK
}

template<typename T>
class ObservableCollection final
{
private:
	std::vector<T> _collection;
	std::function<void()> _action;
	void CollectionChanged() { _action(); }

public:
	template<typename U = T, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
	constexpr explicit ObservableCollection(U&& init)
		:_collection()
	{
		_collection.emplace_back(std::forward<U>(init));
	}

	void SetAction(const std::function<void()>& action) { _action = action; }

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
private:
	std::vector<std::string> _submissionHistory;
	size_t _submissionHistoryIndex{ 0 };
	bool _done{ false };

	class SubmissionView;
	std::string EditSubmission();
	void HandleKey(const MCF::KeyInfo& key, ObservableCollection<std::string>* document,
		SubmissionView* view);

	void HandleEscape(ObservableCollection<std::string>* document, SubmissionView* view);
	void HandleEnter(ObservableCollection<std::string>* document, SubmissionView* view);
	void HandleControlEnter(ObservableCollection<std::string>* document, SubmissionView* view);
	static void InsertLine(ObservableCollection<std::string>* document, SubmissionView* view);

	void HandleLeftArrow(SubmissionView* view);
	void HandleRightArrow(ObservableCollection<std::string>* document, SubmissionView* view);
	void HandleUpArrow(SubmissionView* view);
	void HandleDownArrow(ObservableCollection<std::string>* document, SubmissionView* view);
	void HandleBackspace(ObservableCollection<std::string>* document, SubmissionView* view);
	void HandleDelete(ObservableCollection<std::string>* document, SubmissionView* view);
	void HandleHome(SubmissionView* view);
	void HandleEnd(ObservableCollection<std::string>* document, SubmissionView* view);
	void HandleTab(ObservableCollection<std::string>* document, SubmissionView* view);

	void HandlePageUp(ObservableCollection<std::string>* document, SubmissionView* view);
	void HandlePageDown(ObservableCollection<std::string>* document, SubmissionView* view);
	void UpdateDocumentFromHistory(ObservableCollection<std::string>* document, SubmissionView* view);

	void HandleTyping(ObservableCollection<std::string>* document, SubmissionView* view,
		const std::string& text);

protected:
	virtual void RenderLine(const std::string& line)const;
	virtual void EvaluateMetaCommand(const std::string& input);
	virtual bool IsCompleteSubmission(const std::string& text)const = 0;
	virtual void EvaluateSubmission(const std::string& text) = 0;

	void ClearHistory()noexcept { _submissionHistory.clear(); }

public:
	virtual ~Repl() = default;
	void Run();
};

class Repl::SubmissionView final
{
private:
	std::function<void(std::string)> _lineRenderer;
	const ObservableCollection<std::string>* _submissionDocument;
	const size_t _cursorTop;
	int _renderedLineCount{ 0 };
	size_t _currentLine{ 0 };
	size_t _currentCharacter{ 0 };

	void SubmissionDocumentChanged();
	void Render();
	void UpdateCursorPosition();

public:
	SubmissionView(const std::function<void(std::string)>& lineRenderer,
		ObservableCollection<std::string>& document);

	size_t CurrentLine()const { return _currentLine; }
	void CurrentLine(const size_t value);
	size_t CurrentCharacter()const { return _currentCharacter; }
	void CurrentCharacter(const size_t value);
};

class McfRepl final :public Repl
{
private:
	std::unique_ptr<MCF::Compilation> _previous{ nullptr };
	bool _showTree{ false };
	bool _showProgram{ true };
	MCF::VarMap _variables;

protected:
	// Inherited via Repl
	void RenderLine(const std::string& line)const override;
	void EvaluateMetaCommand(const std::string& input) override;
	bool IsCompleteSubmission(const std::string& text) const override;
	void EvaluateSubmission(const std::string& text) override;

public:
	McfRepl();
	~McfRepl();
};