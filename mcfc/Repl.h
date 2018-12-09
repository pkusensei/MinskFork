#pragma once

#include <functional>
#include <string>
#include <unordered_map>

#include "common.h"

template<typename T>
class ObservableCollection final
{
private:
	std::vector<T> _collection;
	std::function<void()> _action;
	void CollectionChanged() { _action(); }
public:
	void SetAction(const std::function<void()>& other) { _action = other; }
	const T& operator[](size_t index) const { return _collection[index]; }

	const T* begin()const { return &_collection[0]; }
	const T* end()const { auto size = _collection.size(); return &_collection[size - 1]; }
	size_t size()const { return _collection.size(); }
};

class Repl
{
private:
	std::vector<std::string> _submissionHistory;
	int _submissionHistoryIndex{0};
	bool _done{false};

	class SubmissionView;
	std::string EditSubmission();
protected:
	virtual void RenderLine(const std::string& line)const;
	virtual void EvaluateMetaCommand(const std::string& input);
	virtual bool IsCompleteSubmission(const std::string& text)const = 0;
	virtual void EvaluateSubmission(const std::string& text) = 0;

public:
	virtual ~Repl() = default;
	void Run();
};

class Repl::SubmissionView final
{
private:
	std::function<void(std::string)> _lineRenderer;
	const ObservableCollection<std::string>* _submissionDocument;
	const int _cursorTop;
	int _renderedLineCount{0};
	int _currentLine{0};
	int _currentCharacter{0};

	void SubmissionDocumentChanged();
	void Render();
	void UpdateCursorPosition();
public:
	SubmissionView(const std::function<void(std::string)>& lineRenderer, const ObservableCollection<std::string>& document);

	int CurrentLine()const { return _currentLine; }
	void CurrentLine(const int& value);
	int CurrentCharacter()const { return _currentCharacter; }
	void CurrentCharacter(const int& value);
};

namespace MCF {
class Compilation;
}

class McfRepl final :public Repl
{
private:
	std::unique_ptr<MCF::Compilation> _previous{nullptr};
	bool _showTree{true};
	bool _showProgram{true};
	std::unordered_map<MCF::VariableSymbol, MCF::ValueType, MCF::VariableHash> _variables;

protected:
	// Inherited via Repl
	void RenderLine(const std::string& line)const override;
	void EvaluateMetaCommand(const std::string& input) override;
	bool IsCompleteSubmission(const std::string & text) const override;
	void EvaluateSubmission(const std::string & text) override;
public:
	McfRepl();
	~McfRepl();
};