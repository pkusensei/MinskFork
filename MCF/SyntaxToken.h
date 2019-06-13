#pragma once

#include "Symbols.h"
#include "SyntaxKind.h"

namespace MCF {

class TextSpan;
class SyntaxToken;

class MCF_API SyntaxNode
{
private:
	static void PrettyPrint(std::ostream& out, const SyntaxNode* node,
		string indent = "", bool isLast = true);
public:
	virtual ~SyntaxNode() = default;
	virtual SyntaxKind Kind() const = 0;
	virtual TextSpan Span()const;
	virtual const vector<const SyntaxNode*> GetChildren() const = 0;

	SyntaxToken GetLastToken()const;

	void WriteTo(std::ostream& out)const { PrettyPrint(out, this); }
	string ToString() const;
};

class MCF_API SyntaxToken final :public SyntaxNode
{
private:
	SyntaxKind _kind;
	size_t _position;
	string _text;
	ValueType _value;

public:
	SyntaxToken(const SyntaxKind& kind, size_t position,
				const string& text, const ValueType& value);

	bool operator==(const SyntaxToken& other)const noexcept;
	bool operator!=(const SyntaxToken& other)const noexcept;

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return _kind; }
	TextSpan Span()const override;
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr size_t Position() const noexcept { return _position; }
	string Text() const { return _text; }
	ValueType Value() const noexcept { return _value; }
	bool IsMissing()const noexcept { return _text.empty(); }

	SyntaxToken Clone()const;
};

template<typename T, typename = std::enable_if_t<std::is_base_of_v<SyntaxNode, T>>>
class SeparatedSyntaxList final
{
private:
	vector<unique_ptr<SyntaxNode>> _nodesAndSeparators;

public:
	SeparatedSyntaxList(vector<unique_ptr<SyntaxNode>>& list)
		:_nodesAndSeparators(std::move(list))
	{
	}
	SeparatedSyntaxList(SeparatedSyntaxList&& other) = default;
	SeparatedSyntaxList& operator=(SeparatedSyntaxList&& other) = default;

	size_t size()const noexcept
	{
		return (_nodesAndSeparators.size() + 1) / 2;
	}

	const T* operator[](size_t index)const
	{
		return dynamic_cast<const T*>(_nodesAndSeparators.at(index * 2).get());
	}

	const SyntaxToken* GetSeparator(size_t index)const
	{
		if (index == size() - 1) return nullptr;
		return dynamic_cast<const SyntaxToken*>(_nodesAndSeparators.at(index * 2 + 1).get());
	}

	const vector<const SyntaxNode*> GetWithSeparators()const
	{
		//TODO replace with template code
		auto result = vector<const SyntaxNode*>();
		for (const auto& it : _nodesAndSeparators)
			result.emplace_back(it.get());
		return result;
	}

	decltype(auto) begin()const noexcept { return _nodesAndSeparators.begin(); }
	decltype(auto) end()const noexcept { return _nodesAndSeparators.end(); }
};

}//MCF
