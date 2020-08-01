#pragma once

#include <stdexcept>

#include "Symbols.h"
#include "SyntaxKind.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

struct TextLocation;
struct TextSpan;
struct SyntaxToken;
class SyntaxTree;

struct MCF_API SyntaxNode
{
private:
	std::reference_wrapper<const SyntaxTree> _tree;

private:
	static void PrettyPrint(std::ostream& out, const SyntaxNode* node,
							string indent = "", bool isLast = true);

protected:
	explicit SyntaxNode(const SyntaxTree& tree)noexcept
		:_tree(std::cref(tree))
	{
	}

public:
	virtual ~SyntaxNode() = default;
	virtual SyntaxKind Kind() const noexcept = 0;
	virtual TextSpan Span()const;
	virtual const vector<const SyntaxNode*> GetChildren() const = 0;

	const SyntaxTree& Tree()const noexcept { return _tree; }
	const SyntaxNode* Parent()const;
	vector<const SyntaxNode*> Ancestors()const;
	vector<const SyntaxNode*> AncestorsAndSelf()const;

	virtual TextSpan FullSpan()const;
	TextLocation Location()const;
	const SyntaxToken& GetLastToken()const;

	void WriteTo(std::ostream& out)const { PrettyPrint(out, this); }
	string ToString() const;
};

struct MCF_API SyntaxTrivia final
{
	string_view Text;
	const SyntaxTree* Tree;
	size_t Position;
	SyntaxKind Kind;

	SyntaxTrivia(const SyntaxTree& tree, SyntaxKind kind,
				 size_t position, string_view text)noexcept
		: Text(text), Tree(&tree), Position(position), Kind(kind)
	{
	}

	TextSpan Span()const noexcept;

	bool operator==(const SyntaxTrivia& other)const noexcept
	{
		return Kind == other.Kind && Position == other.Position && Text == other.Text;
	}
	bool operator!=(const SyntaxTrivia& other)const noexcept
	{
		return !((*this) == other);
	}
};

struct MCF_API [[nodiscard]] SyntaxToken final :public SyntaxNode
{
	ValueType Value;

	vector<SyntaxTrivia> LeadingTrivia;
	vector<SyntaxTrivia> TrailingTrivia;

	string_view Text;
	size_t Position;
	SyntaxKind SynKind;

public:
	SyntaxToken(const SyntaxTree& tree, SyntaxKind kind, size_t position,
				string_view text, ValueType value,
				vector<SyntaxTrivia> leadingTrivia,
				vector<SyntaxTrivia> trailingTrivia)noexcept
		:SyntaxNode(tree),
		Value(std::move(value)),
		LeadingTrivia(std::move(leadingTrivia)),
		TrailingTrivia(std::move(trailingTrivia)),
		Text(text),	Position(position), SynKind(kind)
	{
	}

	SyntaxToken Clone()const { return *this; }

	bool operator==(const SyntaxToken& other)const noexcept
	{
		return SynKind == other.SynKind && Position == other.Position
			&& Text == other.Text && Value == other.Value;
	}
	bool operator!=(const SyntaxToken& other)const noexcept
	{
		return !(*this == other);
	}

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SynKind; }
	TextSpan Span()const override;
	const vector<const SyntaxNode*> GetChildren() const override;
	TextSpan FullSpan()const override;

	constexpr bool IsMissing()const noexcept { return Text.empty(); }

};

template<typename T, typename = std::enable_if_t<std::is_base_of_v<SyntaxNode, T>>>
class SeparatedSyntaxList final
{
private:
	vector<unique_ptr<SyntaxNode>> _nodesAndSeparators;

public:
	explicit SeparatedSyntaxList(vector<unique_ptr<SyntaxNode>> list)noexcept
		:_nodesAndSeparators(std::move(list))
	{
	}

	size_t size()const noexcept
	{
		return (_nodesAndSeparators.size() + 1) / 2;
	}

	const T* operator[](size_t index)const
	{
		return static_cast<const T*>(_nodesAndSeparators.at(index * 2).get());
	}

	const SyntaxToken* GetSeparator(size_t index)const
	{
		if (index >= size() - 1)
			throw std::out_of_range("Failed to get separator: index out of range.");
		return static_cast<const SyntaxToken*>(_nodesAndSeparators.at(index * 2 + 1).get());
	}

	const vector<const SyntaxNode*> GetWithSeparators()const
	{
		auto result = vector<const SyntaxNode*>();
		for (const auto& it : _nodesAndSeparators)
			result.emplace_back(it.get());
		return result;
	}

	class iter
	{
	public:
		using container = vector<unique_ptr<SyntaxNode>>;
		using traits = std::iterator_traits<container::const_iterator>;

		using value_type = traits::value_type;
		using difference_type = traits::difference_type;
		using reference = traits::reference;
		using pointer = traits::pointer;
		using iterator_category = traits::iterator_category;

	private:
		pointer _begin;
		difference_type _pos;

	public:
		explicit iter(pointer begin, difference_type pos = 0) noexcept
			:_begin(begin), _pos(pos)
		{
		}

		reference operator*()const noexcept { return *(_begin + _pos); }
		pointer operator->()const noexcept { return _begin + _pos; }
		bool operator==(const iter& other)const noexcept
		{
			return _pos == other._pos && _begin == other._begin;
		}
		bool operator!=(const iter& other)const noexcept
		{
			return !(*this == other);
		}
		iter& operator+=(const difference_type& n)noexcept
		{
			_pos += 2 * n;
			return *this;
		}
		iter& operator-=(const difference_type& n)noexcept
		{
			_pos -= 2 * n;
			return *this;
		}
		iter& operator++()noexcept
		{
			_pos += 2;
			return *this;
		}
		iter operator++(int)noexcept
		{
			iter tmp = *this;
			_pos += 2;
			return tmp;
		}
		iter& operator--()noexcept
		{
			_pos -= 2;
			return *this;
		}
		iter operator--(int)noexcept
		{
			iter tmp = *this;
			_pos -= 2;
			return tmp;
		}

	};

	iter begin()const noexcept
	{
		return iter(_nodesAndSeparators.data());
	}

	iter end()const noexcept
	{
		return iter(_nodesAndSeparators.data(), 2 * size());
	}
};

}//MCF

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#endif // defined(_MSC_VER) && !defined(__clang__)
