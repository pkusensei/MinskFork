#pragma once

#include "Symbols.h"
#include "SyntaxKind.h"

// To suppress annoying MSVC warnings about exporting classes/functions
#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4251)
#endif // defined(_MSC_VER) && !defined(__clang__)

namespace MCF {

class TextLocation;
class TextSpan;
class SyntaxToken;
class SyntaxTree;

class MCF_API SyntaxNode
{
private:
	const SyntaxTree* _tree;

	static void PrettyPrint(std::ostream& out, const SyntaxNode* node,
		string indent = "", bool isLast = true);

protected:
	explicit SyntaxNode(const SyntaxTree& tree)noexcept
		:_tree(&tree)
	{
	}

public:
	virtual ~SyntaxNode() = default;
	virtual SyntaxKind Kind() const noexcept = 0;
	virtual TextSpan Span()const;
	virtual const vector<const SyntaxNode*> GetChildren() const = 0;

	virtual TextSpan FullSpan()const;

	constexpr const SyntaxTree& Tree()const noexcept { return *_tree; }
	TextLocation Location()const;
	const SyntaxToken& GetLastToken()const;

	void WriteTo(std::ostream& out)const { PrettyPrint(out, this); }
	string ToString() const;
};

struct MCF_API SyntaxTrivia final
{
	const SyntaxTree* Tree;
	SyntaxKind Kind;
	size_t Position;
	string_view Text;

	SyntaxTrivia(const SyntaxTree& tree, SyntaxKind kind, size_t position, string_view text)
		:Tree(&tree), Kind(kind), Position(position), Text(text)
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

class MCF_API SyntaxToken final :public SyntaxNode
{
private:
	SyntaxKind _kind;
	size_t _position;
	string_view _text;
	ValueType _value;
	vector<SyntaxTrivia> _leadingTrivia;
	vector<SyntaxTrivia> _trailingTrivia;

public:
	SyntaxToken(const SyntaxTree& tree, SyntaxKind kind, size_t position,
				string_view text, ValueType value,
				vector<SyntaxTrivia> leadingTrivia,
				vector<SyntaxTrivia> trailingTrivia)
		:SyntaxNode(tree), _kind(kind), _position(position),
		_text(text), _value(std::move(value)),
		_leadingTrivia(std::move(leadingTrivia)),
		_trailingTrivia(std::move(trailingTrivia))
	{
	}

	bool operator==(const SyntaxToken& other)const noexcept;
	bool operator!=(const SyntaxToken& other)const noexcept;

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return _kind; }
	TextSpan Span()const override;
	const vector<const SyntaxNode*> GetChildren() const override;
	TextSpan FullSpan()const override;

	constexpr size_t Position() const noexcept { return _position; }
	constexpr string_view Text() const { return _text; }
	constexpr const ValueType& Value() const noexcept { return _value; }
	constexpr const vector<SyntaxTrivia>& LeadingTrivia()const noexcept { return _leadingTrivia; }
	constexpr const vector<SyntaxTrivia>& TrailingTrivia()const noexcept { return _trailingTrivia; }
	constexpr bool IsMissing()const noexcept { return _text.empty(); }

	SyntaxToken Clone()const;
};

template<typename T, typename = std::enable_if_t<std::is_base_of_v<SyntaxNode, T>>>
class SeparatedSyntaxList final
{
private:
	vector<unique_ptr<SyntaxNode>> _nodesAndSeparators;

public:
	explicit SeparatedSyntaxList(vector<unique_ptr<SyntaxNode>>& list)noexcept
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
		if (index == size() - 1) return nullptr;
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
