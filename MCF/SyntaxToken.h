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

}//MCF
