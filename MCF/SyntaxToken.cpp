#include "stdafx.h"
#include "SyntaxToken.h"

#include <iostream>
#include <sstream>

#include "helpers.h"
#include "ConsoleHelper.h"
#include "SourceText.h"

namespace MCF {

void SyntaxNode::PrettyPrint(std::ostream & out, const SyntaxNode * node, 
							 string indent, bool isLast)
{
	auto isToConsole = out.rdbuf() == std::cout.rdbuf();
	string marker = isLast ? "+--" : "---";//"©¸©¤©¤" : "©À©¤©¤";
	out << indent;

	if (isToConsole)
		SetConsoleColor(ConsoleColor::DarkGray);
	out << marker;

	if (isToConsole)
		SetConsoleColor(dynamic_cast<const SyntaxToken*>(node) ? ConsoleColor::Blue : ConsoleColor::Cyan);
	out << GetSyntaxKindName(node->Kind());

	auto token = dynamic_cast<const SyntaxToken*>(node);
	if (token != nullptr && token->Value().HasValue())
	{
		out << " " << token->Value();
	}

	if (isToConsole)
		ResetConsoleColor();

	out << '\n';
	indent += isLast ? "   " : "|  ";
	auto children = node->GetChildren();
	if (!children.empty())
	{
		auto lastChild = children.back();
		for (const auto& child : children)
			PrettyPrint(out, child, indent, lastChild == child);
	}
}

TextSpan SyntaxNode::Span() const
{
	auto children = GetChildren();
	auto first = children.front()->Span();
	auto last = children.back()->Span();
	return TextSpan::FromBounds(first.Start(), last.End());
}

SyntaxToken SyntaxNode::GetLastToken() const
{
	auto p = dynamic_cast<const SyntaxToken*>(this);
	if (p) return p->Clone();

	return GetChildren().back()->GetLastToken();
}

string SyntaxNode::ToString() const
{
	std::stringstream ss;
	WriteTo(ss);
	return ss.str();
}

SyntaxToken::SyntaxToken(const SyntaxKind& kind, size_t position,
						 const string& text, const ValueType& value)
	:_kind(kind), _position(position), _text(text), _value(value)
{
}

bool SyntaxToken::operator==(const SyntaxToken & other) const noexcept
{
	return _kind == other._kind && _position == other._position
		&& _text == other._text && _value == other._value;
}

bool SyntaxToken::operator!=(const SyntaxToken & other) const noexcept
{
	return !(*this == other);
}

const vector<const SyntaxNode*> SyntaxToken::GetChildren() const
{
	return vector<const SyntaxNode*>();
}

SyntaxToken SyntaxToken::Clone() const
{
	return SyntaxToken(_kind, _position, _text, _value);
}

TextSpan SyntaxToken::Span() const
{
	return TextSpan(_position, _text.length());
}

}//MCF