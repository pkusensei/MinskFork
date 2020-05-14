#include "SyntaxToken.h"

#include <iostream>
#include <sstream>

#include "ConsoleHelper.h"
#include "Parsing.h"
#include "SourceText.h"
#include "StringHelper.h"

namespace MCF {

void SyntaxNode::PrettyPrint(std::ostream& out, const SyntaxNode* node,
	string indent, bool isLast)
{
	auto isToConsole = out.rdbuf() == std::cout.rdbuf();
	auto marker = isLast ? "+--" : "---";
	out << indent;

	if (isToConsole)
		SetConsoleColor(ConsoleColor::DarkGray);
	out << marker;

	auto token = dynamic_cast<const SyntaxToken*>(node);
	if (isToConsole)
		SetConsoleColor(token ? ConsoleColor::Blue : ConsoleColor::Cyan);
	out << nameof(node->Kind());

	if (token && token->Value().HasValue())
	{
		out << " " << token->Value();
	}

	if (isToConsole)
		ResetConsoleColor();

	out << NEW_LINE;
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

TextLocation SyntaxNode::Location()const
{
	return TextLocation(Tree().Text(), Span());
}

const SyntaxToken& SyntaxNode::GetLastToken() const
{
	auto p = dynamic_cast<const SyntaxToken*>(this);
	if (p) return *p;

	return GetChildren().back()->GetLastToken();
}

string SyntaxNode::ToString() const
{
	std::ostringstream ss;
	WriteTo(ss);
	return ss.str();
}

TextSpan SyntaxTrivia::Span()const noexcept
{
	return Text.length() > 0 ?
		TextSpan(Position, Text.length())
		: TextSpan(Position, 0);
}

bool SyntaxToken::operator==(const SyntaxToken& other) const noexcept
{
	return _kind == other._kind && _position == other._position
		&& _text == other._text && _value == other._value;
}

bool SyntaxToken::operator!=(const SyntaxToken& other) const noexcept
{
	return !(*this == other);
}

const vector<const SyntaxNode*> SyntaxToken::GetChildren() const
{
	return vector<const SyntaxNode*>();
}

SyntaxToken SyntaxToken::Clone() const
{
	return SyntaxToken(Tree(), _kind, _position, _text, _value);
}

TextSpan SyntaxToken::Span() const noexcept
{
	return TextSpan(_position, _text.length());
}

}//MCF