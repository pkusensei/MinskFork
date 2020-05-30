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
	if (node == nullptr)
		return;

	auto isToConsole = out.rdbuf() == std::cout.rdbuf();
	auto isToken = IsToken(node->Kind());

	if (isToken)
	{
		auto token = static_cast<const SyntaxToken*>(node);

		for (const auto& tr : token->LeadingTrivia())
		{
			if (isToConsole)
				SetConsoleColor(ConsoleColor::DarkGray);
			out << indent;
			out << "---";

			if (isToConsole)
				SetConsoleColor(ConsoleColor::DarkGreen);
			out << "L: " << nameof(tr.Kind) << '\n';
		}
	}

	auto hasTrailingTrivia = isToken
		&& !static_cast<const SyntaxToken*>(node)->TrailingTrivia().empty();

	auto tkMarker = !hasTrailingTrivia && isLast ? "+--" : "---";

	if (isToConsole)
		SetConsoleColor(ConsoleColor::DarkGray);
	out << indent;
	out << tkMarker;

	if (isToConsole)
		SetConsoleColor(isToken ? ConsoleColor::Blue : ConsoleColor::Cyan);
	out << nameof(node->Kind());

	if (isToken)
	{
		auto token = static_cast<const SyntaxToken*>(node);
		if (token->Value().HasValue())
		{
			out << " " << token->Value();
		}
	}

	if (isToConsole)
		ResetConsoleColor();

	out << NEW_LINE;

	if (isToken)
	{
		auto token = static_cast<const SyntaxToken*>(node);

		for (const auto& tr : token->TrailingTrivia())
		{
			auto isLastTrailingTrivia = tr == token->TrailingTrivia().back();
			auto trMarker = isLast && isLastTrailingTrivia ? "+--" : "---";
			if (isToConsole)
				SetConsoleColor(ConsoleColor::DarkGray);
			out << indent;
			out << trMarker;

			if (isToConsole)
				SetConsoleColor(ConsoleColor::DarkGreen);
			out << "T: " << nameof(tr.Kind) << '\n';
		}
	}

	indent += isLast ? "   " : "|  ";
	auto children = node->GetChildren();
	if (!children.empty())
	{
		auto lastChild = children.back();
		for (const auto& child : children)
			PrettyPrint(out, child, indent, lastChild == child);
	}
}

const SyntaxNode* SyntaxNode::Parent()const
{
	return _tree->GetParent(*this);
}

vector<const SyntaxNode*> SyntaxNode::Ancestors()const
{
	auto result = AncestorsAndSelf();
	result.erase(result.cbegin());
	return result;
}

vector<const SyntaxNode*> SyntaxNode::AncestorsAndSelf()const
{
	auto result = vector<const SyntaxNode*>();
	auto node = this;
	while (node != nullptr)
	{
		result.push_back(node);
		node = node->Parent();
	}

	return result;
}

TextSpan SyntaxNode::Span() const
{
	auto children = GetChildren();
	auto first = children.front()->Span();
	auto last = children.back()->Span();
	return TextSpan::FromBounds(first.Start(), last.End());
}

TextSpan SyntaxNode::FullSpan()const
{
	auto children = GetChildren();
	auto first = children.front()->FullSpan();
	auto last = children.back()->FullSpan();
	return TextSpan::FromBounds(first.Start(), last.End());
}

TextLocation SyntaxNode::Location()const
{
	return TextLocation(Tree().Text(), Span());
}

const SyntaxToken& SyntaxNode::GetLastToken() const
{
	if (IsToken(Kind()))
		return *static_cast<const SyntaxToken*>(this);

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
	return SyntaxToken(Tree(), _kind, _position, _text, _value,
					   _leadingTrivia, _trailingTrivia);
}

TextSpan SyntaxToken::Span() const
{
	return TextSpan(_position, _text.length());
}

TextSpan SyntaxToken::FullSpan()const
{
	auto start = _leadingTrivia.empty() ?
		Span().Start()
		: _leadingTrivia.front().Span().Start();
	auto end = _trailingTrivia.empty() ?
		Span().End()
		: _trailingTrivia.back().Span().End();
	return TextSpan::FromBounds(start, end);
}

}//MCF