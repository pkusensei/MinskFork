#include "stdafx.h"
#include <cctype>
#include "SyntaxNode.h"
#include "Diagnostic.h"

namespace MCF {

SyntaxToken::SyntaxToken(SyntaxKind kind, int position, const std::string& text, ValueType value)
	:_kind(kind), _position(position), _text(text), _value(value)
{
}

SyntaxToken::~SyntaxToken() = default;

std::vector<SyntaxNode*> SyntaxToken::GetChildren() const
{
	return std::vector<SyntaxNode*>(0);
}

Lexer::Lexer(std::string text)
	:_text(text)
{
}

Lexer::~Lexer() = default;

SyntaxKind Lexer::GetKeywordKind(const std::string & text)
{
	if (text == "true")
		return SyntaxKind::TrueKeyword;
	else if (text == "false")
		return SyntaxKind::FalseKeyword;
	else return SyntaxKind::IdentifierToken;
}

char Lexer::Peek(int offset) const
{
	auto idx = _position + offset;
	if (idx >= _text.length())
		return '\0';
	return _text[idx];
}

SyntaxToken Lexer::Lex()
{
	if (_position >= _text.length())
	{
		auto end = std::string{"\0"};
		return SyntaxToken(SyntaxKind::EndOfFileToken, _position, end, NULL);
	}

	auto start = _position;
	if (std::isdigit(Current()))
	{
		while (std::isdigit(Current()))
			Next();
		auto length = _position - start;
		auto text = _text.substr(start, length);
		unsigned long value;
		try
		{
			value = std::stoul(text);
			return SyntaxToken(SyntaxKind::NumberToken, start, text, value);
		} catch (...)
		{
			_diagnostics->ReportInvalidNumber(TextSpan(start, length, start + length), text, typeid(value));
		}
	}

	if (std::isspace(Current()))
	{
		while (std::isspace(Current()))
			Next();
		auto length = _position - start;
		auto text = _text.substr(start, length);

		return SyntaxToken(SyntaxKind::WhitespaceToken, start, text, NULL);
	}

	if (std::isalpha(Current()))
	{
		while (std::isalpha(Current()))
			Next();
		auto length = _position - start;
		auto text = _text.substr(start, length);
		auto kind = GetKeywordKind(text);

		return SyntaxToken(kind, start, text, NULL);
	}

	switch (Current())
	{
		case '+':
		{
			Next();
			return SyntaxToken(SyntaxKind::PlusToken, start, "+", NULL);
		}
		case '-':
		{
			Next();
			return SyntaxToken(SyntaxKind::MinusToken, start, "-", NULL);
		}
		case '*':
		{
			Next();
			return SyntaxToken(SyntaxKind::StarToken, start, "*", NULL);
		}
		case '/':
		{
			Next();
			return SyntaxToken(SyntaxKind::SlashToken, start, "/", NULL);
		}
	}
	// TODO
}

}//MCF