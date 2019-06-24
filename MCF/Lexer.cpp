#include "Lexer.h"

#include <cctype>

#include "Diagnostic.h"
#include "SyntaxToken.h"

namespace MCF {

Lexer::Lexer(const SourceText& text)
	:_text(&text), _diagnostics(make_unique<DiagnosticBag>()),
	_position(0), _start(0), _kind(SyntaxKind::BadToken), _value(NullValue)
{
}

char Lexer::Peek(int offset) const
{
	size_t idx = _position + offset;
	if (idx >= _text->Length())
		return '\0';
	return (*_text)[idx];
}

SyntaxToken Lexer::Lex()
{
	_start = _position;
	_kind = SyntaxKind::BadToken;
	_value = NullValue;
	auto character = Current();

	switch (character)
	{
		case '\0':
			_kind = SyntaxKind::EndOfFileToken;
			break;
		case '+':
			if (Lookahead() == '+')
			{
				Next(2);
				_kind = SyntaxKind::PlusPlusToken;
			} else
			{
				Next();
				_kind = SyntaxKind::PlusToken;
			}
			break;
		case '-':
			if (Lookahead() == '-')
			{
				Next(2);
				_kind = SyntaxKind::MinusMinusToken;
			} else
			{
				// NOTE "---" works as "-- -" so that "3---5" is -3
				Next();
				_kind = SyntaxKind::MinusToken;
			}
			break;
		case '*':
			Next();
			_kind = SyntaxKind::StarToken;
			break;
		case '/':
			Next();
			_kind = SyntaxKind::SlashToken;
			break;
		case '%':
			Next();
			_kind = SyntaxKind::PercentToken;
			break;
		case '(':
			Next();
			_kind = SyntaxKind::OpenParenthesisToken;
			break;
		case ')':
			Next();
			_kind = SyntaxKind::CloseParenthesisToken;
			break;
		case '{':
			Next();
			_kind = SyntaxKind::OpenBraceToken;
			break;
		case '}':
			Next();
			_kind = SyntaxKind::CloseBraceToken;
			break;
		case ':':
			Next();
			_kind = SyntaxKind::ColonToken;
			break;
		case ',':
			Next();
			_kind = SyntaxKind::CommaToken;
			break;
		case '~':
			Next();
			_kind = SyntaxKind::TildeToken;
			break;
		case '^':
			Next();
			_kind = SyntaxKind::HatToken;
			break;
		case '&':
			if (Lookahead() == '&')
			{
				Next(2);
				_kind = SyntaxKind::AmpersandAmpersandToken;
			} else
			{
				Next();
				_kind = SyntaxKind::AmpersandToken;
			}
			break;
		case '|':
			if (Lookahead() == '|')
			{
				Next(2);
				_kind = SyntaxKind::PipePipeToken;
			} else
			{
				Next();
				_kind = SyntaxKind::PipeToken;
			}
			break;
		case '=':
			if (Lookahead() == '=')
			{
				Next(2);
				_kind = SyntaxKind::EqualsEqualsToken;
			} else
			{
				Next();
				_kind = SyntaxKind::EqualsToken;
			}
			break;
		case '!':
			if (Lookahead() == '=')
			{
				Next(2);
				_kind = SyntaxKind::BangEqualsToken;
			} else
			{
				Next();
				_kind = SyntaxKind::BangToken;
			}
			break;
		case '<':
			if (Lookahead() == '=')
			{
				Next(2);
				_kind = SyntaxKind::LessOrEqualsToken;
			} else
			{
				Next();
				_kind = SyntaxKind::LessToken;
			}
			break;
		case '>':
			if (Lookahead() == '=')
			{
				Next(2);
				_kind = SyntaxKind::GreaterOrEqualsToken;
			} else
			{
				Next();
				_kind = SyntaxKind::GreaterToken;
			}
			break;
		case '"':
			ReadString();
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			ReadNumberToken();
			break;
		case ' ': case '\n': case '\t': case '\r':
			ReadWhiteSpace();
			break;
		default:
			if (std::isalpha(character))
				ReadIdentifierOrKeyword();
			else if (std::isspace(character))
				ReadWhiteSpace();
			else
			{
				_diagnostics->ReportBadCharacter(_position, character);
				Next();
			}
			break;
	}
	auto length = _position - _start;
	auto text = GetText(_kind);
	if (text.empty())
		text = _text->ToString(_start, length);

	return SyntaxToken(_kind, _start, text, _value);
}

void Lexer::ReadString()
{
	Next();
	auto s = string();
	auto done = false;

	while (!done)
	{
		auto c = Current();
		switch (c)
		{
			case '\0': case '\r': case '\n':
			{
				auto span = TextSpan(_start, 1);
				_diagnostics->ReportUnterminatedString(span);
				done = true;
				break;
			}
			case '"':
			{
				if (Lookahead() == '"')
				{
					s.push_back(c);
					Next(2);
				} else
				{
					Next();
					done = true;
				}
				break;
			}
			default:
			{
				s.push_back(c);
				Next();
				break;
			}
		}
	}
	_kind = SyntaxKind::StringToken;
	_value = ValueType(s);
}

void Lexer::ReadWhiteSpace()
{
	while (std::isspace(Current()))
		Next();
	_kind = SyntaxKind::WhitespaceToken;
}

void Lexer::ReadNumberToken()
{
	while (std::isdigit(Current()))
		Next();
	auto length = _position - _start;
	auto text = _text->ToString(_start, length);

	IntegerType value;
	try
	{
		value = StringToInteger(text);
	} catch (...)
	{
		_diagnostics->ReportInvalidNumber(TextSpan(_start, length), text, GetTypeSymbol(TypeEnum::Int));
	}
	_value = ValueType(value);
	_kind = SyntaxKind::NumberToken;
}

void Lexer::ReadIdentifierOrKeyword()
{
	while (std::isalpha(Current()))
		Next();
	auto length = _position - _start;
	auto text = _text->ToString(_start, length);
	_kind = GetKeywordKind(text);
}

}//MCF