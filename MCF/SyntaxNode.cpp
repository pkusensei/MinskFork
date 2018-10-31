#include "stdafx.h"
#include <cctype>
#include <typeinfo>
#include "SyntaxNode.h"
#include "Diagnostic.h"

namespace MCF {

#pragma region SyntaxToken
SyntaxToken::SyntaxToken(SyntaxKind kind, size_t position, const string& text, ValueType value)
	:_kind(kind), _position(position), _text(text), _value(value)
{
}

SyntaxToken::SyntaxToken(const SyntaxToken & other) = default;

SyntaxToken::SyntaxToken(SyntaxToken && other)
	:_kind(std::move(other._kind)), _position(std::move(other._position)), 
	_text(std::move(other._text)), _value(std::move(other._value))
{
}

vector<const SyntaxNode*> SyntaxToken::GetChildren() const
{
	return vector<const SyntaxNode*>(0);
}

#pragma endregion

#pragma region Lexer
Lexer::Lexer(string text)
	:_text(text), _position(0), _diagnostics(std::make_unique<DiagnosticBag>())
{
}

SyntaxKind Lexer::GetKeywordKind(const string & text)
{
	if (text == "true")
		return SyntaxKind::TrueKeyword;
	else if (text == "false")
		return SyntaxKind::FalseKeyword;
	else return SyntaxKind::IdentifierToken;
}

char Lexer::Peek(int offset) const
{
	size_t idx = _position + offset;
	if (idx >= _text.length())
		return '\0';
	return _text[idx];
}

SyntaxToken Lexer::Lex()
{
	if (_position >= _text.length())
	{
		auto end = string{"\0"};
		return SyntaxToken(SyntaxKind::EndOfFileToken, _position, end, ValueType());
	}

	auto start = _position;
	if (std::isdigit(Current()))
	{
		while (std::isdigit(Current()))
			Next();
		auto length = _position - start;
		auto text = _text.substr(start, length);
		// HACK
		unsigned int value;
		try
		{
			value = std::stoul(text);
			return SyntaxToken(SyntaxKind::NumberToken, start, text, std::any(value));
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

		return SyntaxToken(SyntaxKind::WhitespaceToken, start, text, ValueType());
	}

	if (std::isalpha(Current()))
	{
		while (std::isalpha(Current()))
			Next();
		auto length = _position - start;
		auto text = _text.substr(start, length);
		auto kind = GetKeywordKind(text);

		return SyntaxToken(kind, start, text, ValueType());
	}

	switch (Current())
	{
		case '+':
		{
			Next();
			return SyntaxToken(SyntaxKind::PlusToken, start, "+", ValueType());
		}
		case '-':
		{
			Next();
			return SyntaxToken(SyntaxKind::MinusToken, start, "-", ValueType());
		}
		case '*':
		{
			Next();
			return SyntaxToken(SyntaxKind::StarToken, start, "*", ValueType());
		}
		case '/':
		{
			Next();
			return SyntaxToken(SyntaxKind::SlashToken, start, "/", ValueType());
		}
		case '(':
		{
			Next();
			return SyntaxToken(SyntaxKind::OpenParenthesisToken, start, "(", ValueType());
		}
		case ')':
		{
			Next();
			return SyntaxToken(SyntaxKind::CloseParenthesisToken, start, ")", ValueType());
		}
		case '&':
		{
			if (Lookahead() == '&')
			{
				_position += 2;
				return SyntaxToken(SyntaxKind::AmpersandAmpersandToken, start, "&&", ValueType());
			}
			break;
		}
		case '|':
		{
			if (Lookahead() == '|')
			{
				_position += 2;
				return SyntaxToken(SyntaxKind::PipePipeToken, start, "||", ValueType());
			}
			break;
		}
		case '=':
		{
			if (Lookahead() == '=')
			{
				_position += 2;
				return SyntaxToken(SyntaxKind::EqualsEqualsToken, start, "==", ValueType());
			} else
			{
				Next();
				return SyntaxToken(SyntaxKind::EqualsToken, start, "=", ValueType());
			}
		}
		case '!':
		{
			if (Lookahead() == '=')
			{
				_position += 2;
				return SyntaxToken(SyntaxKind::BangEqualsToken, start, "!=", ValueType());
			} else
			{
				Next();
				return SyntaxToken(SyntaxKind::BangToken, start, "!", ValueType());
			}
		}
	}

	// TODO
	//_diagnostics->ReportBadCharacter(_position, Current());

	return SyntaxToken(SyntaxKind::BadToken, _position, _text.substr(_position, 1), ValueType());
}

#pragma endregion

vector<const SyntaxNode*> ExpressionSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>(0);
}

#pragma region AssignmentExpression
AssignmentExpressionSyntax::AssignmentExpressionSyntax(SyntaxToken & identifier, SyntaxToken & equals, unique_ptr<ExpressionSyntax>& expression)
	:_identifierToken(identifier), _equalsToken(equals)
{
	_expression.swap(expression);
}

AssignmentExpressionSyntax::AssignmentExpressionSyntax(AssignmentExpressionSyntax && other)
	: _identifierToken(std::move(other._identifierToken)), 
	_equalsToken(std::move(other._equalsToken))
{
	_expression.swap(other._expression);
}

vector<const SyntaxNode*> AssignmentExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_identifierToken);
	result.emplace_back(&_equalsToken);
	result.emplace_back(_expression.get());
	return result;
}
#pragma endregion

#pragma region UnaryExpression
UnaryExpressionSyntax::UnaryExpressionSyntax(SyntaxToken & operatorToken, unique_ptr<ExpressionSyntax>& operand)
	:_operatorToken(std::move(operatorToken))
{
	_operand.swap(operand);
}

UnaryExpressionSyntax::UnaryExpressionSyntax(UnaryExpressionSyntax && other)
	: _operatorToken(other._operatorToken)
{
	_operand.swap(other._operand);
}

vector<const SyntaxNode*> UnaryExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_operatorToken);
	result.emplace_back(_operand.get());
	return result;
}
#pragma endregion

#pragma region BinaryExpression
BinaryExpressionSyntax::BinaryExpressionSyntax(unique_ptr<ExpressionSyntax>& left, SyntaxToken & operatorToken, unique_ptr<ExpressionSyntax>& right)
	:_operatorToken(operatorToken)
{
	_left.swap(left);
	_right.swap(right);
}

BinaryExpressionSyntax::BinaryExpressionSyntax(BinaryExpressionSyntax && other)
	:_operatorToken(std::move(other._operatorToken))
{
	_left.swap(other._left);
	_right.swap(other._right);
}

vector<const SyntaxNode*> BinaryExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(_left.get());
	result.emplace_back(&_operatorToken);
	result.emplace_back(_right.get());
	return result;
}
#pragma endregion

#pragma region ParenthesizedExpression
ParenthesizedExpressionSyntax::ParenthesizedExpressionSyntax(SyntaxToken & open, unique_ptr<ExpressionSyntax>& expression, SyntaxToken & close)
	:_openParenthesisToken(open), _closeParenthesisToken(close)
{
	_expression.swap(expression);
}

ParenthesizedExpressionSyntax::ParenthesizedExpressionSyntax(ParenthesizedExpressionSyntax && other)
	: _openParenthesisToken(std::move(other._openParenthesisToken)), 
	_closeParenthesisToken(std::move(other._closeParenthesisToken))
{
	_expression.swap(other._expression);
}

vector<const SyntaxNode*> ParenthesizedExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_openParenthesisToken);
	result.emplace_back(_expression.get());
	result.emplace_back(&_closeParenthesisToken);
	return result;
}
#pragma endregion

#pragma region LiteralExpression
LiteralExpressionSyntax::LiteralExpressionSyntax(SyntaxToken& literalToken, ValueType& value)
	:_literalToken(literalToken), _value(value)
{
}

LiteralExpressionSyntax::LiteralExpressionSyntax(SyntaxToken& literalToken)
	: _literalToken(literalToken), _value(literalToken.Value())
{
}

LiteralExpressionSyntax::LiteralExpressionSyntax(LiteralExpressionSyntax && other)
	: _literalToken(std::move(other._literalToken)), _value(std::move(other._value))
{
}

vector<const SyntaxNode*> LiteralExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_literalToken);
	return result;
}
#pragma endregion

#pragma region NameExpression
NameExpressionSyntax::NameExpressionSyntax(SyntaxToken & identifier)
	:_identifierToken(identifier)
{
}

NameExpressionSyntax::NameExpressionSyntax(NameExpressionSyntax && other)
	:_identifierToken(std::move(other._identifierToken))
{
}

vector<const SyntaxNode*> NameExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_identifierToken);
	return result;
}
#pragma endregion

}//MCF