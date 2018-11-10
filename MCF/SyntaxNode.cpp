#include "stdafx.h"
#include "SyntaxNode.h"

#include <cctype>
#include <sstream>
#include <typeinfo>

#include "Diagnostic.h"
#include "SourceText.h"

namespace MCF {

void SyntaxNode::PrettyPrint(std::ostream & out, const SyntaxNode * node, std::string indent, bool isLast)
{
	string marker = isLast ? "+--" : "---";//"©¸©¤©¤" : "©À©¤©¤";
	out << indent << marker << GetSyntaxKindName(node->Kind());
	auto token = dynamic_cast<const SyntaxToken*>(node);
	if (token != nullptr && token->Value().HasValue())
	{
		out << " " << token->Value().GetValue<long>();
	}
	out << std::endl;
	indent += isLast ? "   " : "|  ";
	auto children = node->GetChildren();
	if (children.size() > 0)
	{
		auto lastChild = children.back();
		for (const auto& child : children)
			PrettyPrint(out, child, indent, lastChild == child);
	}
}

TextSpan SyntaxNode::Span() const
{
	auto children = GetChildren();
	auto first = (*children.begin())->Span();
	auto last = (*children.back()).Span();
	return TextSpan::FromBounds(first.Start(), last.End());
}

string SyntaxNode::ToString() const
{
	std::stringstream ss;
	WriteTo(ss);
	return ss.str();
}

#pragma region SyntaxToken
SyntaxToken::SyntaxToken(SyntaxKind kind, size_t position, const string& text, ValueType value)
	:_kind(kind), _position(position), _text(text), _value(value)
{
}

const vector<const SyntaxNode*> SyntaxToken::GetChildren() const
{
	return vector<const SyntaxNode*>(0);
}

TextSpan SyntaxToken::Span() const
{
	return TextSpan(_position, _text.length());
}

#pragma endregion

#pragma region Lexer
Lexer::Lexer(const SourceText& text)
	:_text(text), _diagnostics(std::make_unique<DiagnosticBag>()),
	_position(0), _start(0), _kind(SyntaxKind::BadToken), _value(ValueType())
{
}

char Lexer::Peek(int offset) const
{
	size_t idx = _position + offset;
	if (idx >= _text.Length())
		return '\0';
	return _text[idx];
}

SyntaxToken Lexer::Lex()
{
	_start = _position;
	_kind = SyntaxKind::BadToken;
	_value = ValueType();
	auto character = Current();

	switch (character)
	{
		case '\0':
			_kind = SyntaxKind::EndOfFileToken;
			break;
		case '+':
			Next();
			_kind = SyntaxKind::PlusToken;
			break;
		case '-':
			Next();
			_kind = SyntaxKind::MinusToken;
			break;
		case '*':
			Next();
			_kind = SyntaxKind::StarToken;
			break;
		case '/':
			Next();
			_kind = SyntaxKind::SlashToken;
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
		case '&':
			if (Lookahead() == '&')
			{
				_position += 2;
				_kind = SyntaxKind::AmpersandAmpersandToken;
			}
			break;
		case '|':
			if (Lookahead() == '|')
			{
				_position += 2;
				_kind = SyntaxKind::PipePipeToken;
			}
			break;
		case '=':
			if (Lookahead() == '=')
			{
				_position += 2;
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
				_position += 2;
				_kind = SyntaxKind::BangEqualsToken;
			} else
			{
				Next();
				_kind = SyntaxKind::BangToken;
			}
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
	if (text.length() < 1)
		text = _text.ToString(_start, length);

	return SyntaxToken(_kind, _start, text, _value);
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
	auto text = _text.ToString(_start, length);

	// HACK
	long value;
	try
	{
		value = std::stol(text);
	} catch (...)
	{
		_diagnostics->ReportInvalidNumber(TextSpan(_start, length), text, typeid(value));
	}
	_value = ValueType(value);
	_kind = SyntaxKind::NumberToken;
}

void Lexer::ReadIdentifierOrKeyword()
{
	while (std::isalpha(Current()))
		Next();
	auto length = _position - _start;
	auto text = _text.ToString(_start, length);
	_kind = GetKeywordKind(text);
}
#pragma endregion

const vector<const SyntaxNode*> ExpressionSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>(0);
}

#pragma region AssignmentExpression
AssignmentExpressionSyntax::AssignmentExpressionSyntax(const SyntaxToken & identifier,
													   const SyntaxToken & equals,
													   unique_ptr<ExpressionSyntax>& expression)
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

const vector<const SyntaxNode*> AssignmentExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_identifierToken);
	result.emplace_back(&_equalsToken);
	result.emplace_back(_expression.get());
	return result;
}
#pragma endregion

#pragma region UnaryExpression
UnaryExpressionSyntax::UnaryExpressionSyntax(const SyntaxToken & operatorToken,
											 unique_ptr<ExpressionSyntax>& operand)
	:_operatorToken(std::move(operatorToken))
{
	_operand.swap(operand);
}

UnaryExpressionSyntax::UnaryExpressionSyntax(UnaryExpressionSyntax && other)
	: _operatorToken(other._operatorToken)
{
	_operand.swap(other._operand);
}

const vector<const SyntaxNode*> UnaryExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_operatorToken);
	result.emplace_back(_operand.get());
	return result;
}
#pragma endregion

#pragma region BinaryExpression
BinaryExpressionSyntax::BinaryExpressionSyntax(unique_ptr<ExpressionSyntax>& left,
											   const SyntaxToken & operatorToken,
											   unique_ptr<ExpressionSyntax>& right)
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

const vector<const SyntaxNode*> BinaryExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(_left.get());
	result.emplace_back(&_operatorToken);
	result.emplace_back(_right.get());
	return result;
}
#pragma endregion

#pragma region ParenthesizedExpression
ParenthesizedExpressionSyntax::ParenthesizedExpressionSyntax(const SyntaxToken & open,
															 unique_ptr<ExpressionSyntax>& expression,
															 const SyntaxToken & close)
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

const vector<const SyntaxNode*> ParenthesizedExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_openParenthesisToken);
	result.emplace_back(_expression.get());
	result.emplace_back(&_closeParenthesisToken);
	return result;
}
#pragma endregion

#pragma region LiteralExpression
LiteralExpressionSyntax::LiteralExpressionSyntax(const SyntaxToken& literalToken, const ValueType& value)
	:_literalToken(literalToken), _value(value)
{
}

LiteralExpressionSyntax::LiteralExpressionSyntax(const SyntaxToken& literalToken)
	: _literalToken(literalToken), _value(literalToken.Value())
{
}

LiteralExpressionSyntax::LiteralExpressionSyntax(LiteralExpressionSyntax && other)
	: _literalToken(std::move(other._literalToken)), _value(std::move(other._value))
{
}

const vector<const SyntaxNode*> LiteralExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_literalToken);
	return result;
}
#pragma endregion

#pragma region NameExpression
NameExpressionSyntax::NameExpressionSyntax(const SyntaxToken & identifier)
	:_identifierToken(identifier)
{
}

NameExpressionSyntax::NameExpressionSyntax(NameExpressionSyntax && other)
	: _identifierToken(std::move(other._identifierToken))
{
}

const vector<const SyntaxNode*> NameExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_identifierToken);
	return result;
}
#pragma endregion

const vector<const SyntaxNode*> StatementSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>();
}

BlockStatementSyntax::BlockStatementSyntax(const SyntaxToken & open, vector<unique_ptr<StatementSyntax>>& statements, const SyntaxToken & close)
	:_openBraceToken(open), _closeBraceToken(close), _statements(std::move(statements))
{
}

BlockStatementSyntax::BlockStatementSyntax(BlockStatementSyntax && other)
	: _openBraceToken(std::move(other._openBraceToken)),
	_closeBraceToken(std::move(other._closeBraceToken)),
	_statements(std::move(other._statements))
{
}

const vector<const SyntaxNode*> BlockStatementSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_openBraceToken);
	for (const auto& it : _statements)
		result.emplace_back(it.get());
	result.emplace_back(&_closeBraceToken);
	return result;
}

const vector<const StatementSyntax*> BlockStatementSyntax::Statements() const
{
	auto result = vector<const StatementSyntax*>();
	for (const auto& it : _statements)
		result.emplace_back(it.get());
	return result;
}

VariableDeclarationSyntax::VariableDeclarationSyntax(const SyntaxToken & keyword, const SyntaxToken & identifier,
													 const SyntaxToken & equals, unique_ptr<ExpressionSyntax>& initializer)
	:_keyword(keyword), _identifier(identifier), _equalsToken(equals),
	_initializer(std::move(initializer))
{
}

VariableDeclarationSyntax::VariableDeclarationSyntax(VariableDeclarationSyntax && other)
	: _keyword(std::move(other._keyword)), _identifier(std::move(other._identifier)),
	_equalsToken(std::move(other._equalsToken)), _initializer(std::move(other._initializer))
{
}

const vector<const SyntaxNode*> VariableDeclarationSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(&_keyword);
	result.emplace_back(&_identifier);
	result.emplace_back(&_equalsToken);
	result.emplace_back(_initializer.get());
	return result;
}

ExpressionStatementSyntax::ExpressionStatementSyntax(unique_ptr<ExpressionSyntax>& expression)
	:_expression(std::move(expression))
{
}

ExpressionStatementSyntax::ExpressionStatementSyntax(ExpressionStatementSyntax && other)
	: _expression(std::move(other._expression))
{
}

const vector<const SyntaxNode*> ExpressionStatementSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>();
	result.emplace_back(_expression.get());
	return result;
}

CompilationUnitSyntax::CompilationUnitSyntax(unique_ptr<StatementSyntax>& statement, const SyntaxToken & endOfFile)
	:_statement(std::move(statement)), _endOfFileToken(endOfFile)
{
}

CompilationUnitSyntax::CompilationUnitSyntax(CompilationUnitSyntax && other)
	:_statement(std::move(other._statement)), _endOfFileToken(std::move(other._endOfFileToken))
{
}

const vector<const SyntaxNode*> CompilationUnitSyntax::GetChildren() const
{
	auto result= vector<const SyntaxNode*>();
	result.emplace_back(_statement.get());
	result.emplace_back(&_endOfFileToken);
	return result;
}

}//MCF