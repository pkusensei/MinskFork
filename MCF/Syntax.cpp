#include "stdafx.h"
#include "Syntax.h"

#include <cctype>
#include <sstream>
#include <typeinfo>

#include "Diagnostic.h"
#include "SourceText.h"

namespace MCF {

SyntaxKind GetKeywordKind(const string & text) noexcept
{
	if (text == "else")
		return SyntaxKind::ElseKeyword;
	else if (text == "false")
		return SyntaxKind::FalseKeyword;
	else if (text == "for")
		return SyntaxKind::ForKeyword;
	else if (text == "if")
		return SyntaxKind::IfKeyword;
	else if (text == "let")
		return SyntaxKind::LetKeyword;
	else if (text == "to")
		return SyntaxKind::ToKeyword;
	else if (text == "true")
		return SyntaxKind::TrueKeyword;
	else if (text == "var")
		return SyntaxKind::VarKeyword;
	else if (text == "while")
		return SyntaxKind::WhileKeyword;
	else return SyntaxKind::IdentifierToken;
}

string GetText(const SyntaxKind& kind)
{
	switch (kind)
	{
		case SyntaxKind::PlusToken: return "+";
		case SyntaxKind::MinusToken: return "-";
		case SyntaxKind::StarToken: return "*";
		case SyntaxKind::SlashToken: return "/";
		case SyntaxKind::BangToken: return "!";
		case SyntaxKind::PlusPlusToken: return "++";
		case SyntaxKind::MinusMinusToken: return "--";
		case SyntaxKind::EqualsToken: return "=";
		case SyntaxKind::TildeToken: return "~";
		case SyntaxKind::HatToken: return "^";
		case SyntaxKind::AmpersandToken: return "&";
		case SyntaxKind::AmpersandAmpersandToken: return "&&";
		case SyntaxKind::PipeToken: return "|";
		case SyntaxKind::PipePipeToken: return "||";
		case SyntaxKind::EqualsEqualsToken: return "==";
		case SyntaxKind::BangEqualsToken: return "!=";
		case SyntaxKind::LessToken: return "<";
		case SyntaxKind::LessOrEqualsToken: return "<=";
		case SyntaxKind::GreaterToken: return ">";
		case SyntaxKind::GreaterOrEqualsToken: return ">=";
		case SyntaxKind::OpenParenthesisToken: return "(";
		case SyntaxKind::CloseParenthesisToken: return ")";
		case SyntaxKind::OpenBraceToken: return "{";
		case SyntaxKind::CloseBraceToken: return "}";
		case SyntaxKind::ElseKeyword: return "else";
		case SyntaxKind::FalseKeyword: return "false";
		case SyntaxKind::ForKeyword: return "for";
		case SyntaxKind::IfKeyword: return "if";
		case SyntaxKind::LetKeyword: return "let";
		case SyntaxKind::ToKeyword: return "to";
		case SyntaxKind::TrueKeyword: return "true";
		case SyntaxKind::VarKeyword: return "var";
		case SyntaxKind::WhileKeyword: return "while";
		default: return string();
	}
}

int GetUnaryOperatorPrecedence(const SyntaxKind& kind) noexcept
{
	switch (kind)
	{
		case SyntaxKind::PlusToken:
		case SyntaxKind::MinusToken:
		case SyntaxKind::BangToken:
		case SyntaxKind::PlusPlusToken:
		case SyntaxKind::MinusMinusToken:
		case SyntaxKind::TildeToken:
			return 6;
		default:
			return 0;
	}
}

int GetBinaryOperatorPrecedence(const SyntaxKind& kind) noexcept
{
	switch (kind)
	{
		case SyntaxKind::StarToken:
		case SyntaxKind::SlashToken:
			return 5;
		case SyntaxKind::PlusToken:
		case SyntaxKind::MinusToken:
			return 4;
		case SyntaxKind::EqualsEqualsToken:
		case SyntaxKind::BangEqualsToken:
		case SyntaxKind::LessToken:
		case SyntaxKind::LessOrEqualsToken:
		case SyntaxKind::GreaterToken:
		case SyntaxKind::GreaterOrEqualsToken:
			return 3;
		case SyntaxKind::AmpersandToken:
		case SyntaxKind::AmpersandAmpersandToken:
			return 2;
		case SyntaxKind::PipeToken:
		case SyntaxKind::PipePipeToken:
		case SyntaxKind::HatToken:
			return 1;
		default:
			return 0;
	}
}

vector<SyntaxKind> GetUnaryOperatorKinds()
{
	auto kinds = GetAllSyntaxKinds();
	auto result = vector<SyntaxKind>();
	for (const auto& it : kinds)
		if (GetUnaryOperatorPrecedence(it) > 0)
			result.emplace_back(it);
	return result;
}

vector<SyntaxKind> GetBinaryOperatorKinds()
{
	auto kinds = GetAllSyntaxKinds();
	auto result = vector<SyntaxKind>();
	for (const auto& it : kinds)
		if (GetBinaryOperatorPrecedence(it) > 0)
			result.emplace_back(it);
	return result;
}

void SyntaxNode::PrettyPrint(std::ostream & out, const SyntaxNode * node, string indent, bool isLast)
{
	string marker = isLast ? "+--" : "---";//"©¸©¤©¤" : "©À©¤©¤";
	out << indent << marker << GetSyntaxKindName(node->Kind());
	auto token = dynamic_cast<const SyntaxToken*>(node);
	if (token != nullptr && token->Value().HasValue())
	{
		out << " " << token->Value().GetValue<IntegerType>();
	}
	out << "\n";
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
SyntaxToken::SyntaxToken(const SyntaxKind& kind, size_t position, const string& text, const ValueType& value)
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
	return vector<const SyntaxNode*>(0);
}

TextSpan SyntaxToken::Span() const
{
	return TextSpan(_position, _text.length());
}

#pragma endregion

#pragma region Lexer
Lexer::Lexer(const SourceText& text)
	:_text(&text), _diagnostics(std::make_unique<DiagnosticBag>()),
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
	auto text = _text->ToString(_start, length);
	_kind = GetKeywordKind(text);
}
#pragma endregion

#pragma region Expression

const vector<const SyntaxNode*> ExpressionSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>(0);
}

AssignmentExpressionSyntax::AssignmentExpressionSyntax(const SyntaxToken & identifier, const SyntaxToken & equals,
													   const unique_ptr<ExpressionSyntax>& expression)
	:_identifierToken(identifier), _equalsToken(equals),
	_expression(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(expression)))
{
}

const vector<const SyntaxNode*> AssignmentExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>{
		&_identifierToken,
		&_equalsToken,
		_expression.get()
	};
	return result;
}

UnaryExpressionSyntax::UnaryExpressionSyntax(const SyntaxToken & operatorToken,
											 const unique_ptr<ExpressionSyntax>& operand)
	:_operatorToken(operatorToken),
	_operand(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(operand)))
{
}

const vector<const SyntaxNode*> UnaryExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>{
		&_operatorToken,
		_operand.get()
	};
	return result;
}

BinaryExpressionSyntax::BinaryExpressionSyntax(const unique_ptr<ExpressionSyntax>& left, const SyntaxToken & operatorToken,
											   const unique_ptr<ExpressionSyntax>& right)
	:_operatorToken(operatorToken),
	_left(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(left))),
	_right(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(right)))
{
}

const vector<const SyntaxNode*> BinaryExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>{
		_left.get(),
		&_operatorToken,
		_right.get(),
	};
	return result;
}

ParenthesizedExpressionSyntax::ParenthesizedExpressionSyntax(const SyntaxToken & open, const unique_ptr<ExpressionSyntax>& expression,
															 const SyntaxToken & close)
	:_openParenthesisToken(open), _closeParenthesisToken(close),
	_expression(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(expression)))
{
}

const vector<const SyntaxNode*> ParenthesizedExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>{
		&_openParenthesisToken,
		_expression.get(),
		&_closeParenthesisToken,
	};
	return result;
}

LiteralExpressionSyntax::LiteralExpressionSyntax(const SyntaxToken& literalToken, const ValueType& value)
	:_literalToken(literalToken), _value(value)
{
}

LiteralExpressionSyntax::LiteralExpressionSyntax(const SyntaxToken& literalToken)
	: _literalToken(literalToken), _value(literalToken.Value())
{
}

const vector<const SyntaxNode*> LiteralExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>{&_literalToken};
	return result;
}

NameExpressionSyntax::NameExpressionSyntax(const SyntaxToken & identifier)
	:_identifierToken(identifier)
{
}

const vector<const SyntaxNode*> NameExpressionSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>{&_identifierToken};
	return result;
}

PostfixExpressionSyntax::PostfixExpressionSyntax(const SyntaxToken & identifier, const SyntaxToken& op, const unique_ptr<ExpressionSyntax>& expression)
	:_identifier(identifier), _op(op),
	_expression(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(expression)))
{
}

const vector<const SyntaxNode*> PostfixExpressionSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>{
		&_identifier, &_op, _expression.get()
	};
}

#pragma endregion

#pragma region Statement

const vector<const SyntaxNode*> StatementSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>();
}

BlockStatementSyntax::BlockStatementSyntax(const SyntaxToken & open, const vector<unique_ptr<StatementSyntax>>& statements,
										   const SyntaxToken & close)
	:_openBraceToken(open), _closeBraceToken(close),
	_statements(std::move(std::remove_const_t<vector<unique_ptr<StatementSyntax>>&>(statements)))
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
													 const SyntaxToken & equals, const unique_ptr<ExpressionSyntax>& initializer)
	:_keyword(keyword), _identifier(identifier), _equalsToken(equals),
	_initializer(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(initializer)))
{
}

const vector<const SyntaxNode*> VariableDeclarationSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>{
		&_keyword,
		&_identifier,
		&_equalsToken,
		_initializer.get(),
	};
	return result;
}


ElseClauseSyntax::ElseClauseSyntax(const SyntaxToken & elseKeyword, const unique_ptr<StatementSyntax>& elseStatement)
	:_elseKeyword(elseKeyword),
	_elseStatement(std::move(std::remove_const_t<unique_ptr<StatementSyntax>&>(elseStatement)))
{
}

const vector<const SyntaxNode*> ElseClauseSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>{
		&_elseKeyword,
			_elseStatement.get()
	};
}

IfStatementSyntax::IfStatementSyntax(const SyntaxToken & ifKeyword, const unique_ptr<ExpressionSyntax>& condition,
									 const unique_ptr<StatementSyntax>& thenStatement, const unique_ptr<ElseClauseSyntax>& elseClause)
	:_ifKeyword(ifKeyword),
	_condition(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(condition))),
	_thenStatement(std::move(std::remove_const_t<unique_ptr<StatementSyntax>&>(thenStatement))),
	_elseClause(std::move(std::remove_const_t<unique_ptr<ElseClauseSyntax>&>(elseClause)))
{
}

const vector<const SyntaxNode*> IfStatementSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>{
		&_ifKeyword,
			_condition.get(),
			_thenStatement.get(),
			_elseClause.get()
	};
}

WhileStatementSyntax::WhileStatementSyntax(const SyntaxToken & whileKeyword, const unique_ptr<ExpressionSyntax>& condition,
										   const unique_ptr<StatementSyntax>& body)
	:_whileKeyword(whileKeyword),
	_condition(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(condition))),
	_body(std::move(std::remove_const_t<unique_ptr<StatementSyntax>&>(body)))
{
}

const vector<const SyntaxNode*> WhileStatementSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>{
		&_whileKeyword,
			_condition.get(),
			_body.get()
	};
}

ForStatementSyntax::ForStatementSyntax(const SyntaxToken & keyword, const SyntaxToken & identifier, const SyntaxToken & equals,
									   unique_ptr<ExpressionSyntax>& lowerBound, const SyntaxToken & toKeyword,
									   unique_ptr<ExpressionSyntax>& upperBound, const unique_ptr<StatementSyntax>& body)
	:_keyword(keyword), _identifier(identifier), _equalsToken(equals),
	_lowerBound(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(lowerBound))),
	_toKeyword(toKeyword),
	_upperBound(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(upperBound))),
	_body(std::move(std::remove_const_t<unique_ptr<StatementSyntax>&>(body)))
{
}

const vector<const SyntaxNode*> ForStatementSyntax::GetChildren() const
{
	return vector<const SyntaxNode*>{
		&_keyword,
			&_identifier,
			&_equalsToken,
			_lowerBound.get(),
			&_toKeyword,
			_upperBound.get(),
			_body.get()
	};
}

ExpressionStatementSyntax::ExpressionStatementSyntax(const unique_ptr<ExpressionSyntax>& expression)
	:_expression(std::move(std::remove_const_t<unique_ptr<ExpressionSyntax>&>(expression)))
{
}

const vector<const SyntaxNode*> ExpressionStatementSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>{_expression.get()};
	return result;
}

#pragma endregion

CompilationUnitSyntax::CompilationUnitSyntax(const unique_ptr<StatementSyntax>& statement, const SyntaxToken & endOfFile)
	:_statement(std::move(std::remove_const_t<unique_ptr<StatementSyntax>&>(statement))),
	_endOfFileToken(endOfFile)
{
}

const vector<const SyntaxNode*> CompilationUnitSyntax::GetChildren() const
{
	auto result = vector<const SyntaxNode*>{
		_statement.get(),
		&_endOfFileToken,
	};
	return result;
}

Parser::Parser(const SourceText& text)
	:_text(&text), _position(0), _diagnostics(std::make_unique<DiagnosticBag>())
{
	_tokens = vector<unique_ptr<SyntaxToken>>();
	Lexer lexer(text);
	unique_ptr<SyntaxToken> pToken;
	SyntaxKind kind;
	do
	{
		pToken = std::make_unique<SyntaxToken>(lexer.Lex());
		if (pToken->Kind() != SyntaxKind::WhitespaceToken &&
			pToken->Kind() != SyntaxKind::BadToken)
		{
			kind = pToken->Kind();
			_tokens.emplace_back(std::move(pToken));
		}
	} while (kind != SyntaxKind::EndOfFileToken);
	_diagnostics->AddRange(*lexer.Diagnostics());
}

SyntaxToken* Parser::Peek(int offset) const
{
	auto idx = _position + offset;
	if (idx >= _tokens.size())
		return _tokens.back().get();
	return _tokens[idx].get();
}

SyntaxToken* Parser::Current() const
{
	return Peek(0);
}

SyntaxToken Parser::NextToken()
{
	auto current = Current();
	_position++;
	return *current; // NOTE to clone or not to clone
}

SyntaxToken Parser::MatchToken(const SyntaxKind& kind)
{
	auto current = Current();
	if (current->Kind() == kind)
		return NextToken();
	_diagnostics->ReportUnexpectedToken(current->Span(), current->Kind(), kind);
	return SyntaxToken(kind, current->Position(), string(), NullValue);
}

unique_ptr<StatementSyntax> Parser::ParseStatement()
{
	switch (Current()->Kind())
	{
		case SyntaxKind::OpenBraceToken:
			return ParseBlockStatement();
		case SyntaxKind::LetKeyword:
		case SyntaxKind::VarKeyword:
			return ParseVariableDeclaration();
		case SyntaxKind::IfKeyword:
			return ParseIfStatement();
		case SyntaxKind::WhileKeyword:
			return ParseWhileStatement();
		case SyntaxKind::ForKeyword:
			return ParseForStatement();
		default:
			return ParseExpressionStatement();
	}
}

unique_ptr<StatementSyntax> Parser::ParseBlockStatement()
{
	auto statements = vector<unique_ptr<StatementSyntax>>();
	auto openBraceToken = MatchToken(SyntaxKind::OpenBraceToken);

	while (Current()->Kind() != SyntaxKind::EndOfFileToken &&
		   Current()->Kind() != SyntaxKind::CloseBraceToken)
	{
		auto startToken = Current();

		statements.emplace_back(ParseStatement());

		// Make sure ParseStatement() consumes a token or more
		if (Current() == startToken)
			NextToken();

		startToken = Current();
	}
	auto closeBraceToken = MatchToken(SyntaxKind::CloseBraceToken);
	return std::make_unique<BlockStatementSyntax>(openBraceToken, statements, closeBraceToken);
}

unique_ptr<StatementSyntax> Parser::ParseVariableDeclaration()
{
	auto expected = Current()->Kind() == SyntaxKind::LetKeyword ? SyntaxKind::LetKeyword
		: SyntaxKind::VarKeyword;
	auto keyword = MatchToken(expected);
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto equals = MatchToken(SyntaxKind::EqualsToken);
	auto initializer = ParseExpression();

	return std::make_unique<VariableDeclarationSyntax>(keyword, identifier, equals, initializer);
}

unique_ptr<StatementSyntax> Parser::ParseIfStatement()
{
	auto keyword = MatchToken(SyntaxKind::IfKeyword);
	auto condition = ParseExpression();
	auto statement = ParseStatement();
	auto elseClause = ParseElseClause();
	return std::make_unique<IfStatementSyntax>(keyword, condition, statement, elseClause);
}

unique_ptr<ElseClauseSyntax> Parser::ParseElseClause()
{
	if (Current()->Kind() != SyntaxKind::ElseKeyword)
		return nullptr;
	auto keyword = MatchToken(SyntaxKind::ElseKeyword);
	auto statement = ParseStatement();
	return std::make_unique<ElseClauseSyntax>(keyword, statement);
}

unique_ptr<StatementSyntax> Parser::ParseWhileStatement()
{
	auto keyword = MatchToken(SyntaxKind::WhileKeyword);
	auto condition = ParseExpression();
	auto body = ParseStatement();
	return std::make_unique<WhileStatementSyntax>(keyword, condition, body);
}

unique_ptr<StatementSyntax> Parser::ParseForStatement()
{
	auto keyword = MatchToken(SyntaxKind::ForKeyword);
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto equalsToken = MatchToken(SyntaxKind::EqualsToken);
	auto lowerBound = ParseExpression();
	auto toKeyword = MatchToken(SyntaxKind::ToKeyword);
	auto upperBound = ParseExpression();
	auto body = ParseStatement();
	return std::make_unique<ForStatementSyntax>(keyword, identifier, equalsToken, lowerBound,
												toKeyword, upperBound, body);
}

unique_ptr<StatementSyntax> Parser::ParseExpressionStatement()
{
	auto expression = ParseExpression();
	return std::make_unique<ExpressionStatementSyntax>(expression);
}

unique_ptr<ExpressionSyntax> Parser::ParseExpression()
{
	return ParseAssignmentExpression();
}

unique_ptr<ExpressionSyntax> Parser::ParseAssignmentExpression()
{
	if (Peek(0)->Kind() == SyntaxKind::IdentifierToken &&
		Peek(1)->Kind() == SyntaxKind::EqualsToken)
	{
		auto identifierToken = NextToken();
		auto operatorToken = NextToken();
		auto right = ParseAssignmentExpression();
		return std::make_unique<AssignmentExpressionSyntax>(identifierToken, operatorToken, right);
	}
	return ParseBinaryExpression();
}

unique_ptr<ExpressionSyntax> Parser::ParseBinaryExpression(int parentPrecedence)
{
	auto left = std::make_unique<ExpressionSyntax>();
	auto unaryOperatorPrecedence = GetUnaryOperatorPrecedence(Current()->Kind());
	if (unaryOperatorPrecedence != 0 &&
		unaryOperatorPrecedence >= parentPrecedence)
	{
		auto operatorToken = NextToken();
		if (operatorToken.Kind() == SyntaxKind::PlusPlusToken || operatorToken.Kind() == SyntaxKind::MinusMinusToken)
		{
			_diagnostics->ReportUnexpectedToken(operatorToken.Span(), operatorToken.Kind(), SyntaxKind::IdentifierToken);
		}
		auto operand = ParseBinaryExpression(unaryOperatorPrecedence);
		left = std::make_unique<UnaryExpressionSyntax>(operatorToken, operand);
	} else
	{
		left = ParsePrimaryExpression();
		while (Current()->Kind() == SyntaxKind::PlusPlusToken || Current()->Kind() == SyntaxKind::MinusMinusToken)
		{
			if (left->Kind() != SyntaxKind::ParenthesizedExpression&&
				left->Kind() != SyntaxKind::PostfixExpression&&
				left->Kind() != SyntaxKind::NameExpression)
			{
				_diagnostics->ReportExpressionNotSupportPostfixOperator(Current()->Span(), Current()->Text(), left->Kind());
				break;
			}
			left = ParsePostfixExpression(left);
		}
	}

	while (true)
	{
		auto precedence = GetBinaryOperatorPrecedence(Current()->Kind());
		if (precedence == 0 || precedence <= parentPrecedence)
			break;
		auto operatorToken = NextToken();
		auto right = ParseBinaryExpression(precedence);
		left = std::make_unique<BinaryExpressionSyntax>(left, operatorToken, right);
	}
	return left;
}

unique_ptr<ExpressionSyntax> Parser::ParsePostfixExpression(const unique_ptr<ExpressionSyntax>& expression)
{
	auto operatorToken = NextToken();
	auto pre = dynamic_cast<ParenthesizedExpressionSyntax*>(expression.get());
	auto pfe = dynamic_cast<PostfixExpressionSyntax*>(expression.get());
	auto ne = dynamic_cast<NameExpressionSyntax*>(expression.get());
	if (pre)
	{
		auto ae = dynamic_cast<const AssignmentExpressionSyntax*>(pre->Expression());
		return std::make_unique<PostfixExpressionSyntax>(ae->IdentifierToken(), operatorToken, expression);
	} else if (pfe)
		return std::make_unique<PostfixExpressionSyntax>(pfe->IdentifierToken(), operatorToken, expression);
	else if (ne)
		return std::make_unique<PostfixExpressionSyntax>(ne->IdentifierToken(), operatorToken, expression);
	else
		throw std::invalid_argument("Unexpected expression " + GetSyntaxKindName(expression->Kind()));
}

unique_ptr<ExpressionSyntax> Parser::ParsePrimaryExpression()
{
	switch (Current()->Kind())
	{
		case SyntaxKind::OpenParenthesisToken:
			return ParseParenthesizedExpression();
		case SyntaxKind::TrueKeyword:
		case SyntaxKind::FalseKeyword:
			return ParseBooleanLiteral();
		case SyntaxKind::NumberToken:
			return ParseNumberLiteral();
		case SyntaxKind::IdentifierToken:
		default:
			return ParseNameExpression();
	}
}

unique_ptr<ExpressionSyntax> Parser::ParseParenthesizedExpression()
{
	auto left = MatchToken(SyntaxKind::OpenParenthesisToken);
	auto expression = ParseExpression();
	auto right = MatchToken(SyntaxKind::CloseParenthesisToken);
	return std::make_unique<ParenthesizedExpressionSyntax>(left, expression, right);
}

unique_ptr<ExpressionSyntax> Parser::ParseBooleanLiteral()
{
	auto isTrue = Current()->Kind() == SyntaxKind::TrueKeyword;
	auto keywordToken = isTrue ? MatchToken(SyntaxKind::TrueKeyword) : MatchToken(SyntaxKind::FalseKeyword);
	return std::make_unique<LiteralExpressionSyntax>(keywordToken, isTrue);
}

unique_ptr<ExpressionSyntax> Parser::ParseNumberLiteral()
{
	auto numberToken = MatchToken(SyntaxKind::NumberToken);
	return std::make_unique<LiteralExpressionSyntax>(numberToken);
}

unique_ptr<ExpressionSyntax> Parser::ParseNameExpression()
{
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	return std::make_unique<NameExpressionSyntax>(identifier);
}

unique_ptr<CompilationUnitSyntax> Parser::ParseCompilationUnit()
{
	auto statement = ParseStatement();
	auto endOfFileToken = MatchToken(SyntaxKind::EndOfFileToken);
	return std::make_unique<CompilationUnitSyntax>(statement, endOfFileToken);
}

SyntaxTree::SyntaxTree(const SourceText& text)
	:_text(std::make_unique<SourceText>(text)), _diagnostics(std::make_unique<DiagnosticBag>())
{
	Parser parser(text);
	_root = parser.ParseCompilationUnit();
	_diagnostics->AddRange(*parser.Diagnostics());
}

SyntaxTree::SyntaxTree(SyntaxTree && other) = default;
SyntaxTree & SyntaxTree::operator=(SyntaxTree && other) = default;
SyntaxTree::~SyntaxTree() = default;

unique_ptr<SyntaxTree> SyntaxTree::Parse(const string & text)
{
	auto sourceText = SourceText::From(text);
	return Parse(sourceText);
}

unique_ptr<SyntaxTree> SyntaxTree::Parse(const SourceText & text)
{
	return std::make_unique<SyntaxTree>(text);
}

vector<unique_ptr<SyntaxToken>> SyntaxTree::ParseTokens(const string & text)
{
	auto source = SourceText::From(text);
	return ParseTokens(source);
}

vector<unique_ptr<SyntaxToken>> SyntaxTree::ParseTokens(const SourceText & text)
{
	Lexer lexer(text);
	auto result = vector<unique_ptr<SyntaxToken>>();
	while (true)
	{
		auto pToken = std::make_unique<SyntaxToken>(lexer.Lex());
		if (pToken->Kind() == SyntaxKind::EndOfFileToken)
			break;
		result.emplace_back(std::move(pToken));
	}
	return result;
}

}//MCF