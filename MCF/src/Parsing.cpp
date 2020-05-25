#include "Parsing.h"

#include <fstream>

#include "Diagnostic.h"
#include "ReflectionHelper.h"
#include "StringHelper.h"

namespace MCF {

const vector<const SyntaxNode*> UsingDirective::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(_keyword, _fileName);
}

const vector<const SyntaxNode*> ParameterSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(_identifier, _type);
}

const vector<const SyntaxNode*> FunctionDeclarationSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<SyntaxNode>(_funcKeyword, _identifier, _openParenthesisToken);
	auto nodes = _parameters.GetWithSeparators();
	result.insert(result.end(), nodes.begin(), nodes.end());
	if (_type.has_value())
	{
		auto rest = MakeVecOfRaw<SyntaxNode>(_closeParenthesisToken, *_type, _body);
		result.insert(result.end(), rest.begin(), rest.end());
	} else
	{
		auto rest = MakeVecOfRaw<SyntaxNode>(_closeParenthesisToken, _body);
		result.insert(result.end(), rest.begin(), rest.end());
	}
	return result;
}

const vector<const SyntaxNode*> GlobalStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<SyntaxNode>(_statement);
}

const vector<const SyntaxNode*> CompilationUnitSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<SyntaxNode, MemberSyntax>(
		_members.begin(), _members.end());
	auto rest = MakeVecOfRaw<SyntaxNode>(_endOfFileToken);
	result.insert(result.end(), rest.begin(), rest.end());
	return result;
}

class Lexer final
{
private:
	const SyntaxTree& _tree;
	const SourceText& _text;
	DiagnosticBag _diagnostics;

	size_t _position;
	size_t _start;
	SyntaxKind _kind;
	ValueType _value;

	vector<SyntaxTrivia> _trivias;

	char Peek(int offset) const;
	char Current() const { return Peek(0); }
	char Lookahead() const { return Peek(1); }
	constexpr void Next(size_t step = 1) noexcept { _position += step; }

	void ReadTrivia(bool leading);
	void ReadLineBreak();
	void ReadWhiteSpace();
	void ReadSingleLineComment();
	void ReadMultiLineComment();
	void ReadToken();
	void ReadString();
	void ReadNumberToken();
	void ReadIdentifierOrKeyword();

public:
	explicit Lexer(const SyntaxTree& text);

	[[nodiscard]] SyntaxToken Lex();
	constexpr const DiagnosticBag& Diagnostics()const& noexcept { return _diagnostics; }
	DiagnosticBag&& Diagnostics() && noexcept { return std::move(_diagnostics); }
};

Lexer::Lexer(const SyntaxTree& tree)
	:_tree(tree), _text(tree.Text()), _diagnostics(),
	_position(0), _start(0), _kind(SyntaxKind::BadToken), _value(NULL_VALUE)
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
	ReadTrivia(true);

	vector<SyntaxTrivia> leading = std::move(_trivias);
	auto tkStart = _position;

	ReadToken();

	auto tkKind = _kind;
	ValueType tkValue = std::move(_value);
	auto tkLength = _position - _start;

	ReadTrivia(false);

	vector<SyntaxTrivia> trailing = std::move(_trivias);

	auto tkText = GetText(tkKind);
	if (tkText.empty())
		tkText = _text.ToString(tkStart, tkLength);

	return SyntaxToken(_tree, tkKind, tkStart, tkText, std::move(tkValue),
		std::move(leading), std::move(trailing));
}

void Lexer::ReadTrivia(bool leading)
{
	_trivias.clear();
	bool done = false;

	while (!done)
	{
		_start = _position;
		_kind = SyntaxKind::BadToken;
		_value = NULL_VALUE;

		switch (Current())
		{
			case'\0':
				done = true;
				break;
			case '/':
				if (Lookahead() == '/')
				{
					ReadSingleLineComment();
				} else if (Lookahead() == '*')
				{
					ReadMultiLineComment();
				} else
				{
					done = true;
				}
				break;
			case '\n':
			case '\r':
				if (!leading)
					done = true;
				ReadLineBreak();
				break;
			case ' ':
			case '\t':
				ReadWhiteSpace();
				break;
			default:
				if (std::isspace(Current()))
					ReadWhiteSpace();
				else
					done = true;
				break;
		}
		auto length = _position - _start;
		if (length > 0)
		{
			auto text = _text.ToString(_start, length);
			_trivias.emplace_back(_tree, _kind, _start, text);
		}
	}
}

void Lexer::ReadLineBreak()
{
	if (Current() == '\r' && Lookahead() == '\n')
		Next(2);
	else Next();

	_kind = SyntaxKind::LineBreakTrivia;
}

void Lexer::ReadWhiteSpace()
{
	bool done = false;

	while (!done)
	{
		switch (Current())
		{
			case '\0':
			case '\r':
			case '\n':
				done = true;
				break;
			default:
				if (!std::isspace(Current()))
					done = true;
				else
					Next();
				break;
		}
	}
	_kind = SyntaxKind::WhitespaceTrivia;
}

void Lexer::ReadSingleLineComment()
{
	Next(2);
	auto done = false;
	while (!done)
	{
		switch (Current())
		{
			case '\0':
			case '\r':
			case '\n':
				done = true;
				break;
			default:
				Next();
				break;
		}
	}

	_kind = SyntaxKind::SingleLineCommentTrivia;
}

void Lexer::ReadMultiLineComment()
{
	Next(2);
	auto done = false;

	while (!done)
	{
		switch (Current())
		{
			case '\0':
			{
				auto span = TextSpan(_start, 2);
				auto location = TextLocation(_text, span);
				_diagnostics.ReportUnterminatedMultiLineComment(std::move(location));
				done = true;
				break;
			}
			case '*':
				if (Lookahead() == '/')
				{
					Next();
					done = true;
				}
				Next();
				break;
			default:
				Next();
				break;
		}
	}

	_kind = SyntaxKind::MultiLineCommentTrivia;
}

void Lexer::ReadToken()
{
	_start = _position;
	_kind = SyntaxKind::BadToken;
	_value = NULL_VALUE;
	auto c = Current();

	switch (c)
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
		case '_':
			ReadIdentifierOrKeyword();
			break;
		default:
			if (std::isalpha(c))
				ReadIdentifierOrKeyword();
			else
			{
				auto span = TextSpan(_position, 1);
				auto location = TextLocation(_text, std::move(span));
				_diagnostics.ReportBadCharacter(std::move(location), c);
				Next();
			}
			break;
	}
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
				auto location = TextLocation(_text, std::move(span));
				_diagnostics.ReportUnterminatedString(std::move(location));
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
	s.shrink_to_fit();
	_kind = SyntaxKind::StringToken;
	_value = ValueType(std::move(s));
}

void Lexer::ReadNumberToken()
{
	while (std::isdigit(Current()))
		Next();
	auto length = _position - _start;
	auto text = _text.ToString(_start, length);

	try
	{
		_value = ValueType(StringToInteger(text));
		_kind = SyntaxKind::NumberToken;
	} catch (...)
	{
		auto span = TextSpan(_start, length);
		_diagnostics.ReportInvalidNumber(TextLocation(_text, std::move(span)),
			text, TYPE_INT);
	}
}

void Lexer::ReadIdentifierOrKeyword()
{
	while (std::isalnum(Current()) || Current() == '_')
		Next();

	auto length = _position - _start;
	auto text = _text.ToString(_start, length);
	_kind = GetKeywordKind(text);
}

class Parser final
{
private:
	const SyntaxTree& _tree;
	const SourceText& _text;
	vector<SyntaxToken> _tokens;
	size_t _position;
	DiagnosticBag _diagnostics;

	vector<SyntaxTree> _usings;

	const SyntaxToken& Peek(int offset = 0) const;
	const SyntaxToken& Current() const;
	const SyntaxToken& NextToken();
	[[nodiscard]] SyntaxToken MatchToken(SyntaxKind kind);

	vector<unique_ptr<MemberSyntax>> ParseMembers();
	unique_ptr<MemberSyntax> ParseMember();
	unique_ptr<MemberSyntax> ParseFunctionDeclaration();
	SeparatedSyntaxList<ParameterSyntax> ParseParameterList();
	unique_ptr<ParameterSyntax> ParseParameter();
	unique_ptr<MemberSyntax> ParseGlobalStatement();
	unique_ptr<MemberSyntax> ParseUsingDirective();

	unique_ptr<StatementSyntax> ParseStatement();
	unique_ptr<BlockStatementSyntax> ParseBlockStatement();
	unique_ptr<StatementSyntax> ParseVariableDeclaration();
	std::optional<TypeClauseSyntax> ParseOptionalTypeClause();
	TypeClauseSyntax ParseTypeClause();
	unique_ptr<StatementSyntax> ParseIfStatement();
	unique_ptr<ElseClauseSyntax> ParseElseClause();
	unique_ptr<StatementSyntax> ParseWhileStatement();
	unique_ptr<StatementSyntax> ParseDoWhileStatement();
	unique_ptr<StatementSyntax> ParseForStatement();
	unique_ptr<StatementSyntax> ParseBreakStatement();
	unique_ptr<StatementSyntax> ParseContinueStatement();
	unique_ptr<StatementSyntax> ParseReturnStatement();
	unique_ptr<ExpressionStatementSyntax> ParseExpressionStatement();

	unique_ptr<ExpressionSyntax> ParseExpression();
	unique_ptr<ExpressionSyntax> ParseAssignmentExpression();
	unique_ptr<ExpressionSyntax> ParseBinaryExpression(int parentPrecedence = 0);
	unique_ptr<ExpressionSyntax> ParsePostfixExpression(unique_ptr<ExpressionSyntax> expression);

	unique_ptr<ExpressionSyntax> ParsePrimaryExpression();
	unique_ptr<ExpressionSyntax> ParseParenthesizedExpression();
	unique_ptr<ExpressionSyntax> ParseBooleanLiteral();
	unique_ptr<ExpressionSyntax> ParseNumberLiteral();
	unique_ptr<ExpressionSyntax> ParseStringLiteral();
	unique_ptr<ExpressionSyntax> ParseNameOrCallExpression();
	unique_ptr<ExpressionSyntax> ParseCallExpression();
	unique_ptr<ExpressionSyntax> ParseNameExpression();

	SeparatedSyntaxList<ExpressionSyntax> ParseArguments();

public:
	explicit Parser(const SyntaxTree& tree);

	constexpr const DiagnosticBag& Diagnostics()const& noexcept { return _diagnostics; }
	DiagnosticBag&& Diagnostics() && noexcept { return std::move(_diagnostics); }

	unique_ptr<CompilationUnitSyntax> ParseCompilationUnit();
	vector<SyntaxTree> Usings() && noexcept { return std::move(_usings); };
};

Parser::Parser(const SyntaxTree& tree)
	:_tree(tree), _text(tree.Text()), _tokens(),
	_position(0), _diagnostics()
{
	auto badTokens = vector<SyntaxToken>();

	auto lexer = Lexer(tree);
	auto kind = SyntaxKind::BadToken;
	do
	{
		auto token = lexer.Lex();
		kind = token.Kind();
		if (token.Kind() == SyntaxKind::BadToken)
			badTokens.push_back(std::move(token));
		else
		{
			if (!badTokens.empty())
			{
				auto leading = token.LeadingTrivia();
				auto index = 0;

				for (const auto& tk : badTokens)
				{
					for (const auto& lt : tk.LeadingTrivia())
					{
						leading.insert(leading.cbegin() + index, lt);
						++index;
					}

					leading.emplace(leading.cbegin() + index,
						tree, SyntaxKind::SkippedTextTrivia, tk.Position(), tk.Text());
					++index;

					for (const auto& tt : tk.TrailingTrivia())
					{
						leading.insert(leading.cbegin() + index, tt);
						++index;
					}
				}
				badTokens.clear();
				_tokens.emplace_back(token.Tree(), token.Kind(), token.Position(), token.Text(),
					token.Value(), std::move(leading), token.TrailingTrivia());
			} else
				_tokens.push_back(std::move(token));
		}
	} while (kind != SyntaxKind::EndOfFileToken);

	_diagnostics.AddRange(std::move(lexer).Diagnostics());
}

const SyntaxToken& Parser::Peek(int offset) const
{
	auto idx = _position + offset;
	if (idx >= _tokens.size())
		return _tokens.back();
	return _tokens.at(idx);
}

const SyntaxToken& Parser::Current() const
{
	return Peek(0);
}

const SyntaxToken& Parser::NextToken()
{
	auto& current = Current();
	_position++;
	return current; // NOTE to clone or not to clone
}

SyntaxToken Parser::MatchToken(SyntaxKind kind)
{
	auto& current = Current();
	if (current.Kind() == kind)
		return NextToken().Clone();
	_diagnostics.ReportUnexpectedToken(current.Location(), current.Kind(), kind);
	return SyntaxToken(_tree, kind, current.Position(), string(), NULL_VALUE,
		vector<SyntaxTrivia>(), vector<SyntaxTrivia>());
}

vector<unique_ptr<MemberSyntax>> Parser::ParseMembers()
{
	auto members = vector<unique_ptr<MemberSyntax>>();
	while (Current().Kind() != SyntaxKind::EndOfFileToken)
	{
		auto startToken = Current();
		auto member = ParseMember();
		members.push_back(std::move(member));

		if (Current() == startToken)
			NextToken();
	}
	return members;
}

unique_ptr<MemberSyntax> Parser::ParseMember()
{
	if (Current().Kind() == SyntaxKind::FunctionKeyword)
		return ParseFunctionDeclaration();
	else if (Current().Kind() == SyntaxKind::UsingKeyworld)
		return ParseUsingDirective();
	return ParseGlobalStatement();
}

unique_ptr<MemberSyntax> Parser::ParseFunctionDeclaration()
{
	auto functionKeyword = MatchToken(SyntaxKind::FunctionKeyword);
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto openParenthesisToken = MatchToken(SyntaxKind::OpenParenthesisToken);
	auto parameters = ParseParameterList();
	auto closeParenthesisToken = MatchToken(SyntaxKind::CloseParenthesisToken);
	auto type = ParseOptionalTypeClause();
	auto body = ParseBlockStatement();

	return make_unique<FunctionDeclarationSyntax>(
		_tree, functionKeyword, identifier,
		openParenthesisToken, std::move(parameters),
		closeParenthesisToken, std::move(type), std::move(body));
}

SeparatedSyntaxList<ParameterSyntax> Parser::ParseParameterList()
{
	auto nodes = vector<unique_ptr<SyntaxNode>>();
	auto parseNextParameter = true;

	while (parseNextParameter
		&& Current().Kind() != SyntaxKind::CloseParenthesisToken
		&& Current().Kind() != SyntaxKind::EndOfFileToken)
	{
		auto param = ParseParameter();
		nodes.push_back(std::move(param));

		if (Current().Kind() == SyntaxKind::CommaToken)
		{
			auto comma = MatchToken(SyntaxKind::CommaToken);
			nodes.push_back(make_unique<SyntaxToken>(comma));
		} else
		{
			parseNextParameter = false;
		}
	}

	return SeparatedSyntaxList<ParameterSyntax>(nodes);
}

unique_ptr<ParameterSyntax> Parser::ParseParameter()
{
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto type = ParseTypeClause();
	return make_unique<ParameterSyntax>(_tree, identifier, std::move(type));
}

unique_ptr<MemberSyntax> Parser::ParseGlobalStatement()
{
	return make_unique<GlobalStatementSyntax>(_tree, ParseStatement());
}

unique_ptr<MemberSyntax> Parser::ParseUsingDirective()
{
	auto keyword = MatchToken(SyntaxKind::UsingKeyworld);
	auto fileNameToken = MatchToken(SyntaxKind::StringToken);

	auto filename = fileNameToken.Value().GetValue<string>();
	auto current = _tree.Text().FilePath();
	current.replace_filename(filename);
	if (fs::exists(current))
	{
		auto tree = SyntaxTree::Load(current);
		_usings.push_back(std::move(tree));
	} else
	{
		_diagnostics.ReportSourceFileNotExist(fileNameToken.Location(), filename);
	}

	return make_unique<UsingDirective>(_tree, std::move(keyword), std::move(fileNameToken));
}

unique_ptr<StatementSyntax> Parser::ParseStatement()
{
#define PARSE_STMT(keyword) \
case SyntaxKind::keyword##Keyword: \
	return Parse##keyword##Statement();

	switch (Current().Kind())
	{
		case SyntaxKind::OpenBraceToken:
			return ParseBlockStatement();
		case SyntaxKind::LetKeyword:
		case SyntaxKind::VarKeyword:
			return ParseVariableDeclaration();
		case SyntaxKind::DoKeyword:
			return ParseDoWhileStatement();

			PARSE_STMT(If);
			PARSE_STMT(While);
			PARSE_STMT(For);
			PARSE_STMT(Break);
			PARSE_STMT(Continue);
			PARSE_STMT(Return);

		default:
			return ParseExpressionStatement();
	}
#undef PARSE_STMT
}

unique_ptr<BlockStatementSyntax> Parser::ParseBlockStatement()
{
	auto statements = vector<unique_ptr<StatementSyntax>>();
	auto openBraceToken = MatchToken(SyntaxKind::OpenBraceToken);

	while (Current().Kind() != SyntaxKind::EndOfFileToken
		&& Current().Kind() != SyntaxKind::CloseBraceToken)
	{
		auto startToken = Current();

		statements.push_back(ParseStatement());

		// Make sure ParseStatement() consumes a token or more
		if (Current() == startToken)
			NextToken();

		startToken = Current();
	}
	auto closeBraceToken = MatchToken(SyntaxKind::CloseBraceToken);
	return make_unique<BlockStatementSyntax>(
		_tree, openBraceToken,
		std::move(statements), closeBraceToken);
}

unique_ptr<StatementSyntax> Parser::ParseVariableDeclaration()
{
	auto expected =
		Current().Kind() == SyntaxKind::LetKeyword ?
		SyntaxKind::LetKeyword : SyntaxKind::VarKeyword;
	auto keyword = MatchToken(expected);
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto typeClause = ParseOptionalTypeClause();
	auto equals = MatchToken(SyntaxKind::EqualsToken);
	auto initializer = ParseExpression();

	return make_unique<VariableDeclarationSyntax>(
		_tree, keyword, identifier,
		std::move(typeClause), equals, std::move(initializer));
}

std::optional<TypeClauseSyntax> Parser::ParseOptionalTypeClause()
{
	if (Current().Kind() != SyntaxKind::ColonToken) return std::nullopt;
	return ParseTypeClause();
}

TypeClauseSyntax Parser::ParseTypeClause()
{
	auto colon = MatchToken(SyntaxKind::ColonToken);
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	return TypeClauseSyntax(_tree, colon, identifier);
}

unique_ptr<StatementSyntax> Parser::ParseIfStatement()
{
	auto keyword = MatchToken(SyntaxKind::IfKeyword);
	auto condition = ParseExpression();
	auto statement = ParseStatement();
	auto elseClause = ParseElseClause();
	return make_unique<IfStatementSyntax>(
		_tree, keyword, std::move(condition), std::move(statement),
		std::move(elseClause));
}

unique_ptr<ElseClauseSyntax> Parser::ParseElseClause()
{
	if (Current().Kind() != SyntaxKind::ElseKeyword)
		return nullptr;
	auto keyword = MatchToken(SyntaxKind::ElseKeyword);
	auto statement = ParseStatement();
	return make_unique<ElseClauseSyntax>(_tree, keyword, std::move(statement));
}

unique_ptr<StatementSyntax> Parser::ParseWhileStatement()
{
	auto keyword = MatchToken(SyntaxKind::WhileKeyword);
	auto condition = ParseExpression();
	auto body = ParseStatement();
	return make_unique<WhileStatementSyntax>(
		_tree, keyword, std::move(condition), std::move(body));
}

unique_ptr<StatementSyntax> Parser::ParseDoWhileStatement()
{
	auto doKeyword = MatchToken(SyntaxKind::DoKeyword);
	auto body = ParseStatement();
	auto whileKeyword = MatchToken(SyntaxKind::WhileKeyword);
	auto condition = ParseExpression();

	return make_unique<DoWhileStatementSyntax>(
		_tree, doKeyword, std::move(body), whileKeyword, std::move(condition));
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
	return make_unique<ForStatementSyntax>(
		_tree, keyword, identifier, equalsToken,
		std::move(lowerBound), toKeyword, std::move(upperBound), std::move(body));
}

unique_ptr<StatementSyntax> Parser::ParseBreakStatement()
{
	auto token = MatchToken(SyntaxKind::BreakKeyword);
	return make_unique<BreakStatementSyntax>(_tree, token);
}

unique_ptr<StatementSyntax> Parser::ParseContinueStatement()
{
	auto token = MatchToken(SyntaxKind::ContinueKeyword);
	return make_unique<ContinueStatementSyntax>(_tree, token);
}

unique_ptr<StatementSyntax> Parser::ParseReturnStatement()
{
	auto keyword = MatchToken(SyntaxKind::ReturnKeyword);
	auto keywordLine = _text.GetLineIndex(keyword.Span().Start());
	auto currentLine = _text.GetLineIndex(Current().Span().Start());
	auto isEof = Current().Kind() == SyntaxKind::EndOfFileToken;
	auto sameLine = !isEof && keywordLine == currentLine;
	auto expression = sameLine ? ParseExpression() : nullptr;
	return make_unique<ReturnStatementSyntax>(_tree, keyword, std::move(expression));
}

unique_ptr<ExpressionStatementSyntax> Parser::ParseExpressionStatement()
{
	auto expression = ParseExpression();
	return make_unique<ExpressionStatementSyntax>(_tree, expression);
}

unique_ptr<ExpressionSyntax> Parser::ParseExpression()
{
	return ParseAssignmentExpression();
}

unique_ptr<ExpressionSyntax> Parser::ParseAssignmentExpression()
{
	if (Peek(0).Kind() == SyntaxKind::IdentifierToken
		&& Peek(1).Kind() == SyntaxKind::EqualsToken)
	{
		auto identifierToken = NextToken();
		auto operatorToken = NextToken();
		auto right = ParseAssignmentExpression();
		return make_unique<AssignmentExpressionSyntax>(
			_tree, identifierToken, operatorToken, std::move(right));
	}
	return ParseBinaryExpression();
}

unique_ptr<ExpressionSyntax> Parser::ParseBinaryExpression(int parentPrecedence)
{
	unique_ptr<ExpressionSyntax> left = nullptr;
	auto unaryOperatorPrecedence = GetUnaryOperatorPrecedence(Current().Kind());
	if (unaryOperatorPrecedence != 0
		&& unaryOperatorPrecedence >= parentPrecedence)
	{
		auto operatorToken = NextToken();
		auto operand = ParseBinaryExpression(unaryOperatorPrecedence);
		left = make_unique<UnaryExpressionSyntax>(
			_tree, operatorToken, std::move(operand));
	} else
	{
		left = ParsePrimaryExpression();
		while (Current().Kind() == SyntaxKind::PlusPlusToken
			|| Current().Kind() == SyntaxKind::MinusMinusToken)
		{
			if (left->Kind() != SyntaxKind::ParenthesizedExpression
				&& left->Kind() != SyntaxKind::PostfixExpression
				&& left->Kind() != SyntaxKind::NameExpression)
			{
				_diagnostics.ReportExpressionNotSupportPostfixOperator(
					Current().Location(), Current().Text(), left->Kind());
				break;
			}
			left = ParsePostfixExpression(std::move(left));
		}
	}

	while (true)
	{
		auto precedence = GetBinaryOperatorPrecedence(Current().Kind());
		if (precedence == 0 || precedence <= parentPrecedence)
			break;
		auto operatorToken = NextToken();
		auto right = ParseBinaryExpression(precedence);
		left = make_unique<BinaryExpressionSyntax>(
			_tree, std::move(left), operatorToken, std::move(right));
	}
	return left;
}

unique_ptr<ExpressionSyntax> Parser::ParsePostfixExpression(unique_ptr<ExpressionSyntax> expression)
{
	auto operatorToken = NextToken();
	if (expression->Kind() == SyntaxKind::ParenthesizedExpression)
	{
		auto pre = static_cast<ParenthesizedExpressionSyntax*>(expression.get());
		auto ae = static_cast<const AssignmentExpressionSyntax*>(pre->Expression());
		return make_unique<PostfixExpressionSyntax>(_tree, ae->IdentifierToken(),
			operatorToken, std::move(expression));
	} else if (expression->Kind() == SyntaxKind::PostfixExpression)
	{
		auto pfe = static_cast<PostfixExpressionSyntax*>(expression.get());
		return make_unique<PostfixExpressionSyntax>(_tree, pfe->IdentifierToken(),
			operatorToken, std::move(expression));
	} else if (expression->Kind() == SyntaxKind::NameExpression)
	{
		auto ne = static_cast<NameExpressionSyntax*>(expression.get());
		return make_unique<PostfixExpressionSyntax>(_tree, ne->IdentifierToken(),
			operatorToken, std::move(expression));
	} else
		throw std::invalid_argument(BuildStringFrom("Unexpected expression: ",
			nameof(expression->Kind())));
}

unique_ptr<ExpressionSyntax> Parser::ParsePrimaryExpression()
{
	switch (Current().Kind())
	{
		case SyntaxKind::OpenParenthesisToken:
			return ParseParenthesizedExpression();
		case SyntaxKind::TrueKeyword:
		case SyntaxKind::FalseKeyword:
			return ParseBooleanLiteral();
		case SyntaxKind::NumberToken:
			return ParseNumberLiteral();
		case SyntaxKind::StringToken:
			return ParseStringLiteral();
		case SyntaxKind::IdentifierToken:
		default:
			return ParseNameOrCallExpression();
	}
}

unique_ptr<ExpressionSyntax> Parser::ParseParenthesizedExpression()
{
	auto left = MatchToken(SyntaxKind::OpenParenthesisToken);
	auto expression = ParseExpression();
	auto right = MatchToken(SyntaxKind::CloseParenthesisToken);
	return make_unique<ParenthesizedExpressionSyntax>(
		_tree, left, std::move(expression), right);
}

unique_ptr<ExpressionSyntax> Parser::ParseBooleanLiteral()
{
	auto isTrue = Current().Kind() == SyntaxKind::TrueKeyword;
	auto keywordToken = isTrue ?
		MatchToken(SyntaxKind::TrueKeyword) : MatchToken(SyntaxKind::FalseKeyword);
	return make_unique<LiteralExpressionSyntax>(_tree, keywordToken, isTrue);
}

unique_ptr<ExpressionSyntax> Parser::ParseNumberLiteral()
{
	auto numberToken = MatchToken(SyntaxKind::NumberToken);
	return make_unique<LiteralExpressionSyntax>(_tree, numberToken);
}

unique_ptr<ExpressionSyntax> Parser::ParseStringLiteral()
{
	auto stringToken = MatchToken(SyntaxKind::StringToken);
	return make_unique<LiteralExpressionSyntax>(_tree, stringToken);
}

unique_ptr<ExpressionSyntax> Parser::ParseNameOrCallExpression()
{
	if (Peek(0).Kind() == SyntaxKind::IdentifierToken
		&& Peek(1).Kind() == SyntaxKind::OpenParenthesisToken)
		return ParseCallExpression();
	return ParseNameExpression();
}

unique_ptr<ExpressionSyntax> Parser::ParseCallExpression()
{
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto open = MatchToken(SyntaxKind::OpenParenthesisToken);
	auto arguments = ParseArguments();
	auto close = MatchToken(SyntaxKind::CloseParenthesisToken);

	return make_unique<CallExpressionSyntax>(
		_tree, identifier, open, std::move(arguments), close);
}

SeparatedSyntaxList<ExpressionSyntax> Parser::ParseArguments()
{
	auto nodesAndSeparators = vector<unique_ptr<SyntaxNode>>();
	auto parseNextArgument = true;

	while (parseNextArgument
		&& Current().Kind() != SyntaxKind::CloseParenthesisToken
		&& Current().Kind() != SyntaxKind::EndOfFileToken)
	{
		auto expression = ParseExpression();
		nodesAndSeparators.push_back(std::move(expression));

		if (Current().Kind() == SyntaxKind::CommaToken)
		{
			auto comma = MatchToken(SyntaxKind::CommaToken);
			nodesAndSeparators.push_back(make_unique<SyntaxToken>(comma));
		} else
		{
			parseNextArgument = false;
		}
	}

	return SeparatedSyntaxList<ExpressionSyntax>(nodesAndSeparators);
}

unique_ptr<ExpressionSyntax> Parser::ParseNameExpression()
{
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	return make_unique<NameExpressionSyntax>(_tree, identifier);
}

unique_ptr<CompilationUnitSyntax> Parser::ParseCompilationUnit()
{
	auto members = ParseMembers();
	auto endOfFileToken = MatchToken(SyntaxKind::EndOfFileToken);
	return make_unique<CompilationUnitSyntax>(
		_tree, std::move(members), endOfFileToken);
}

/// These defaulted methods stay here 
/// so that compiler sees delcaration of SourceText & DiagnosticBag
/// when instantiating  
SyntaxTree::SyntaxTree(SyntaxTree&& other) = default;
SyntaxTree& SyntaxTree::operator=(SyntaxTree&& other) = default;
SyntaxTree::~SyntaxTree() = default;

auto SyntaxTree::ParseTree(const SyntaxTree& tree)->
std::tuple<unique_ptr<CompilationUnitSyntax>, vector<SyntaxTree>, unique_ptr<DiagnosticBag>>
{
	auto parser = Parser(tree);
	auto root = parser.ParseCompilationUnit();
	auto usings = std::move(parser).Usings();
	auto bag = make_unique<DiagnosticBag>(std::move(parser).Diagnostics());
	return { std::move(root), std::move(usings), std::move(bag) };
}

SyntaxTree SyntaxTree::Load(const fs::path& path)
{
	auto file = std::ifstream(path);
	auto ss = std::stringstream();
	ss << file.rdbuf();
	auto sourceText = make_unique<SourceText>(SourceText::From(ss.str(), path));
	return Parse(std::move(sourceText));
}

SyntaxTree SyntaxTree::Parse(string_view text)
{
	auto sourceText = make_unique<SourceText>(SourceText::From(string(text)));
	return Parse(std::move(sourceText));
}

SyntaxTree SyntaxTree::Parse(unique_ptr<SourceText> text)
{
	return SyntaxTree(std::move(text), ParseTree);
}

std::pair<vector<SyntaxToken>, SyntaxTree>
SyntaxTree::ParseTokens(string_view text, bool includeEndOfFile)
{
	auto source = make_unique<SourceText>(SourceText::From(string(text)));
	return ParseTokens(std::move(source), includeEndOfFile);
}

std::pair<vector<SyntaxToken>, SyntaxTree>
SyntaxTree::ParseTokens(string_view text, DiagnosticBag& diagnostics, bool includeEndOfFile)
{
	auto source = make_unique<SourceText>(SourceText::From(string(text)));
	return ParseTokens(std::move(source), diagnostics, includeEndOfFile);
}

std::pair<vector<SyntaxToken>, SyntaxTree>
SyntaxTree::ParseTokens(unique_ptr<SourceText> text, bool includeEndOfFile)
{
	auto _ = DiagnosticBag();
	return ParseTokens(std::move(text), _, includeEndOfFile);
}

std::pair<vector<SyntaxToken>, SyntaxTree>
SyntaxTree::ParseTokens(unique_ptr<SourceText> text, DiagnosticBag& diagnostics, bool includeEndOfFile)
{
	auto tokens = vector<SyntaxToken>();

	auto parseTokens = [&tokens, includeEndOfFile](const SyntaxTree& tree)
	{
		unique_ptr<CompilationUnitSyntax> root = nullptr;
		auto lexer = Lexer(tree);
		while (true)
		{
			auto token = lexer.Lex();
			if (token.Kind() == SyntaxKind::EndOfFileToken)
				root = make_unique<CompilationUnitSyntax>(tree,
					vector<unique_ptr<MemberSyntax>>(), token.Clone());

			if (token.Kind() != SyntaxKind::EndOfFileToken || includeEndOfFile)
				tokens.push_back(std::move(token));
			if (token.Kind() == SyntaxKind::EndOfFileToken)
				break;
		}
		auto diags = make_unique<DiagnosticBag>(std::move(lexer).Diagnostics());
		return std::make_tuple(std::move(root), vector<SyntaxTree>(), std::move(diags));
	};

	auto tree = SyntaxTree(std::move(text), parseTokens);
	diagnostics.AddRange(std::move(tree).Diagnostics());
	return std::make_pair(tokens, std::move(tree));
}

vector<unique_ptr<SyntaxTree>> SyntaxTree::Flatten(unique_ptr<SyntaxTree> tree)
{
	auto result = vector<unique_ptr<SyntaxTree>>();

	// Different bahaviors of move ctors between unique_ptr and SyntaxTree
	// i.e. moved-from unique_ptr == nullptr
	// whereas SyntaxTree needs a check as such.
	// Otherwise the flatten process introcudes an empty unique_ptr<SyntaxTree>
	if (tree == nullptr || tree->_text == nullptr)
		return result;

	for (auto& u : tree->_usings)
	{
		auto trees = Flatten(make_unique<SyntaxTree>(std::move(u)));
		result.insert(result.end(), std::make_move_iterator(trees.begin()),
			std::make_move_iterator(trees.end()));
	}

	result.push_back(std::move(tree));

	return result;
}

}//MCF
