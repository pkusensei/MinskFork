#include "stdafx.h"
#include "Parsing.h"

#include "Diagnostic.h"
#include "Lexer.h"
#include "ReflectionHelper.h"
#include "SourceText.h"

namespace MCF {

#pragma region expression

AssignmentExpressionSyntax::AssignmentExpressionSyntax(const SyntaxToken & identifier,
													   const SyntaxToken & equals,
													   unique_ptr<ExpressionSyntax>& expression)
	:_identifierToken(identifier), _equalsToken(equals),
	_expression(std::move(expression))
{
}

const vector<const SyntaxNode*> AssignmentExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_identifierToken, _equalsToken, _expression);
}

UnaryExpressionSyntax::UnaryExpressionSyntax(const SyntaxToken & operatorToken,
											 unique_ptr<ExpressionSyntax>& operand)
	:_operatorToken(operatorToken), _operand(std::move(operand))
{
}

const vector<const SyntaxNode*> UnaryExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_operatorToken, _operand);
}

BinaryExpressionSyntax::BinaryExpressionSyntax(unique_ptr<ExpressionSyntax>& left,
											   const SyntaxToken & operatorToken,
											   unique_ptr<ExpressionSyntax>& right)
	:_operatorToken(operatorToken), _left(std::move(left)), _right(std::move(right))
{
}

const vector<const SyntaxNode*> BinaryExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_left, _operatorToken, _right);
}

ParenthesizedExpressionSyntax::ParenthesizedExpressionSyntax(const SyntaxToken & open,
															 unique_ptr<ExpressionSyntax>& expression,
															 const SyntaxToken & close)
	:_openParenthesisToken(open), _closeParenthesisToken(close),
	_expression(std::move(expression))
{
}

const vector<const SyntaxNode*> ParenthesizedExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_openParenthesisToken, _expression,
										  _closeParenthesisToken);
}

LiteralExpressionSyntax::LiteralExpressionSyntax(const SyntaxToken& literalToken,
												 const ValueType& value)
	:_literalToken(literalToken), _value(value)
{
}

LiteralExpressionSyntax::LiteralExpressionSyntax(const SyntaxToken& literalToken)
	: _literalToken(literalToken), _value(literalToken.Value())
{
}

const vector<const SyntaxNode*> LiteralExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_literalToken);
}

NameExpressionSyntax::NameExpressionSyntax(const SyntaxToken & identifier)
	:_identifierToken(identifier)
{
}

const vector<const SyntaxNode*> NameExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_identifierToken);
}

CallExpressionSyntax::CallExpressionSyntax(const SyntaxToken & identifier,
										   const SyntaxToken & open,
										   SeparatedSyntaxList<ExpressionSyntax>& arguments, const SyntaxToken & close)
	: _identifier(identifier), _openParenthesisToken(open),
	_arguments(std::move(arguments)), _closeParenthesisToken(close)
{
}

const vector<const SyntaxNode*> CallExpressionSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<const SyntaxNode>(_identifier, _openParenthesisToken);
	auto nodes = _arguments.GetWithSeparators();
	result.insert(result.end(), nodes.begin(), nodes.end());
	auto rest = MakeVecOfRaw<const SyntaxNode>(_closeParenthesisToken);
	result.insert(result.end(), rest.begin(), rest.end());
	return result;
}

PostfixExpressionSyntax::PostfixExpressionSyntax(const SyntaxToken & identifier,
												 const SyntaxToken& op,
												 unique_ptr<ExpressionSyntax>& expression)
	:_identifier(identifier), _op(op), _expression(std::move(expression))
{
}

const vector<const SyntaxNode*> PostfixExpressionSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_identifier, _op, _expression);
}

#pragma endregion
#pragma region statement

BlockStatementSyntax::BlockStatementSyntax(const SyntaxToken & open,
										   vector<unique_ptr<StatementSyntax>>& statements,
										   const SyntaxToken & close)
	:_openBraceToken(open), _closeBraceToken(close),
	_statements(std::move(statements))
{
}

const vector<const SyntaxNode*> BlockStatementSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<const SyntaxNode>(_openBraceToken);
	auto rest = MakeVecOfRaw<const SyntaxNode, const StatementSyntax>(_statements.begin(), _statements.end());
	result.insert(result.end(), rest.begin(), rest.end());
	auto close = MakeVecOfRaw<const SyntaxNode>(_closeBraceToken);
	result.insert(result.end(), close.begin(), close.end());
	return result;
}

const vector<const StatementSyntax*> BlockStatementSyntax::Statements() const
{
	auto result = vector<const StatementSyntax*>();
	for (const auto& it : _statements)
		result.emplace_back(it.get());
	return result;
}

TypeClauseSyntax::TypeClauseSyntax(const SyntaxToken & colon,
								   const SyntaxToken & identifier)
	:_colonToken(colon), _identifier(identifier)
{
}

const vector<const SyntaxNode*> TypeClauseSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_colonToken, _identifier);
}

VariableDeclarationSyntax::VariableDeclarationSyntax(const SyntaxToken & keyword,
													 const SyntaxToken & identifier,
													 const std::optional<TypeClauseSyntax>& typeClause,
													 const SyntaxToken & equals,
													 unique_ptr<ExpressionSyntax>& initializer)
	: _keyword(keyword), _identifier(identifier),
	_typeClause(typeClause), _equalsToken(equals),
	_initializer(std::move(initializer))
{
}

const vector<const SyntaxNode*> VariableDeclarationSyntax::GetChildren() const
{
	if (_typeClause.has_value())
		return MakeVecOfRaw<const SyntaxNode>(_keyword, _identifier, *_typeClause,
											  _equalsToken, _initializer);
	else
		return MakeVecOfRaw<const SyntaxNode>(_keyword, _identifier,
											  _equalsToken, _initializer);
}


ElseClauseSyntax::ElseClauseSyntax(const SyntaxToken & elseKeyword,
								   unique_ptr<StatementSyntax>& elseStatement)
	:_elseKeyword(elseKeyword),
	_elseStatement(std::move(elseStatement))
{
}

const vector<const SyntaxNode*> ElseClauseSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_elseKeyword, _elseStatement);
}

IfStatementSyntax::IfStatementSyntax(const SyntaxToken & ifKeyword,
									 unique_ptr<ExpressionSyntax>& condition,
									 unique_ptr<StatementSyntax>& thenStatement,
									 unique_ptr<ElseClauseSyntax>& elseClause)
	:_ifKeyword(ifKeyword), _condition(std::move(condition)),
	_thenStatement(std::move(thenStatement)), _elseClause(std::move(elseClause))
{
}

const vector<const SyntaxNode*> IfStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_ifKeyword, _condition,
										  _thenStatement, _elseClause);
}

WhileStatementSyntax::WhileStatementSyntax(const SyntaxToken & whileKeyword,
										   unique_ptr<ExpressionSyntax>& condition,
										   unique_ptr<StatementSyntax>& body)
	:_whileKeyword(whileKeyword), _condition(std::move(condition)),
	_body(std::move(body))
{
}

const vector<const SyntaxNode*> WhileStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_whileKeyword, _condition, _body);
}

DoWhileStatementSyntax::DoWhileStatementSyntax(const SyntaxToken & doKeyword,
											   unique_ptr<StatementSyntax>& body,
											   const SyntaxToken & whileKeyword,
											   unique_ptr<ExpressionSyntax>& condition)
	:_doKeyword(doKeyword), _body(std::move(body)),
	_whileKeyword(whileKeyword), _condition(std::move(condition))
{
}

const vector<const SyntaxNode*> DoWhileStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_doKeyword, _body, _whileKeyword, _condition);
}

ForStatementSyntax::ForStatementSyntax(const SyntaxToken & keyword,
									   const SyntaxToken & identifier,
									   const SyntaxToken & equals,
									   unique_ptr<ExpressionSyntax>& lowerBound,
									   const SyntaxToken & toKeyword,
									   unique_ptr<ExpressionSyntax>& upperBound,
									   unique_ptr<StatementSyntax>& body)
	:_keyword(keyword), _identifier(identifier), _equalsToken(equals),
	_lowerBound(std::move(lowerBound)), _toKeyword(toKeyword),
	_upperBound(std::move(upperBound)), _body(std::move(body))
{
}

const vector<const SyntaxNode*> ForStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_keyword, _identifier, _equalsToken,
										  _lowerBound, _toKeyword, _upperBound, _body);
}

ExpressionStatementSyntax::ExpressionStatementSyntax(unique_ptr<ExpressionSyntax>& expression)
	:_expression(std::move(expression))
{
}

const vector<const SyntaxNode*> ExpressionStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_expression);
}

#pragma endregion

ParameterSyntax::ParameterSyntax(const SyntaxToken & identifier,
								 const TypeClauseSyntax& type)
	:_identifier(identifier), _type(type)
{
}

const vector<const SyntaxNode*> ParameterSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_identifier, _type);
}

FunctionDeclarationSyntax::FunctionDeclarationSyntax(const SyntaxToken & funcKeyword,
													 const SyntaxToken & identifier,
													 const SyntaxToken & openParenthesisToken,
													 SeparatedSyntaxList<ParameterSyntax>& params,
													 const SyntaxToken & closeParenthesisToken,
													 const std::optional<TypeClauseSyntax>& type,
													 unique_ptr<BlockStatementSyntax>& body)
	:_funcKeyword(funcKeyword), _identifier(identifier), _openParenthesisToken(openParenthesisToken),
	_parameters(std::move(params)), _closeParenthesisToken(closeParenthesisToken),
	_type(type), _body(std::move(body))
{
}

const vector<const SyntaxNode*> FunctionDeclarationSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<const SyntaxNode>(_funcKeyword, _identifier, _openParenthesisToken);
	auto nodes = _parameters.GetWithSeparators();
	result.insert(result.end(), nodes.begin(), nodes.end());
	if (_type.has_value())
	{
		auto rest = MakeVecOfRaw<const SyntaxNode>(_closeParenthesisToken, *_type, _body);
		result.insert(result.end(), rest.begin(), rest.end());
	} else
	{
		auto rest = MakeVecOfRaw<const SyntaxNode>(_closeParenthesisToken, _body);
		result.insert(result.end(), rest.begin(), rest.end());
	}
	return result;
}

GlobalStatementSyntax::GlobalStatementSyntax(unique_ptr<StatementSyntax>& statement)
	:_statement(std::move(statement))
{
}

const vector<const SyntaxNode*> GlobalStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_statement);
}

CompilationUnitSyntax::CompilationUnitSyntax(vector<unique_ptr<MemberSyntax>>& members,
											 const SyntaxToken & endOfFile)
	: _members(std::move(members)), _endOfFileToken(endOfFile)
{
}

const vector<const SyntaxNode*> CompilationUnitSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<const SyntaxNode, const MemberSyntax>(_members.begin(), _members.end());
	auto rest = MakeVecOfRaw<const SyntaxNode>(_endOfFileToken);
	result.insert(result.end(), rest.begin(), rest.end());
	return result;
}

Parser::Parser(const SourceText& text)
	:_text(&text), _tokens(), _position(0), _diagnostics(make_unique<DiagnosticBag>())
{
	auto lexer = Lexer(text);
	auto kind = SyntaxKind::BadToken;
	do
	{
		auto token = lexer.Lex();
		if (token.Kind() != SyntaxKind::WhitespaceToken &&
			token.Kind() != SyntaxKind::BadToken)
		{
			kind = token.Kind();
			_tokens.emplace_back(token);
		}
	} while (kind != SyntaxKind::EndOfFileToken);
	_diagnostics->AddRange(*lexer.Diagnostics());
}

const SyntaxToken* Parser::Peek(int offset) const
{
	auto idx = _position + offset;
	if (idx >= _tokens.size())
		return &(_tokens.back());
	return &_tokens.at(idx);
}

const SyntaxToken* Parser::Current() const
{
	return Peek(0);
}

SyntaxToken Parser::NextToken()
{
	auto current = Current();
	_position++;
	return current->Clone(); // NOTE to clone or not to clone
}

SyntaxToken Parser::MatchToken(const SyntaxKind& kind)
{
	auto current = Current();
	if (current->Kind() == kind)
		return NextToken();
	_diagnostics->ReportUnexpectedToken(current->Span(), current->Kind(), kind);
	return SyntaxToken(kind, current->Position(), string(), NullValue);
}

vector<unique_ptr<MemberSyntax>> Parser::ParseMembers()
{
	auto members = vector<unique_ptr<MemberSyntax>>();
	while (Current()->Kind() != SyntaxKind::EndOfFileToken)
	{
		auto startToken = Current();
		auto member = ParseMember();
		members.emplace_back(std::move(member));

		if (Current() == startToken)
			NextToken();
	}
	return members;
}

unique_ptr<MemberSyntax> Parser::ParseMember()
{
	if (Current()->Kind() == SyntaxKind::FunctionKeyword)
		return ParseFunctionDeclaration();
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

	return make_unique<FunctionDeclarationSyntax>(functionKeyword, identifier,
												  openParenthesisToken, parameters,
												  closeParenthesisToken, type, body);
}

SeparatedSyntaxList<ParameterSyntax> Parser::ParseParameterList()
{
	auto nodes = vector<unique_ptr<SyntaxNode>>();

	while (Current()->Kind() != SyntaxKind::CloseParenthesisToken
		   && Current()->Kind() != SyntaxKind::EndOfFileToken)
	{
		auto param = ParseParameter();
		nodes.emplace_back(std::move(param));

		if (Current()->Kind() != SyntaxKind::CloseParenthesisToken)
		{
			auto comma = MatchToken(SyntaxKind::CommaToken);
			nodes.emplace_back(make_unique<SyntaxToken>(comma));
		}
	}

	return SeparatedSyntaxList<ParameterSyntax>(nodes);
}

unique_ptr<ParameterSyntax> Parser::ParseParameter()
{
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto type = ParseTypeClause();
	return make_unique<ParameterSyntax>(identifier, type);
}

unique_ptr<MemberSyntax> Parser::ParseGlobalStatement()
{
	auto statement = ParseStatement();
	return make_unique<GlobalStatementSyntax>(statement);
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
		case SyntaxKind::DoKeyword:
			return ParseDoWhileStatement();
		case SyntaxKind::ForKeyword:
			return ParseForStatement();
		default:
			return ParseExpressionStatement();
	}
}

unique_ptr<BlockStatementSyntax> Parser::ParseBlockStatement()
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
	return make_unique<BlockStatementSyntax>(openBraceToken, statements, closeBraceToken);
}

unique_ptr<StatementSyntax> Parser::ParseVariableDeclaration()
{
	auto expected =
		Current()->Kind() == SyntaxKind::LetKeyword ?
		SyntaxKind::LetKeyword : SyntaxKind::VarKeyword;
	auto keyword = MatchToken(expected);
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto typeClause = ParseOptionalTypeClause();
	auto equals = MatchToken(SyntaxKind::EqualsToken);
	auto initializer = ParseExpression();

	return make_unique<VariableDeclarationSyntax>(keyword, identifier, typeClause,
												  equals, initializer);
}

std::optional<TypeClauseSyntax> Parser::ParseOptionalTypeClause()
{
	if (Current()->Kind() != SyntaxKind::ColonToken) return std::nullopt;
	return ParseTypeClause();
}

TypeClauseSyntax Parser::ParseTypeClause()
{
	auto colon = MatchToken(SyntaxKind::ColonToken);
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	return TypeClauseSyntax(colon, identifier);
}

unique_ptr<StatementSyntax> Parser::ParseIfStatement()
{
	auto keyword = MatchToken(SyntaxKind::IfKeyword);
	auto condition = ParseExpression();
	auto statement = ParseStatement();
	auto elseClause = ParseElseClause();
	return make_unique<IfStatementSyntax>(keyword, condition, statement, elseClause);
}

unique_ptr<ElseClauseSyntax> Parser::ParseElseClause()
{
	if (Current()->Kind() != SyntaxKind::ElseKeyword)
		return nullptr;
	auto keyword = MatchToken(SyntaxKind::ElseKeyword);
	auto statement = ParseStatement();
	return make_unique<ElseClauseSyntax>(keyword, statement);
}

unique_ptr<StatementSyntax> Parser::ParseWhileStatement()
{
	auto keyword = MatchToken(SyntaxKind::WhileKeyword);
	auto condition = ParseExpression();
	auto body = ParseStatement();
	return make_unique<WhileStatementSyntax>(keyword, condition, body);
}

unique_ptr<StatementSyntax> Parser::ParseDoWhileStatement()
{
	auto doKeyword = MatchToken(SyntaxKind::DoKeyword);
	auto body = ParseStatement();
	auto whileKeyword = MatchToken(SyntaxKind::WhileKeyword);
	auto condition = ParseExpression();

	return make_unique<DoWhileStatementSyntax>(doKeyword, body, whileKeyword, condition);
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
	return make_unique<ForStatementSyntax>(keyword, identifier, equalsToken, lowerBound,
										   toKeyword, upperBound, body);
}

unique_ptr<ExpressionStatementSyntax> Parser::ParseExpressionStatement()
{
	auto expression = ParseExpression();
	return make_unique<ExpressionStatementSyntax>(expression);
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
		return make_unique<AssignmentExpressionSyntax>(identifierToken, operatorToken, right);
	}
	return ParseBinaryExpression();
}

unique_ptr<ExpressionSyntax> Parser::ParseBinaryExpression(int parentPrecedence)
{
	unique_ptr<ExpressionSyntax> left = nullptr;
	auto unaryOperatorPrecedence = GetUnaryOperatorPrecedence(Current()->Kind());
	if (unaryOperatorPrecedence != 0
		&& unaryOperatorPrecedence >= parentPrecedence)
	{
		auto operatorToken = NextToken();
		//if (operatorToken.Kind() == SyntaxKind::PlusPlusToken || operatorToken.Kind() == SyntaxKind::MinusMinusToken)
		//{
		//	_diagnostics->ReportUnexpectedToken(operatorToken.Span(), operatorToken.Kind(), SyntaxKind::IdentifierToken);
		//}
		auto operand = ParseBinaryExpression(unaryOperatorPrecedence);
		left = make_unique<UnaryExpressionSyntax>(operatorToken, operand);
	} else
	{
		left = ParsePrimaryExpression();
		while (Current()->Kind() == SyntaxKind::PlusPlusToken
			   || Current()->Kind() == SyntaxKind::MinusMinusToken)
		{
			if (left->Kind() != SyntaxKind::ParenthesizedExpression
				&& left->Kind() != SyntaxKind::PostfixExpression
				&& left->Kind() != SyntaxKind::NameExpression)
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
		left = make_unique<BinaryExpressionSyntax>(left, operatorToken, right);
	}
	return left;
}

unique_ptr<ExpressionSyntax> Parser::ParsePostfixExpression(unique_ptr<ExpressionSyntax>& expression)
{
	auto operatorToken = NextToken();
	auto pre = dynamic_cast<ParenthesizedExpressionSyntax*>(expression.get());
	auto pfe = dynamic_cast<PostfixExpressionSyntax*>(expression.get());
	auto ne = dynamic_cast<NameExpressionSyntax*>(expression.get());
	if (pre)
	{
		auto ae = dynamic_cast<const AssignmentExpressionSyntax*>(pre->Expression());
		return make_unique<PostfixExpressionSyntax>(ae->IdentifierToken(), operatorToken, expression);
	} else if (pfe)
		return make_unique<PostfixExpressionSyntax>(pfe->IdentifierToken(), operatorToken, expression);
	else if (ne)
		return make_unique<PostfixExpressionSyntax>(ne->IdentifierToken(), operatorToken, expression);
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
	return make_unique<ParenthesizedExpressionSyntax>(left, expression, right);
}

unique_ptr<ExpressionSyntax> Parser::ParseBooleanLiteral()
{
	auto isTrue = Current()->Kind() == SyntaxKind::TrueKeyword;
	auto keywordToken = isTrue ? MatchToken(SyntaxKind::TrueKeyword) : MatchToken(SyntaxKind::FalseKeyword);
	return make_unique<LiteralExpressionSyntax>(keywordToken, isTrue);
}

unique_ptr<ExpressionSyntax> Parser::ParseNumberLiteral()
{
	auto numberToken = MatchToken(SyntaxKind::NumberToken);
	return make_unique<LiteralExpressionSyntax>(numberToken);
}

unique_ptr<ExpressionSyntax> Parser::ParseStringLiteral()
{
	auto stringToken = MatchToken(SyntaxKind::StringToken);
	return make_unique<LiteralExpressionSyntax>(stringToken);
}

unique_ptr<ExpressionSyntax> Parser::ParseNameOrCallExpression()
{
	if (Peek(0)->Kind() == SyntaxKind::IdentifierToken
		&& Peek(1)->Kind() == SyntaxKind::OpenParenthesisToken)
		return ParseCallExpression();
	return ParseNameExpression();
}

unique_ptr<ExpressionSyntax> Parser::ParseCallExpression()
{
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	auto open = MatchToken(SyntaxKind::OpenParenthesisToken);
	auto arguments = ParseArguments();
	auto close = MatchToken(SyntaxKind::CloseParenthesisToken);

	return make_unique<CallExpressionSyntax>(identifier, open, arguments, close);
}

SeparatedSyntaxList<ExpressionSyntax> Parser::ParseArguments()
{
	auto nodesAndSeparators = vector<unique_ptr<SyntaxNode>>();
	while (Current()->Kind() != SyntaxKind::CloseParenthesisToken
		   && Current()->Kind() != SyntaxKind::EndOfFileToken)
	{
		auto expression = ParseExpression();
		nodesAndSeparators.emplace_back(std::move(expression));

		if (Current()->Kind() != SyntaxKind::CloseParenthesisToken)
		{
			auto comma = MatchToken(SyntaxKind::CommaToken);
			nodesAndSeparators.emplace_back(make_unique<SyntaxToken>(comma));
		}
	}

	return SeparatedSyntaxList<ExpressionSyntax>(nodesAndSeparators);
}

unique_ptr<ExpressionSyntax> Parser::ParseNameExpression()
{
	auto identifier = MatchToken(SyntaxKind::IdentifierToken);
	return make_unique<NameExpressionSyntax>(identifier);
}

unique_ptr<CompilationUnitSyntax> Parser::ParseCompilationUnit()
{
	auto members = ParseMembers();
	auto endOfFileToken = MatchToken(SyntaxKind::EndOfFileToken);
	return make_unique<CompilationUnitSyntax>(members, endOfFileToken);
}

SyntaxTree::SyntaxTree(const SourceText& text)
	:_text(make_unique<SourceText>(text)), _diagnostics(make_unique<DiagnosticBag>())
{
	auto parser = Parser(text);
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
	return make_unique<SyntaxTree>(text);
}

vector<SyntaxToken> SyntaxTree::ParseTokens(const string & text)
{
	auto source = SourceText::From(text);
	return ParseTokens(source);
}

vector<SyntaxToken> SyntaxTree::ParseTokens(const string & text, DiagnosticBag& diagnostics)
{
	auto source = SourceText::From(text);
	return ParseTokens(source, diagnostics);
}

vector<SyntaxToken> SyntaxTree::ParseTokens(const SourceText & text)
{
	auto v = DiagnosticBag();
	return ParseTokens(text, v);
}

vector<SyntaxToken> SyntaxTree::ParseTokens(const SourceText & text, DiagnosticBag& diagnostics)
{
	auto lexTokens = [](Lexer& lexer) {
		auto result = vector<SyntaxToken>();
		while (true)
		{
			auto token = lexer.Lex();
			if (token.Kind() == SyntaxKind::EndOfFileToken)
				break;
			result.emplace_back(token);
		}
		return result;
	};

	auto lexer = Lexer(text);
	auto result = lexTokens(lexer);
	diagnostics.AddRange(*lexer.Diagnostics());
	return result;
}

}//MCF
