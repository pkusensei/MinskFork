#include "Parsing.h"

#include <stdexcept>

#include "Diagnostic.h"
#include "helpers.h"
#include "Lexer.h"
#include "ReflectionHelper.h"

namespace MCF {

const vector<const SyntaxNode*> ParameterSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_identifier, _type);
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

const vector<const SyntaxNode*> GlobalStatementSyntax::GetChildren() const
{
	return MakeVecOfRaw<const SyntaxNode>(_statement);
}

const vector<const SyntaxNode*> CompilationUnitSyntax::GetChildren() const
{
	auto result = MakeVecOfRaw<const SyntaxNode, const MemberSyntax>(
		_members.begin(), _members.end());
	auto rest = MakeVecOfRaw<const SyntaxNode>(_endOfFileToken);
	result.insert(result.end(), rest.begin(), rest.end());
	return result;
}

Parser::Parser(const SyntaxTree& tree)
	:_tree(tree), _text(tree.Text()), _tokens(),
	_position(0), _diagnostics(make_unique<DiagnosticBag>())
{
	auto lexer = Lexer(tree);
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

SyntaxToken Parser::MatchToken(const SyntaxKind& kind)
{
	auto& current = Current();
	if (current.Kind() == kind)
		return NextToken().Clone();
	_diagnostics->ReportUnexpectedToken(current.Location(), current.Kind(), kind);
	return SyntaxToken(_tree, kind, current.Position(), string(), NullValue);
}

vector<unique_ptr<MemberSyntax>> Parser::ParseMembers()
{
	auto members = vector<unique_ptr<MemberSyntax>>();
	while (Current().Kind() != SyntaxKind::EndOfFileToken)
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
	if (Current().Kind() == SyntaxKind::FunctionKeyword)
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
		nodes.emplace_back(std::move(param));

		if (Current().Kind() == SyntaxKind::CommaToken)
		{
			auto comma = MatchToken(SyntaxKind::CommaToken);
			nodes.emplace_back(make_unique<SyntaxToken>(comma));
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

unique_ptr<StatementSyntax> Parser::ParseStatement()
{
	switch (Current().Kind())
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
		case SyntaxKind::BreakKeyword:
			return ParseBreakStatement();
		case SyntaxKind::ContinueKeyword:
			return ParseContinueStatement();
		case SyntaxKind::ReturnKeyword:
			return ParseReturnStatement();
		default:
			return ParseExpressionStatement();
	}
}

unique_ptr<BlockStatementSyntax> Parser::ParseBlockStatement()
{
	auto statements = vector<unique_ptr<StatementSyntax>>();
	auto openBraceToken = MatchToken(SyntaxKind::OpenBraceToken);

	while (Current().Kind() != SyntaxKind::EndOfFileToken
		&& Current().Kind() != SyntaxKind::CloseBraceToken)
	{
		auto startToken = Current();

		statements.emplace_back(ParseStatement());

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
		//if (operatorToken.Kind() == SyntaxKind::PlusPlusToken || operatorToken.Kind() == SyntaxKind::MinusMinusToken)
		//{
		//	_diagnostics->ReportUnexpectedToken(operatorToken.Span(), operatorToken.Kind(), SyntaxKind::IdentifierToken);
		//}
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
				_diagnostics->ReportExpressionNotSupportPostfixOperator(
					Current().Location(), Current().Text(), left->Kind());
				break;
			}
			left = ParsePostfixExpression(left);
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

unique_ptr<ExpressionSyntax> Parser::ParsePostfixExpression(unique_ptr<ExpressionSyntax>& expression)
{
	auto operatorToken = NextToken();
	auto pre = dynamic_cast<ParenthesizedExpressionSyntax*>(expression.get());
	auto pfe = dynamic_cast<PostfixExpressionSyntax*>(expression.get());
	auto ne = dynamic_cast<NameExpressionSyntax*>(expression.get());
	if (pre)
	{
		auto ae = dynamic_cast<const AssignmentExpressionSyntax*>(pre->Expression());
		return make_unique<PostfixExpressionSyntax>(_tree, ae->IdentifierToken(), operatorToken, std::move(expression));
	} else if (pfe)
		return make_unique<PostfixExpressionSyntax>(_tree, pfe->IdentifierToken(), operatorToken, std::move(expression));
	else if (ne)
		return make_unique<PostfixExpressionSyntax>(_tree, ne->IdentifierToken(), operatorToken, std::move(expression));
	else
		throw std::invalid_argument(BuildStringFrom("Unexpected expression ", GetSyntaxKindName(expression->Kind())));
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
		nodesAndSeparators.emplace_back(std::move(expression));

		if (Current().Kind() == SyntaxKind::CommaToken)
		{
			auto comma = MatchToken(SyntaxKind::CommaToken);
			nodesAndSeparators.emplace_back(make_unique<SyntaxToken>(comma));
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
std::pair<unique_ptr<CompilationUnitSyntax>, unique_ptr<DiagnosticBag>>
{
	auto parser = Parser(tree);
	auto root = parser.ParseCompilationUnit();
	auto diag = parser.FetchDiagnostics();
	return { std::move(root),std::move(diag) };
}

unique_ptr<SyntaxTree> SyntaxTree::Parse(string_view text)
{
	auto sourceText = SourceText::From(text);
	return Parse(std::move(sourceText));
}

unique_ptr<SyntaxTree> SyntaxTree::Parse(unique_ptr<SourceText> text)
{
	return unique_ptr<SyntaxTree>(new SyntaxTree(std::move(text), ParseTree));
}

std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
SyntaxTree::ParseTokens(string_view text)
{
	auto source = SourceText::From(text);
	return ParseTokens(std::move(source));
}

std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
SyntaxTree::ParseTokens(string_view text, DiagnosticBag& diagnostics)
{
	auto source = SourceText::From(text);
	return ParseTokens(std::move(source), diagnostics);
}

std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
SyntaxTree::ParseTokens(unique_ptr<SourceText> text)
{
	auto _ = DiagnosticBag();
	return ParseTokens(std::move(text), _);
}

std::pair<vector<SyntaxToken>, unique_ptr<SyntaxTree>>
SyntaxTree::ParseTokens(unique_ptr<SourceText> text, DiagnosticBag& diagnostics)
{
	auto tokens = vector<SyntaxToken>();

	auto parseTokens = [&tokens](const SyntaxTree& tree)mutable
	{
		unique_ptr<CompilationUnitSyntax> root = nullptr;
		auto lexer = Lexer(tree);
		while (true)
		{
			auto token = lexer.Lex();
			if (token.Kind() == SyntaxKind::EndOfFileToken)
			{
				root = make_unique<CompilationUnitSyntax>(tree,
					vector<unique_ptr<MemberSyntax>>(), std::move(token));
				break;
			}
			tokens.push_back(std::move(token));
		}
		return std::make_pair(std::move(root), lexer.FetchDiagnostics());
	};

	auto tree = unique_ptr<SyntaxTree>(new SyntaxTree(std::move(text), parseTokens));
	diagnostics.AddRange(*tree->Diagnostics());
	return { tokens, std::move(tree) };
}

}//MCF
