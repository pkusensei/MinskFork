#include "SyntaxKind.h"

#include <array>
#include <stdexcept>

#include "EnumHelper.h"
#include "StringHelper.h"

namespace MCF {

const std::array<SyntaxKind, SYNTAXKIND_COUNT>& AllSyntaxKinds
= GetAllEnumValue<SyntaxKind, SYNTAXKIND_COUNT>(SyntaxKind::BadToken, SyntaxKind::PostfixExpression);

string_view nameof(SyntaxKind kind) noexcept
{
#define NAME(kind) \
case SyntaxKind::kind: return #kind;

	switch (kind)
	{
		NAME(BadToken);

		NAME(SkippedTextTrivia);
		NAME(LineBreakTrivia);
		NAME(WhitespaceTrivia);
		NAME(SingleLineCommentTrivia);
		NAME(MultiLineCommentTrivia);

		NAME(EndOfFileToken);
		NAME(NumberToken);
		NAME(StringToken);
		NAME(PlusToken);
		NAME(PlusEqualsToken);
		NAME(MinusToken);
		NAME(MinusEqualsToken);
		NAME(StarToken);
		NAME(StarEqualsToken);
		NAME(SlashToken);
		NAME(SlashEqualsToken);
		NAME(PercentToken);
		NAME(PercentEqualsToken);
		NAME(BangToken);
		NAME(PlusPlusToken);
		NAME(MinusMinusToken);
		NAME(EqualsToken);
		NAME(TildeToken);
		NAME(HatToken);
		NAME(HatEqualsToken);
		NAME(AmpersandToken);
		NAME(AmpersandEqualsToken);
		NAME(AmpersandAmpersandToken);
		NAME(PipeToken);
		NAME(PipeEqualsToken);
		NAME(PipePipeToken);
		NAME(EqualsEqualsToken);
		NAME(BangEqualsToken);
		NAME(LessToken);
		NAME(LessOrEqualsToken);
		NAME(GreaterToken);
		NAME(GreaterOrEqualsToken);
		NAME(OpenParenthesisToken);
		NAME(CloseParenthesisToken);
		NAME(OpenBraceToken);
		NAME(CloseBraceToken);
		NAME(ColonToken);
		NAME(CommaToken);
		NAME(IdentifierToken);

		NAME(BreakKeyword);
		NAME(ContinueKeyword);
		NAME(ElseKeyword);
		NAME(FalseKeyword);
		NAME(ForKeyword);
		NAME(FunctionKeyword);
		NAME(IfKeyword);
		NAME(LetKeyword);
		NAME(ReturnKeyword);
		NAME(ToKeyword);
		NAME(TrueKeyword);
		NAME(VarKeyword);
		NAME(WhileKeyword);
		NAME(DoKeyword);
		NAME(UsingKeyword);

		NAME(CompilationUnit);
		NAME(FunctionDeclaration);
		NAME(GlobalStatement);
		NAME(Parameter);
		NAME(TypeClause);
		NAME(ElseClause);
		NAME(UsingDirective);

		NAME(BlockStatement);
		NAME(VariableDeclaration);
		NAME(IfStatement);
		NAME(WhileStatement);
		NAME(DoWhileStatement);
		NAME(ForStatement);
		NAME(BreakStatement);
		NAME(ContinueStatement);
		NAME(ReturnStatement);
		NAME(ExpressionStatement);

		NAME(LiteralExpression);
		NAME(NameExpression);
		NAME(UnaryExpression);
		NAME(BinaryExpression);
		NAME(CompoundAssignmentExpression);
		NAME(ParenthesizedExpression);
		NAME(CallExpression);
		NAME(AssignmentExpression);
		NAME(PostfixExpression);

		default:
			return string_view();
	}

#undef NAME
}

SyntaxKind GetKeywordKind(string_view text) noexcept
{
	if (text == "break")
		return SyntaxKind::BreakKeyword;
	else if (text == "continue")
		return SyntaxKind::ContinueKeyword;
	else if (text == "else")
		return SyntaxKind::ElseKeyword;
	else if (text == "false")
		return SyntaxKind::FalseKeyword;
	else if (text == "for")
		return SyntaxKind::ForKeyword;
	else if (text == "function")
		return SyntaxKind::FunctionKeyword;
	else if (text == "if")
		return SyntaxKind::IfKeyword;
	else if (text == "let")
		return SyntaxKind::LetKeyword;
	else if (text == "return")
		return SyntaxKind::ReturnKeyword;
	else if (text == "to")
		return SyntaxKind::ToKeyword;
	else if (text == "true")
		return SyntaxKind::TrueKeyword;
	else if (text == "var")
		return SyntaxKind::VarKeyword;
	else if (text == "while")
		return SyntaxKind::WhileKeyword;
	else if (text == "do")
		return SyntaxKind::DoKeyword;
	else if (text == "using")
		return SyntaxKind::UsingKeyword;
	else return SyntaxKind::IdentifierToken;
}

string_view GetText(SyntaxKind kind)
{
	switch (kind)
	{
		case SyntaxKind::PlusToken: return "+";
		case SyntaxKind::PlusEqualsToken: return "+=";
		case SyntaxKind::MinusToken: return "-";
		case SyntaxKind::MinusEqualsToken: return "-=";
		case SyntaxKind::StarToken: return "*";
		case SyntaxKind::StarEqualsToken: return "*=";
		case SyntaxKind::SlashToken: return "/";
		case SyntaxKind::SlashEqualsToken: return "/=";
		case SyntaxKind::PercentToken: return "%";
		case SyntaxKind::PercentEqualsToken: return "%=";
		case SyntaxKind::BangToken: return "!";
		case SyntaxKind::PlusPlusToken: return "++";
		case SyntaxKind::MinusMinusToken: return "--";
		case SyntaxKind::EqualsToken: return "=";
		case SyntaxKind::TildeToken: return "~";
		case SyntaxKind::HatToken: return "^";
		case SyntaxKind::HatEqualsToken: return "^=";
		case SyntaxKind::AmpersandToken: return "&";
		case SyntaxKind::AmpersandEqualsToken: return "&=";
		case SyntaxKind::AmpersandAmpersandToken: return "&&";
		case SyntaxKind::PipeToken: return "|";
		case SyntaxKind::PipeEqualsToken: return "|=";
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
		case SyntaxKind::ColonToken: return ":";
		case SyntaxKind::CommaToken: return ",";
		case SyntaxKind::BreakKeyword: return "break";
		case SyntaxKind::ContinueKeyword: return "continue";
		case SyntaxKind::ElseKeyword: return "else";
		case SyntaxKind::FalseKeyword: return "false";
		case SyntaxKind::ForKeyword: return "for";
		case SyntaxKind::FunctionKeyword: return "function";
		case SyntaxKind::IfKeyword: return "if";
		case SyntaxKind::LetKeyword: return "let";
		case SyntaxKind::ReturnKeyword: return "return";
		case SyntaxKind::ToKeyword: return "to";
		case SyntaxKind::TrueKeyword: return "true";
		case SyntaxKind::VarKeyword: return "var";
		case SyntaxKind::WhileKeyword: return "while";
		case SyntaxKind::DoKeyword: return "do";
		case SyntaxKind::UsingKeyword: return "using";
		default: return string_view();
	}
}

bool IsComment(SyntaxKind kind)noexcept
{
	return kind == SyntaxKind::SingleLineCommentTrivia ||
		kind == SyntaxKind::MultiLineCommentTrivia;
}

bool IsTrivia(SyntaxKind kind)noexcept
{
	switch (kind)
	{
		case SyntaxKind::SkippedTextTrivia:
		case SyntaxKind::LineBreakTrivia:
		case SyntaxKind::WhitespaceTrivia:
		case SyntaxKind::SingleLineCommentTrivia:
		case SyntaxKind::MultiLineCommentTrivia:
			return true;
		default:
			return false;
	}
}

bool IsKeyword(SyntaxKind kind)
{
	return StringEndsWith(nameof(kind), "Keyword");
}

bool IsToken(SyntaxKind kind)
{
	return !IsTrivia(kind) &&
		(IsKeyword(kind) || StringEndsWith(nameof(kind), "Token"));
}

int GetUnaryOperatorPrecedence(SyntaxKind kind) noexcept
{
	switch (kind)
	{
		case SyntaxKind::PlusToken:
		case SyntaxKind::MinusToken:
		case SyntaxKind::BangToken:
		case SyntaxKind::TildeToken:
			return 6;
		default:
			return 0;
	}
}

int GetBinaryOperatorPrecedence(SyntaxKind kind) noexcept
{
	switch (kind)
	{
		case SyntaxKind::StarToken:
		case SyntaxKind::SlashToken:
		case SyntaxKind::PercentToken:
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

const vector<SyntaxKind>& GetUnaryOperatorKinds()
{
	auto build = []()
	{
		auto result = vector<SyntaxKind>();
		for (const auto& it : AllSyntaxKinds)
			if (GetUnaryOperatorPrecedence(it) > 0)
				result.emplace_back(it);
		result.shrink_to_fit();
		return result;
	};
	static const auto result = build();
	return result;
}

const vector<SyntaxKind>& GetBinaryOperatorKinds()
{
	auto build = []()
	{
		auto result = vector<SyntaxKind>();
		for (const auto& it : AllSyntaxKinds)
			if (GetBinaryOperatorPrecedence(it) > 0)
				result.emplace_back(it);
		result.shrink_to_fit();
		return result;
	};
	static const auto result = build();
	return result;
}

SyntaxKind GetBinaryOperatorOfAssignmentOperator(SyntaxKind kind)
{
	switch (kind)
	{
		case SyntaxKind::PlusEqualsToken:
			return SyntaxKind::PlusToken;
		case SyntaxKind::MinusEqualsToken:
			return SyntaxKind::MinusToken;
		case SyntaxKind::StarEqualsToken:
			return SyntaxKind::StarToken;
		case SyntaxKind::SlashEqualsToken:
			return SyntaxKind::SlashToken;
		case SyntaxKind::PercentEqualsToken:
			return SyntaxKind::PercentToken;
		case SyntaxKind::AmpersandEqualsToken:
			return SyntaxKind::AmpersandToken;
		case SyntaxKind::PipeEqualsToken:
			return SyntaxKind::PipeToken;
		case SyntaxKind::HatEqualsToken:
			return SyntaxKind::HatToken;
		default:
			throw std::invalid_argument(
				BuildStringFrom("Unexpected syntax: '", nameof(kind), "'."));
	}
}

}//MCF