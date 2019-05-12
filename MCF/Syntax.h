#pragma once

#include "Symbols.h"
#include "SyntaxKind.h"

namespace MCF {

class DiagnosticBag;
class TextSpan;
class SourceText;
class SyntaxTree;
class SyntaxToken;

class MCF_API SyntaxNode
{
private:
	static void PrettyPrint(std::ostream& out, const SyntaxNode* node, string indent = "", bool isLast = true);
public:
	virtual ~SyntaxNode() = default;
	virtual SyntaxKind Kind() const = 0;
	virtual TextSpan Span()const;
	virtual const vector<const SyntaxNode*> GetChildren() const = 0;

	SyntaxToken GetLastToken()const;

	void WriteTo(std::ostream& out)const { PrettyPrint(out, this); }
	string ToString() const;
};

class MCF_API SyntaxToken final :public SyntaxNode
{
private:
	SyntaxKind _kind;
	size_t _position;
	string _text;
	ValueType _value;

public:
	SyntaxToken(const SyntaxKind& kind, size_t position, const string& text, const ValueType& value);

	bool operator==(const SyntaxToken& other)const noexcept;
	bool operator!=(const SyntaxToken& other)const noexcept;

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return _kind; }
	TextSpan Span()const override;
	const vector<const SyntaxNode*> GetChildren() const override;

	constexpr size_t Position() const noexcept { return _position; }
	string Text() const { return _text; }
	ValueType Value() const noexcept { return _value; }
	bool IsMissing()const noexcept { return _text.empty(); }

	SyntaxToken Clone()const;
};

class Lexer final
{
private:
	const SourceText* _text;
	unique_ptr<DiagnosticBag> _diagnostics;

	size_t _position;
	size_t _start;
	SyntaxKind _kind;
	ValueType _value;

	char Peek(int offset) const;
	char Current() const { return Peek(0); }
	char Lookahead() const { return Peek(1); }
	constexpr void Next(size_t step = 1) noexcept { _position += step; }

	void ReadString();
	void ReadWhiteSpace();
	void ReadNumberToken();
	void ReadIdentifierOrKeyword();

public:
	explicit Lexer(const SourceText& text);

	SyntaxToken Lex();
	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }
};

#pragma region Expression

class ExpressionSyntax :public SyntaxNode
{
};

class AssignmentExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _expression;

public:
	AssignmentExpressionSyntax(const SyntaxToken& identifier, const SyntaxToken& equals,
							   const unique_ptr<ExpressionSyntax>& expression);
	AssignmentExpressionSyntax(AssignmentExpressionSyntax&&) = default;
	AssignmentExpressionSyntax& operator=(AssignmentExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::AssignmentExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IdentifierToken() const { return _identifierToken; }
	SyntaxToken EqualsToken() const { return _equalsToken; }
	const ExpressionSyntax* Expression() const noexcept { return _expression.get(); }
};

class UnaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _operand;
public:
	UnaryExpressionSyntax(const SyntaxToken& operatorToken, const unique_ptr<ExpressionSyntax>& operand);
	UnaryExpressionSyntax(UnaryExpressionSyntax&&) = default;
	UnaryExpressionSyntax& operator=(UnaryExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::UnaryExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OperatorToken() const { return _operatorToken; }
	const ExpressionSyntax* Operand() const noexcept { return _operand.get(); }
};

class BinaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _left;
	unique_ptr<ExpressionSyntax> _right;
public:
	BinaryExpressionSyntax(const unique_ptr<ExpressionSyntax>& left, const SyntaxToken& operatorToken, const unique_ptr<ExpressionSyntax>& right);
	BinaryExpressionSyntax(BinaryExpressionSyntax&&) = default;
	BinaryExpressionSyntax& operator=(BinaryExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BinaryExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OperatorToken() const { return _operatorToken; }
	const ExpressionSyntax* Left()const noexcept { return _left.get(); }
	const ExpressionSyntax* Right()const noexcept { return _right.get(); }
};

class ParenthesizedExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _openParenthesisToken;
	SyntaxToken _closeParenthesisToken;
	unique_ptr<ExpressionSyntax> _expression;
public:
	ParenthesizedExpressionSyntax(const SyntaxToken& open, const unique_ptr<ExpressionSyntax>& expression, const SyntaxToken& close);
	ParenthesizedExpressionSyntax(ParenthesizedExpressionSyntax&&) = default;
	ParenthesizedExpressionSyntax& operator=(ParenthesizedExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ParenthesizedExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OpenParenthesisToken()const { return _openParenthesisToken; }
	SyntaxToken CloseParenthesisToken() const { return _closeParenthesisToken; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

class LiteralExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _literalToken;
	ValueType _value;
public:
	LiteralExpressionSyntax(const SyntaxToken& literalToken, const ValueType& value);
	explicit LiteralExpressionSyntax(const SyntaxToken& literalToken);
	LiteralExpressionSyntax(LiteralExpressionSyntax&&) = default;
	LiteralExpressionSyntax& operator=(LiteralExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::LiteralExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken LiteralToken()const { return _literalToken; }
	ValueType Value()const { return _value; }
};

class NameExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;
public:
	explicit NameExpressionSyntax(const SyntaxToken& identifier);
	NameExpressionSyntax(NameExpressionSyntax&&) = default;
	NameExpressionSyntax& operator=(NameExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::NameExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IdentifierToken()const { return _identifierToken; }
};

template<typename T, typename = std::enable_if_t<std::is_base_of_v<SyntaxNode, T>>>
class SeparatedSyntaxList final
{
private:
	vector<unique_ptr<SyntaxNode>> _nodesAndSeparators;

public:
	SeparatedSyntaxList(const vector<unique_ptr<SyntaxNode>>& list)
		:_nodesAndSeparators(std::move(std::remove_const_t<vector<unique_ptr<SyntaxNode>>&>(list)))
	{
	}
	SeparatedSyntaxList(SeparatedSyntaxList&& other) = default;
	SeparatedSyntaxList& operator=(SeparatedSyntaxList&& other) = default;

	size_t size()const noexcept
	{
		return (_nodesAndSeparators.size() + 1) / 2;
	}
	
	const T* operator[](size_t index)const
	{
		return _nodesAndSeparators.at(index * 2).get();
	}
	
	const SyntaxToken* GetSeparator(size_t index)const
	{
		if (index == size() - 1) return nullptr;
		return _nodesAndSeparators.at(index * 2 + 1).get();
	}

	const vector<const SyntaxNode*> GetWithSeparators()const
	{
		//TODO replace with template code
		auto result = vector<const SyntaxNode*>();
		for (const auto& it : _nodesAndSeparators)
			result.emplace_back(it.get());
		return result;
	}

	decltype(auto) begin()const { return _nodesAndSeparators.begin(); }
	decltype(auto) end()const { return _nodesAndSeparators.end(); }
};

class CallExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifier;
	SyntaxToken _openParenthesisToken;
	SeparatedSyntaxList<ExpressionSyntax> _arguments;
	SyntaxToken _closeParenthesisToken;

public:
	CallExpressionSyntax(const SyntaxToken& identifier, const SyntaxToken& open,
						 const SeparatedSyntaxList<ExpressionSyntax>& arguments, const SyntaxToken& close);
	CallExpressionSyntax(CallExpressionSyntax&& other) = default;
	CallExpressionSyntax& operator=(CallExpressionSyntax&& other) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::CallExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken Identifier()const { return _identifier; }
	SyntaxToken OpenParenthesisToken()const { return _openParenthesisToken; }
	const SeparatedSyntaxList<ExpressionSyntax>* Arguments()const noexcept { return &_arguments; }
	SyntaxToken CloseParenthesisToken()const { return _closeParenthesisToken; }
};

class PostfixExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifier;
	SyntaxToken _op;
	unique_ptr<ExpressionSyntax> _expression;
public:
	PostfixExpressionSyntax(const SyntaxToken& identifier, const SyntaxToken& op, const unique_ptr<ExpressionSyntax>& expression);
	PostfixExpressionSyntax(PostfixExpressionSyntax&&) = default;
	PostfixExpressionSyntax& operator=(PostfixExpressionSyntax&&) = default;

	// Inherited via ExpressionSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::PostfixExpression; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IdentifierToken()const { return _identifier; }
	SyntaxToken Op()const { return _op; }
	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

#pragma endregion

#pragma region Statement

class StatementSyntax :public SyntaxNode
{
};

class BlockStatementSyntax final : public StatementSyntax
{
private:
	SyntaxToken _openBraceToken;
	SyntaxToken _closeBraceToken;
	vector<unique_ptr<StatementSyntax>> _statements;

public:
	BlockStatementSyntax(const SyntaxToken& open, const vector<unique_ptr<StatementSyntax>>& statements, const SyntaxToken& close);
	BlockStatementSyntax(BlockStatementSyntax&&) = default;
	BlockStatementSyntax& operator=(BlockStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BlockStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OpenBraceToken()const { return _openBraceToken; }
	SyntaxToken CloseBraceToken()const { return _closeBraceToken; }
	const vector<const StatementSyntax*> Statements()const;
};

class VariableDeclarationSyntax final : public StatementSyntax
{
private:
	SyntaxToken _keyword;
	SyntaxToken _identifier;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _initializer;
public:
	VariableDeclarationSyntax(const SyntaxToken& keyword, const SyntaxToken& identifier,
							  const SyntaxToken& equals, const unique_ptr<ExpressionSyntax>& initializer);
	VariableDeclarationSyntax(VariableDeclarationSyntax&&) = default;
	VariableDeclarationSyntax& operator=(VariableDeclarationSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::VariableDeclaration; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken Keyword()const { return _keyword; }
	SyntaxToken Identifier()const { return _identifier; }
	SyntaxToken EqualsToken()const { return _equalsToken; }
	const ExpressionSyntax* Initializer()const noexcept { return _initializer.get(); }
};

class ElseClauseSyntax final :public SyntaxNode
{
private:
	SyntaxToken _elseKeyword;
	unique_ptr<StatementSyntax> _elseStatement;
public:
	ElseClauseSyntax(const SyntaxToken& elseKeyword, const unique_ptr<StatementSyntax>& elseStatement);
	ElseClauseSyntax(ElseClauseSyntax&&) = default;
	ElseClauseSyntax& operator=(ElseClauseSyntax&&) = default;

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ElseClause; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken ElseKeyword()const { return _elseKeyword; }
	const StatementSyntax* ElseStatement()const noexcept { return _elseStatement.get(); }
};

class IfStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _ifKeyword;
	unique_ptr<ExpressionSyntax> _condition;
	unique_ptr<StatementSyntax> _thenStatement;
	unique_ptr<ElseClauseSyntax> _elseClause;
public:
	IfStatementSyntax(const SyntaxToken& ifKeyword, const unique_ptr<ExpressionSyntax>& condition,
					  const unique_ptr<StatementSyntax>& thenStatement, const unique_ptr<ElseClauseSyntax>& elseClause);
	IfStatementSyntax(IfStatementSyntax&&) = default;
	IfStatementSyntax& operator=(IfStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::IfStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IfKeyword()const { return _ifKeyword; }
	const ExpressionSyntax* Condition()const noexcept { return _condition.get(); }
	const StatementSyntax* ThenStatement()const noexcept { return _thenStatement.get(); }
	const ElseClauseSyntax* ElseClause()const noexcept { return _elseClause.get(); }
};

class WhileStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _whileKeyword;
	unique_ptr<ExpressionSyntax> _condition;
	unique_ptr<StatementSyntax> _body;
public:
	WhileStatementSyntax(const SyntaxToken& whileKeyword, const unique_ptr<ExpressionSyntax>& condition,
						 const unique_ptr<StatementSyntax>& body);
	WhileStatementSyntax(WhileStatementSyntax&&) = default;
	WhileStatementSyntax& operator=(WhileStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::WhileStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken WhileKeyword()const { return _whileKeyword; }
	const ExpressionSyntax* Condition()const noexcept { return _condition.get(); }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
};

class ForStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _keyword;
	SyntaxToken _identifier;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _lowerBound;
	SyntaxToken _toKeyword;
	unique_ptr<ExpressionSyntax> _upperBound;
	unique_ptr<StatementSyntax> _body;
public:
	ForStatementSyntax(const SyntaxToken& keyword, const SyntaxToken& identifier, const SyntaxToken& equals,
					   unique_ptr<ExpressionSyntax>& lowerBound, const SyntaxToken& toKeyword,
					   unique_ptr<ExpressionSyntax>& upperBound, const unique_ptr<StatementSyntax>& body);
	ForStatementSyntax(ForStatementSyntax&&) = default;
	ForStatementSyntax& operator=(ForStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ForStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken Keyword() const { return _keyword; }
	SyntaxToken Identifier() const { return _identifier; }
	SyntaxToken EqualsToken()const { return _equalsToken; }
	const ExpressionSyntax* LowerBound()const noexcept { return _lowerBound.get(); }
	SyntaxToken ToKeyword()const { return _toKeyword; }
	const ExpressionSyntax* UpperBound()const noexcept { return _upperBound.get(); }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
};

class ExpressionStatementSyntax final : public StatementSyntax
{
private:
	unique_ptr<ExpressionSyntax> _expression;

public:
	ExpressionStatementSyntax(const unique_ptr<ExpressionSyntax>& expression);
	ExpressionStatementSyntax(ExpressionStatementSyntax&&) = default;
	ExpressionStatementSyntax& operator=(ExpressionStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ExpressionStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

#pragma endregion

class CompilationUnitSyntax final :public SyntaxNode
{
private:
	unique_ptr<StatementSyntax> _statement;
	SyntaxToken _endOfFileToken;

public:
	CompilationUnitSyntax(const unique_ptr<StatementSyntax>& statement, const SyntaxToken& endOfFile);
	CompilationUnitSyntax(CompilationUnitSyntax&&) = default;
	CompilationUnitSyntax& operator=(CompilationUnitSyntax&&) = default;

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::CompilationUnit; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const StatementSyntax* Statement()const noexcept { return _statement.get(); }
	SyntaxToken EndOfFileToken()const { return _endOfFileToken; }
};

class Parser final
{
private:
	const SourceText* _text;
	vector<SyntaxToken> _tokens;
	size_t _position;
	unique_ptr<DiagnosticBag> _diagnostics;

	const SyntaxToken* Peek(int offset) const;
	const SyntaxToken* Current() const;
	SyntaxToken NextToken();
	SyntaxToken MatchToken(const SyntaxKind& kind);

	unique_ptr<StatementSyntax> ParseStatement();
	unique_ptr<StatementSyntax> ParseBlockStatement();
	unique_ptr<StatementSyntax> ParseVariableDeclaration();
	unique_ptr<StatementSyntax> ParseIfStatement();
	unique_ptr<ElseClauseSyntax> ParseElseClause();
	unique_ptr<StatementSyntax> ParseWhileStatement();
	unique_ptr<StatementSyntax> ParseForStatement();
	unique_ptr<StatementSyntax> ParseExpressionStatement();

	unique_ptr<ExpressionSyntax> ParseExpression();
	unique_ptr<ExpressionSyntax> ParseAssignmentExpression();
	unique_ptr<ExpressionSyntax> ParseBinaryExpression(int parentPrecedence = 0);
	unique_ptr<ExpressionSyntax> ParsePostfixExpression(const unique_ptr<ExpressionSyntax>& expression);

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
	explicit Parser(const SourceText& text);

	DiagnosticBag* Diagnostics()const noexcept { return _diagnostics.get(); }
	unique_ptr<CompilationUnitSyntax> ParseCompilationUnit();
};

class MCF_API SyntaxTree final
{
private:
	unique_ptr<SourceText> _text;
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<CompilationUnitSyntax> _root;

public:
	explicit SyntaxTree(const SourceText& text);

	SyntaxTree(SyntaxTree&& other);
	SyntaxTree& operator=(SyntaxTree&& other);
	~SyntaxTree();

	const SourceText& Text() const { return *_text; }
	const CompilationUnitSyntax* Root()const noexcept { return _root.get(); }
	DiagnosticBag* Diagnostics() const noexcept { return _diagnostics.get(); }

	static unique_ptr<SyntaxTree> Parse(const string& text);
	static unique_ptr<SyntaxTree> Parse(const SourceText& text);
	static vector<SyntaxToken> ParseTokens(const string& text);
	static vector<SyntaxToken> ParseTokens(const string& text, DiagnosticBag& diagnostics);
	static vector<SyntaxToken> ParseTokens(const SourceText& text);
	static vector<SyntaxToken> ParseTokens(const SourceText& text, DiagnosticBag& diagnostics);

};

}//MCF
