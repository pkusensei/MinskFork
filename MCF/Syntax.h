#pragma once

#include "common.h"

namespace MCF {

class DiagnosticBag;
class TextSpan;
class SourceText;
class SyntaxTree;


class MCF_API SyntaxNode
{
private:
	static void PrettyPrint(std::ostream& out, const SyntaxNode* node, string indent = "", bool isLast = true);
public:
	virtual ~SyntaxNode() = default;
	virtual SyntaxKind Kind() const = 0;
	virtual TextSpan Span()const;
	virtual const vector<const SyntaxNode*> GetChildren() const = 0;

	void WriteTo(std::ostream& out)const { PrettyPrint(out, this); }
	string ToString() const;
};

class SyntaxToken final :public SyntaxNode
{
private:
	SyntaxKind _kind;
	size_t _position;
	string _text;
	ValueType _value;

public:
	SyntaxToken(SyntaxKind kind, size_t position, const string& text, const ValueType& value);
	virtual ~SyntaxToken() = default;
	SyntaxToken(const SyntaxToken& other) = default;
	SyntaxToken(SyntaxToken&& other);
	SyntaxToken& operator=(const SyntaxToken& other) = default;

	bool operator==(const SyntaxToken& other)const;
	bool operator!=(const SyntaxToken& other)const;

	// Inherited via SyntaxNode
	virtual SyntaxKind Kind() const override { return _kind; }
	virtual TextSpan Span()const override;
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	size_t Position() const { return _position; }
	string Text() const { return _text; }
	ValueType Value() const { return _value; }
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
	void Next() { _position++; }

	void ReadWhiteSpace();
	void ReadNumberToken();
	void ReadIdentifierOrKeyword();
public:
	explicit Lexer(const SourceText& text);
	~Lexer() = default;

	SyntaxToken Lex();
	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
};

#pragma region Expression

class ExpressionSyntax :public SyntaxNode
{
public:
	virtual ~ExpressionSyntax() = default;
	// Inherited via SyntaxNode
	virtual SyntaxKind Kind() const override { return SyntaxKind::BadToken; } // HACK
	virtual const vector<const SyntaxNode*> GetChildren() const override;
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
	virtual ~AssignmentExpressionSyntax() = default;
	AssignmentExpressionSyntax(AssignmentExpressionSyntax&& other);

	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::AssignmentExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IdentifierToken() const { return _identifierToken; }
	SyntaxToken EqualsToken() const { return _equalsToken; }
	const ExpressionSyntax* Expression() const { return _expression.get(); }
};

class UnaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _operand;
public:
	UnaryExpressionSyntax(const SyntaxToken& operatorToken, const unique_ptr<ExpressionSyntax>& operand);
	virtual ~UnaryExpressionSyntax() = default;
	UnaryExpressionSyntax(UnaryExpressionSyntax&& other);

	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::UnaryExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OperatorToken() const { return _operatorToken; }
	const ExpressionSyntax* Operand() const { return _operand.get(); }
};

class BinaryExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _operatorToken;
	unique_ptr<ExpressionSyntax> _left;
	unique_ptr<ExpressionSyntax> _right;
public:
	BinaryExpressionSyntax(const unique_ptr<ExpressionSyntax>& left, const SyntaxToken& operatorToken, const unique_ptr<ExpressionSyntax>& right);
	virtual ~BinaryExpressionSyntax() = default;
	BinaryExpressionSyntax(BinaryExpressionSyntax&& other);

	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::BinaryExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OperatorToken() const { return _operatorToken; }
	const ExpressionSyntax* Left()const { return _left.get(); }
	const ExpressionSyntax* Right()const { return _right.get(); }
};

class ParenthesizedExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _openParenthesisToken;
	SyntaxToken _closeParenthesisToken;
	unique_ptr<ExpressionSyntax> _expression;
public:
	ParenthesizedExpressionSyntax(const SyntaxToken& open, const unique_ptr<ExpressionSyntax>& expression, const SyntaxToken& close);
	virtual ~ParenthesizedExpressionSyntax() = default;
	ParenthesizedExpressionSyntax(ParenthesizedExpressionSyntax&& other);
	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::ParenthesizedExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OpenParenthesisToken()const { return _openParenthesisToken; }
	SyntaxToken CloseParenthesisToken() const { return _closeParenthesisToken; }
	const ExpressionSyntax* Expression()const { return _expression.get(); }
};

class LiteralExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _literalToken;
	ValueType _value;
public:
	LiteralExpressionSyntax(const SyntaxToken& literalToken, const ValueType& value);
	explicit LiteralExpressionSyntax(const SyntaxToken& literalToken);
	virtual ~LiteralExpressionSyntax() = default;
	LiteralExpressionSyntax(LiteralExpressionSyntax&& other);
	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::LiteralExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken LiteralToken()const { return _literalToken; }
	ValueType Value()const { return _value; }
};

class NameExpressionSyntax final :public ExpressionSyntax
{
private:
	SyntaxToken _identifierToken;
public:
	explicit NameExpressionSyntax(const SyntaxToken& identifier);
	virtual ~NameExpressionSyntax() = default;
	NameExpressionSyntax(NameExpressionSyntax&& other);
	// Inherited via ExpressionSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::NameExpression; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IdentifierToken()const { return _identifierToken; }
};

#pragma endregion

#pragma region Statement

class StatementSyntax :public SyntaxNode
{
public:
	virtual ~StatementSyntax() = default;
	// Inherited via SyntaxNode
	virtual SyntaxKind Kind() const override { return SyntaxKind::BadToken; } // HACK
	virtual const vector<const SyntaxNode*> GetChildren() const override;
};

class BlockStatementSyntax final : public StatementSyntax
{
private:
	SyntaxToken _openBraceToken;
	SyntaxToken _closeBraceToken;
	vector<unique_ptr<StatementSyntax>> _statements;

public:
	BlockStatementSyntax(const SyntaxToken& open, const vector<unique_ptr<StatementSyntax>>& statements, const SyntaxToken& close);
	virtual ~BlockStatementSyntax() = default;
	BlockStatementSyntax(BlockStatementSyntax&& other);

	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::BlockStatement; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

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
	virtual ~VariableDeclarationSyntax() = default;
	VariableDeclarationSyntax(VariableDeclarationSyntax&& other);

	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::VariableDeclaration; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken Keyword()const { return _keyword; }
	SyntaxToken Identifier()const { return _identifier; }
	SyntaxToken EqualsToken()const { return _equalsToken; }
	const ExpressionSyntax* Initializer()const { return _initializer.get(); }
};

class ElseClauseSyntax final :public SyntaxNode
{
private:
	SyntaxToken _elseKeyword;
	unique_ptr<StatementSyntax> _elseStatement;
public:
	ElseClauseSyntax(const SyntaxToken& elseKeyword, const unique_ptr<StatementSyntax>& elseStatement);
	virtual ~ElseClauseSyntax() = default;
	ElseClauseSyntax(ElseClauseSyntax&&) = default;

	// Inherited via SyntaxNode
	virtual SyntaxKind Kind() const override { return SyntaxKind::ElseClause; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken ElseKeyword()const { return _elseKeyword; }
	const StatementSyntax* ElseStatement()const { return _elseStatement.get(); }
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
	virtual ~IfStatementSyntax() = default;
	IfStatementSyntax(IfStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::IfStatement; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken IfKeyword()const { return _ifKeyword; }
	const ExpressionSyntax* Condition()const { return _condition.get(); }
	const StatementSyntax* ThenStatement()const { return _thenStatement.get(); }
	const ElseClauseSyntax* ElseClause()const { return _elseClause.get(); }
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
	virtual ~WhileStatementSyntax() = default;
	WhileStatementSyntax(WhileStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::WhileStatement; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken WhileKeyword()const { return _whileKeyword; }
	const ExpressionSyntax* Condition()const { return _condition.get(); }
	const StatementSyntax* Body()const { return _body.get(); }
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
	virtual ~ForStatementSyntax() = default;
	ForStatementSyntax(ForStatementSyntax&&) = default;
	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::ForStatement; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken Keyword() const { return _keyword; }
	SyntaxToken Identifier() const { return _identifier; }
	SyntaxToken EqualsToken()const { return _equalsToken; }
	const ExpressionSyntax* LowerBound()const { return _lowerBound.get(); }
	SyntaxToken ToKeyword()const { return _toKeyword; }
	const ExpressionSyntax* UpperBound()const { return _upperBound.get(); }
	const StatementSyntax* Body()const { return _body.get(); }
};

class ExpressionStatementSyntax final : public StatementSyntax
{
private:
	unique_ptr<ExpressionSyntax> _expression;

public:
	ExpressionStatementSyntax(const unique_ptr<ExpressionSyntax>& expression);
	virtual ~ExpressionStatementSyntax() = default;
	ExpressionStatementSyntax(ExpressionStatementSyntax&& other);

	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::ExpressionStatement; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	const ExpressionSyntax* Expression()const { return _expression.get(); }
};

#pragma endregion

class CompilationUnitSyntax final :public SyntaxNode
{
private:
	unique_ptr<StatementSyntax> _statement;
	SyntaxToken _endOfFileToken;

public:
	CompilationUnitSyntax(const unique_ptr<StatementSyntax>& statement, const SyntaxToken& endOfFile);
	virtual ~CompilationUnitSyntax() = default;
	CompilationUnitSyntax(CompilationUnitSyntax&& other);

	// Inherited via SyntaxNode
	virtual SyntaxKind Kind() const override { return SyntaxKind::CompilationUnit; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	const StatementSyntax* Statement()const { return _statement.get(); }
	SyntaxToken EndOfFileToken()const { return _endOfFileToken; }
};

class Parser final
{
private:
	const SourceText* _text;
	vector<unique_ptr<SyntaxToken>> _tokens;
	size_t _position;
	unique_ptr<DiagnosticBag> _diagnostics;

	SyntaxToken* Peek(int offset) const;
	SyntaxToken* Current() const;
	SyntaxToken NextToken();
	SyntaxToken MatchToken(SyntaxKind kind);

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
	unique_ptr<ExpressionSyntax> ParsePrimaryExpression();

	unique_ptr<ExpressionSyntax> ParseParenthesizedExpression();
	unique_ptr<ExpressionSyntax> ParseBooleanLiteral();
	unique_ptr<ExpressionSyntax> ParseNumberLiteral();
	unique_ptr<ExpressionSyntax> ParseNameExpression();

public:
	explicit Parser(const SourceText& text);
	~Parser();

	DiagnosticBag* Diagnostics()const { return _diagnostics.get(); }
	unique_ptr<CompilationUnitSyntax> ParseCompilationUnit();
};

class MCF_API SyntaxTree final
{
private:
	unique_ptr<SourceText> _text;
	unique_ptr<DiagnosticBag> _diagnostics;
	unique_ptr<CompilationUnitSyntax> _root;

	SyntaxTree(const SourceText& text);

public:
	~SyntaxTree();
	SyntaxTree(SyntaxTree&& other);

	const SourceText& Text() const { return *_text; }
	const CompilationUnitSyntax* Root()const { return _root.get(); }
	DiagnosticBag* Diagnostics() const { return _diagnostics.get(); }

	static SyntaxTree Parse(const string& text);
	static SyntaxTree Parse(const SourceText& text);
	static vector<unique_ptr<SyntaxToken>> ParseTokens(const string& text);
	static vector<unique_ptr<SyntaxToken>> ParseTokens(const SourceText& text);
};

}//MCF
