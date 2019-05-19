#pragma once

#include "SyntaxToken.h"

namespace MCF {

class DiagnosticBag;
class SourceText;

#pragma region expression

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
							   unique_ptr<ExpressionSyntax>& expression);
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
	UnaryExpressionSyntax(const SyntaxToken& operatorToken,
						  unique_ptr<ExpressionSyntax>& operand);
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
	BinaryExpressionSyntax(unique_ptr<ExpressionSyntax>& left,
						   const SyntaxToken& operatorToken,
						   unique_ptr<ExpressionSyntax>& right);
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
	ParenthesizedExpressionSyntax(const SyntaxToken& open,
								  unique_ptr<ExpressionSyntax>& expression,
								  const SyntaxToken& close);
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
	SeparatedSyntaxList(vector<unique_ptr<SyntaxNode>>& list)
		:_nodesAndSeparators(std::move(list))
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
		return dynamic_cast<const T*>(_nodesAndSeparators.at(index * 2).get());
	}

	const SyntaxToken* GetSeparator(size_t index)const
	{
		if (index == size() - 1) return nullptr;
		return dynamic_cast<const SyntaxToken*>(_nodesAndSeparators.at(index * 2 + 1).get());
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
						 SeparatedSyntaxList<ExpressionSyntax>& arguments,
						 const SyntaxToken& close);
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
	PostfixExpressionSyntax(const SyntaxToken& identifier, const SyntaxToken& op,
							unique_ptr<ExpressionSyntax>& expression);
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
#pragma region statement

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
	BlockStatementSyntax(const SyntaxToken& open,
						 vector<unique_ptr<StatementSyntax>>& statements,
						 const SyntaxToken& close);
	BlockStatementSyntax(BlockStatementSyntax&&) = default;
	BlockStatementSyntax& operator=(BlockStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::BlockStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken OpenBraceToken()const { return _openBraceToken; }
	SyntaxToken CloseBraceToken()const { return _closeBraceToken; }
	const vector<const StatementSyntax*> Statements()const;
};

class TypeClauseSyntax final :public SyntaxNode
{
private:
	SyntaxToken _colonToken;
	SyntaxToken _identifier;

public:
	TypeClauseSyntax(const SyntaxToken& colon, const SyntaxToken& identifier);

	// Inherited via SyntaxNode
	SyntaxKind Kind() const override { return SyntaxKind::TypeClause; };
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken ColonToken()const { return _colonToken; }
	SyntaxToken Identifier()const { return _identifier; }
};

class VariableDeclarationSyntax final : public StatementSyntax
{
private:
	SyntaxToken _keyword;
	SyntaxToken _identifier;
	unique_ptr<TypeClauseSyntax> _typeClause;
	SyntaxToken _equalsToken;
	unique_ptr<ExpressionSyntax> _initializer;
public:
	VariableDeclarationSyntax(const SyntaxToken& keyword, const SyntaxToken& identifier,
							  unique_ptr<TypeClauseSyntax>& typeClause,
							  const SyntaxToken& equals,
							  unique_ptr<ExpressionSyntax>& initializer);
	VariableDeclarationSyntax(VariableDeclarationSyntax&&) = default;
	VariableDeclarationSyntax& operator=(VariableDeclarationSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::VariableDeclaration; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken Keyword()const { return _keyword; }
	SyntaxToken Identifier()const { return _identifier; }
	const TypeClauseSyntax* TypeClause()const { return _typeClause.get(); }
	SyntaxToken EqualsToken()const { return _equalsToken; }
	const ExpressionSyntax* Initializer()const noexcept { return _initializer.get(); }
};

class ElseClauseSyntax final :public SyntaxNode
{
private:
	SyntaxToken _elseKeyword;
	unique_ptr<StatementSyntax> _elseStatement;

public:
	ElseClauseSyntax(const SyntaxToken& elseKeyword, unique_ptr<StatementSyntax>& elseStatement);
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
	IfStatementSyntax(const SyntaxToken& ifKeyword,
					  unique_ptr<ExpressionSyntax>& condition,
					  unique_ptr<StatementSyntax>& thenStatement,
					  unique_ptr<ElseClauseSyntax>& elseClause);
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
	WhileStatementSyntax(const SyntaxToken& whileKeyword,
						 unique_ptr<ExpressionSyntax>& condition,
						 unique_ptr<StatementSyntax>& body);
	WhileStatementSyntax(WhileStatementSyntax&&) = default;
	WhileStatementSyntax& operator=(WhileStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::WhileStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken WhileKeyword()const { return _whileKeyword; }
	const ExpressionSyntax* Condition()const noexcept { return _condition.get(); }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
};

class DoWhileStatementSyntax final :public StatementSyntax
{
private:
	SyntaxToken _doKeyword;
	unique_ptr<StatementSyntax> _body;
	SyntaxToken _whileKeyword;
	unique_ptr<ExpressionSyntax> _condition;

public:
	DoWhileStatementSyntax(const SyntaxToken& doKeyword,
						   unique_ptr<StatementSyntax>& body,
						   const SyntaxToken& whileKeyword,
						   unique_ptr<ExpressionSyntax>& condition);
	DoWhileStatementSyntax(DoWhileStatementSyntax&&) = default;
	DoWhileStatementSyntax& operator=(DoWhileStatementSyntax&&) = default;


	// Inherited via StatementSyntax
	virtual SyntaxKind Kind() const override { return SyntaxKind::DoWhileStatement; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken DoKeyword()const { return _doKeyword; }
	const StatementSyntax* Body()const noexcept { return _body.get(); }
	SyntaxToken WhileKeyword()const { return _whileKeyword; }
	const ExpressionSyntax* Condition()const noexcept { return _condition.get(); }
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
	ForStatementSyntax(const SyntaxToken& keyword, const SyntaxToken& identifier,
					   const SyntaxToken& equals, unique_ptr<ExpressionSyntax>& lowerBound,
					   const SyntaxToken& toKeyword, unique_ptr<ExpressionSyntax>& upperBound,
					   unique_ptr<StatementSyntax>& body);
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
	ExpressionStatementSyntax(unique_ptr<ExpressionSyntax>& expression);
	ExpressionStatementSyntax(ExpressionStatementSyntax&&) = default;
	ExpressionStatementSyntax& operator=(ExpressionStatementSyntax&&) = default;

	// Inherited via StatementSyntax
	SyntaxKind Kind() const noexcept override { return SyntaxKind::ExpressionStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const ExpressionSyntax* Expression()const noexcept { return _expression.get(); }
};

#pragma endregion

class MemberSyntax :public SyntaxNode
{
};

class ParameterSyntax final :public SyntaxNode
{
private:
	SyntaxToken _identifier;
	unique_ptr<TypeClauseSyntax> _type;

public:
	ParameterSyntax(const SyntaxToken& identifier, unique_ptr<TypeClauseSyntax>& type);

	// Inherited via SyntaxNode
	virtual SyntaxKind Kind() const override { return SyntaxKind::Parameter; }
	virtual const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken Identifier()const { return _identifier; }
	const TypeClauseSyntax* Type()const noexcept { return _type.get(); }
};

class FunctionDeclarationSyntax :public MemberSyntax
{
private:
	SyntaxToken _funcKeyword;
	SyntaxToken _identifier;
	SyntaxToken _openParenthesisToken;
	SeparatedSyntaxList<ParameterSyntax> _parameters;
	SyntaxToken _closeParenthesisToken;
	unique_ptr<TypeClauseSyntax> _type;
	unique_ptr<BlockStatementSyntax> _body;

public:
	FunctionDeclarationSyntax(const SyntaxToken& funcKeyword, const SyntaxToken& identifier,
							  const SyntaxToken& openParenthesisToken,
							  SeparatedSyntaxList<ParameterSyntax>& params,
							  const SyntaxToken& closeParenthesisToken,
							  unique_ptr<TypeClauseSyntax>& type,
							  unique_ptr<BlockStatementSyntax>& body);
	FunctionDeclarationSyntax(FunctionDeclarationSyntax&&) = default;
	FunctionDeclarationSyntax& operator=(FunctionDeclarationSyntax&&) = default;
	// Inherited via MemberSyntax
	SyntaxKind Kind() const override { return SyntaxKind::FunctionDeclaration; }
	const vector<const SyntaxNode*> GetChildren() const override;

	SyntaxToken FunctionKeyword()const { return _funcKeyword; }
	SyntaxToken Identifier()const { return _identifier; }
	SyntaxToken OpenParenthesisToken()const { return _openParenthesisToken; }
	decltype(auto) Parameters()const noexcept { return &_parameters; }
	SyntaxToken CloseParenthesisToken()const { return _closeParenthesisToken; }
	const TypeClauseSyntax* Type()const noexcept { return _type.get(); }
	const BlockStatementSyntax* Body()const noexcept { return _body.get(); }
};

class GlobalStatementSyntax final :public MemberSyntax
{
private:
	unique_ptr<StatementSyntax> _statement;

public:
	GlobalStatementSyntax(unique_ptr<StatementSyntax>& statement);
	GlobalStatementSyntax(GlobalStatementSyntax&&) = default;
	GlobalStatementSyntax& operator=(GlobalStatementSyntax&&) = default;

	// Inherited via MemberSyntax
	SyntaxKind Kind() const override { return SyntaxKind::GlobalStatement; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const StatementSyntax* Statement()const noexcept { return _statement.get(); }
};

class CompilationUnitSyntax final :public SyntaxNode
{
private:
	vector<unique_ptr<MemberSyntax>> _members;
	SyntaxToken _endOfFileToken;

public:
	CompilationUnitSyntax(vector<unique_ptr<MemberSyntax>>& members,
						  const SyntaxToken& endOfFile);
	CompilationUnitSyntax(CompilationUnitSyntax&&) = default;
	CompilationUnitSyntax& operator=(CompilationUnitSyntax&&) = default;

	// Inherited via SyntaxNode
	SyntaxKind Kind() const noexcept override { return SyntaxKind::CompilationUnit; }
	const vector<const SyntaxNode*> GetChildren() const override;

	const vector<unique_ptr<MemberSyntax>>& Members()const noexcept { return _members; }
	SyntaxToken EndOfFileToken()const { return _endOfFileToken; }
};

class Parser final
{
private:
	[[maybe_unused]] const SourceText* _text;
	vector<SyntaxToken> _tokens;
	size_t _position;
	unique_ptr<DiagnosticBag> _diagnostics;

	const SyntaxToken* Peek(int offset) const;
	const SyntaxToken* Current() const;
	SyntaxToken NextToken();
	SyntaxToken MatchToken(const SyntaxKind& kind);

	vector<unique_ptr<MemberSyntax>> ParseMembers();
	unique_ptr<MemberSyntax> ParseMember();
	unique_ptr<MemberSyntax> ParseFunctionDeclaration();
	SeparatedSyntaxList<ParameterSyntax> ParseParameterList();
	unique_ptr<ParameterSyntax> ParseParameter();
	unique_ptr<MemberSyntax> ParseGlobalStatement();

	unique_ptr<StatementSyntax> ParseStatement();
	unique_ptr<BlockStatementSyntax> ParseBlockStatement();
	unique_ptr<StatementSyntax> ParseVariableDeclaration();
	unique_ptr<TypeClauseSyntax> ParseOptionalTypeClause();
	unique_ptr<TypeClauseSyntax> ParseTypeClause();
	unique_ptr<StatementSyntax> ParseIfStatement();
	unique_ptr<ElseClauseSyntax> ParseElseClause();
	unique_ptr<StatementSyntax> ParseWhileStatement();
	unique_ptr<StatementSyntax> ParseDoWhileStatement();
	unique_ptr<StatementSyntax> ParseForStatement();
	unique_ptr<ExpressionStatementSyntax> ParseExpressionStatement();

	unique_ptr<ExpressionSyntax> ParseExpression();
	unique_ptr<ExpressionSyntax> ParseAssignmentExpression();
	unique_ptr<ExpressionSyntax> ParseBinaryExpression(int parentPrecedence = 0);
	unique_ptr<ExpressionSyntax> ParsePostfixExpression(unique_ptr<ExpressionSyntax>& expression);

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
