#include "BoundStatements.h"

#include "BoundExpressions.h"

namespace MCF {

unique_ptr<BoundStatement> BoundBlockStatement::Clone()const
{
	auto stmts = vector<unique_ptr<BoundStatement>>{};
	for (const auto& s : Statements)
		stmts.push_back(s->Clone());
	return make_unique<BoundBlockStatement>(Syntax(), std::move(stmts));
}

bool BoundBlockStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& b = static_cast<const BoundBlockStatement&>(other);
		if (Statements.size() == b.Statements.size())
			return std::equal(Statements.cbegin(), Statements.cend(),
							  b.Statements.cbegin(),
							  [](const auto& it1, const auto& it2)
							  {
								  return *it1 == *it2;
							  });
	}
	return false;
}

unique_ptr<BoundStatement> BoundVariableDeclaration::Clone()const
{
	return make_unique<BoundVariableDeclaration>(Syntax(),
												 Variable->UniqueCloneAs<VariableSymbol>(),
												 Initializer->Clone());
}

bool BoundVariableDeclaration::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& v = static_cast<const BoundVariableDeclaration&>(other);
		return *Variable == *v.Variable && *Initializer == *v.Initializer;
	}
	return false;
}

unique_ptr<BoundStatement> BoundIfStatement::Clone()const
{
	if (ElseStatement)
		return make_unique<BoundIfStatement>(Syntax(), Condition->Clone(),
											 ThenStatement->Clone(), ElseStatement->Clone());
	else return make_unique<BoundIfStatement>(Syntax(), Condition->Clone(),
											  ThenStatement->Clone(), nullptr);

}

bool BoundIfStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& i = static_cast<const BoundIfStatement&>(other);
		if (*Condition == *i.Condition && *ThenStatement == *i.ThenStatement)
		{
			if (ElseStatement && i.ElseStatement)
				return *ElseStatement == *i.ElseStatement;
			else return !ElseStatement && !i.ElseStatement;
		}
	}
	return false;
}

unique_ptr<BoundStatement> BoundWhileStatement::Clone()const
{
	return make_unique<BoundWhileStatement>(Syntax(), Condition->Clone(), Body->Clone(),
											BreakLabel, ContinueLabel);
}

bool BoundWhileStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& w = static_cast<const BoundWhileStatement&>(other);
		return BreakLabel == w.BreakLabel && ContinueLabel == w.ContinueLabel
			&& *Condition == *w.Condition && *Body == *w.Body;
	}
	return false;
}

unique_ptr<BoundStatement> BoundDoWhileStatement::Clone()const
{
	return make_unique<BoundDoWhileStatement>(Syntax(), Body->Clone(), Condition->Clone(),
											  BreakLabel, ContinueLabel);
}

bool BoundDoWhileStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& d = static_cast<const BoundDoWhileStatement&>(other);
		return BreakLabel == d.BreakLabel && ContinueLabel == d.ContinueLabel
			&& *Condition == *d.Condition && *Body == *d.Body;
	}
	return false;
}

unique_ptr<BoundStatement> BoundForStatement::Clone()const
{
	return make_unique<BoundForStatement>(Syntax(), Variable->UniqueCloneAs<VariableSymbol>(),
										  LowerBound->Clone(), UpperBound->Clone(),
										  Body->Clone(), BreakLabel, ContinueLabel);
}

bool BoundForStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& f = static_cast<const BoundForStatement&>(other);
		return BreakLabel == f.BreakLabel && ContinueLabel == f.ContinueLabel
			&& *Variable == *f.Variable && *Body == *f.Body
			&& *LowerBound == *f.LowerBound && *UpperBound == *f.UpperBound;
	}
	return false;
}

bool BoundLabelStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& l = static_cast<const BoundLabelStatement&>(other);
		return Label == l.Label;
	}
	return false;
}

bool BoundGotoStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& g = static_cast<const BoundGotoStatement&>(other);
		return Label == g.Label;
	}
	return false;
}

unique_ptr<BoundStatement> BoundConditionalGotoStatement::Clone()const
{
	return make_unique<BoundConditionalGotoStatement>(Syntax(), Label,
													  Condition->Clone(), JumpIfTrue);
}

bool BoundConditionalGotoStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& c = static_cast<const BoundConditionalGotoStatement&>(other);
		return Label == c.Label && JumpIfTrue == c.JumpIfTrue
			&& *Condition == *c.Condition;
	}
	return false;
}

unique_ptr<BoundStatement> BoundReturnStatement::Clone()const
{
	if (Expression)
		return make_unique<BoundReturnStatement>(Syntax(), Expression->Clone());
	else return make_unique<BoundReturnStatement>(Syntax(), nullptr);
}

bool BoundReturnStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& r = static_cast<const BoundReturnStatement&>(other);
		if (Expression && r.Expression)
			return *Expression == *r.Expression;
		else return !Expression && !r.Expression;
	}
	return false;
}

unique_ptr<BoundStatement> BoundExpressionStatement::Clone()const
{
	return make_unique<BoundExpressionStatement>(Syntax(), Expression->Clone());
}

bool BoundExpressionStatement::Equals(const BoundStatement& other)const noexcept
{
	if (BoundStatement::Equals(other))
	{
		auto& e = static_cast<const BoundExpressionStatement&>(other);
		return *Expression == *e.Expression;
	}
	return false;
}

} // MCF