#include "ControlFlowGraph.h"

#include <algorithm>
#include <cassert>
#include <sstream>

#include "BoundExpressions.h"
#include "BoundNodePrinter.h"
#include "BoundStatements.h"
#include "StringHelper.h"
#include "SyntaxKind.h"

namespace MCF {

string ControlFlowGraph::BasicBlock::ToString() const
{
	if (IsStart)
		return "<Start>";
	else if (IsEnd)
		return "<End>";
	else
	{
		auto s = std::ostringstream();
		for (const auto& it : Statements)
			Write(*it, s);
		return s.str();
	}
}

string ControlFlowGraph::BasicBlockBranch::ToString() const
{
	if (Condition == nullptr)
		return string();
	return Condition->ToString();
}

class ControlFlowGraph::BasicBlockBuilder final
{
private:
	int _blockId = 1;
	vector<BoundStatement*> _statements;
	vector<unique_ptr<BasicBlock>> _blocks;

	void EndBlock();
	void StartBlock() { EndBlock(); }

public:
	[[nodiscard]] vector<unique_ptr<BasicBlock>> Build(const BoundBlockStatement& block);
};

void ControlFlowGraph::BasicBlockBuilder::EndBlock()
{
	if (!_statements.empty())
	{
		// NOTE using id to add == support
		//      intends to have start as 1, end as 0
		auto block = make_unique<BasicBlock>(++_blockId);
		auto& s = block->Statements;
		s.insert(s.end(), _statements.begin(), _statements.end());
		_blocks.push_back(std::move(block));
		_statements.clear();
	}
}

vector<unique_ptr<ControlFlowGraph::BasicBlock>>
ControlFlowGraph::BasicBlockBuilder::Build(const BoundBlockStatement& block)
{
	for (const auto& statement : block.Statements)
	{
		switch (statement->Kind())
		{
			case BoundNodeKind::LabelStatement:
				StartBlock();
				_statements.push_back(statement.get());
				break;
			case BoundNodeKind::GotoStatement:
			case BoundNodeKind::ConditionalGotoStatement:
			case BoundNodeKind::ReturnStatement:
				_statements.push_back(statement.get());
				StartBlock();
				break;
			case BoundNodeKind::NopStatement:
			case BoundNodeKind::VariableDeclaration:
			case BoundNodeKind::ExpressionStatement:
				_statements.push_back(statement.get());
				break;
			default:
				throw std::invalid_argument(BuildStringFrom("Unexpected statement: "
															, nameof(statement->Kind())));
		}
	}
	EndBlock();
	return std::move(_blocks);
}

class ControlFlowGraph::GraphBuilder final
{
private:
	std::unordered_map<BoundStatement*, BasicBlock*> _blockFromStatement;
	std::unordered_map<BoundLabel, BasicBlock*, LabelHash> _blockFromLabel;
	vector<unique_ptr<BasicBlockBranch>> _branches;
	unique_ptr<BasicBlock> _start;
	unique_ptr<BasicBlock> _end;

	void Connect(BasicBlock& from, BasicBlock& to,
				 shared_ptr<BoundExpression> condition = nullptr);
	void RemoveBlock(vector<unique_ptr<BasicBlock>>& blocks, BasicBlock& block);
	shared_ptr<BoundExpression> Negate(const shared_ptr<BoundExpression>& condition)const;

public:
	explicit GraphBuilder()
		:_start(make_unique<BasicBlock>(1, true)),
		_end(make_unique<BasicBlock>(0, false, true))
	{
	}

	[[nodiscard]] ControlFlowGraph Build(vector<unique_ptr<BasicBlock>>& blocks);
};

void ControlFlowGraph::GraphBuilder::Connect(BasicBlock& from, BasicBlock& to,
											 shared_ptr<BoundExpression> condition)
{
	if (condition && condition->Kind() == BoundNodeKind::LiteralExpression)
	{
		auto p = std::static_pointer_cast<BoundLiteralExpression>(condition);
		if (p->Value().Type() == TYPE_BOOL && p->Value().ToBoolean())
		{
			condition = nullptr;
		} else
			return;
	}

	auto branch = make_unique<BasicBlockBranch>(from, to, std::move(condition));
	_branches.push_back(std::move(branch));
	auto& last = _branches.back();
	from.Outgoing.push_back(last.get());
	to.Incoming.push_back(last.get());
}

template<typename T, typename Pred>
void VectorErase_If(vector<T>& vec, Pred pred)
{
	auto it = std::remove_if(vec.begin(), vec.end(), pred);
	vec.erase(it, vec.end());
}

void ControlFlowGraph::GraphBuilder::RemoveBlock(vector<unique_ptr<BasicBlock>>& blocks,
												 BasicBlock& block)
{
	auto capturePtrToErase = [](const auto* p)
	{
		return [p = p](const auto& it) { return *it == *p; };
	};

	for (const auto& branch : block.Incoming)
	{
		auto& outgoing = branch->From->Outgoing;
		VectorErase_If(outgoing, capturePtrToErase(branch));
		VectorErase_If(_branches, capturePtrToErase(branch));
	}
	for (const auto& branch : block.Outgoing)
	{
		auto& incoming = branch->To->Incoming;
		VectorErase_If(incoming, capturePtrToErase(branch));
		VectorErase_If(_branches, capturePtrToErase(branch));
	}
	VectorErase_If(blocks, capturePtrToErase(&block));
}

shared_ptr<BoundExpression> ControlFlowGraph::GraphBuilder::Negate(
	const shared_ptr<BoundExpression>& condition) const
{
	assert(condition->Type() == TYPE_BOOL);

	auto op = BoundUnaryOperator::Bind(SyntaxKind::BangToken, TYPE_BOOL);
	assert(op.IsUseful);

	auto& s = condition->Syntax();
	auto negated = make_shared<BoundUnaryExpression>(s, op, condition);

	if (negated->ConstantValue().HasValue())
		return make_shared<BoundLiteralExpression>(s, negated->ConstantValue());
	return negated;
}

ControlFlowGraph ControlFlowGraph::GraphBuilder::Build(vector<unique_ptr<BasicBlock>>& blocks)
{
	if (blocks.empty())
		Connect(*_start, *_end);
	else
		Connect(*_start, *(blocks.front()));

	for (auto& block : blocks)
	{
		for (const auto& statement : block->Statements)
		{
			_blockFromStatement.emplace(statement, block.get());
			if (statement->Kind() == BoundNodeKind::LabelStatement)
			{
				auto p = static_cast<BoundLabelStatement*>(statement);
				_blockFromLabel.emplace(p->Label, block.get());
			}
		}
	}

	for (size_t i = 0; i < blocks.size(); ++i)
	{
		auto& current = blocks.at(i);
		auto& next = i == blocks.size() - 1 ? _end : blocks.at(i + 1);

		for (const auto& statement : current->Statements)
		{
			auto isLastStatementInBlock = statement == current->Statements.back();
			switch (statement->Kind())
			{
				case BoundNodeKind::GotoStatement:
				{
					auto gs = static_cast<BoundGotoStatement*>(statement);
					auto& toBlock = _blockFromLabel.at(gs->Label);
					Connect(*current, *toBlock);
					break;
				}
				case BoundNodeKind::ConditionalGotoStatement:
				{
					auto cgs = static_cast<BoundConditionalGotoStatement*>(statement);
					auto& thenBlock = _blockFromLabel.at(cgs->Label);
					auto& elseBlock = next;
					auto negatedCondition = Negate(cgs->Condition);
					auto thenCondition = cgs->JumpIfTrue ?
						cgs->Condition : negatedCondition;
					auto elseCondition = cgs->JumpIfTrue ?
						negatedCondition : cgs->Condition;
					Connect(*current, *thenBlock, std::move(thenCondition));
					Connect(*current, *elseBlock, std::move(elseCondition));
					break;
				}
				case BoundNodeKind::ReturnStatement:
					Connect(*current, *_end);
					break;
				case BoundNodeKind::NopStatement:
				case BoundNodeKind::VariableDeclaration:
				case BoundNodeKind::LabelStatement:
				case BoundNodeKind::ExpressionStatement:
				{
					if (isLastStatementInBlock)
						Connect(*current, *next);
					break;
				}
				default:
					throw std::invalid_argument(BuildStringFrom("Unexpected statement: "
																, nameof(statement->Kind())));
			}
		}
	}

	auto loopAgain = true;
	while (loopAgain)
	{
		loopAgain = false;
		for (auto& block : blocks)
		{
			if (block->Incoming.empty())
			{
				RemoveBlock(blocks, *block);
				loopAgain = true;
				break;
			}
		}
	}

	blocks.insert(blocks.begin(), std::move(_start));
	blocks.push_back(std::move(_end));
	return ControlFlowGraph{ *blocks.front(), *blocks.back(),
		std::move(blocks), std::move(_branches) };
}

void ControlFlowGraph::WriteTo(std::ostream& out) const
{
	auto quote = [](string& text)
	{
		auto result = string(TrimStringEnd(text));
		StringReplaceAll(result, "\\", "\\\\");
		StringReplaceAll(result, "\"", "\\\"");
		StringReplaceAll(result, &NEW_LINE, "\\l");
		return '"' + result + '"';
	};

	out << "digraph G {" << NEW_LINE;

	for (const auto& b : Blocks)
	{
		auto id = b->Id;
		auto label = b->ToString();
		label = quote(label);
		out << "    N" << id << " [label = " << label << ", shape = box]" << NEW_LINE;
	}
	for (const auto& b : Branches)
	{
		auto fromId = b->From->Id;
		auto toId = b->To->Id;
		auto label = b->ToString();
		label = quote(label);
		out << "    N" << fromId << " -> N" << toId << " [label = " << label << "]" << NEW_LINE;
	}
	out << '}' << NEW_LINE;
}

ControlFlowGraph ControlFlowGraph::Create(const BoundBlockStatement& body)
{
	auto blockBuilder = BasicBlockBuilder();
	auto blocks = blockBuilder.Build(body);
	auto graphBuilder = GraphBuilder();
	return graphBuilder.Build(blocks);
}

bool ControlFlowGraph::AllPathsReturn(const BoundBlockStatement& body)
{
	auto graph = Create(body);
	for (const auto& branch : graph.End->Incoming)
	{
		auto& statements = branch->From->Statements;
		if (statements.empty()) return false;

		auto& lastStatement = statements.back();
		if (lastStatement == nullptr
			|| lastStatement->Kind() != BoundNodeKind::ReturnStatement)
			return false;
	}
	return true;
}

}//MCF
