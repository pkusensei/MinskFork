#include "ControlFlowGraph.h"

#include <algorithm>
#include <sstream>

#include "BoundExpressions.h"
#include "BoundNodePrinter.h"
#include "BoundStatements.h"
#include "helpers.h"
#include "SyntaxKind.h"

namespace MCF {

string ControlFlowGraph::BasicBlock::ToString() const
{
	if (IsStart())
		return "<Start>";
	else if (IsEnd())
		return "<End>";
	else
	{
		auto s = std::ostringstream();
		auto writer = BoundNodePrinter(s);
		for (const auto& it : _statements)
			writer.Write(it);
		return s.str();
	}
}

string ControlFlowGraph::BasicBlockBranch::ToString() const
{
	if (_condition == nullptr)
		return string();
	return _condition->ToString();
}

void ControlFlowGraph::BasicBlockBuilder::EndBlock()
{
	if (!_statements.empty())
	{
		//NOTE using id to add == support
		//intends to have start as 1, end as 0
		auto block = make_unique<BasicBlock>(++_blockId);
		auto& s = block->Statements();
		s.insert(s.end(), _statements.begin(), _statements.end());
		_blocks.push_back(std::move(block));
		_statements.clear();
	}
}

vector<unique_ptr<ControlFlowGraph::BasicBlock>>
ControlFlowGraph::BasicBlockBuilder::Build(const BoundBlockStatement* block)
{
	for (const auto& statement : block->Statements())
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
			case BoundNodeKind::VariableDeclaration:
			case BoundNodeKind::ExpressionStatement:
				_statements.push_back(statement.get());
				break;
			default:
				throw std::invalid_argument(BuildStringFrom("Unexpected statement"
					, nameof(statement->Kind())));
		}
	}
	EndBlock();
	return std::move(_blocks);
}

void ControlFlowGraph::GraphBuilder::Connect(BasicBlock& from, BasicBlock& to,
	const std::optional<shared_ptr<BoundExpression>>& condition)
{
	auto ptr = condition.value_or(nullptr);
	auto p = std::dynamic_pointer_cast<BoundLiteralExpression>(ptr);
	if (p)
	{
		try
		{
			auto value = p->Value().ToBoolean();
			if (value)
				ptr = nullptr;
			else return;
		} catch (...)
		{
			return;
		}
	}

	auto branch = make_unique<BasicBlockBranch>(&from, &to, ptr);
	_branches.push_back(std::move(branch));
	auto& last = _branches.back();
	from.Outgoing().push_back(last.get());
	to.Incoming().push_back(last.get());
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

	for (const auto& branch : block.Incoming())
	{
		auto& outgoing = branch->From()->Outgoing();
		VectorErase_If(outgoing, capturePtrToErase(branch));
		VectorErase_If(_branches, capturePtrToErase(branch));
	}
	for (const auto& branch : block.Outgoing())
	{
		auto& incoming = branch->To()->Incoming();
		VectorErase_If(incoming, capturePtrToErase(branch));
		VectorErase_If(_branches, capturePtrToErase(branch));
	}
	VectorErase_If(blocks, capturePtrToErase(&block));
}

shared_ptr<BoundExpression> ControlFlowGraph::GraphBuilder::Negate(
	const shared_ptr<BoundExpression>& condition) const
{
	if (condition->Kind() == BoundNodeKind::LiteralExpression)
	{
		try
		{
			auto p = static_cast<const BoundLiteralExpression*>(condition.get());
			auto value = p->Value().ToBoolean();
			return make_shared<BoundLiteralExpression>(!value);
		} catch (...)
		{
			;//NOTE discard exception
		}
	}

	auto op = BoundUnaryOperator::Bind(SyntaxKind::BangToken, TypeSymbol::Get(TypeEnum::Bool));
	return make_shared<BoundUnaryExpression>(op, condition);
}

ControlFlowGraph ControlFlowGraph::GraphBuilder::Build(vector<unique_ptr<BasicBlock>>& blocks)
{
	if (blocks.empty())
		Connect(*_start, *_end);
	else
		Connect(*_start, *(blocks.front()));

	for (auto& block : blocks)
	{
		for (const auto& statement : block->Statements())
		{
			_blockFromStatement.emplace(statement, block.get());
			if (statement->Kind() == BoundNodeKind::LabelStatement)
			{
				auto p = static_cast<BoundLabelStatement*>(statement);
				_blockFromLabel.emplace(p->Label(), block.get());
			}
		}
	}

	for (size_t i = 0; i < blocks.size(); ++i)
	{
		auto& current = blocks.at(i);
		auto& next = i == blocks.size() - 1 ? _end : blocks.at(i + 1);

		for (const auto& statement : current->Statements())
		{
			auto isLastStatementInBlock = statement == current->Statements().back();
			switch (statement->Kind())
			{
				case BoundNodeKind::GotoStatement:
				{
					auto gs = static_cast<BoundGotoStatement*>(statement);
					auto& toBlock = _blockFromLabel.at(gs->Label());
					Connect(*current, *toBlock);
					break;
				}
				case BoundNodeKind::ConditionalGotoStatement:
				{
					auto cgs = static_cast<BoundConditionalGotoStatement*>(statement);
					auto& thenBlock = _blockFromLabel.at(cgs->Label());
					auto& elseBlock = next;
					auto negatedCondition = Negate(cgs->Condition());
					auto thenCondition = cgs->JumpIfTrue() ?
						cgs->Condition() : negatedCondition;
					auto elseCondition = cgs->JumpIfTrue() ?
						negatedCondition : cgs->Condition();
					Connect(*current, *thenBlock, thenCondition);
					Connect(*current, *elseBlock, elseCondition);
					break;
				}
				case BoundNodeKind::ReturnStatement:
					Connect(*current, *_end);
					break;
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
			if (block->Incoming().empty())
			{
				RemoveBlock(blocks, *block);
				loopAgain = true;
				break;
			}
		}
	}
	auto start = _start.get();
	auto end = _end.get();
	blocks.insert(blocks.begin(), std::move(_start));
	blocks.push_back(std::move(_end));
	return ControlFlowGraph(start, end, blocks, _branches);
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

	for (const auto& b : _blocks)
	{
		auto id = b->Id();
		auto label = b->ToString();
		label = quote(label);
		out << "    N" << id << " [label = " << label << ", shape = box]" << NEW_LINE;
	}
	for (const auto& b : _branches)
	{
		auto fromId = b->From()->Id();
		auto toId = b->To()->Id();
		auto label = b->ToString();
		label = quote(label);
		out << "    N" << fromId << " -> N" << toId << " [label = " << label << "]" << NEW_LINE;
	}
	out << '}' << NEW_LINE;
}

ControlFlowGraph ControlFlowGraph::Create(const BoundBlockStatement* body)
{
	auto blockBuilder = BasicBlockBuilder();
	auto blocks = blockBuilder.Build(body);
	auto graphBuilder = GraphBuilder();
	return graphBuilder.Build(blocks);
}

bool ControlFlowGraph::AllPathsReturn(const BoundBlockStatement* body)
{
	auto graph = Create(body);
	for (const auto& branch : graph.End()->Incoming())
	{
		auto& statements = branch->From()->Statements();
		if (statements.empty()) return false;

		auto& lastStatement = statements.back();
		if (lastStatement == nullptr
			|| lastStatement->Kind() != BoundNodeKind::ReturnStatement)
			return false;
	}
	return true;
}

}//MCF
