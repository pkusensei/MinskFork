#pragma once

#include <optional>
#include <unordered_map>

#include "BoundLabel.h"

namespace MCF {

struct BoundExpression;
struct BoundStatement;
struct BoundBlockStatement;

struct ControlFlowGraph final
{
	struct BasicBlock;
	struct BasicBlockBranch;
	class BasicBlockBuilder;
	class GraphBuilder;

	vector<unique_ptr<BasicBlock>> Blocks;
	vector<unique_ptr<BasicBlockBranch>> Branches;
	BasicBlock* Start;
	BasicBlock* End;

private:
	explicit ControlFlowGraph(BasicBlock* start, BasicBlock* end,
							  vector<unique_ptr<BasicBlock>> blocks,
							  vector<unique_ptr<BasicBlockBranch>> branches)noexcept
		:Blocks(std::move(blocks)), Branches(std::move(branches)),
		Start(start), End(end)
	{
	}

public:

	void WriteTo(std::ostream& out)const;

	[[nodiscard]] static ControlFlowGraph Create(const BoundBlockStatement* body);
	[[nodiscard]] static bool AllPathsReturn(const BoundBlockStatement* body);
};

struct ControlFlowGraph::BasicBlock final
{
	vector<BoundStatement*> Statements;
	vector<BasicBlockBranch*> Incoming;
	vector<BasicBlockBranch*> Outgoing;

	int Id;
	bool IsStart;
	bool IsEnd;

public:
	explicit BasicBlock(int id = 0, bool isStart = false, bool isEnd = false)noexcept
		: Id(id), IsStart(isStart), IsEnd(isEnd)
	{
	}

	string ToString()const;
	constexpr bool operator==(const BasicBlock& other)const noexcept { return Id == other.Id; }
	constexpr bool operator!=(const BasicBlock& other)const noexcept { return !(*this == other); }

};

struct ControlFlowGraph::BasicBlockBranch final
{
	shared_ptr<BoundExpression> Condition;
	BasicBlock* From;
	BasicBlock* To;

public:
	BasicBlockBranch(BasicBlock* from, BasicBlock* to,
					 shared_ptr<BoundExpression> condition = nullptr)noexcept
		:Condition(std::move(condition)), From(from), To(to)
	{
	}

	string ToString()const;
	constexpr bool operator==(const BasicBlockBranch& other)const noexcept
	{
		return From == other.From && To == other.To;
	}
	constexpr bool operator!=(const BasicBlockBranch& other)const noexcept
	{
		return !(*this == other);
	}

};

}//MCF
