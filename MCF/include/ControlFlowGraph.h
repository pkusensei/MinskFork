#pragma once

#include <optional>
#include <unordered_map>

#include "BoundLabel.h"

namespace MCF {

class BoundExpression;
class BoundStatement;
class BoundBlockStatement;

class ControlFlowGraph final
{
public:
	class BasicBlock;
	class BasicBlockBranch;
	class BasicBlockBuilder;
	class GraphBuilder;

private:
	BasicBlock* _start;
	BasicBlock* _end;
	vector<unique_ptr<BasicBlock>> _blocks;
	vector<unique_ptr<BasicBlockBranch>> _branches;

	ControlFlowGraph(BasicBlock* start, BasicBlock* end,
		vector<unique_ptr<BasicBlock>>& blocks,
		vector<unique_ptr<BasicBlockBranch>>& branches);

public:
	constexpr BasicBlock* Start()const noexcept { return _start; }
	constexpr BasicBlock* End()const noexcept { return _end; }
	constexpr const vector<unique_ptr<BasicBlock>>& Blocks()const noexcept { return  _blocks; }
	constexpr const vector<unique_ptr<BasicBlockBranch>>& Branches()const noexcept { return _branches; }

	void WriteTo(std::ostream& out)const;

	static ControlFlowGraph Create(const BoundBlockStatement* body);
	static bool AllPathsReturn(const BoundBlockStatement* body);
};

class ControlFlowGraph::BasicBlock final
{
private:
	int _id;
	bool _isStart;
	bool _isEnd;

	vector<BoundStatement*> _statements;
	vector<BasicBlockBranch*> _incoming;
	vector<BasicBlockBranch*> _outgoing;

public:
	explicit BasicBlock(int id = 0, bool isStart = false, bool isEnd = false)noexcept
		: _id(id), _isStart(isStart), _isEnd(isEnd)
	{
	}

	constexpr int Id() const noexcept { return _id; }
	constexpr bool IsStart()const noexcept { return _isStart; }
	constexpr bool IsEnd()const noexcept { return _isEnd; }

	constexpr vector<BoundStatement*>& Statements() noexcept { return _statements; }
	constexpr vector<BasicBlockBranch*>& Incoming()noexcept { return _incoming; }
	constexpr vector<BasicBlockBranch*>& Outgoing()noexcept { return _outgoing; }

	string ToString()const;
	constexpr bool operator==(const BasicBlock& other)const noexcept { return _id == other._id; }
	constexpr bool operator!=(const BasicBlock& other)const noexcept { return !(*this == other); }

};

class ControlFlowGraph::BasicBlockBranch final
{
private:
	BasicBlock* _from;
	BasicBlock* _to;
	shared_ptr<BoundExpression> _condition;

public:
	BasicBlockBranch(BasicBlock* from, BasicBlock* to,
		const shared_ptr<BoundExpression>& condition = nullptr)
		:_from(from), _to(to), _condition(condition)
	{
	}

	constexpr BasicBlock* From() const noexcept { return _from; }
	constexpr BasicBlock* To() const noexcept { return _to; }
	constexpr const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }

	string ToString()const;
	constexpr bool operator==(const BasicBlockBranch& other)const noexcept
	{
		return _from == other._from && _to == other._to;
	}
	constexpr bool operator!=(const BasicBlockBranch& other)const noexcept { return !(*this == other); }

};

}//MCF
