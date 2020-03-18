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
	class BasicBlockBranch;
	class BasicBlockBuilder;
	class GraphBuilder;

public:
	class BasicBlock final
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

	class BasicBlockBranch final
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

private:
	BasicBlock* _start;
	BasicBlock* _end;
	vector<unique_ptr<BasicBlock>> _blocks;
	vector<unique_ptr<BasicBlockBranch>> _branches;

	ControlFlowGraph(BasicBlock* start, BasicBlock* end,
		vector<unique_ptr<BasicBlock>>& blocks,
		vector<unique_ptr<BasicBlockBranch>>& branches)
		:_start(start), _end(end), _blocks(std::move(blocks)),
		_branches(std::move(branches))
	{
	}

public:
	constexpr BasicBlock* Start()const noexcept { return _start; }
	constexpr BasicBlock* End()const noexcept { return _end; }
	constexpr const vector<unique_ptr<BasicBlock>>& Blocks()const noexcept { return  _blocks; }
	constexpr const vector<unique_ptr<BasicBlockBranch>>& Branches()const noexcept { return _branches; }

	void WriteTo(std::ostream& out)const;

	static ControlFlowGraph Create(const BoundBlockStatement* body);
	static bool AllPathsReturn(const BoundBlockStatement* body);
};

class ControlFlowGraph::BasicBlockBuilder final
{
private:
	int _blockId = 1;
	vector<BoundStatement*> _statements;
	vector<unique_ptr<BasicBlock>> _blocks;

	void EndBlock();
	void StartBlock() { EndBlock(); }

public:
	vector<unique_ptr<BasicBlock>> Build(const BoundBlockStatement* block);
};

class ControlFlowGraph::GraphBuilder final
{
private:
	std::unordered_map<BoundStatement*, BasicBlock*> _blockFromStatement;
	std::unordered_map<BoundLabel, BasicBlock*, LabelHash> _blockFromLabel;
	vector<unique_ptr<BasicBlockBranch>> _branches;
	unique_ptr<BasicBlock> _start;
	unique_ptr<BasicBlock> _end;

	void Connect(BasicBlock& from, BasicBlock& to,
		const std::optional<shared_ptr<BoundExpression>>& condition = std::nullopt);
	void RemoveBlock(vector<unique_ptr<BasicBlock>>& blocks, BasicBlock& block);
	shared_ptr<BoundExpression> Negate(const shared_ptr<BoundExpression>& condition)const;

public:
	GraphBuilder()
		:_start(make_unique<BasicBlock>(1, true)),
		_end(make_unique<BasicBlock>(0, false, true))
	{
	}
	ControlFlowGraph Build(vector<unique_ptr<BasicBlock>>& blocks);

};

}//MCF
