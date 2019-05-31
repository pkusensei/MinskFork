#pragma once

#include <optional>

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

		vector<BoundStatement*> _statements;
		vector<BasicBlockBranch*> _incoming;
		vector<BasicBlockBranch*> _outgoing;

	public:
		explicit BasicBlock(int id = -1, bool isStart = false)
			: _id(id), _isStart(isStart)
		{
		}

		bool IsStart()const noexcept { return _isStart; }
		bool IsEnd()const noexcept { return !_isStart; }

		vector<BoundStatement*>& Statements() noexcept { return _statements; }
		vector<BasicBlockBranch*>& Incoming()noexcept { return _incoming; }
		vector<BasicBlockBranch*>& Outgoing()noexcept { return _outgoing; }

		string ToString()const;
		bool operator==(const BasicBlock& other)const noexcept { return _id == other._id; }
		bool operator!=(const BasicBlock& other)const noexcept { return !(*this == other); }

	};

	class BasicBlockBranch final
	{
	private:
		BasicBlock* _from;
		BasicBlock* _to;
		shared_ptr<BoundExpression> _condition;

	public:
		BasicBlockBranch(BasicBlock& from, BasicBlock& to,
			const shared_ptr<BoundExpression>& condition = nullptr)
			:_from(&from), _to(&to), _condition(condition)
		{
		}

		BasicBlock* From() const noexcept { return _from; }
		BasicBlock* To() const noexcept { return _to; }
		const shared_ptr<BoundExpression>& Condition()const noexcept { return _condition; }

		string ToString()const;
		bool operator==(const BasicBlockBranch& other)const noexcept
		{
			return _from == other._from && _to == other._to;
		}
		bool operator!=(const BasicBlockBranch& other)const noexcept { return !(*this == other); }

	};

private:
	BasicBlock _start;
	BasicBlock _end;
	vector<BasicBlock> _blocks;
	vector<BasicBlockBranch> _branches;

	ControlFlowGraph(BasicBlock& start, BasicBlock& end, vector<BasicBlock>& blocks,
		vector<BasicBlockBranch> branches);
	void WriteTo(std::ostream& out)const;
};

class ControlFlowGraph::BasicBlockBuilder final
{
private:
	int _blockId = 0;
	vector<BoundStatement*> _statements;
	vector<BasicBlock> _blocks;

	void EndBlock();
	void StartBlock() { EndBlock(); }

public:
	vector<BasicBlock> Build(const BoundBlockStatement* block);
};

class ControlFlowGraph::GraphBuilder final
{
private:
	std::unordered_map<BoundStatement*, BasicBlock> _blockFromStatement;
	std::unordered_map<BoundLabel, BasicBlock, LabelHash> _blockFromLabel;
	vector<BasicBlockBranch> _branches;
	BasicBlock _start;
	BasicBlock _end;

	void Connect(BasicBlock& from, BasicBlock& to,
		const std::optional<shared_ptr<BoundExpression>>& condition = std::nullopt);
	void RemoveBlock(vector<BasicBlock>& blocks, BasicBlock& block);
	shared_ptr<BoundExpression> Negate(const shared_ptr<BoundExpression>& condition)const;

public:
	GraphBuilder() :_start(BasicBlock(0, true)), _end(BasicBlock(-1)) {}
	ControlFlowGraph Build(vector<BasicBlock>& blocks);
};

}//MCF
