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

}//MCF
