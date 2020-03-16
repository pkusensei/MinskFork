#pragma once

#include <string>
#include <string_view>
#include <vector>

namespace MCF {
enum class SyntaxKind;
class SyntaxNode;
}

class AssertingHelper final
{
private:
	std::vector<const MCF::SyntaxNode*> _nodes;
	bool _hasErrors;
	size_t _position;

	bool MarkFailed()
	{
		_hasErrors = true;
		return false;
	}

public:
	explicit AssertingHelper(const MCF::SyntaxNode* node);
	~AssertingHelper();
	void AssertNode(MCF::SyntaxKind kind);
	void AssertToken(MCF::SyntaxKind kind, std::string_view text);
};
