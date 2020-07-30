#include "Authoring.h"

#include "Parsing.h"

using namespace MCF;

namespace {

Classification Classify(SyntaxKind kind)
{
	auto isKeyword = IsKeyword(kind);
	auto isIdentifier = kind == SyntaxKind::IdentifierToken;
	auto isNumber = kind == SyntaxKind::NumberToken;
	auto isString = kind == SyntaxKind::StringToken;
	auto isComment = IsComment(kind);

	if (isKeyword)
		return Classification::Keyword;
	else if (isIdentifier)
		return Classification::Identifier;
	else if (isNumber)
		return Classification::Number;
	else if (isString)
		return Classification::String;
	else if (isComment)
		return Classification::Comment;
	else
		return Classification::Text;
}

void AddClassification(SyntaxKind elementKind, const TextSpan& elementSpan,
					   const TextSpan& span, std::vector<ClassifiedSpan>& result)
{
	if (!elementSpan.OverlapsWith(span))
		return;

	auto adjustedStart = std::max(elementSpan.Start, span.Start);
	auto adjustedEnd = std::min(elementSpan.End(), span.End());
	auto adjustedSpan = TextSpan::FromBounds(adjustedStart, adjustedEnd);
	auto classification = Classify(elementKind);

	result.emplace_back(std::move(adjustedSpan), classification);
}

void ClassifyTrivia(const SyntaxTrivia& tr, const TextSpan& span,
					std::vector<ClassifiedSpan>& result)
{
	AddClassification(tr.Kind, tr.Span(), span, result);
}

void ClassifyToken(const SyntaxToken& token, const TextSpan& span,
				   std::vector<ClassifiedSpan>& result)
{
	std::for_each(token.LeadingTrivia.cbegin(), token.LeadingTrivia.cend(),
				  [&span, &result](const auto& lt) { ClassifyTrivia(lt, span, result); });

	AddClassification(token.Kind(), token.Span(), span, result);

	std::for_each(token.TrailingTrivia.cbegin(), token.TrailingTrivia.cend(),
				  [&span, &result](const auto& rt) { ClassifyTrivia(rt, span, result); });
}

void ClassifyNode(const SyntaxNode* node, const TextSpan& span,
				  std::vector<ClassifiedSpan>& result)
{
	if (node == nullptr || !node->FullSpan().OverlapsWith(span))
		return;

	if (IsToken(node->Kind()))
	{
		auto token = static_cast<const SyntaxToken*>(node);
		ClassifyToken(*token, span, result);
	}

	auto children = node->GetChildren();
	std::for_each(children.cbegin(), children.cend(),
				  [&span, &result](const auto p) { ClassifyNode(p, span, result); });
}

} //namespace

std::vector<ClassifiedSpan> Classify(const SyntaxTree& tree, const TextSpan& span)
{
	auto result = std::vector<ClassifiedSpan>();
	ClassifyNode(tree.Root(), span, result);
	return result;
}