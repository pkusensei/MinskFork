#pragma once

#include "common.h"

namespace MCF {

class BoundStatement;
class BoundBlockStatement;

struct FunctionSymbol;

[[nodiscard]] unique_ptr<BoundBlockStatement> Lower(const FunctionSymbol& func,
													shared_ptr<BoundStatement> statement);

}//MCF
