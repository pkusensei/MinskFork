#pragma once

#include "common.h"

namespace MCF {

class BoundStatement;
class BoundBlockStatement;

unique_ptr<BoundBlockStatement> Lower(shared_ptr<BoundStatement> statement);

}//MCF
