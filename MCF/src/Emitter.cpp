#include "Emitter.h"

#include <fstream>

#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4100 4141 4146 4244 4245 4267 4324 4458 4624)
#else //defined(_MSC_VER) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif //defined(_MSC_VER) && !defined(__clang__)

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/Optional.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(pop)
#else
#pragma GCC diagnostic pop
#endif

#include "Binding.h"
#include "BoundExpressions.h"
#include "BoundStatements.h"
#include "StringHelper.h"
#include "Parsing.h"

namespace MCF {

namespace {
constexpr auto BYTE_BITS = 8U;
constexpr auto INT_BITS = sizeof(IntegerType) * BYTE_BITS;
} //namespace

struct DebugInfo
{
	llvm::DIBuilder DIBuilder;
	SymbolMap<TypeSymbol, llvm::DIType*> DITypes;
	vector<llvm::DIScope*> LexicalBlocks;
	llvm::IRBuilder<llvm::LLVMContext>& Builder;
	llvm::DICompileUnit* CU{ nullptr };

	explicit DebugInfo(llvm::Module& m, llvm::IRBuilder<llvm::LLVMContext>& bld,
					   const fs::path& src)
		:DIBuilder{ m }, Builder{ bld },
		CU{ DIBuilder.createCompileUnit(llvm::dwarf::DW_LANG_C,
									   DIBuilder.createFile(src.filename().string(), "."),
									   "MCFC", false, "", 0) }
	{
	}

	llvm::DISubroutineType* CreateFunctionType(const FunctionSymbol&);
	void EmitLocation(const SyntaxNode*);
	llvm::DIType* GetType(const TypeSymbol&);
};

llvm::DISubroutineType* DebugInfo::CreateFunctionType(const FunctionSymbol& fs)
{
	llvm::SmallVector<llvm::Metadata*, 8> types;
	auto t = GetType(fs.Type);
	types.push_back(t);

	for (const auto& p : fs.Parameters)
		types.push_back(GetType(p.Type));

	return DIBuilder.createSubroutineType(DIBuilder.getOrCreateTypeArray(types));
}

void DebugInfo::EmitLocation(const SyntaxNode* node)
{
	if (node == nullptr)
	{
		Builder.SetCurrentDebugLocation(llvm::DebugLoc());
		return;
	}

	assert(CU && "Set up llvm::DICompileUnit* first");
	auto scope = LexicalBlocks.empty() ? CU : LexicalBlocks.back();
	auto loc = node->Location();
	Builder.SetCurrentDebugLocation(
		llvm::DebugLoc::get(loc.StartLine(), loc.StartCharacter(), scope));
}

llvm::DIType* DebugInfo::GetType(const TypeSymbol& ts)
{
	if (DITypes.find(ts) != DITypes.end())
	{
		return DITypes.at(ts);
	} else
	{
		llvm::DIType* t{ nullptr };
		if (ts == TYPE_BOOL)
		{
			t = DIBuilder.createBasicType("bool", BYTE_BITS,
										  llvm::dwarf::DW_ATE_boolean);
			DITypes.emplace(ts, t);
		} else if (ts == TYPE_INT)
		{
			t = DIBuilder.createBasicType("int", INT_BITS,
										  llvm::dwarf::DW_ATE_signed);
			DITypes.emplace(ts, t);
		} else if (ts == TYPE_STRING)
		{
			auto charType = DIBuilder.createBasicType("char", BYTE_BITS,
													  llvm::dwarf::DW_ATE_signed_char);
			t = DIBuilder.createPointerType(charType,
											sizeof(std::nullptr_t) * BYTE_BITS);
			DITypes.emplace(ts, t);
		} else if (ts == TYPE_VOID)
		{
			// HACK what to do with void? 
			t = DIBuilder.createBasicType("void", INT_BITS,
										  llvm::dwarf::DW_ATE_signed);
			DITypes.emplace(ts, t);
		} else
		{
			throw std::invalid_argument(BuildStringFrom("Invalid Type: ", ts.Name));
		}

		assert(t && "Should return valid llvm::DIType*.");
		return t;
	}
}

class Emitter
{
private:
	llvm::LLVMContext _context;
	llvm::IRBuilder<llvm::LLVMContext> _builder;
	unique_ptr<llvm::Module> _module;
	unique_ptr<DebugInfo> _debugInfo;
	llvm::TargetMachine* _targetMachine;

	SymbolMap<TypeSymbol, llvm::Type*> _knownTypes;
	llvm::Type* _boolType;
	llvm::Type* _charType;
	llvm::Type* _intType;

	llvm::Function* _inputFunc;
	llvm::Function* _putsFunc; 	// delegate print to C puts function
	llvm::Function* _rndFunc;
	llvm::Function* _strConcatFunc;

	llvm::Function* _boolToStrFunc;
	llvm::Function* _intToStrFunc;
	llvm::Function* _strToBoolFunc;
	llvm::Function* _strToIntFunc;

	llvm::Function* _strEqualFunc;
	llvm::Function* _nopIntrinsic;

	// current working function
	llvm::Function* _function;

	size_t _gotoCount = 0;

	// finished(terminated) blocks
	vector<BoundLabel> _labels;

	// basic blocks created from labels
	std::unordered_map<BoundLabel, llvm::BasicBlock*, LabelHash> _labelBBMap;

	// local variables
	std::unordered_map<string_view, llvm::AllocaInst*> _locals;

	DiagnosticBag _diagnostics;

	void EmitFunctionDeclaration(const FunctionSymbol& function);
	void EmitFunctionBody(const FunctionSymbol& function, const BoundBlockStatement& body);

	void EmitStatement(const BoundStatement& node);
	void EmitNopStatement(const BoundNopStatement& node);
	void EmitVariableDeclaration(const BoundVariableDeclaration& node);
	void EmitLabelStatement(const BoundLabelStatement& node);
	void EmitGotoStatement(const BoundGotoStatement& node);
	void EmitConditionalGotoStatement(const BoundConditionalGotoStatement& node);
	void EmitReturnStatement(const BoundReturnStatement& node);
	void EmitExpressionStatement(const BoundExpressionStatement& node);

	llvm::Value* EmitExpression(const BoundExpression& node);
	llvm::Value* EmitLiteralExpression(const BoundLiteralExpression& node);
	llvm::Value* EmitVariableExpression(const BoundVariableExpression& node);
	llvm::Value* EmitAssignmentExpression(const BoundAssignmentExpression& node);
	llvm::Value* EmitUnaryExpression(const BoundUnaryExpression& node);
	llvm::Value* EmitBinaryExpression(const BoundBinaryExpression& node);
	llvm::Value* EmitCallExpression(const BoundCallExpression& node);
	llvm::Value* EmitConversionExpression(const BoundConversionExpression& node);
	llvm::Value* EmitPostfixExpression(const BoundPostfixExpression& node);

	void InitKnownTypes();
	void InitExternFunctions();
	void InitTargetAndModule();
	[[nodiscard]] llvm::AllocaInst* CreateEntryBlockAlloca(llvm::Type* type,
														   string_view varName)const;
	void CreateBlocksFromLabels(const BoundBlockStatement& body);
	void FixPrevLabel();

	llvm::Value* ConvertToStr(llvm::Value* value);

public:
	explicit Emitter(const string& moduleName, const fs::path& src);

	DiagnosticBag Emit(const BoundProgram& program, const fs::path& outputPath);
};

Emitter::Emitter(const string& moduleName, const fs::path& src)
	:_context(),
	_builder(_context),
	_module(std::make_unique<llvm::Module>(moduleName, _context)),
	_debugInfo(std::make_unique<DebugInfo>(*_module, _builder, src)),
	_targetMachine(nullptr),
	_diagnostics()
{
	InitTargetAndModule();
	InitKnownTypes();
	InitExternFunctions();
}

void Emitter::EmitFunctionDeclaration(const FunctionSymbol& function)
{
	auto retType = _knownTypes.at(function.Type);
	auto args = vector<llvm::Type*>();

	std::for_each(function.Parameters.cbegin(), function.Parameters.cend(),
				  [this, &args](const auto& p)
				  {
					  args.push_back(_knownTypes.at(p.Type));
				  });

	auto f = llvm::FunctionType::get(retType, args, false);
	llvm::Function::Create(f, llvm::Function::ExternalLinkage,
						   string(function.Name), *_module);
}

void Emitter::EmitFunctionBody(const FunctionSymbol& fs, const BoundBlockStatement& body)
{
	auto func = _module->getFunction(string(fs.Name));
	if (func == nullptr)
	{
		_diagnostics.ReportFunctionDeclarationNotEmitted(fs.Name);
		return;
	} else if (!func->empty())
	{
		_diagnostics.ReportFunctionViolateODR(fs.Name);
		return;
	}

	assert((fs.Parameters.size() == func->arg_size())
		   && "Invalid number of arguments.");

	auto entry = llvm::BasicBlock::Create(_context, "entry", func);
	_builder.SetInsertPoint(entry);

	llvm::DIFile* unit = _debugInfo->DIBuilder.createFile(_debugInfo->CU->getFilename(),
														  _debugInfo->CU->getDirectory());
	llvm::DIScope* fcontext = unit;

	// start emitting new function
	_function = func;
	_gotoCount = 0;
	_labels.clear();
	_labelBBMap.clear();

	// TODO inserted main() has no declaration
	auto lineNumber = fs.Declaration->Location().StartLine();
	auto scopeLine = lineNumber;
	auto sp = _debugInfo->DIBuilder.createFunction(
		fcontext, string(fs.Name), llvm::StringRef(), unit, lineNumber,
		_debugInfo->CreateFunctionType(fs), scopeLine,
		llvm::DINode::FlagPrototyped, llvm::DISubprogram::SPFlagDefinition);
	_function->setSubprogram(sp);

	_debugInfo->LexicalBlocks.push_back(sp);
	_debugInfo->EmitLocation(nullptr);

	// Place all parameters into _locals table
	_locals.clear();
	size_t i = 0;
	for (auto& arg : func->args())
	{
		auto allocaInst = CreateEntryBlockAlloca(arg.getType(), fs.Parameters.at(i).Name);

		auto dbgArg = _debugInfo->DIBuilder.createParameterVariable(
			sp, arg.getName(), i + 1, unit, lineNumber,
			_debugInfo->GetType(fs.Parameters.at(i).Type),
			true);

		_debugInfo->DIBuilder.insertDeclare(allocaInst, dbgArg,
											_debugInfo->DIBuilder.createExpression(),
											llvm::DebugLoc::get(lineNumber, 0, sp),
											_builder.GetInsertBlock());

		_builder.CreateStore(&arg, allocaInst);
		_locals.emplace(fs.Parameters.at(i).Name, allocaInst);
		++i;
	}

	_debugInfo->EmitLocation(&body.Syntax());

	try
	{
		CreateBlocksFromLabels(body);

		for (const auto& it : body.Statements())
			EmitStatement(*it);

		llvm::verifyFunction(*func);
	} catch (const std::exception& e)
	{
		_diagnostics.ReportCannotEmitFunctionBody(e.what());
		func->eraseFromParent();
	}

	// Pop off the function block 
	// regardless of whether it is successfully emitted or not
	_debugInfo->LexicalBlocks.pop_back();
}

void Emitter::EmitStatement(const BoundStatement& node)
{
#define EMIT_STMT(kind) \
case BoundNodeKind::kind: Emit##kind(static_cast<const Bound##kind&>(node)); break;

	switch (node.Kind())
	{
		EMIT_STMT(NopStatement);
		EMIT_STMT(VariableDeclaration);
		EMIT_STMT(LabelStatement);
		EMIT_STMT(GotoStatement);
		EMIT_STMT(ConditionalGotoStatement);
		EMIT_STMT(ReturnStatement);
		EMIT_STMT(ExpressionStatement);

		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected node: ", nameof(node.Kind())));
	}

#undef EMIT_STMT
}

void Emitter::EmitNopStatement(const BoundNopStatement& node)
{
	_debugInfo->EmitLocation(&node.Syntax());
	_builder.CreateCall(_nopIntrinsic);
}

void Emitter::EmitVariableDeclaration(const BoundVariableDeclaration& node)
{
	_debugInfo->EmitLocation(&node.Syntax());

	auto value = EmitExpression(*node.Initializer());
	auto type = value->getType();
	auto allocaInst = CreateEntryBlockAlloca(type, node.Variable()->Name);
	_locals.emplace(node.Variable()->Name, allocaInst);
	_builder.CreateStore(value, allocaInst);
}

void Emitter::EmitLabelStatement(const BoundLabelStatement& node)
{
	auto nextBlock = _labelBBMap.at(node.Label());

	auto currentBlock = _builder.GetInsertBlock();
	if (currentBlock->getTerminator() == nullptr)
	{
		_builder.CreateBr(nextBlock);
		FixPrevLabel();
	}
	_labels.push_back(node.Label());

	_function->getBasicBlockList().push_back(nextBlock);
	_builder.SetInsertPoint(nextBlock);
}

void Emitter::EmitGotoStatement(const BoundGotoStatement& node)
{
	if (_labelBBMap.find(node.Label()) != _labelBBMap.end())
	{
		auto bb = _labelBBMap.at(node.Label());
		_builder.CreateBr(bb);
		FixPrevLabel();
	} else
	{
		_diagnostics.ReportBasicBlockNotCreatedFromLabel(node.Label().Name);
	}
}

void Emitter::EmitConditionalGotoStatement(const BoundConditionalGotoStatement& node)
{
	auto cond = EmitExpression(*node.Condition());
	if (cond == nullptr)
		return;

	auto jumpIfTrue = node.JumpIfTrue() ?
		llvm::ConstantInt::getTrue(_context)
		: llvm::ConstantInt::getFalse(_context);
	auto cmp = new llvm::ICmpInst(llvm::CmpInst::Predicate::ICMP_EQ, cond, jumpIfTrue);

	if (_labelBBMap.find(node.Label()) != _labelBBMap.end())
	{
		auto jumpTo = _labelBBMap.at(node.Label());
		auto name = ".exit" + std::to_string(_gotoCount);
		auto exit = llvm::BasicBlock::Create(_context, name);

		cmp = _builder.Insert(cmp);
		_builder.CreateCondBr(cmp, jumpTo, exit);

		FixPrevLabel();
		_labels.emplace_back(std::move(name));

		_function->getBasicBlockList().push_back(exit);
		_builder.SetInsertPoint(exit);
	} else
	{
		_diagnostics.ReportBasicBlockNotCreatedFromLabel(node.Label().Name);
	}
}

void Emitter::EmitReturnStatement(const BoundReturnStatement& node)
{
	_debugInfo->EmitLocation(&node.Syntax());

	if (node.Expression() != nullptr)
		_builder.CreateRet(EmitExpression(*node.Expression()));
	else _builder.CreateRetVoid();
}

void Emitter::EmitExpressionStatement(const BoundExpressionStatement& node)
{
	//_debugInfo->EmitLocation(&node.Syntax());

	EmitExpression(*node.Expression());
}

llvm::Value* Emitter::EmitExpression(const BoundExpression& node)
{
#define EMIT_EXPR(kind) \
case BoundNodeKind::kind: return Emit##kind(static_cast<const Bound##kind&>(node));

	switch (node.Kind())
	{
		EMIT_EXPR(LiteralExpression);
		EMIT_EXPR(VariableExpression);
		EMIT_EXPR(AssignmentExpression);
		EMIT_EXPR(UnaryExpression);
		EMIT_EXPR(BinaryExpression);
		EMIT_EXPR(CallExpression);
		EMIT_EXPR(ConversionExpression);
		EMIT_EXPR(PostfixExpression);

		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected node: ", nameof(node.Kind())));
	}

#undef EMIT_EXPR
}

llvm::Value* Emitter::EmitLiteralExpression(const BoundLiteralExpression& node)
{
	_debugInfo->EmitLocation(&node.Syntax());

	auto type = node.Type();
	if (type == TYPE_BOOL)
	{
		auto v = node.Value().ToBoolean();
		return v ? llvm::ConstantInt::getTrue(_context)
			: llvm::ConstantInt::getFalse(_context);
	} else if (type == TYPE_INT)
	{
		auto v = node.Value().ToInteger();
		return llvm::ConstantInt::get(_context, llvm::APInt(INT_BITS, v, true));
	} else if (type == TYPE_STRING)
	{
		auto v = node.Value().ToString();
		auto charType = _charType;

		auto chars = vector<llvm::Constant*>();
		std::for_each(v.cbegin(), v.cend(),
					  [charType, &chars](const auto c)
					  {
						  chars.push_back(llvm::ConstantInt::get(charType, c));
					  });

		chars.push_back(llvm::ConstantInt::get(charType, 0));

		auto strType = llvm::ArrayType::get(charType, chars.size());

		static size_t i = 0;
		auto name = ".str" + std::to_string(i++);
		auto global = static_cast<llvm::GlobalVariable*>(_module->getOrInsertGlobal(name, strType));
		global->setInitializer(llvm::ConstantArray::get(strType, chars));
		global->setConstant(true);
		global->setLinkage(llvm::GlobalValue::LinkageTypes::PrivateLinkage);
		global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

		return llvm::ConstantExpr::getBitCast(global, charType->getPointerTo());
	} else
	{
		throw std::invalid_argument("Unexpected value: " + node.Value().ToString());
	}
}

llvm::Value* Emitter::EmitVariableExpression(const BoundVariableExpression& node)
{
	auto name = node.Variable()->Name;
	if (_locals.find(name) != _locals.end())
	{
		_debugInfo->EmitLocation(&node.Syntax());

		auto v = _locals.at(name);
		return _builder.CreateLoad(v, string(name));
	} else
	{
		_diagnostics.ReportVariableNotEmitted(node.Variable()->Name);
		return nullptr;
	}
}

llvm::Value* Emitter::EmitAssignmentExpression(const BoundAssignmentExpression& node)
{
	auto value = EmitExpression(*node.Expression());
	auto name = node.Variable()->Name;
	if (_locals.find(name) != _locals.end())
	{
		_debugInfo->EmitLocation(&node.Syntax());

		auto allocaInst = _locals.at(name);
		return _builder.CreateStore(value, allocaInst);
	} else
	{
		_diagnostics.ReportVariableNotEmitted(node.Variable()->Name);
		return nullptr;
	}
}

llvm::Value* Emitter::EmitUnaryExpression(const BoundUnaryExpression& node)
{
	_debugInfo->EmitLocation(&node.Syntax());

	auto value = EmitExpression(*node.Operand());
	switch (node.Op().Kind())
	{
		case BoundUnaryOperatorKind::Identity:
			return value;
		case BoundUnaryOperatorKind::Negation:
		{
			auto op = llvm::BinaryOperator::CreateNeg(value);
			return _builder.Insert(op);
		}
		case BoundUnaryOperatorKind::LogicalNegation:
		{
			auto false_ = llvm::ConstantInt::getFalse(_context);
			auto op = new llvm::ICmpInst(llvm::CmpInst::Predicate::ICMP_EQ, false_, value);
			return _builder.Insert(op);
		}
		case BoundUnaryOperatorKind::OnesComplement:
		{
			auto op = llvm::BinaryOperator::CreateNot(value);
			return _builder.Insert(op);
		}
		default:
			throw std::invalid_argument(
				BuildStringFrom("Unexpected unary operator ",
								GetText(node.Op().SynKind()),
								'(', node.Operand()->Type().Name, ").")
			);
	}
}

llvm::Value* Emitter::EmitBinaryExpression(const BoundBinaryExpression& node)
{
	_debugInfo->EmitLocation(&node.Syntax());

	auto lhs = EmitExpression(*node.Left());
	auto rhs = EmitExpression(*node.Right());

	if (node.Op().Kind() == BoundBinaryOperatorKind::Addition)
	{
		if (node.Left()->Type() == TYPE_STRING
			&& node.Right()->Type() == TYPE_STRING)
		{
			return _builder.CreateCall(_strConcatFunc, { lhs, rhs });
		}
	}

	if (node.Op().Kind() == BoundBinaryOperatorKind::Equals)
	{
		if (node.Left()->Type() == TYPE_ANY
			&& node.Right()->Type() == TYPE_ANY)
		{
			// HACK convert integral values into strings
			auto lstr = ConvertToStr(lhs);
			auto rstr = ConvertToStr(rhs);
			return _builder.CreateCall(_strEqualFunc, { lstr, rstr });
		} else if (node.Left()->Type() == TYPE_STRING
				   && node.Right()->Type() == TYPE_STRING)
		{
			// NOTE strings are stored either in
			// a) runtime container
			// b) as constant in binary
			return _builder.CreateCall(_strEqualFunc, { lhs, rhs });
		}
	}

	if (node.Op().Kind() == BoundBinaryOperatorKind::NotEquals)
	{
		auto compareToFalse = [this](llvm::CallInst* v)
		{
			auto false_ = llvm::ConstantInt::getFalse(_context);
			auto op = new llvm::ICmpInst(llvm::CmpInst::Predicate::ICMP_EQ, false_, v);
			return _builder.Insert(op);
		};

		llvm::CallInst* value = nullptr;

		if (node.Left()->Type() == TYPE_ANY
			&& node.Right()->Type() == TYPE_ANY)
		{
			auto lstr = ConvertToStr(lhs);
			auto rstr = ConvertToStr(rhs);
			value = _builder.CreateCall(_strEqualFunc, { lstr, rstr });
			return compareToFalse(value);
		} else if (node.Left()->Type() == TYPE_STRING
				   && node.Right()->Type() == TYPE_STRING)
		{
			value = _builder.CreateCall(_strEqualFunc, { lhs, rhs });
			return compareToFalse(value);
		}
	}

	auto binary = [this, lhs, rhs](llvm::Instruction::BinaryOps ins)
	{
		auto op = llvm::BinaryOperator::Create(ins, lhs, rhs);
		return _builder.Insert(op);
	};
	auto compare = [this, lhs, rhs](llvm::CmpInst::Predicate p)
	{
		auto op = new llvm::ICmpInst(p, lhs, rhs);
		return _builder.Insert(op);
	};

	switch (node.Op().Kind())
	{
		// TODO should these be macro-ed? 
		case BoundBinaryOperatorKind::Addition:
			return binary(llvm::Instruction::Add);
		case BoundBinaryOperatorKind::Subtraction:
			return binary(llvm::Instruction::Sub);
		case BoundBinaryOperatorKind::Multiplication:
			return binary(llvm::Instruction::Mul);
		case BoundBinaryOperatorKind::Division:
			return binary(llvm::Instruction::SDiv);
		case BoundBinaryOperatorKind::Modulus:
			return binary(llvm::Instruction::BinaryOps::SRem);

		case BoundBinaryOperatorKind::LogicalAnd:
		case BoundBinaryOperatorKind::BitwiseAnd:
			return binary(llvm::Instruction::And);
		case BoundBinaryOperatorKind::LogicalOr:
		case BoundBinaryOperatorKind::BitwiseOr:
			return binary(llvm::Instruction::Or);
		case BoundBinaryOperatorKind::BitwiseXor:
			return binary(llvm::Instruction::Xor);

		case BoundBinaryOperatorKind::Equals:
			return compare(llvm::CmpInst::Predicate::ICMP_EQ);
		case BoundBinaryOperatorKind::NotEquals:
			return compare(llvm::CmpInst::Predicate::ICMP_NE);
		case BoundBinaryOperatorKind::Less:
			return compare(llvm::CmpInst::Predicate::ICMP_SLT);
		case BoundBinaryOperatorKind::LessOrEquals:
			return compare(llvm::CmpInst::Predicate::ICMP_SLE);
		case BoundBinaryOperatorKind::Greater:
			return compare(llvm::CmpInst::Predicate::ICMP_SGT);
		case BoundBinaryOperatorKind::GreaterOrEquals:
			return compare(llvm::CmpInst::Predicate::ICMP_SGE);

		default:
			throw std::invalid_argument(
				BuildStringFrom("Unexpected binary operator ",
								GetText(node.Op().SynKind()),
								'(', node.Left()->Type().Name, ", ",
								node.Right()->Type().Name, ")")
			);
	}
}

llvm::Value* Emitter::EmitCallExpression(const BoundCallExpression& node)
{
	_debugInfo->EmitLocation(&node.Syntax());

	if (*(node.Function()) == BUILTIN_INPUT)
	{
		return _builder.CreateCall(_inputFunc);
	} else if (*(node.Function()) == BUILTIN_PRINT)
	{
		auto value = EmitExpression(*node.Arguments().at(0));

		llvm::Value* str = ConvertToStr(value);
		return _builder.CreateCall(_putsFunc, str);

	} else if (*(node.Function()) == BUILTIN_RND)
	{
		auto value = EmitExpression(*node.Arguments().at(0));
		return _builder.CreateCall(_rndFunc, value);
	}

	auto callee = _module->getFunction(string(node.Function()->Name));
	if (callee == nullptr)
	{
		_diagnostics.ReportFunctionDeclarationNotEmitted(node.Function()->Name);
		return nullptr;
	}
	if (callee->arg_size() != node.Arguments().size())
	{
		_diagnostics.ReportWrongArgumentCountEmitted(node.Function()->Name,
													 node.Arguments().size(),
													 callee->arg_size());
		return nullptr;
	}

	auto args = vector<llvm::Value*>();
	for (const auto& arg : node.Arguments())
		args.push_back(EmitExpression(*arg));

	if (callee->getReturnType()->isVoidTy())
		return _builder.CreateCall(callee, args);
	return _builder.CreateCall(callee, args, string(node.Function()->Name));
}

llvm::Value* Emitter::EmitConversionExpression(const BoundConversionExpression& node)
{
	_debugInfo->EmitLocation(&node.Syntax());

	auto value = EmitExpression(*node.Expression());
	if (value == nullptr)
		return nullptr;
	if (node.Type() == TYPE_ANY)
	{
		return value;
	} else if (node.Type() == TYPE_BOOL)
	{
		if (value->getType() == _charType->getPointerTo())
			return _builder.CreateCall(_strToBoolFunc, value);
		return value;
	} else if (node.Type() == TYPE_INT)
	{
		if (value->getType() == _charType->getPointerTo())
			return _builder.CreateCall(_strToIntFunc, value);
		return value;
	} else if (node.Type() == TYPE_STRING)
	{
		return ConvertToStr(value);
	} else
	{
		// NOTE implicit conversions
		//      int -> bool non-zero -> true
		//      bool -> int
		throw std::invalid_argument(
			BuildStringFrom("Unexpected convertion from '",
							node.Expression()->Type().Name,
							"' to '", node.Type().Name, "'.")
		);
	}
}

llvm::Value* Emitter::EmitPostfixExpression(const BoundPostfixExpression& node)
{
	auto value = EmitExpression(*node.Expression());
	auto name = node.Variable()->Name;
	if (_locals.find(name) != _locals.end())
	{
		_debugInfo->EmitLocation(&node.Syntax());

		auto unit = llvm::ConstantInt::get(_intType, 1, true);
		auto allocaInst = _locals.at(name);
		switch (node.OperatorKind())
		{
			case BoundPostfixOperatorEnum::Increment:
			{
				auto op = llvm::BinaryOperator::Create(
					llvm::Instruction::BinaryOps::Add, value, unit);
				auto result = _builder.Insert(op);
				_builder.CreateStore(result, allocaInst);
				return result;
			}
			case BoundPostfixOperatorEnum::Decrement:
			{
				auto op = llvm::BinaryOperator::Create(
					llvm::Instruction::BinaryOps::Sub, value, unit);
				auto result = _builder.Insert(op);
				_builder.CreateStore(result, allocaInst);
				return result;
			}
			default:
				throw std::invalid_argument("Unknown BoundPostfixOperator");
		}
	} else
	{
		_diagnostics.ReportVariableNotEmitted(node.Variable()->Name);
		return nullptr;
	}
}

DiagnosticBag Emitter::Emit(const BoundProgram& program, const fs::path& outputPath)
{
	if (!_diagnostics.empty())
		return std::move(_diagnostics);

	auto result = DiagnosticBag();

	for (const auto& [func, _] : program.Functions())
		EmitFunctionDeclaration(*func);
	for (const auto& [func, body] : program.Functions())
		EmitFunctionBody(*func, *body);

	if (!_diagnostics.empty())
		return std::move(_diagnostics);

	std::error_code ec;
	auto dest = llvm::raw_fd_ostream(outputPath.string(), ec, llvm::sys::fs::OF_None);
	if (ec)
	{
		result.ReportCannotOpenOutputFile(ec.message());
		return result;
	}

	_debugInfo->DIBuilder.finalize();

	auto pass = llvm::legacy::PassManager();
	auto fileType = llvm::CodeGenFileType::CGFT_ObjectFile;
	if (_targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType))
	{
		result.ReportCannotEmitFileType();
		return result;
	}

	pass.run(*_module);
	dest.flush();
	return result;
}

void Emitter::InitKnownTypes()
{
	_boolType = llvm::ConstantInt::getTrue(_context)->getType();
	_charType = _builder.getInt8Ty();
	_intType = _builder.getIntNTy(INT_BITS);

	_knownTypes.emplace(TYPE_BOOL, _boolType);
	_knownTypes.emplace(TYPE_INT, _intType);
	_knownTypes.emplace(TYPE_STRING, _charType->getPointerTo());
	_knownTypes.emplace(TYPE_VOID, _builder.getVoidTy());
}

void Emitter::InitExternFunctions()
{
	auto ft = llvm::FunctionType::get(_charType->getPointerTo(), false);
	_inputFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "input", *_module);

	ft = llvm::FunctionType::get(_builder.getInt32Ty(), _charType->getPointerTo(), false);
	_putsFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "puts", *_module);

	ft = llvm::FunctionType::get(_intType, _intType, false);
	_rndFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "rnd", *_module);

	auto args = vector<llvm::Type*>(2, _charType->getPointerTo());
	ft = llvm::FunctionType::get(_charType->getPointerTo(), args, false);
	_strConcatFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "strConcat", *_module);

	ft = llvm::FunctionType::get(_charType->getPointerTo(), _boolType, false);
	_boolToStrFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "boolToStr", *_module);

	ft = llvm::FunctionType::get(_charType->getPointerTo(), _intType, false);
	_intToStrFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "intToStr", *_module);

	ft = llvm::FunctionType::get(_boolType, _charType->getPointerTo(), false);
	_strToBoolFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "strToBool", *_module);

	ft = llvm::FunctionType::get(_intType, _charType->getPointerTo(), false);
	_strToIntFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "strToInt", *_module);

	args = vector<llvm::Type*>(2, _charType->getPointerTo());
	ft = llvm::FunctionType::get(_boolType, args, false);
	_strEqualFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "strEqual", *_module);

	ft = llvm::FunctionType::get(_knownTypes.at(TYPE_VOID), false);
	_nopIntrinsic =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "llvm.donothing", *_module);

}

void Emitter::InitTargetAndModule()
{
	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

	_module->addModuleFlag(llvm::Module::Warning, "Debug Version",
						   llvm::DEBUG_METADATA_VERSION);

	auto targetTriple = llvm::sys::getDefaultTargetTriple();
	_module->setTargetTriple(targetTriple);
	string error;
	auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
	if (target == nullptr)
	{
		_diagnostics.ReportRequestedTargetNotFound(error);
		return;
	}

	auto cpu = "generic";
	auto features = "";
	auto opt = llvm::TargetOptions();
	auto relocModel = llvm::Optional<llvm::Reloc::Model>();
	_targetMachine = target->createTargetMachine(
		targetTriple, cpu, features, opt, relocModel);
	_module->setDataLayout(_targetMachine->createDataLayout());
}

llvm::AllocaInst* Emitter::CreateEntryBlockAlloca(llvm::Type* type, string_view varName)const
{
	auto builder = llvm::IRBuilder<>(&_function->getEntryBlock(),
									 _function->getEntryBlock().begin());
	return builder.CreateAlloca(type, 0, string(varName));
}

void Emitter::CreateBlocksFromLabels(const BoundBlockStatement& body)
{
	for (const auto& it : body.Statements())
	{
		if (it->Kind() == BoundNodeKind::LabelStatement)
		{
			auto& node = static_cast<const BoundLabelStatement&>(*it);
			auto bb = llvm::BasicBlock::Create(_context, string(node.Label().Name));
			_labelBBMap.emplace(node.Label(), bb);
		}
	}
}

void Emitter::FixPrevLabel()
{
	if (!_labels.empty())
	{
		const auto& prevLabel = _labels.back();
		_labelBBMap.insert_or_assign(prevLabel, _builder.GetInsertBlock());
	}
}

llvm::Value* Emitter::ConvertToStr(llvm::Value* value)
{
	if (value->getType() == _boolType)
		return _builder.CreateCall(_boolToStrFunc, value);
	else if (value->getType() == _intType)
		return _builder.CreateCall(_intToStrFunc, value);
	else if (value->getType() == _charType->getPointerTo())
		return value;
	else
		throw std::invalid_argument("Cannot convert type to str.");
}

namespace {

void WriteLibFile(const fs::path& libPath)
{
	static constexpr auto libContent = R"(
#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <unordered_set>

using IntegerType = int;

struct UniqueStringHash
{
    size_t operator()(const std::unique_ptr<std::string> &s) const noexcept
    {
        return std::hash<std::string>{}(*s);
    }
};

struct UniqueStringEqual
{
    bool operator()(const std::unique_ptr<std::string> &s1,
                    const std::unique_ptr<std::string> &s2) const noexcept
    {
        return (*s1) == (*s2);
    }
};

static auto strs = std::unordered_set<std::unique_ptr<std::string>,
                                      UniqueStringHash, UniqueStringEqual>();

extern "C" const char *boolToStr(bool v)
{
    std::string result = v ? "True" : "False";
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}

extern "C" const char *intToStr(IntegerType v)
{
    auto result = std::to_string(v);
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}

extern "C" bool strToBool(const char *s) noexcept
{
    return std::strcmp(s, "true") == 0 || std::strcmp(s, "True") == 0;
}

extern "C" IntegerType strToInt(const char *s)
{
    return std::stoi(s);
}

extern "C" const char *input()
{
    auto result = std::string();
    std::getline(std::cin, result);
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}

extern "C" IntegerType rnd(IntegerType max)
{
    static auto rd = std::random_device();
    auto mt = std::mt19937(rd());
    auto dist = std::uniform_int_distribution<IntegerType>(0, max);

    return dist(mt);
}

extern "C" const char *strConcat(const char *s1, const char *s2)
{
    auto result = std::string(s1) + s2;
    auto [it, _] = strs.insert(std::make_unique<std::string>(std::move(result)));
    return (*it)->c_str();
}

extern "C" bool strEqual(const char *a, const char *b) noexcept
{
    return std::strcmp(a, b) == 0;
}
)";

	std::ofstream out{ libPath };
	out << libContent;
}

}// namespace

DiagnosticBag Emit(const BoundProgram& program, const string& moduleName,
				   const fs::path& srcPath, const fs::path& outPath)
{
	if (program.Diagnostics().HasErrors())
		return std::move(program).Diagnostics();

	auto e = Emitter(moduleName, srcPath);
	auto d = e.Emit(program, outPath);

	auto libPath = srcPath.parent_path().append("lib.cpp");
	WriteLibFile(libPath);

	// Let's link against C++ "lib" and runtime
	string cmd = "clang-cl.exe -fuse-ld=lld -Z7 -MTd /std:c++17 ";
	cmd += libPath.string();
	cmd += " " + outPath.string();
	cmd += " -o " + srcPath.parent_path().append(moduleName + ".exe").string();
	std::system(cmd.c_str());

	return d;
}

} //MCF