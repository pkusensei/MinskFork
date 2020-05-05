#include "Emitter.h"

#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4068 4100 4141 4146 4244 4245 4267 4291 4324 4458 4624)
#else //defined(_MSC_VER) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wlanguage-extension-token"
#endif //defined(_MSC_VER) && !defined(__clang__)

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/Optional.h>
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
#include "Diagnostic.h"
#include "helpers.h"

namespace MCF {

constexpr auto INT_BITS = sizeof(IntegerType) * 8;

class Emitter
{
private:
	llvm::LLVMContext _context;
	llvm::IRBuilder<llvm::LLVMContext> _builder;
	unique_ptr<llvm::Module> _module;
	llvm::TargetMachine* _targetMachine;

	llvm::Type* _boolType;
	llvm::Type* _charType;
	llvm::Type* _intType;

	std::unordered_map<TypeSymbol, llvm::Type*, SymbolHash, SymbolEqual> _knownTypes;

	llvm::Function* _inputFunc;
	llvm::Function* _putsFunc; 	// NOTE delegate print to C puts function
	llvm::Function* _rndFunc;
	llvm::Function* _strConcatFunc;

	llvm::Function* _boolToStrFunc;
	llvm::Function* _intToStrFunc;
	llvm::Function* _strToBoolFunc;
	llvm::Function* _strToIntFunc;

	llvm::Function* _ptrEqualFunc;
	llvm::Function* _strEqualFunc;

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
	void InitTarget();
	llvm::AllocaInst* CreateEntryBlockAlloca(llvm::Type* type, string_view varName)const;
	void CreateBlocksFromLabels(const BoundBlockStatement& body);
	void FixPrevLabel();

public:
	explicit Emitter(const string& moduleName);

	constexpr DiagnosticBag& Diagnostics() noexcept { return _diagnostics; }
	DiagnosticBag Emit(const BoundProgram& program, const fs::path& outputPath);
};

Emitter::Emitter(const string& moduleName)
	:_context(),
	_builder(_context),
	_module(std::make_unique<llvm::Module>(moduleName, _context)),
	_targetMachine(nullptr),
	_diagnostics()
{
	InitKnownTypes();
	InitExternFunctions();
	InitTarget();
}

void Emitter::EmitFunctionDeclaration(const FunctionSymbol& function)
{
	auto retType = _knownTypes.at(function.Type());
	auto args = vector<llvm::Type*>();
	std::transform(function.Parameters().cbegin(), function.Parameters().cend(),
		std::back_inserter(args),
		[this](const auto& p) { return _knownTypes.at(p.Type()); });

	auto f = llvm::FunctionType::get(retType, args, false);
	llvm::Function::Create(f, llvm::Function::ExternalLinkage,
		string(function.Name()), *_module);
}

void Emitter::EmitFunctionBody(const FunctionSymbol& fs, const BoundBlockStatement& body)
{
	auto func = _module->getFunction(string(fs.Name()));
	if (func == nullptr)
	{
		_diagnostics.ReportFunctionDeclarationNotEmitted(fs.Name());
		return;
	} else if (!func->empty())
	{
		_diagnostics.ReportFunctionViolateODR(fs.Name());
		return;
	}
	auto entry = llvm::BasicBlock::Create(_context, "entry", func);
	_builder.SetInsertPoint(entry);

	// start emitting new function
	_function = func;
	_gotoCount = 0;
	_labels.clear();
	_labelBBMap.clear();

	// Place all parameters into _locals table
	_locals.clear();
	size_t i = 0;
	for (auto& arg : func->args())
	{
		auto allocaInst = CreateEntryBlockAlloca(arg.getType(), fs.Parameters().at(i).Name());
		_builder.CreateStore(&arg, allocaInst);
		_locals.emplace(fs.Parameters().at(i).Name(), allocaInst);
		++i;
	}

	try
	{
		CreateBlocksFromLabels(body);

		for (const auto& it : body.Statements())
			EmitStatement(*it);

		llvm::verifyFunction(*func);
	} catch (const std::exception& e)
	{
		_diagnostics.ReportCannotEmitFunctionBody(e.what());
		func->removeFromParent();
	}
}

void Emitter::EmitStatement(const BoundStatement& node)
{
	switch (node.Kind())
	{
		case BoundNodeKind::VariableDeclaration:
			EmitVariableDeclaration(static_cast<const BoundVariableDeclaration&>(node));
			break;
		case BoundNodeKind::LabelStatement:
			EmitLabelStatement(static_cast<const BoundLabelStatement&>(node));
			break;
		case BoundNodeKind::GotoStatement:
			EmitGotoStatement(static_cast<const BoundGotoStatement&>(node));
			break;
		case BoundNodeKind::ConditionalGotoStatement:
			EmitConditionalGotoStatement(static_cast<const BoundConditionalGotoStatement&>(node));
			break;
		case BoundNodeKind::ReturnStatement:
			EmitReturnStatement(static_cast<const BoundReturnStatement&>(node));
			break;
		case BoundNodeKind::ExpressionStatement:
			EmitExpressionStatement(static_cast<const BoundExpressionStatement&>(node));
			break;
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected node: ", nameof(node.Kind())));
	}
}

void Emitter::EmitVariableDeclaration(const BoundVariableDeclaration& node)
{
	auto value = EmitExpression(*node.Initializer());
	auto type = value->getType();
	auto allocaInst = CreateEntryBlockAlloca(type, node.Variable()->Name());
	_locals.emplace(node.Variable()->Name(), allocaInst);
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
	try
	{
		auto bb = _labelBBMap.at(node.Label());
		_builder.CreateBr(bb);
		FixPrevLabel();
	} catch (const std::out_of_range&)
	{
		_diagnostics.ReportBasicBlockNotCreatedFromLabel(node.Label().Name());
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

	try
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
	} catch (const std::out_of_range&)
	{
		_diagnostics.ReportBasicBlockNotCreatedFromLabel(node.Label().Name());
	}
}

void Emitter::EmitReturnStatement(const BoundReturnStatement& node)
{
	if (node.Expression() != nullptr)
		_builder.CreateRet(EmitExpression(*node.Expression()));
	else _builder.CreateRetVoid();
}

void Emitter::EmitExpressionStatement(const BoundExpressionStatement& node)
{
	EmitExpression(*node.Expression());
}

llvm::Value* Emitter::EmitExpression(const BoundExpression& node)
{
	switch (node.Kind())
	{
		case BoundNodeKind::LiteralExpression:
			return EmitLiteralExpression(static_cast<const BoundLiteralExpression&>(node));
		case BoundNodeKind::VariableExpression:
			return EmitVariableExpression(static_cast<const BoundVariableExpression&>(node));
		case BoundNodeKind::AssignmentExpression:
			return EmitAssignmentExpression(static_cast<const BoundAssignmentExpression&>(node));
		case BoundNodeKind::UnaryExpression:
			return EmitUnaryExpression(static_cast<const BoundUnaryExpression&>(node));
		case BoundNodeKind::BinaryExpression:
			return EmitBinaryExpression(static_cast<const BoundBinaryExpression&>(node));
		case BoundNodeKind::CallExpression:
			return EmitCallExpression(static_cast<const BoundCallExpression&>(node));
		case BoundNodeKind::ConversionExpression:
			return EmitConversionExpression(static_cast<const BoundConversionExpression&>(node));
		case BoundNodeKind::PostfixExpression:
			return EmitPostfixExpression(static_cast<const BoundPostfixExpression&>(node));
		default:
			throw std::invalid_argument(BuildStringFrom("Unexpected node: ", nameof(node.Kind())));
	}
}

llvm::Value* Emitter::EmitLiteralExpression(const BoundLiteralExpression& node)
{
	auto type = node.Type();
	if (type == TypeSymbol(TypeEnum::Bool))
	{
		auto v = node.Value().ToBoolean();
		return v ? llvm::ConstantInt::getTrue(_context)
			: llvm::ConstantInt::getFalse(_context);
	} else if (type == TypeSymbol(TypeEnum::Int))
	{
		auto v = node.Value().ToInteger();
		return llvm::ConstantInt::get(_context, llvm::APInt(INT_BITS, v, true));
	} else if (type == TypeSymbol(TypeEnum::String))
	{
		auto v = node.Value().ToString();
		auto charType = _charType;

		auto chars = vector<llvm::Constant*>();
		std::transform(v.cbegin(), v.cend(), std::back_inserter(chars),
			[charType](const auto c) { return llvm::ConstantInt::get(charType, c); });
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
	try
	{
		auto v = _locals.at(node.Variable()->Name());
		return _builder.CreateLoad(v, string(node.Variable()->Name().data()));
	} catch (const std::out_of_range&)
	{
		_diagnostics.ReportVariableNotEmitted(node.Variable()->Name());
		return nullptr;
	}
}

llvm::Value* Emitter::EmitAssignmentExpression(const BoundAssignmentExpression& node)
{
	auto value = EmitExpression(*node.Expression());
	try
	{
		auto allocaInst = _locals.at(node.Variable()->Name());
		return _builder.CreateStore(value, allocaInst);
	} catch (const std::out_of_range&)
	{
		_diagnostics.ReportVariableNotEmitted(node.Variable()->Name());
		return nullptr;
	}
}

llvm::Value* Emitter::EmitUnaryExpression(const BoundUnaryExpression& node)
{
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
			throw std::invalid_argument(BuildStringFrom("Unexpected unary operator ",
				GetText(node.Op().SynKind()),
				'(', node.Operand()->Type().Name(), ").")
			);
	}
}

llvm::Value* Emitter::EmitBinaryExpression(const BoundBinaryExpression& node)
{
	auto lhs = EmitExpression(*node.Left());
	auto rhs = EmitExpression(*node.Right());

	if (node.Op().Kind() == BoundBinaryOperatorKind::Addition)
	{
		if (node.Left()->Type() == TypeSymbol(TypeEnum::String)
			&& node.Right()->Type() == TypeSymbol(TypeEnum::String))
		{
			return _builder.CreateCall(_strConcatFunc, { lhs, rhs });
		}
	}

	if (node.Op().Kind() == BoundBinaryOperatorKind::Equals)
	{
		/*
		* NOTE "any" objects treated as addresses/pointers
		*      but strings are stored either in
		*      a) runtime container
		*      b) as constant in binary
		*/
		if (node.Left()->Type() == TypeSymbol(TypeEnum::Any)
			&& node.Right()->Type() == TypeSymbol(TypeEnum::Any))
		{
			return _builder.CreateCall(_ptrEqualFunc, { lhs, rhs });
		} else if (node.Left()->Type() == TypeSymbol(TypeEnum::String)
			&& node.Right()->Type() == TypeSymbol(TypeEnum::String))
		{
			return _builder.CreateCall(_strEqualFunc, { lhs, rhs });
		}
	}

	if (node.Op().Kind() == BoundBinaryOperatorKind::NotEquals)
	{
		llvm::CallInst* value = nullptr;
		auto compareToFalse = [this](llvm::CallInst* v)
		{
			auto false_ = llvm::ConstantInt::getFalse(_context);
			auto op = new llvm::ICmpInst(llvm::CmpInst::Predicate::ICMP_EQ, false_, v);
			return _builder.Insert(op);
		};

		if (node.Left()->Type() == TypeSymbol(TypeEnum::Any)
			&& node.Right()->Type() == TypeSymbol(TypeEnum::Any))
		{
			value = _builder.CreateCall(_ptrEqualFunc, { lhs, rhs });
			return compareToFalse(value);
		} else if (node.Left()->Type() == TypeSymbol(TypeEnum::String)
			&& node.Right()->Type() == TypeSymbol(TypeEnum::String))
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
		case BoundBinaryOperatorKind::Addition:
			return binary(llvm::Instruction::Add);
		case BoundBinaryOperatorKind::Subtraction:
			return binary(llvm::Instruction::Sub);
		case BoundBinaryOperatorKind::Multiplication:
			return binary(llvm::Instruction::Mul);
		case BoundBinaryOperatorKind::Division:
			return binary(llvm::Instruction::SDiv);

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
			throw std::invalid_argument(BuildStringFrom("Unexpected binary operator ",
				GetText(node.Op().SynKind()),
				'(', node.Left()->Type().Name(), ", ",
				node.Right()->Type().Name(), ")")
			);
	}
}

llvm::Value* Emitter::EmitCallExpression(const BoundCallExpression& node)
{
	if (*(node.Function()) == GetBuiltinFunction(BuiltinFuncEnum::Input))
	{
		return _builder.CreateCall(_inputFunc);
	} else if (*(node.Function()) == GetBuiltinFunction(BuiltinFuncEnum::Print))
	{
		auto value = EmitExpression(*node.Arguments().at(0));
		return _builder.CreateCall(_putsFunc, value);
	} else if (*(node.Function()) == GetBuiltinFunction(BuiltinFuncEnum::Rnd))
	{
		auto value = EmitExpression(*node.Arguments().at(0));
		return _builder.CreateCall(_rndFunc, value);
	}

	auto callee = _module->getFunction(string(node.Function()->Name()));
	if (callee == nullptr)
	{
		_diagnostics.ReportFunctionDeclarationNotEmitted(node.Function()->Name());
		return nullptr;
	}
	if (callee->arg_size() != node.Arguments().size())
	{
		_diagnostics.ReportWrongArgumentCountEmitted(node.Function()->Name(),
			node.Arguments().size(), callee->arg_size());
		return nullptr;
	}

	auto args = vector<llvm::Value*>();
	for (const auto& arg : node.Arguments())
		args.push_back(EmitExpression(*arg));

	if (callee->getReturnType()->isVoidTy())
		return _builder.CreateCall(callee, args);
	return _builder.CreateCall(callee, args, string(node.Function()->Name()));
}

llvm::Value* Emitter::EmitConversionExpression(const BoundConversionExpression& node)
{
	auto value = EmitExpression(*node.Expression());
	if (value == nullptr)
		return nullptr;
	if (node.Type() == TypeSymbol(TypeEnum::Any))
	{
		return value;
	} else if (node.Type() == TypeSymbol(TypeEnum::Bool))
	{
		if (value->getType() == _charType->getPointerTo())
			return _builder.CreateCall(_strToBoolFunc, value);
		return value;
	} else if (node.Type() == TypeSymbol(TypeEnum::Int))
	{
		if (value->getType() == _charType->getPointerTo())
			return _builder.CreateCall(_strToIntFunc, value);
		return value;
	} else if (node.Type() == TypeSymbol(TypeEnum::String))
	{
		if (value->getType() == _boolType)
			return _builder.CreateCall(_boolToStrFunc, value);
		if (value->getType() == _intType)
			return _builder.CreateCall(_intToStrFunc, value);
		return value;
	} else
	{
		// NOTE implicit conversions
		//      int -> bool non-zero -> true
		//      bool -> int
		throw std::invalid_argument(BuildStringFrom("Unexpected convertion from '",
			node.Expression()->Type().Name(), "' to '",
			node.Type().Name(), "'."));
	}
}

llvm::Value* Emitter::EmitPostfixExpression(const BoundPostfixExpression& node)
{
	auto value = EmitExpression(*node.Expression());
	try
	{
		auto unit = llvm::ConstantInt::get(_intType, 1, true);
		auto allocaInst = _locals.at(node.Variable()->Name());
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
	} catch (const std::out_of_range&)
	{
		_diagnostics.ReportVariableNotEmitted(node.Variable()->Name());
		return nullptr;
	}
}

DiagnosticBag Emitter::Emit(const BoundProgram& program, const fs::path& outputPath)
{
	if (!_diagnostics.empty())
		return _diagnostics;

	auto result = DiagnosticBag();

	for (const auto& [func, _] : program.Functions())
		EmitFunctionDeclaration(*func);
	for (const auto& [func, body] : program.Functions())
		EmitFunctionBody(*func, *body);

	if (!_diagnostics.empty())
		return _diagnostics;

	std::error_code ec;
	auto dest = llvm::raw_fd_ostream(outputPath.string(), ec, llvm::sys::fs::OF_None);
	if (ec)
	{
		result.ReportCannotOpenOutputFile(ec.message());
		return result;
	}

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

	_knownTypes.emplace(TypeSymbol(TypeEnum::Bool), _boolType);
	_knownTypes.emplace(TypeSymbol(TypeEnum::Int), _intType);
	_knownTypes.emplace(TypeSymbol(TypeEnum::String), _charType->getPointerTo());
	_knownTypes.emplace(TypeSymbol(TypeEnum::Void), _builder.getVoidTy());
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

	// NOTE there is no void* in LLVM representation
	auto voidPtrType = _charType->getPointerTo();
	args = vector<llvm::Type*>(2, voidPtrType);
	ft = llvm::FunctionType::get(_boolType, args, false);
	_ptrEqualFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "ptrEqual", *_module);

	args = vector<llvm::Type*>(2, _charType->getPointerTo());
	ft = llvm::FunctionType::get(_boolType, args, false);
	_strEqualFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "strEqual", *_module);
}

void Emitter::InitTarget()
{
	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

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
			auto node = static_cast<const BoundLabelStatement&>(*it);
			auto bb = llvm::BasicBlock::Create(_context, string(node.Label().Name()));
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

DiagnosticBag Emit(const BoundProgram& program, const string& moduleName,
	const fs::path& outPath)
{
	if (!program.Diagnostics().empty())
		return program.Diagnostics();

	auto e = Emitter(moduleName);
	return e.Emit(program, outPath);
}

} //MCF