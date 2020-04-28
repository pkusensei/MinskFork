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

	llvm::Type* _charType;

	std::unordered_map<TypeSymbol, llvm::Type*, SymbolHash, SymbolEqual> _knownTypes;
	llvm::Function* _inputFunc;
	llvm::Function* _putsFunc; 	// NOTE delegate print to C puts function
	llvm::Function* _strConcatFunc;

	// current working function and local variables
	std::unordered_map<string_view, llvm::AllocaInst*> _locals;
	llvm::Function* _function;

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

	void InitKnownTypes();
	void InitExternFunctions();
	void InitTarget();
	llvm::AllocaInst* CreateEntryBlockAlloca(llvm::Type* type, string_view varName)const;

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
	_charType(_builder.getInt8Ty()),
	_inputFunc(nullptr),
	_putsFunc(nullptr),
	_strConcatFunc(nullptr),
	_function(nullptr),
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
	for (const auto& it : function.Parameters())
	{
		args.push_back(_knownTypes.at(it.Type()));
	}
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

	// Place all parameters into _locals table
	_function = func;
	_locals.clear();
	size_t i = 0;
	for (auto& arg : func->args())
	{
		auto alloca = CreateEntryBlockAlloca(arg.getType(), fs.Parameters().at(i).Name());
		_builder.CreateStore(&arg, alloca);
		_locals.emplace(fs.Parameters().at(i).Name(), alloca);
		++i;
	}

	try
	{
		for (const auto& it : body.Statements())
			EmitStatement(*it);

		//TODO move this to bound tree
		if (fs.Type() == TypeSymbol(TypeEnum::Void))
			_builder.CreateRetVoid();

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
	(void)node;
}

void Emitter::EmitGotoStatement(const BoundGotoStatement& node)
{
	(void)node;
}

void Emitter::EmitConditionalGotoStatement(const BoundConditionalGotoStatement& node)
{
	(void)node;
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
			[charType](const auto& c) { return llvm::ConstantInt::get(charType, c); });
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
		_diagnostics.ReportUndefinedVariable(std::nullopt, node.Variable()->Name());
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
	(void)node;
	return nullptr;
}

llvm::Value* Emitter::EmitBinaryExpression(const BoundBinaryExpression& node)
{
	if (node.Op().Kind() == BoundBinaryOperatorKind::Addition)
	{
		if (node.Left()->Type() == TypeSymbol(TypeEnum::String)
			&& node.Right()->Type() == TypeSymbol(TypeEnum::String))
		{
			auto lhs = EmitExpression(*node.Left());
			auto rhs = EmitExpression(*node.Right());
			return _builder.CreateCall(_strConcatFunc, { lhs, rhs });
		}
	}
	return nullptr;
}

llvm::Value* Emitter::EmitCallExpression(const BoundCallExpression& node)
{
	if (*(node.Function()) == GetBuiltinFunction(BuiltinFuncEnum::Input))
	{
		return _builder.CreateCall(_inputFunc);
	} else if (*(node.Function()) == GetBuiltinFunction(BuiltinFuncEnum::Print))
	{
		auto value = EmitExpression(*node.Arguments()[0]);
		return _builder.CreateCall(_putsFunc, value);
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
	(void)node;
	return nullptr;
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
	auto fileType = llvm::TargetMachine::CGFT_ObjectFile;
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
	_knownTypes.emplace(TypeSymbol(TypeEnum::Bool), llvm::ConstantInt::getTrue(_context)->getType());
	_knownTypes.emplace(TypeSymbol(TypeEnum::Int), _builder.getIntNTy(INT_BITS));
	_knownTypes.emplace(TypeSymbol(TypeEnum::String), _charType->getPointerTo());
	_knownTypes.emplace(TypeSymbol(TypeEnum::Void), _builder.getVoidTy());
}

void Emitter::InitExternFunctions()
{
	auto ft = llvm::FunctionType::get(_charType->getPointerTo(), false);
	_inputFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "input", _module.get());

	auto args = vector<llvm::Type*>{ _charType->getPointerTo() };
	ft = llvm::FunctionType::get(_builder.getInt32Ty(), args, false);
	_putsFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "puts", _module.get());

	args = vector<llvm::Type*>(2, _charType->getPointerTo());
	ft = llvm::FunctionType::get(_charType->getPointerTo(), args, false);
	_strConcatFunc =
		llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "strConcat", _module.get());
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

DiagnosticBag Emit(const BoundProgram& program, const string& moduleName, const fs::path& outPath)
{
	if (!program.Diagnostics().empty())
		return program.Diagnostics();

	auto e = Emitter(moduleName);
	return e.Emit(program, outPath);
}

} //MCF