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

	std::unordered_map<TypeSymbol, llvm::Type*, SymbolHash, SymbolEqual> _knownTypes;
	std::unordered_map<string_view, llvm::Value*> _locals;
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

public:
	explicit Emitter(const string& moduleName);

	constexpr DiagnosticBag& Diagnostics() noexcept { return _diagnostics; }
	DiagnosticBag Emit(const BoundProgram& program, const fs::path& outputPath);
};

Emitter::Emitter(const string& moduleName)
	:_context(), _builder(_context),
	_module(std::make_unique<llvm::Module>(moduleName, _context)),
	_targetMachine(nullptr),
	_diagnostics()
{
	_knownTypes.emplace(TypeSymbol(TypeEnum::Bool), llvm::ConstantInt::getTrue(_context)->getType());
	_knownTypes.emplace(TypeSymbol(TypeEnum::Int), _builder.getInt32Ty());

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

void Emitter::EmitFunctionBody(const FunctionSymbol& function, const BoundBlockStatement& body)
{
	auto func = _module->getFunction(string(function.Name()));
	if (func == nullptr)
	{
		_diagnostics.ReportFunctionDeclarationNotFound(function.Name());
		return;
	} else if (!func->empty())
	{
		_diagnostics.ReportFunctionViolateODR(function.Name());
		return;
	}
	auto entry = llvm::BasicBlock::Create(_context, "entry", func);
	_builder.SetInsertPoint(entry);

	// Place all parameters into _locals table
	_locals.clear();
	size_t i = 0;
	for (auto& arg : func->args())
	{
		_locals.emplace(function.Parameters().at(i).Name(), &arg);
		++i;
	}

	try
	{
		for (const auto& it : body.Statements())
			EmitStatement(*it);
		llvm::verifyFunction(*func);
	} catch (const std::exception& e)
	{
		_diagnostics.ReportCannotCreateFunctionBody(e.what());
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
			throw std::invalid_argument(BuildStringFrom("Unexpected node:", nameof(node.Kind())));
	}
}

void Emitter::EmitVariableDeclaration(const BoundVariableDeclaration& node)
{
	(void)node;
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
			throw std::invalid_argument(BuildStringFrom("Unexpected node:", nameof(node.Kind())));
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
	} else
	{
		throw std::invalid_argument("Only supports bool and int for now");
	}
	return nullptr;
}

llvm::Value* Emitter::EmitVariableExpression(const BoundVariableExpression& node)
{
	try
	{
		return _locals.at(node.Variable()->Name());
	} catch (const std::out_of_range&)
	{
		_diagnostics.ReportUndefinedVariable(std::nullopt, node.Variable()->Name());
		return nullptr;
	}
}

llvm::Value* Emitter::EmitAssignmentExpression(const BoundAssignmentExpression& node)
{
	(void)node;
	return nullptr;
}

llvm::Value* Emitter::EmitUnaryExpression(const BoundUnaryExpression& node)
{
	(void)node;
	return nullptr;
}

llvm::Value* Emitter::EmitBinaryExpression(const BoundBinaryExpression& node)
{
	(void)node;
	return nullptr;
}

llvm::Value* Emitter::EmitCallExpression(const BoundCallExpression& node)
{
	auto callee = _module->getFunction(string(node.Function()->Name()));
	if (callee == nullptr)
	{
		_diagnostics.ReportUndefinedFunction(std::nullopt, node.Function()->Name());
		return nullptr;
	}
	if (callee->arg_size() != node.Arguments().size())
	{
		_diagnostics.ReportWrongArgumentCount(std::nullopt, node.Function()->Name(),
			callee->arg_size(), node.Arguments().size());
		return nullptr;
	}
	auto args = vector<llvm::Value*>();
	for (const auto& arg : node.Arguments())
		args.push_back(EmitExpression(*arg));
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

	//
	// create a stub function
	// int test() { return 42; }
	// 
	//auto test = llvm::FunctionType::get(_builder.getInt32Ty(), false);
	//auto func = llvm::Function::Create(test, llvm::Function::ExternalLinkage,
	//	"test", _module.get());
	//auto entry = llvm::BasicBlock::Create(_context, "entrypoint", func);
	//auto value = llvm::ConstantInt::get(_context, llvm::APInt(INT_BITS, 42, true));
	//_builder.SetInsertPoint(entry);
	//_builder.CreateRet(value);

	for (const auto& [func, body] : program.Functions())
	{
		EmitFunctionDeclaration(*func);
		EmitFunctionBody(*func, *body);
	}

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

DiagnosticBag Emit(const BoundProgram& program, const string& moduleName, const fs::path& outPath)
{
	if (!program.Diagnostics().empty())
		return program.Diagnostics();

	auto e = Emitter(moduleName);
	return e.Emit(program, outPath);
}

} //MCF