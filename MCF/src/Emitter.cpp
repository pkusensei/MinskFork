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
#include "BoundStatements.h"
#include "Diagnostic.h"

namespace MCF {

class Emitter
{
private:
	llvm::LLVMContext _context;
	llvm::IRBuilder<llvm::LLVMContext> _builder;
	unique_ptr<llvm::Module> _module;
	llvm::TargetMachine* _targetMachine;

	DiagnosticBag _diagnostics;

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

DiagnosticBag Emitter::Emit(const BoundProgram& program, const fs::path& outputPath)
{
	(void)program;

	auto result = DiagnosticBag();

	//
	// create a stub function
	// int test() { return 42; }
	// 
	auto test = llvm::FunctionType::get(_builder.getInt32Ty(), false);
	auto func = llvm::Function::Create(test, llvm::Function::ExternalLinkage,
		"test", _module.get());
	auto entry = llvm::BasicBlock::Create(_context, "entrypoint", func);
	auto value = llvm::ConstantInt::get(_context, llvm::APInt(sizeof(IntegerType) * 8, 42, true));
	_builder.SetInsertPoint(entry);
	_builder.CreateRet(value);

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
	if (!e.Diagnostics().empty())
		return e.Diagnostics();
	return e.Emit(program, outPath);
}

} //MCF