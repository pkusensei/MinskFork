#include "Emitter.h"

#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4068 4100 4141 4146 4244 4245 4291 4324 4458 4624)
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

#include "common.h"
#include "Diagnostic.h"

namespace MCF {

DiagnosticBag Emit(const string& moduleName, const fs::path& outPath)
{
	static auto context = llvm::LLVMContext();
	static auto builder = llvm::IRBuilder<>(context);
	static auto module = std::make_unique<llvm::Module>(moduleName, context);

	auto result = DiagnosticBag();

	//
	// create a stub function
	// int test() { return 42; }
	// 
	auto test = llvm::FunctionType::get(builder.getInt32Ty(), false);
	auto func = llvm::Function::Create(test, llvm::Function::ExternalLinkage,
		"test", module.get());
	auto entry = llvm::BasicBlock::Create(context, "entrypoint", func);
	auto value = llvm::ConstantInt::get(context, llvm::APInt(32, 42, true));
	builder.SetInsertPoint(entry);
	builder.CreateRet(value);

	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

	auto targetTriple = llvm::sys::getDefaultTargetTriple();
	module->setTargetTriple(targetTriple);
	string error;
	auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
	if (target == nullptr)
	{
		result.ReportRequestedTargetNotFound(error);
		return result;
	}

	auto cpu = "generic";
	auto features = "";
	auto opt = llvm::TargetOptions();
	auto relocModel = llvm::Optional<llvm::Reloc::Model>();
	auto targetMachine = target->createTargetMachine(
		targetTriple, cpu, features, opt, relocModel);
	module->setDataLayout(targetMachine->createDataLayout());

	std::error_code ec;
	auto dest = llvm::raw_fd_ostream(outPath.string(), ec, llvm::sys::fs::OF_None);
	if (ec)
	{
		result.ReportCannotOpenOutputFile(ec.message());
		return result;
	}

	auto pass = llvm::legacy::PassManager();
	auto fileType = llvm::TargetMachine::CGFT_ObjectFile;
	if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType))
	{
		result.ReportCannotEmitFileType();
		return result;
	}

	pass.run(*module);
	dest.flush();
	return result;
}

} //MCF