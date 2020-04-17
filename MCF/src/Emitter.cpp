#include "Emitter.h"

#if defined(_MSC_VER) && !defined(__clang__)
#pragma warning(push)
#pragma warning(disable: 4068 4100 4141 4146 4244 4245 4291 4324 4458 4624)
#else //defined(_MSC_VER) && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wlanguage-extension-token"
#endif //defined(_MSC_VER) && !defined(__clang__)

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

DiagnosticBag Emit(const string& outPath)
{
	static auto context = llvm::LLVMContext();
	static auto builder = llvm::IRBuilder<>(context);
	static auto module = std::make_unique<llvm::Module>("test", context);

	auto result = DiagnosticBag();

	auto funcType = llvm::FunctionType::get(builder.getVoidTy(), false);
	auto mainFunc = llvm::Function::Create(funcType,
		llvm::Function::ExternalLinkage, "main", module.get());

	auto entry = llvm::BasicBlock::Create(context, "entrypoint", mainFunc);
	builder.SetInsertPoint(entry);
	auto text = builder.CreateGlobalStringPtr("Hello world from mcf.\n");

	auto putsArgs = vector<llvm::Type*>();
	putsArgs.push_back(builder.getInt8Ty()->getPointerTo());
	auto argsRef = llvm::ArrayRef<llvm::Type*>(putsArgs);

	auto putsType = llvm::FunctionType::get(builder.getInt32Ty(), argsRef, false);
	auto putsFunc = module->getOrInsertFunction("puts", putsType);

	builder.CreateCall(putsFunc, text);
	builder.CreateRetVoid();

	//module->dump();

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
		result.ReportRequestedTargetNotFound(error);

	auto cpu = "generic";
	auto features = "";
	auto opt = llvm::TargetOptions();
	auto relocModel = llvm::Optional<llvm::Reloc::Model>();
	auto targetMachine = target->createTargetMachine(
		targetTriple, cpu, features, opt, relocModel);
	module->setDataLayout(targetMachine->createDataLayout());

	std::error_code ec;
	auto dest = llvm::raw_fd_ostream(outPath, ec, llvm::sys::fs::OF_None);
	if (ec)
		result.ReportCannotOpenOutputFile(ec.message());

	auto pass = llvm::legacy::PassManager();
	auto fileType = llvm::TargetMachine::CGFT_ObjectFile;
	if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType))
		result.ReportCannotEmitFileType();

	pass.run(*module);
	dest.flush();
	return result;
}

} //MCF