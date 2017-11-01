module oxd.gen;

import
		std.file,
		std.stdio,
		std.process,

		oxd;


alias cgen = Codegen.instance;

final class Codegen
{
	__gshared Codegen instance;

	this()
	{
		LLVMInitializeX86Target();
		LLVMInitializeX86TargetMC();
		LLVMInitializeX86TargetInfo();
		LLVMInitializeX86AsmPrinter();

		ctx = LLVMGetGlobalContext();
		mod = LLVMModuleCreateWithNameInContext(``, ctx);
		bd = LLVMCreateBuilderInContext(ctx);
	}

	~this()
	{
		LLVMDisposeBuilder(bd);
		LLVMDisposeModule(mod);
	}

	void process(Scope sc)
	{
		foreach(c; sc.syms)
		{
			if(auto s = cast(ScopeFunc)c)
			{
				s.fn.gen(s);
			}
		}

		{
			char *err;

			if(LLVMVerifyModule(mod, LLVMReturnStatusAction, &err))
			{
				auto s = err.to!string;
				LLVMDisposeMessage(err);

				LLVMDumpModule(mod);
				throwError(`module verification failed: %s`, s);
			}

			auto file = `test.ll`;

			chdir(`bin`);
			LLVMPrintModuleToFile(mod, file.ptr, &err);
		}

		version(linux)
		{
			environment[`PATH`] = environment[`PATH`] ~ `:/opt/llvm/bin`;
		}

		executeShell(`opt -Oz test.ll | llc -O3 --x86-asm-syntax=intel -o test.asm`).output.writeln;
		executeShell(`opt -Oz test.ll | llc -O3 -filetype=obj -o test.o -data-sections -function-sections`).output.writeln;

		version(linux)
		{
			executeShell(`gcc test.o -o test`);
			executeShell(`./test`).writeln;
		}
		else
		{
			executeShell(`link /libpath:lib msvcrt.lib kernel32.lib test.o /opt:ref,icf /subsystem:console /manifest:no /out:test.exe`);
			executeShell(`test.exe`).writeln;
		}

		std.file.remove(`test.o`);
	}

	LLVMModuleRef mod;
	LLVMContextRef ctx;
	LLVMBuilderRef bd;
}
