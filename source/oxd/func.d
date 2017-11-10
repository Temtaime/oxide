module oxd.func;

import
		std.range,
		std.string,
		std.algorithm,

		oxd;


enum
{
	F_VAR_ARG	= 1,
	F_BODY		= 2,
	F_DONE		= 4,
}

final class Func
{
	@property fn()
	{
		processDecl;
		return _fn;
	}

	void gen(Scope sc)
	{
		processDecl;

		if(flags & F_BODY && !(flags & F_DONE))
		{
			flags |= F_DONE;

			FuncCodegen(this).process(new Scope(sc));
		}
	}

	Type tp;
	FuncArg[] args;

	const(ParseTree)[] bd;

	uint id;
	ubyte flags;
private:
	void processDecl()
	{
		if(!_fn)
		{
			tp = tp.resolve;
			args.each!((ref a) => a.tp = a.tp.resolve);

			auto as = args
							.map!(a => a.tp.toLLVM)
							.array;

			auto ty = LLVMFunctionType(tp.toLLVM, as.ptr, cast(uint)as.length, !!(flags & F_VAR_ARG));
			_fn = LLVMAddFunction(cgen.mod, lex.name(id).toStringz, ty);
		}
	}

	LLVMValueRef _fn;
}

struct FuncArg
{
	uint id;
	Type tp;
}

final class Block
{
	this(LLVMValueRef fn)
	{
		bl = LLVMAppendBasicBlockInContext(cgen.ctx, fn, ``);
	}

	void activate()
	{
		LLVMPositionBuilderAtEnd(cgen.bd, bl);
	}

	LLVMBasicBlockRef bl;
}
