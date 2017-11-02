module oxd.func;

import
		std.range,
		std.string,
		std.algorithm,

		oxd;


enum
{
	F_VARARG	= 1,
	F_NORET		= 2,
}

final class Func
{
	void gen(Scope sc)
	{
		tp = tp.resolve;

		args.each!((ref a) => a.tp = a.tp.resolve);

		auto as = args
						.map!(a => a.tp.toLLVM)
						.array;

		auto ty = LLVMFunctionType(tp.toLLVM, as.ptr, cast(uint)as.length, !!(flags & F_VARARG));

		fn = LLVMAddFunction(cgen.mod, lex.name(id).toStringz, ty);
	}

	Type tp;
	FuncArg[] args;

	const(ParseTree)[] bd;

	uint id;
	ubyte flags;

	LLVMValueRef fn;
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
