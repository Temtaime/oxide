module oxd.func;

import
		std.range,
		std.string,

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

		{
			auto ty = LLVMFunctionType(tp.toLLVM, null, 0, !!(flags & F_VARARG));

			fn = LLVMAddFunction(cgen.mod, lex.name(id).toStringz, ty);
		}

		FuncCodegen(this).process(bd, new Scope(sc));
	}

	Type tp;
	//TypeStruct st;

	//FuncArg[] args;
	//BlockStmt body_;

	//FuncGen gen;
	const(ParseTree)[] bd;

	uint id;
	ubyte flags;

	LLVMValueRef fn;
}

/*struct FuncArg
{
	Type tp;
	Expr e;
}*/

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
