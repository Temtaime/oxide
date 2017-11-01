module oxd.var;

import
		std.conv,
		std.range,
		std.stdio,
		std.bigint,
		std.traits,
		std.string,
		std.datetime,
		std.algorithm,

		oxd;


class Var
{
	this(Type t, LLVMValueRef v)
	{
		tp = t;
		_vr = v;
	}

	//bool isConst() { return !!cast(TypeConst)t; }

	auto cast_(Type to, bool ex = false)
	{
		if(tp == to) return this; // equal

		/*if(auto ta = cast(TypeInt)t)
		{
			if(auto tb = cast(TypeInt)to)
			{
				if(ta.bits == tb.bits) return new Var(tb, expr); // sign

				if(ta.bits < tb.bits)
				{
					return new Var(tb, new ExtendGenExpr(expr, tb, ta.signed));
				}
			}
		}*/

		//throwError(`can't cast from type %s to %s`, t.toString, to.toString);
		assert(false);
	}

	void assign(Var v)
	{
		throwError(`not a l-value`);
	}

	LLVMValueRef value()
	{
		return _vr;
	}

	LLVMValueRef addr()
	{
		throwError(`can't take an address`);
		return null;
	}

	Type tp;
private:
	LLVMValueRef _vr;
}

final class MemVar : Var
{
	this(Type t)
	{
		super(t, LLVMBuildAlloca(cgen.bd, t.toLLVM, ``));
	}

	override void assign(Var v)
	{
		LLVMBuildStore(cgen.bd, v.value, _vr);
	}

	override LLVMValueRef value()
	{
		return LLVMBuildLoad(cgen.bd, _vr, ``);
	}

	//override GenExpr expr() { return new LoadGenExpr(_e); }
	override LLVMValueRef addr() { return _vr; }
}

/*class Storage
{
	LLVMValueRef value()
	{
		return _ex;
	}

	LLVMValueRef addr()
	{
		throwError(`can't take an address`);
		return null;
	}

private:
	LLVMValueRef _ex;
}


class MemStorage
{
	override LLVMValueRef value()
	{
		return LLVMBuildLoad(cgen.bd, _ex, ``);
	}

	LLVMValueRef addr()
	{
		return _ex;
	}
}*/
