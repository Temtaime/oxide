module oxd.op;

import
		std.traits,
		oxd;


enum
{
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,

	OP_EQU,
	OP_NEQU,
	OP_LES,
	OP_GRE,
	OP_ELES,
	OP_EGRE,

	OP_NEG,
	OP_PLUS,
	OP_INV,
	OP_ADDR,
	OP_DEREF,
}

__gshared ubyte[string] opTable;

struct Op
{
	static unary(ubyte op, Var a)
	{
		if(auto r = typeSwitchOne!mathInt(a.tp.unqual, a, op)) return r;

		return null;
	}

	static binary(ubyte op, Var a, Var b)
	{
		if(auto r = typeSwitch!mathInts(a.tp.unqual, b.tp.unqual, a, b, op)) return r;
		//if(auto r = typeSwitch!mathPtrInt(a.t.unqual, b.t.unqual, a, b, op)) return r;

		assert(false);
	}

private:
	// UNARY
	static mathInt(TypeInt a, Var u, ubyte op)
	{
		/*if(op == OP_PLUS)
		{
			return new Var(u.t, u.expr);
		}*/

		if(a == typeBool)
		{
			return null;
		}

		final switch(op)
		{
		//case OP_PLUS:
		//	return new Var(v.t, );
		case OP_NEG:
			if(a.signed)
			{
				return new Var(u.tp, LLVMBuildNSWNeg(cgen.bd, u.value, ``));
			}

			return new Var(u.tp, LLVMBuildNeg(cgen.bd, u.value, ``));
		}
	}

	// POINTERS
	/*static mathIntPtr(TypeInt a, TypePtr b, Var u, Var v, ubyte op)
	{
		return op == OP_ADD ? mathPtrInt(b, a, v, u, op) : null;
	}

	static mathPtrInt(TypePtr a, TypeInt b, Var u, Var v, ubyte op)
	{
		if(op == OP_ADD || OP_SUB)
		{
			v = v.cast_(typeInt);

			if(op == OP_SUB)
			{
				v = unary(OP_NEG, v);
			}

			auto s = [ v.expr ];
			return new Var(u.t, new OffsetGenExpr(u.expr, s));
		}

		return null;
	}*/

	// INTEGERS
	static mathInts(TypeInt a, TypeInt b, Var u, Var v, ubyte op)
	{
		auto ty = a.promote.common(b.promote);

		u = u.cast_(ty);
		v = v.cast_(ty);

		auto g = makeExpr(u, v, op, (cast(TypeInt)ty).signed);

		if(op >= OP_EQU && op <= OP_EGRE)
		{
			ty = typeBool;
		}
		/*else if(u.isConst && v.isConst)
		{
			ty = create!TypeConst(ty);
		}*/

		return new Var(ty, g);
	}

private:
	static makeExpr(Var a, Var b, ubyte op, bool signed)
	{
		auto
				u = a.value,
				v = b.value;

		final switch(op)
		{
		case OP_ADD:
			return LLVMBuildAdd(cgen.bd, u, v, ``);
		case OP_SUB:
			return LLVMBuildSub(cgen.bd, u, v, ``);
		case OP_MUL:
			if(signed)
			{
				return LLVMBuildNSWMul(cgen.bd, u, v, ``);
			}
			else
			{
				return LLVMBuildMul(cgen.bd, u, v, ``);
			}

		case OP_DIV:
			if(signed)
			{
				return LLVMBuildSDiv(cgen.bd, u, v, ``);
			}
			else
			{
				return LLVMBuildUDiv(cgen.bd, u, v, ``);
			}

		case OP_EQU:
		case OP_NEQU:
			auto table = [ LLVMIntEQ, LLVMIntNE ];

			return LLVMBuildICmp(cgen.bd, table[op - OP_EQU], u, v, ``);

		case OP_LES:
		case OP_GRE:
		case OP_ELES:
		case OP_EGRE:
			auto table = signed
								? [ LLVMIntSLT, LLVMIntSGT, LLVMIntSLE, LLVMIntSGE ]
								: [ LLVMIntULT, LLVMIntUGT, LLVMIntULE, LLVMIntUGE ];

			return LLVMBuildICmp(cgen.bd, table[op - OP_LES], u, v, ``);
		}
	}
}

void createOpTable()
{
	opTable =
	[
		`+`: OP_ADD,
		`-`: OP_SUB,
		`*`: OP_MUL,
		`/`: OP_DIV,

		`==`: OP_EQU,
		`!=`: OP_NEQU,
		`<`: OP_LES,
		`>`: OP_GRE,
		`<=`: OP_ELES,
		`>=`: OP_EGRE,
	];
}

private:

auto typeSwitch(alias F)(Type u, Type v, Var e, Var q, ubyte op)
{
	alias Args = ParameterTypeTuple!F;

	alias A = Args[0];
	alias B = Args[1];

	if(auto a = cast(A)u)
	if(auto b = cast(B)v) return F(a, b, e, q, op);

	return null;
}

auto typeSwitchOne(alias F)(Type u, Var e, ubyte op)
{
	alias A = ParameterTypeTuple!F[0];

	if(auto a = cast(A)u) return F(a, e, op);

	return null;
}
