module oxd.gen.expr;

import
		std.conv,
		std.array,
		std.stdio,
		std.range,
		std.format,
		std.bigint,
		std.string,
		std.algorithm,

		oxd;


struct ExprGen
{
	this(Scope s)
	{
		sc = s;
	}

	Var process(ref in ParseTree t)
	{
		switch(t.name)
		{
		case `OXD.FuncExpr`:
			auto id = lex.id(t.firstMatch);
			auto es = t.children.map!(a => process(a)).array;

			{
				auto s = sc.find(id);

				if(auto sf = cast(ScopeFunc)s)
				{
					auto f = sf.fn;

					auto vr = LLVMBuildCall(cgen.bd, f.fn, es.map!(a => a.value).array.ptr, cast(uint)es.length, ``);
					return new Var(f.tp, vr);
				}
				else
				{
					auto n = lex.name(id);

					!s || throwError(`%s is not a function`, n);
					throwError(`use of undeclared function %s`, n);
				}
			}

			assert(false);

		case `OXD.AsgExpr`:
			Var v;

			foreach(c; t.children.retro.map!(a => process(a)))
			{
				if(v)
				{
					c.assign(v);
				}

				v = c;
			}

			return v;

		case `OXD.UnaryExpr`:
			auto e = process(t.lastChild);

			final switch(t.firstMatch)
			{
			case `&`:
				return new Var(create!TypePtr(e.tp), e.addr);
			case `*`:
				auto tp = cast(TypePtr)e.tp;
				tp || throwError(`expression cannot be dereferenced`);
				return new MemVar(tp.tp, e.value);
			}

		case `OXD.EquExpr`:
		case `OXD.CmpExpr`:
		case `OXD.AddExpr`:
		case `OXD.MulExpr`:
			Var v;
			auto ops = t.children[1..$].stride(2).map!(a => opTable[a.firstMatch]);

			foreach(c; t.children.stride(2).map!(a => process(a)))
			{
				if(v)
				{
					v = Op.binary(ops.front, v, c);
					ops.popFront;
				}
				else
				{
					v = c;
				}
			}

			return v;

		case `OXD.StringLiteral`:
			auto s = format(`[%s]`, t.firstMatch).to!(string[]).front;
			auto v = LLVMConstString(s.ptr, cast(uint)s.length, false);

			auto g = LLVMAddGlobal(cgen.mod, LLVMTypeOf(v), ``);
			LLVMSetInitializer(g, v);

			auto z = LLVMConstNull(typeInt.toLLVM);
			v = LLVMBuildGEP(cgen.bd, g, [ z, z ].ptr, 2, ``);

			return new Var(create!TypePtr(typeUbyte), v);

		case `OXD.IntegerLiteral`:
			auto tp = typeInt;
			return new Var(tp, LLVMConstIntOfString(tp.toLLVM, t.firstMatch.toStringz, 10));

		case `OXD.Identifier`:
			auto v = cast(ScopeVar)sc.find(lex.id(t.firstMatch));

			v || throwError(`unknown identifier`);
			return v.var;

		default:
			assert(false, t.name);
		}
	}

	Scope sc;
}
