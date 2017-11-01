module oxd.type.typestruct;

import
		std.meta,
		std.algorithm,
		oxd;


final:

/*class StructScope
{
	mixin MakeCtor;

	Scope sc;

	uint[] args;
	Stmt[] members;
}

class TypeStruct : Type
{
	this(StructScope ss, Type[] args)
	{
		_ss = ss;
		_args = args;

		sc = new ScopeStruct(ss.sc, this);

		_t = LLVMStructCreateNamed(cgen.ctx, ``);
	}

	void process()
	{
		if(!_done)
		{
			{
				auto en = Enscoper(sc);

				foreach(i, id; _ss.args)
				{
					auto a = _args[i];

					sc.decl(id, new SymType(a));
				}

				foreach(T; AliasSeq!(VarStmt, FuncStmt))
				{
					auto r = _ss.members.map!(a => cast(T)a).filter!(a => a);

					static if(is(T == FuncStmt))
					{
						r.each!(a => a.decl2(this));
						r.each!(a => a.exec);
					}
					else
					{
						r.each!(a => a.exec2);
					}
				}
			}

			auto arr = sc.syms
								.values
								.map!(a => cast(SymVar)a)
								.filter!(a => a)
								.array
								.sort!((a, b) => a.num < b.num)
								.map!(a => a.v.t.toLLVM)
								.array;

			_done = true;
			LLVMStructSetBody(_t, arr.ptr, cast(uint)arr.length, false);
		}
	}

	override LLVMTypeRef toLLVM() const
	{
		return cast(LLVMTypeRef)_t;
	}

	//Scope sc;
private:
	LLVMTypeRef _t;

	Type[] _args;
	StructScope _ss;

	bool _done;
}
*/
