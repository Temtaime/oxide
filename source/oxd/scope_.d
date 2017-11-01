module oxd.scope_;

import
		oxd;


enum
{
	SC_FIND_IN_CURRENT	= 1,
}

class Scope
{
	this(Scope sc)
	{
		base = sc;
	}

	void dump()
	{
		syms.keys.map!(a => lex.name(a)).log;
	}

	void declare(uint id, Scope s)
	{
		find(id) && throwError(`symbol %s is already declared`, lex.name(id));

		syms[id] = s;
	}

	Scope find(uint id, ubyte flags = 0)
	{
		if(auto p = id in syms)
		{
			return *p;
		}

		if(flags & SC_FIND_IN_CURRENT)
		{
			return null;
		}

		return base ? base.find(id, flags) : null;
	}

	Scope base;
	Scope[uint] syms;
}

class ScopeFunc : Scope
{
	this(Scope sc, Func f)
	{
		super(sc);

		fn = f;
	}

	Func fn;
}

class ScopeVar : Scope
{
	this(Scope sc, Var v)
	{
		super(sc);

		var = v;
	}

	Var var;
}
