module oxd.lexer;

import
		std.stdio,
		std.array,
		std.typecons,
		std.algorithm,

		oxd;


alias lex = Lexer.instance;

final class Lexer
{
	__gshared instance = new Lexer;

	auto create(string s)
	{
		return treeOf(s);
	}

	auto id(string s)
	{
		if(auto p = s in _names)
		{
			return *p;
		}

		return _names[s] = cast(uint)_names.length + 1;
	}

	auto name(uint id)
	{
		foreach(k, v; _names)
		{
			if(v == id) return k;
		}

		assert(false);
	}

private:
	auto treeOf(string s)
	{
		if(auto t = s in _tree)
		{
			return t;
		}

		auto r = OXD(s);
		r.successful || throwError(r.failMsg);

		//r.writeln;
		optimize(r = r.firstChild);
		//r.writeln;

		return &(_tree[s] = r);
	}

	static void optimize(ref ParseTree t, bool process = false)
	{
		if(t.name == `OXD.Expr`)
		{
			return optimize(t = t.firstChild, true);
		}

		static immutable unopt = [ `OXD.FuncExpr`, `OXD.DotBody`, `OXD.CastExpr` ];

		if((process |= t.name == `OXD.AsgExpr`) && !unopt.canFind(t.name) && t.children.length == 1)
		{
			return optimize(t = t.firstChild, true);
		}

		t.children.each!((ref a) => optimize(a, process));
	}

	uint[string] _names;
	ParseTree[string] _tree;
}
