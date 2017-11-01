module oxd.tree;

import
		std.conv,
		std.array,
		std.stdio,
		std.range,
		std.string,
		std.algorithm,

		oxd;


struct TreeProcessor
{
	void process(Scope sc, ref ParseTree t)
	{
		switch(t.name)
		{
		case `OXD.Main`:
			foreach(ref c; t.children)
			{
				process(sc, c);
			}

			break;

		case `OXD.FuncStmt`:
			auto f = new Func;
			auto fs = new ScopeFunc(sc, f);

			f.tp = new TypeDeclaration(t.firstChild);
			f.id = lex.id(t.children[1].firstMatch);
			f.bd = t.children[2..$].find!(a => a.name != `OXD.Args`);

			sc.declare(f.id, fs);
			break;

		default:
			assert(false, t.toString);
		}
	}
}
