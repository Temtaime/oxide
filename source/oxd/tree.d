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

			{
				auto arr = t.children[2..$];
				auto args = arr.until!(a => a.name != `OXD.Arg`).array;

				foreach(ref c; args)
				{
					if(c.children.length)
					{
						FuncArg a =
						{
							tp: new TypeDeclaration(c.firstChild)
						};

						if(c.children.length > 1)
						{
							a.id = lex.id(c.lastChild.firstMatch);
						}

						f.args ~= a;
					}
					else
					{
						f.flags |= F_VAR_ARG;
						(&c) == &args.back || throwError(`extra arguments`);
					}
				}

				if(t.lastMatch != `;`)
				{
					f.flags |= F_BODY;
					f.bd = arr[args.length..$];
				}
			}

			sc.declare(f.id, fs);
			break;

		default:
			assert(false, t.toString);
		}
	}
}
