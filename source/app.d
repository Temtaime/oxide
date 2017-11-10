import
		std.file,
		std.conv,
		std.stdio,
		std.range,
		std.string,
		std.process,
		std.algorithm,

		oxd;


shared static this()
{
	createTypes;
	createOpTable;

	cgen = new Codegen;
}

shared static ~this()
{
	cgen.destroy;
}

void main(string[] args)
{
	auto files = [ `test.d` ];

	try
	{
		auto s = files[0].readText;

		auto p = lex.create(s);
		//log(*p);

		{
			auto sc = new Scope(null);

			TreeProcessor().process(sc, *p);
			cgen.process(sc);
		}
		//auto sc = ScopeBuilder().fromTree(*p);
		//exe.makeModule(files);
	}
	catch(Exception e)
	{
		writefln(`%s:%d - %s`, e.file, e.line, e.msg);
	}
}
