module oxd.misc;

import
		std.conv,
		std.range,
		std.format,
		std.string,
		std.bigint,
		oxd.grammar;


bool throwError(string file = __FILE__, uint line = __LINE__, A...)(string fmt, auto ref in A args)
{
    static if(args.length) fmt = format(fmt, args);

    throw new Exception(fmt, file, line);
}

string firstMatch(bool Lower = false, T)(ref inout T t)
{
	static if(Lower)
		return t.matches.front.toLower;
	else
		return t.matches.front;
}

string lastMatch(bool Lower = false, T)(ref inout T t)
{
	static if(Lower)
		return t.matches.back.toLower;
	else
		return t.matches.back;
}

ref firstChild(ref inout ParseTree t) { return t.children.front; }
ref firstChild(inout ParseTree *t) { return t.children.front; }

ref lastChild(ref inout ParseTree t) { return t.children.back; }
ref lastChild(inout ParseTree *t) { return t.children.back; }

auto makeSpaces(uint n) { return cast(string)' '.repeat(n).array; }
auto makeStars(uint n) { return cast(string)'*'.repeat(n).array; }

auto makeDup(T)(auto ref in T v)
{
	auto res = new T;
	*res = cast()v; // TODO: HACK
	return res;
}

auto toDouble(ref in BigInt v)
{
	return v >= long.min && v <= long.max ? double(v.toLong) : v.toDecimalString.to!double;
}

mixin template MakeCtor()
{
	alias T = typeof(this);

	static gen()
	{
		import std.traits, std.meta;

		string	s = `this(`,
				b;

		foreach(n; FieldNameTuple!T)
		{
			alias R = Alias!(__traits(getMember, T, n));

			static if(__traits(getProtection, R) == `public`)
			{
				auto v = n ~ `_`;

				s ~= typeof(R).stringof ~ ` ` ~ v ~ `,`;
				b ~= n ~ `=` ~ v ~ `;`;
			}
		}

		return s ~ `){` ~ b ~ `}`;
	}

	mixin(gen);
}
