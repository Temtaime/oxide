module oxd.type.resolver;

import
		std.array,
		std.algorithm,
		oxd;


//alias resv = Resolver.instance;

/*final class Resolver
{
	__gshared instance = new Resolver;

	Type get(Type t, bool name)
	{
		scope(exit)
		{
			if(!_arr.length)
			{
				_delay.byKeyValue.filter!(a => !a.value).each!(a => get(a.key, false));
			}
		}

		if(name)
		{
			return impl(t, true);
		}

		_arr.canFind(t) && throwError(`circular reference resolving %s`, t.toString);
		_arr ~= t;

		auto r = impl(t, false);
		_arr.popBack;

		return r;
	}

private:
	Type impl(Type t, bool name)
	{
		if(auto ty = cast(TypeInt)t) return ty;
		if(auto ty = cast(TypeVoid)t) return ty;
		if(auto ty = cast(TypeAuto)t) return ty;

		if(auto ty = cast(TypePtr)t)
		{
			return create!TypePtr(get(ty.tp, true));
		}

		if(auto ty = cast(TypeConst)t)
		{
			return create!TypeConst(get(ty.tp, name));
		}

		if(auto ty = cast(TypeArray)t)
		{
			return create!TypeArray(get(ty.tp, false), ty.size);
		}

		if(auto ty = cast(TypeIncomplete)t) with(ty)
		{
			auto s = sc.find(id);
			s || throwError(`unresolved type %s`, lex.name(id));

			if(auto sa = cast(SymType)s)
			{
				return get(sa.tp, name);
			}

			if(auto sa = cast(SymStruct)s)
			{
				return get(sa.make(ty.args), name);
			}

			throwError(`%s is not a typename/alias`, lex.name(id));
		}

		if(auto ty = cast(TypeStruct)t)
		{
			if(name)
			{
				if(!(ty in _delay)) _delay[ty] = false;
			}
			else
			{
				_delay[ty] = true;
				ty.process;
			}

			return ty;
		}

		assert(false, t.toString);
	}

	Type[] _arr;
	bool[TypeStruct] _delay;
}
*/
