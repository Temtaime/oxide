module oxd.type;

import
		std.path,
		std.stdio,
		std.range,
		std.traits,
		std.string,
		std.typecons,
		std.algorithm,

		oxd;

public import oxd.type.typeint;
public import oxd.type.resolver;
public import oxd.type.typestruct;


__gshared TypeInt	typeBool,
					typeUbyte,
					typeInt,
					typeLong,
					typeCent;

void createTypes()
{
	typeBool = create!TypeInt(1, false);
	typeUbyte = create!TypeInt(8, false);
	typeInt = create!TypeInt(32, true);
	typeLong = create!TypeInt(64, true);
	typeCent = create!TypeInt(128, true);
}

auto create(T : Type)(ParameterTypeTuple!(T.__ctor) args)
{
	alias K = Tuple!(typeof(args));
	__gshared T[K] aa;

	auto k = K(args);

	if(auto p = k in aa)
	{
		//writefln("reusing %s", T.stringof);
		return *p;
	}

	return aa[k] = new T(args);
}

alias createType = create;

abstract class Type
{
	Type common(Type)
	{
		assert(false);
	}

	final resolve()
	{
		if(auto t = cast(TypeDeclaration)this)
		{
			return t.make;
		}

		return this;
	}

	/*final decl()
	{
		auto t = cast(TypeDeclaration)this;

		assert(t);
		return t.make;
	}

	final resolve(bool name = false)
	{
		return resv.get(this, name);
	}*/

	LLVMTypeRef toLLVM() const { assert(false); }

	Type unqual() { return this; }
}

final:

class TypeDeclaration : Type
{
	mixin MakeCtor;

	Type make()
	{
		return make(p);
	}

	const(ParseTree) p;
private:
	static Type make(ref in ParseTree p)
	{
		switch(p.name)
		{
		//case `BDC.TypeConst`:
		//	return create!TypeConst(make(p.firstChild));

		//case `BDC.TypePtr`:
		//	return create!TypePtr(make(p.firstChild));

		//case `BDC.TypeSlice`:
		//	auto t = make(p.firstChild);
		//	return create!TypeIncomplete(lex.id(`__slice`), [ t ], exe.scope_);

		case `OXD.TypeSpecial`:
			final switch(p.firstMatch)
			{
			case `void`:
				return TypeVoid.instance;
			case `auto`:
				return TypeAuto.instance;
			}

		case `OXD.TypeInt`:
			final switch(p.firstMatch)
			{
			//case `uint`:
			//	return typeUint;
			case `int`:
				return typeInt;
			case `bool`:
				return typeBool;
			}

		//case `BDC.TypeUint`:
		//	return create!TypeInt(p.firstChild.firstMatch.to!uint, false);

		/*case `BDC.TypeUser`:
			auto args = p
							.children
							.map!(a => make(a))
							.array;

			return create!TypeIncomplete(lex.id(p.firstMatch), args, exe.scope_);*/

		default:
			assert(false, p.name);
		}
	}
}

class TypeVoid : Type
{
	private this() {}

	override LLVMTypeRef toLLVM() const
	{
		return LLVMVoidType();
	}

	__gshared instance = new TypeVoid;
}

class TypeAuto : Type
{
	private this() {}

	__gshared instance = new TypeAuto;
}

/*

class TypeIncomplete : Type
{
	private mixin MakeCtor;

	uint id;
	Type[] args;
	Scope sc;
}



class TypePtr : Type
{
	private mixin MakeCtor;

	override LLVMTypeRef toLLVM() const
	{
		return LLVMPointerType(tp.toLLVM, 0);
	}

	Type tp;
}

class TypeConst : Type
{
	private mixin MakeCtor;

	override LLVMTypeRef toLLVM() const
	{
		return tp.toLLVM;
	}

	override Type unqual()
	{
		return tp;
	}

	Type tp;
}

class TypeArray : Type
{
	private mixin MakeCtor;

	override LLVMTypeRef toLLVM() const
	{
		return LLVMArrayType(tp.toLLVM, size);
	}

	Type tp;
	uint size;
}
*/
