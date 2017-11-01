module oxd.type.typeint;

import
		std.algorithm,
		oxd;


final class TypeInt : Type
{
	package this(uint b, bool s)
	{
		bits = b;
		signed = s;

		assert(bits > 1 || !signed);
	}

	auto promote()
	{
		if(bits < 32) return typeInt;
		if(bits > 32 && bits < 64) return typeLong;
		if(bits > 64 && bits < 128) return typeCent;

		return this;
	}

	override Type common(Type t)
	{
		if(auto r = cast(TypeInt)t)
		{
			return create!TypeInt(max(bits, r.bits), signed && r.signed);
		}

		return null;
	}

	override LLVMTypeRef toLLVM() const
	{
		return LLVMIntType(bits);
	}

const:
	uint bits;
	bool signed;
}
