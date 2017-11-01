module oxd.func;

import
		std.range,
		std.string,

		oxd;


enum
{
	F_VARARG	= 1,
	F_NORET		= 2,
}

final class Func
{
	void gen(Scope sc)
	{
		tp = tp.resolve;

		{
			auto ty = LLVMFunctionType(tp.toLLVM, null, 0, !!(flags & F_VARARG));

			fn = LLVMAddFunction(cgen.mod, lex.name(id).toStringz, ty);
		}

		FuncCodegen(this).process(bd, new Scope(sc));
	}

	Type tp;
	//TypeStruct st;

	//FuncArg[] args;
	//BlockStmt body_;

	//FuncGen gen;
	const(ParseTree)[] bd;

	uint id;
	ubyte flags;

	LLVMValueRef fn;
}

struct FuncCodegen
{
	this(Func f)
	{
		_f = f;

		_cur = new Block(f.fn);
		_ret = new Block(f.fn);

		_cur.activate;
		_retval = new MemVar(f.tp);
	}

	~this()
	{
		_ret.activate;

		if(_retval)
		{
			LLVMBuildRet(cgen.bd, _retval.value);
		}
		else
		{
			LLVMBuildRetVoid(cgen.bd);
		}
	}

	void process(in ParseTree[] arr, Scope sc)
	{
		processBlock(arr, sc);

		doesRet || _f.tp is TypeVoid.instance || throwError(`function does not return value`);
	}

	void process(ref in ParseTree t, Scope sc)
	{
		switch(t.name)
		{
		case `OXD.VarStmt`:
			auto tp = new TypeDeclaration(t.firstChild).resolve;

			foreach(ref c; t.children[1..$])
			{
				auto v = new MemVar(tp);

				sc.declare(lex.id(c.firstMatch), new ScopeVar(sc, v));

				if(c.children.length > 1)
				{
					v.assign(ExprGen(sc).process(c.lastChild));
				}
			}

			break;

		case `OXD.IfStmt`:
			auto e = ExprGen(sc).process(t.firstChild).cast_(typeBool, true);
			auto hasElse = t.children.length > 2;

			{
				Block cont;

				auto bt = makeBlock;
				auto bf = makeBlock;

				LLVMBuildCondBr(cgen.bd, e.value, bt.bl, bf.bl);

				// main block
				reblock(bt);
				processBlock(t.children[1], sc);

				if(doesRet)
				{
					_flags &= ~EX_RETURN;
				}
				else
				{
					cont = hasElse ? makeBlock : bf;
					LLVMBuildBr(cgen.bd, cont.bl);
				}

				// else block
				if(hasElse)
				{
					reblock(bf);
					processBlock(t.children[2], sc);

					if(doesRet)
					{
						if(cont)
						{
							_flags &= ~EX_RETURN;
						}
						else
						{
							break;
						}
					}
					else
					{
						if(!cont)
						{
							cont = makeBlock;
						}

						LLVMBuildBr(cgen.bd, cont.bl);
					}
				}
				else
				{
					cont = bf;
				}

				reblock(cont);
			}

			break;

		case `OXD.ScopeStmt`:
			processBlock(t.children, sc);
			break;

		case `OXD.ExprStmt`:
			ExprGen(sc).process(t.firstChild);
			break;

		case `OXD.ReturnStmt`:
			_flags |= EX_RETURN;

			if(t.children.length)
			{
				auto v = ExprGen(sc).process(t.firstChild);
				_retval.assign(v);
			}

			LLVMBuildBr(cgen.bd, _ret.bl);
			break;

		default:
			assert(false, t.name);
		}
	}

private:
	enum
	{
		EX_RETURN	= 1,
	}

	auto makeBlock()
	{
		return new Block(_f.fn);
	}

	auto reblock()
	{
		return reblock(makeBlock);
	}

	auto reblock(Block b)
	{
		scope(exit)
		{
			_cur = b;
			_cur.activate;
		}

		return _cur;
	}

	auto processBlock(ref in ParseTree t, Scope sc)
	{
		return processBlock((&t)[0..1], sc);
	}

	auto processBlock(in ParseTree[] arr, Scope sc)
	{
		sc = new Scope(sc);

		foreach(ref c; arr)
		{
			!doesRet || throwError(`unreachable`);
			process(c, sc);
		}

		return sc;
	}

	const doesRet()
	{
		return !!(_flags & EX_RETURN);
	}

	Func _f;
	ubyte _flags;

	Block
			_cur,
			_ret;

	Var _retval;
}



/*struct FuncArg
{
	Type tp;
	Expr e;
}*/

final class Block
{
	this(LLVMValueRef fn)
	{
		bl = LLVMAppendBasicBlockInContext(cgen.ctx, fn, ``);
	}

	void activate()
	{
		LLVMPositionBuilderAtEnd(cgen.bd, bl);
	}

	LLVMBasicBlockRef bl;
}
