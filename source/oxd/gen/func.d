module oxd.gen.func;

import
		oxd;


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

	void process(Scope sc)
	{
		sc = new Scope(sc);

		foreach(uint i, ref a; _f.args)
		{
			if(a.id)
			{
				auto v = new Var(a.tp, LLVMGetParam(_f.fn, i));
				sc.declare(a.id, new ScopeVar(sc, v));
			}
		}

		processBlock(_f.bd, sc, false);
		doesRet || _f.tp is TypeVoid.instance || throwError(`function does not return value`);
	}

private:
	enum
	{
		EX_RETURN	= 1,
		EX_LOOP		= 2,

		EX_BR		= EX_RETURN | EX_LOOP
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

				if(c.children.length > 1)
				{
					v.assign(ExprGen(sc).process(c.lastChild));
				}

				sc.declare(lex.id(c.firstMatch), new ScopeVar(sc, v));
			}

			break;

		case `OXD.WhileStmt`:
			auto cond = makeBlock;
			LLVMBuildBr(cgen.bd, cond.bl);

			reblock(cond);
			auto e = ExprGen(sc).process(t.firstChild).cast_(typeBool, true);

			auto main = makeBlock;
			auto next = makeBlock;

			LLVMBuildCondBr(cgen.bd, e.value, main.bl, next.bl);

			reblock(main);
			processBlock(t.lastChild, sc);

			if(doesRet)
			{
				_flags &= ~EX_RETURN;
			}
			else
			{
				LLVMBuildBr(cgen.bd, cond.bl);
			}

			reblock(next);
			break;

		case `OXD.IfStmt`:
			auto e = ExprGen(sc).process(t.firstChild).cast_(typeBool, true);
			auto hasElse = t.children.length > 2;

			{
				Block next;

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
					next = hasElse ? makeBlock : bf;
					LLVMBuildBr(cgen.bd, next.bl);
				}

				// else block
				if(hasElse)
				{
					reblock(bf);
					processBlock(t.children[2], sc);

					if(doesRet)
					{
						if(next)
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
						if(!next)
						{
							next = makeBlock;
						}

						LLVMBuildBr(cgen.bd, next.bl);
					}
				}
				else
				{
					next = bf;
				}

				reblock(next);
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

	auto processBlock(ref in ParseTree t, Scope sc, bool nsc = true)
	{
		return processBlock((&t)[0..1], sc, nsc);
	}

	auto processBlock(in ParseTree[] arr, Scope sc, bool nsc = true)
	{
		if(nsc)
		{
			sc = new Scope(sc);
		}

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
