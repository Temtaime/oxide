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

			sc = new ScopeLoop(sc, cond, next);

			LLVMBuildCondBr(cgen.bd, e.value, main.bl, next.bl);
			auto m = processBlock(main, t.lastChild, sc, false);

			if(!(m & EX_BR))
			{
				LLVMBuildBr(cgen.bd, cond.bl);
			}

			reblock(next);
			break;

		case `OXD.BreakStmt`:
			auto s = sc.loop;
			s || throwError(`break used outside of loop`);

			_flags |= EX_LOOP;
			LLVMBuildBr(cgen.bd, s.next.bl);
			break;

		case `OXD.ContinueStmt`:
			auto s = sc.loop;
			s || throwError(`continue used outside of loop`);

			_flags |= EX_LOOP;
			LLVMBuildBr(cgen.bd, s.cond.bl);
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
				auto cnt = 0;
				auto m = processBlock(bt, t.children[1], sc) & EX_BR;

				if(m)
				{
					cnt++;
				}
				else
				{
					next = hasElse ? makeBlock : bf;
					LLVMBuildBr(cgen.bd, next.bl);
				}

				// else block
				if(hasElse)
				{
					auto f = processBlock(bf, t.lastChild, sc) & EX_BR;

					if(f)
					{
						cnt++;
						m |= f;
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

				if(cnt == 2)
				{
					_flags |= m;
				}
				else
				{
					reblock(next);
				}
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

	auto processBlock(Block b, ref in ParseTree t, Scope sc, bool nsc = true)
	{
		reblock(b);
		processBlock(t, sc, nsc);

		scope(exit)
		{
			_flags &= ~EX_BR;
		}

		return _flags;
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
			!doesBr || throwError(`unreachable`);
			process(c, sc);
		}

		return sc;
	}

	const doesRet()
	{
		return !!(_flags & EX_RETURN);
	}

	const doesBr()
	{
		return !!(_flags & EX_BR);
	}

	Func _f;
	ubyte _flags;

	Block
			_cur,
			_ret;

	Var _retval;
}
