/++
This module was automatically generated from the following grammar:


OXD:
	Main				< (AliasStmt / StructDecl / FuncStmt)* eoi
###
	PrimaryExpr			< ParExpr / StringLiteral / DoubleLiteral / IntegerLiteral / BoolLiteral / NullLiteral
	ParExpr				< '(' CommaExpr ')'

	DotBody				< '.' (IndexExpr / FuncExpr / Identifier) DotBody{extractChilds}*
	DotExpr				< (IndexExpr / FuncExpr / ThisExpr / Identifier) DotBody{extractChilds}?

	ValueExpr			< CastExpr / DotExpr / PrimaryExpr
	ThisExpr			< "this"i

	CastExpr			< "cast" '(' Type{extractChilds} ')' PreIncExpr
	FuncExpr			< ;Identifier '(' CommaExpr{extractChilds}? ')'
	IndexExpr			< DotExpr '[' CommaExpr{extractChilds} ']'

	UnaryExpr			< ValueExpr / UnaryOp UnaryExpr
	UnaryOp				< [-+!*&]

	IncOp				< "++" / "--"
	PostIncExpr			< UnaryExpr IncOp?
	PreIncExpr			< IncOp? PostIncExpr

	MulExpr				< PreIncExpr (^[*/] MulExpr{extractChilds})*
	AddExpr				< MulExpr (^[-+] AddExpr{extractChilds})*
	CmpExpr				< AddExpr (^("<=" / ">=" / '<' / '>') CmpExpr{extractChilds})*
	EquExpr				< CmpExpr (^("==" / "!=") EquExpr{extractChilds})*
	AndExpr				< EquExpr ("&&" AndExpr{extractChilds})*
	OrExpr				< AndExpr ("||" OrExpr{extractChilds})*
	CondExpr			< OrExpr ('?' AsgExpr ':' CondExpr)?
	AsgExpr				< DotExpr '=' AsgExpr{extractChilds} / CondExpr

	CommaExpr			< AsgExpr (',' AsgExpr)*
	Expr 				< CommaExpr
###
	Stmt				< IfStmt / BreakStmt / ForStmt / WhileStmt / ContinueStmt / VarStmt / ReturnStmt / ExprStmt / ScopeStmt

	Arg					< Type{extractChilds} VarDecl?
	Args				< List(Arg, ','){extractChilds}? (',' "...")?
	FuncStmt			< Type{extractChilds} Identifier '(' Args{extractChilds} ')' (';' / '{' Stmt{extractChilds}* '}')

	BreakStmt			< "break" ';'
	ContinueStmt		< "continue" ';'

	IfStmt				< "if" '(' AsgExpr ')' Stmt{extractChilds} ("else" Stmt{extractChilds})?

	VarDecl				< Identifier ('=' AsgExpr)?
	VarStmt				< Type{extractChilds} List(VarDecl, ','){extractChilds} ';'

	TemplateArgs		< ('(' List(Identifier, ','){extractChilds} ')')
	StructDecl			< "struct" Identifier TemplateArgs? '{' (VarStmt / FuncStmt)* '}'

	ScopeStmt			< '{' Stmt{extractChilds}* '}'
	ReturnStmt			< "return" Expr? ';'

	ForStmt				< "for" '(' (VarStmt / ';') AsgExpr ';' Expr? ')' Stmt{extractChilds}
	WhileStmt			< "while" '(' AsgExpr ')' Stmt{extractChilds}

	AliasStmt			< "alias" ;Identifier '=' Type{extractChilds} ';'

	ExprStmt			< Expr ';'
###
	Identifier			<~ identifier{failOnKeyword}
###
	Type				< TypeBasic{extractChilds} / TypeSpecial

	TypeSpecial			< "void" / "auto"
	TypeBasic			< TypeConst / TypePtr / TypeSlice / TypeInt / TypeUser

	TypeUser			< ;Identifier ('!' (Type{extractChilds} / '(' List(Type{extractChilds}, ','){extractChilds} ')'))?

	TypeConst			< "const" '(' TypeBasic{extractChilds} ')'
	TypePtr				< Type{extractChilds} '*'
	TypeSlice			< Type{extractChilds} "[]"

	TypeInt				< "bool" / "int" / "uint" / ("__uint" / "__int") '('  Integer ')'
###
	Spacing				<- (space / eol / Comment / CommentMulti)*
	Comment				<~ "//" (!eol .)* eol
	CommentMulti		<~ "/*" (CommentMulti / (!"*/" .))* "*/"

	EscapeSequence		<~ backslash (doublequote / backslash)
	StringLiteral		<~ doublequote (EscapeSequence / !doublequote .)* doublequote

	NullLiteral			< "null"
	BoolLiteral			< "true" / "false"

	IntegerLiteral		<~ Sign? Integer
	Integer				<~ digit+

	DoubleLiteral		<~ Sign? Integer '.' Integer? ('e'i Sign? Integer)?
	Sign				< [-+]
###
	List(Elem, Sep)		< Elem (Sep Elem)*


+/
module oxd.grammar;


import
		std.array,
		std.algorithm;


auto extractChilds(ParseTree p)
{
	return p.successful ? p.children[0] : p;
}

auto failOnKeyword(ParseTree p)
{
	static immutable words = [ "this","cast","break","continue","if","else","struct","return","for","while","alias","void","auto","const","bool","int","uint","__uint","__int","null","true","false", ];

	p.successful &= !words.canFind(p.matches.front);
	return p;
}


public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

/** Left-recursive cycles:
TypePtr <- Type <- TypeBasic
Type <- TypeBasic <- TypeSlice
DotExpr <- IndexExpr
*/

/** Rules that stop left-recursive cycles, followed by rules for which
 *  memoization is blocked during recursion:
TypePtr: TypePtr, Type, TypeBasic, TypeSlice, DotExpr, IndexExpr
Type: TypePtr, Type, TypeBasic, TypeSlice, DotExpr, IndexExpr
DotExpr: TypePtr, Type, TypeBasic, TypeSlice, DotExpr, IndexExpr
*/

struct GenericOXD(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct OXD
    {
    enum name = "OXD";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    import std.algorithm: canFind, countUntil, remove;
    static size_t[] blockMemoAtPos;
    static this()
    {
        rules["Main"] = toDelegate(&Main);
        rules["PrimaryExpr"] = toDelegate(&PrimaryExpr);
        rules["ParExpr"] = toDelegate(&ParExpr);
        rules["DotBody"] = toDelegate(&DotBody);
        rules["DotExpr"] = toDelegate(&DotExpr);
        rules["ValueExpr"] = toDelegate(&ValueExpr);
        rules["ThisExpr"] = toDelegate(&ThisExpr);
        rules["CastExpr"] = toDelegate(&CastExpr);
        rules["FuncExpr"] = toDelegate(&FuncExpr);
        rules["IndexExpr"] = toDelegate(&IndexExpr);
        rules["UnaryExpr"] = toDelegate(&UnaryExpr);
        rules["UnaryOp"] = toDelegate(&UnaryOp);
        rules["IncOp"] = toDelegate(&IncOp);
        rules["PostIncExpr"] = toDelegate(&PostIncExpr);
        rules["PreIncExpr"] = toDelegate(&PreIncExpr);
        rules["MulExpr"] = toDelegate(&MulExpr);
        rules["AddExpr"] = toDelegate(&AddExpr);
        rules["CmpExpr"] = toDelegate(&CmpExpr);
        rules["EquExpr"] = toDelegate(&EquExpr);
        rules["AndExpr"] = toDelegate(&AndExpr);
        rules["OrExpr"] = toDelegate(&OrExpr);
        rules["CondExpr"] = toDelegate(&CondExpr);
        rules["AsgExpr"] = toDelegate(&AsgExpr);
        rules["CommaExpr"] = toDelegate(&CommaExpr);
        rules["Expr"] = toDelegate(&Expr);
        rules["Stmt"] = toDelegate(&Stmt);
        rules["Arg"] = toDelegate(&Arg);
        rules["Args"] = toDelegate(&Args);
        rules["FuncStmt"] = toDelegate(&FuncStmt);
        rules["BreakStmt"] = toDelegate(&BreakStmt);
        rules["ContinueStmt"] = toDelegate(&ContinueStmt);
        rules["IfStmt"] = toDelegate(&IfStmt);
        rules["VarDecl"] = toDelegate(&VarDecl);
        rules["VarStmt"] = toDelegate(&VarStmt);
        rules["TemplateArgs"] = toDelegate(&TemplateArgs);
        rules["StructDecl"] = toDelegate(&StructDecl);
        rules["ScopeStmt"] = toDelegate(&ScopeStmt);
        rules["ReturnStmt"] = toDelegate(&ReturnStmt);
        rules["ForStmt"] = toDelegate(&ForStmt);
        rules["WhileStmt"] = toDelegate(&WhileStmt);
        rules["AliasStmt"] = toDelegate(&AliasStmt);
        rules["ExprStmt"] = toDelegate(&ExprStmt);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["Type"] = toDelegate(&Type);
        rules["TypeSpecial"] = toDelegate(&TypeSpecial);
        rules["TypeBasic"] = toDelegate(&TypeBasic);
        rules["TypeUser"] = toDelegate(&TypeUser);
        rules["TypeConst"] = toDelegate(&TypeConst);
        rules["TypePtr"] = toDelegate(&TypePtr);
        rules["TypeSlice"] = toDelegate(&TypeSlice);
        rules["TypeInt"] = toDelegate(&TypeInt);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
		import std.algorithm : startsWith;
        return s.startsWith("OXD.");
    }
    mixin decimateTree;

    static TParseTree Main(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AliasStmt, Spacing), pegged.peg.wrapAround!(Spacing, StructDecl, Spacing), pegged.peg.wrapAround!(Spacing, FuncStmt, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "OXD.Main")(p);
        }
        else
        {
            if (auto m = tuple(`Main`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AliasStmt, Spacing), pegged.peg.wrapAround!(Spacing, StructDecl, Spacing), pegged.peg.wrapAround!(Spacing, FuncStmt, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "OXD.Main"), "Main")(p);
                memo[tuple(`Main`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Main(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AliasStmt, Spacing), pegged.peg.wrapAround!(Spacing, StructDecl, Spacing), pegged.peg.wrapAround!(Spacing, FuncStmt, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "OXD.Main")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, AliasStmt, Spacing), pegged.peg.wrapAround!(Spacing, StructDecl, Spacing), pegged.peg.wrapAround!(Spacing, FuncStmt, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, eoi, Spacing)), "OXD.Main"), "Main")(TParseTree("", false,[], s));
        }
    }
    static string Main(GetName g)
    {
        return "OXD.Main";
    }

    static TParseTree PrimaryExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ParExpr, Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing), pegged.peg.wrapAround!(Spacing, DoubleLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, BoolLiteral, Spacing), pegged.peg.wrapAround!(Spacing, NullLiteral, Spacing)), "OXD.PrimaryExpr")(p);
        }
        else
        {
            if (auto m = tuple(`PrimaryExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ParExpr, Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing), pegged.peg.wrapAround!(Spacing, DoubleLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, BoolLiteral, Spacing), pegged.peg.wrapAround!(Spacing, NullLiteral, Spacing)), "OXD.PrimaryExpr"), "PrimaryExpr")(p);
                memo[tuple(`PrimaryExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PrimaryExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ParExpr, Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing), pegged.peg.wrapAround!(Spacing, DoubleLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, BoolLiteral, Spacing), pegged.peg.wrapAround!(Spacing, NullLiteral, Spacing)), "OXD.PrimaryExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ParExpr, Spacing), pegged.peg.wrapAround!(Spacing, StringLiteral, Spacing), pegged.peg.wrapAround!(Spacing, DoubleLiteral, Spacing), pegged.peg.wrapAround!(Spacing, IntegerLiteral, Spacing), pegged.peg.wrapAround!(Spacing, BoolLiteral, Spacing), pegged.peg.wrapAround!(Spacing, NullLiteral, Spacing)), "OXD.PrimaryExpr"), "PrimaryExpr")(TParseTree("", false,[], s));
        }
    }
    static string PrimaryExpr(GetName g)
    {
        return "OXD.PrimaryExpr";
    }

    static TParseTree ParExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.ParExpr")(p);
        }
        else
        {
            if (auto m = tuple(`ParExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.ParExpr"), "ParExpr")(p);
                memo[tuple(`ParExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.ParExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.ParExpr"), "ParExpr")(TParseTree("", false,[], s));
        }
    }
    static string ParExpr(GetName g)
    {
        return "OXD.ParExpr";
    }

    static TParseTree DotBody(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpr, Spacing), pegged.peg.wrapAround!(Spacing, FuncExpr, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, DotBody, Spacing), extractChilds))), "OXD.DotBody")(p);
        }
        else
        {
            if (auto m = tuple(`DotBody`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpr, Spacing), pegged.peg.wrapAround!(Spacing, FuncExpr, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, DotBody, Spacing), extractChilds))), "OXD.DotBody"), "DotBody")(p);
                memo[tuple(`DotBody`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DotBody(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpr, Spacing), pegged.peg.wrapAround!(Spacing, FuncExpr, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, DotBody, Spacing), extractChilds))), "OXD.DotBody")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("."), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpr, Spacing), pegged.peg.wrapAround!(Spacing, FuncExpr, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, DotBody, Spacing), extractChilds))), "OXD.DotBody"), "DotBody")(TParseTree("", false,[], s));
        }
    }
    static string DotBody(GetName g)
    {
        return "OXD.DotBody";
    }

    static TParseTree DotExpr(TParseTree p)
    {
        if(__ctfe)
        {
            assert(false, "DotExpr is left-recursive, which is not supported at compile-time. Consider using asModule().");
        }
        else
        {
            static TParseTree[size_t /*position*/] seed;
            if (auto s = p.end in seed)
                return *s;
            if (!blockMemoAtPos.canFind(p.end))
                if (auto m = tuple(`DotExpr`, p.end) in memo)
                    return *m;
            auto current = fail(p);
            seed[p.end] = current;
            blockMemoAtPos ~= p.end;
            while (true)
            {
                auto result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpr, Spacing), pegged.peg.wrapAround!(Spacing, FuncExpr, Spacing), pegged.peg.wrapAround!(Spacing, ThisExpr, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, DotBody, Spacing), extractChilds))), "OXD.DotExpr"), "DotExpr")(p);
                if (result.end > current.end ||
                    (!current.successful && result.successful) /* null-match */)
                {
                    current = result;
                    seed[p.end] = current;
                } else {
                    seed.remove(p.end);
                    assert(blockMemoAtPos.canFind(p.end));
                    blockMemoAtPos = blockMemoAtPos.remove(countUntil(blockMemoAtPos, p.end));
                    memo[tuple(`DotExpr`, p.end)] = current;
                    return current;
                }
            }
        }
    }

    static TParseTree DotExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpr, Spacing), pegged.peg.wrapAround!(Spacing, FuncExpr, Spacing), pegged.peg.wrapAround!(Spacing, ThisExpr, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, DotBody, Spacing), extractChilds))), "OXD.DotExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IndexExpr, Spacing), pegged.peg.wrapAround!(Spacing, FuncExpr, Spacing), pegged.peg.wrapAround!(Spacing, ThisExpr, Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), Spacing), pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, DotBody, Spacing), extractChilds))), "OXD.DotExpr"), "DotExpr")(TParseTree("", false,[], s));
        }
    }
    static string DotExpr(GetName g)
    {
        return "OXD.DotExpr";
    }

    static TParseTree ValueExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CastExpr, Spacing), pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing)), "OXD.ValueExpr")(p);
        }
        else
        {
            if (auto m = tuple(`ValueExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CastExpr, Spacing), pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing)), "OXD.ValueExpr"), "ValueExpr")(p);
                memo[tuple(`ValueExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ValueExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CastExpr, Spacing), pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing)), "OXD.ValueExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, CastExpr, Spacing), pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, PrimaryExpr, Spacing)), "OXD.ValueExpr"), "ValueExpr")(TParseTree("", false,[], s));
        }
    }
    static string ValueExpr(GetName g)
    {
        return "OXD.ValueExpr";
    }

    static TParseTree ThisExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.caseInsensitiveLiteral!("this"), Spacing), "OXD.ThisExpr")(p);
        }
        else
        {
            if (auto m = tuple(`ThisExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.caseInsensitiveLiteral!("this"), Spacing), "OXD.ThisExpr"), "ThisExpr")(p);
                memo[tuple(`ThisExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ThisExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.caseInsensitiveLiteral!("this"), Spacing), "OXD.ThisExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.caseInsensitiveLiteral!("this"), Spacing), "OXD.ThisExpr"), "ThisExpr")(TParseTree("", false,[], s));
        }
    }
    static string ThisExpr(GetName g)
    {
        return "OXD.ThisExpr";
    }

    static TParseTree CastExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, PreIncExpr, Spacing)), "OXD.CastExpr")(p);
        }
        else
        {
            if (auto m = tuple(`CastExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, PreIncExpr, Spacing)), "OXD.CastExpr"), "CastExpr")(p);
                memo[tuple(`CastExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CastExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, PreIncExpr, Spacing)), "OXD.CastExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("cast"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, PreIncExpr, Spacing)), "OXD.CastExpr"), "CastExpr")(TParseTree("", false,[], s));
        }
    }
    static string CastExpr(GetName g)
    {
        return "OXD.CastExpr";
    }

    static TParseTree FuncExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.FuncExpr")(p);
        }
        else
        {
            if (auto m = tuple(`FuncExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.FuncExpr"), "FuncExpr")(p);
                memo[tuple(`FuncExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FuncExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.FuncExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.FuncExpr"), "FuncExpr")(TParseTree("", false,[], s));
        }
    }
    static string FuncExpr(GetName g)
    {
        return "OXD.FuncExpr";
    }

    static TParseTree IndexExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "OXD.IndexExpr")(p);
        }
        else
        {
            if (blockMemoAtPos.canFind(p.end))
                return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "OXD.IndexExpr"), "IndexExpr")(p);
            if (auto m = tuple(`IndexExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "OXD.IndexExpr"), "IndexExpr")(p);
                memo[tuple(`IndexExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IndexExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "OXD.IndexExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("["), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("]"), Spacing)), "OXD.IndexExpr"), "IndexExpr")(TParseTree("", false,[], s));
        }
    }
    static string IndexExpr(GetName g)
    {
        return "OXD.IndexExpr";
    }

    static TParseTree UnaryExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ValueExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing))), "OXD.UnaryExpr")(p);
        }
        else
        {
            if (auto m = tuple(`UnaryExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ValueExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing))), "OXD.UnaryExpr"), "UnaryExpr")(p);
                memo[tuple(`UnaryExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnaryExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ValueExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing))), "OXD.UnaryExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, ValueExpr, Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryOp, Spacing), pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing))), "OXD.UnaryExpr"), "UnaryExpr")(TParseTree("", false,[], s));
        }
    }
    static string UnaryExpr(GetName g)
    {
        return "OXD.UnaryExpr";
    }

    static TParseTree UnaryOp(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("!"), pegged.peg.literal!("*"), pegged.peg.literal!("&")), Spacing), "OXD.UnaryOp")(p);
        }
        else
        {
            if (auto m = tuple(`UnaryOp`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("!"), pegged.peg.literal!("*"), pegged.peg.literal!("&")), Spacing), "OXD.UnaryOp"), "UnaryOp")(p);
                memo[tuple(`UnaryOp`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UnaryOp(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("!"), pegged.peg.literal!("*"), pegged.peg.literal!("&")), Spacing), "OXD.UnaryOp")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("!"), pegged.peg.literal!("*"), pegged.peg.literal!("&")), Spacing), "OXD.UnaryOp"), "UnaryOp")(TParseTree("", false,[], s));
        }
    }
    static string UnaryOp(GetName g)
    {
        return "OXD.UnaryOp";
    }

    static TParseTree IncOp(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), "OXD.IncOp")(p);
        }
        else
        {
            if (auto m = tuple(`IncOp`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), "OXD.IncOp"), "IncOp")(p);
                memo[tuple(`IncOp`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IncOp(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), "OXD.IncOp")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("++"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("--"), Spacing)), "OXD.IncOp"), "IncOp")(TParseTree("", false,[], s));
        }
    }
    static string IncOp(GetName g)
    {
        return "OXD.IncOp";
    }

    static TParseTree PostIncExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IncOp, Spacing))), "OXD.PostIncExpr")(p);
        }
        else
        {
            if (auto m = tuple(`PostIncExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IncOp, Spacing))), "OXD.PostIncExpr"), "PostIncExpr")(p);
                memo[tuple(`PostIncExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PostIncExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IncOp, Spacing))), "OXD.PostIncExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, UnaryExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IncOp, Spacing))), "OXD.PostIncExpr"), "PostIncExpr")(TParseTree("", false,[], s));
        }
    }
    static string PostIncExpr(GetName g)
    {
        return "OXD.PostIncExpr";
    }

    static TParseTree PreIncExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IncOp, Spacing)), pegged.peg.wrapAround!(Spacing, PostIncExpr, Spacing)), "OXD.PreIncExpr")(p);
        }
        else
        {
            if (auto m = tuple(`PreIncExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IncOp, Spacing)), pegged.peg.wrapAround!(Spacing, PostIncExpr, Spacing)), "OXD.PreIncExpr"), "PreIncExpr")(p);
                memo[tuple(`PreIncExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PreIncExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IncOp, Spacing)), pegged.peg.wrapAround!(Spacing, PostIncExpr, Spacing)), "OXD.PreIncExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.wrapAround!(Spacing, IncOp, Spacing)), pegged.peg.wrapAround!(Spacing, PostIncExpr, Spacing)), "OXD.PreIncExpr"), "PreIncExpr")(TParseTree("", false,[], s));
        }
    }
    static string PreIncExpr(GetName g)
    {
        return "OXD.PreIncExpr";
    }

    static TParseTree MulExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PreIncExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("/")), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), extractChilds)), Spacing))), "OXD.MulExpr")(p);
        }
        else
        {
            if (auto m = tuple(`MulExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PreIncExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("/")), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), extractChilds)), Spacing))), "OXD.MulExpr"), "MulExpr")(p);
                memo[tuple(`MulExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MulExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PreIncExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("/")), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), extractChilds)), Spacing))), "OXD.MulExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, PreIncExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("*"), pegged.peg.literal!("/")), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), extractChilds)), Spacing))), "OXD.MulExpr"), "MulExpr")(TParseTree("", false,[], s));
        }
    }
    static string MulExpr(GetName g)
    {
        return "OXD.MulExpr";
    }

    static TParseTree AddExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), extractChilds)), Spacing))), "OXD.AddExpr")(p);
        }
        else
        {
            if (auto m = tuple(`AddExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), extractChilds)), Spacing))), "OXD.AddExpr"), "AddExpr")(p);
                memo[tuple(`AddExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AddExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), extractChilds)), Spacing))), "OXD.AddExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, MulExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), extractChilds)), Spacing))), "OXD.AddExpr"), "AddExpr")(TParseTree("", false,[], s));
        }
    }
    static string AddExpr(GetName g)
    {
        return "OXD.AddExpr";
    }

    static TParseTree CmpExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CmpExpr, Spacing), extractChilds)), Spacing))), "OXD.CmpExpr")(p);
        }
        else
        {
            if (auto m = tuple(`CmpExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CmpExpr, Spacing), extractChilds)), Spacing))), "OXD.CmpExpr"), "CmpExpr")(p);
                memo[tuple(`CmpExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CmpExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CmpExpr, Spacing), extractChilds)), Spacing))), "OXD.CmpExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AddExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("<"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(">"), Spacing)), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, CmpExpr, Spacing), extractChilds)), Spacing))), "OXD.CmpExpr"), "CmpExpr")(TParseTree("", false,[], s));
        }
    }
    static string CmpExpr(GetName g)
    {
        return "OXD.CmpExpr";
    }

    static TParseTree EquExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CmpExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, EquExpr, Spacing), extractChilds)), Spacing))), "OXD.EquExpr")(p);
        }
        else
        {
            if (auto m = tuple(`EquExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CmpExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, EquExpr, Spacing), extractChilds)), Spacing))), "OXD.EquExpr"), "EquExpr")(p);
                memo[tuple(`EquExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EquExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CmpExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, EquExpr, Spacing), extractChilds)), Spacing))), "OXD.EquExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, CmpExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.keep!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("=="), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!="), Spacing)), Spacing)), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, EquExpr, Spacing), extractChilds)), Spacing))), "OXD.EquExpr"), "EquExpr")(TParseTree("", false,[], s));
        }
    }
    static string EquExpr(GetName g)
    {
        return "OXD.EquExpr";
    }

    static TParseTree AndExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EquExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), extractChilds)), Spacing))), "OXD.AndExpr")(p);
        }
        else
        {
            if (auto m = tuple(`AndExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EquExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), extractChilds)), Spacing))), "OXD.AndExpr"), "AndExpr")(p);
                memo[tuple(`AndExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AndExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EquExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), extractChilds)), Spacing))), "OXD.AndExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, EquExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("&&"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), extractChilds)), Spacing))), "OXD.AndExpr"), "AndExpr")(TParseTree("", false,[], s));
        }
    }
    static string AndExpr(GetName g)
    {
        return "OXD.AndExpr";
    }

    static TParseTree OrExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), extractChilds)), Spacing))), "OXD.OrExpr")(p);
        }
        else
        {
            if (auto m = tuple(`OrExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), extractChilds)), Spacing))), "OXD.OrExpr"), "OrExpr")(p);
                memo[tuple(`OrExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OrExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), extractChilds)), Spacing))), "OXD.OrExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AndExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("||"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), extractChilds)), Spacing))), "OXD.OrExpr"), "OrExpr")(TParseTree("", false,[], s));
        }
    }
    static string OrExpr(GetName g)
    {
        return "OXD.OrExpr";
    }

    static TParseTree CondExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, CondExpr, Spacing)), Spacing))), "OXD.CondExpr")(p);
        }
        else
        {
            if (auto m = tuple(`CondExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, CondExpr, Spacing)), Spacing))), "OXD.CondExpr"), "CondExpr")(p);
                memo[tuple(`CondExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CondExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, CondExpr, Spacing)), Spacing))), "OXD.CondExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, OrExpr, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("?"), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(":"), Spacing), pegged.peg.wrapAround!(Spacing, CondExpr, Spacing)), Spacing))), "OXD.CondExpr"), "CondExpr")(TParseTree("", false,[], s));
        }
    }
    static string CondExpr(GetName g)
    {
        return "OXD.CondExpr";
    }

    static TParseTree AsgExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, CondExpr, Spacing)), "OXD.AsgExpr")(p);
        }
        else
        {
            if (auto m = tuple(`AsgExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, CondExpr, Spacing)), "OXD.AsgExpr"), "AsgExpr")(p);
                memo[tuple(`AsgExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AsgExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, CondExpr, Spacing)), "OXD.AsgExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, DotExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, CondExpr, Spacing)), "OXD.AsgExpr"), "AsgExpr")(TParseTree("", false,[], s));
        }
    }
    static string AsgExpr(GetName g)
    {
        return "OXD.AsgExpr";
    }

    static TParseTree CommaExpr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing)), Spacing))), "OXD.CommaExpr")(p);
        }
        else
        {
            if (auto m = tuple(`CommaExpr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing)), Spacing))), "OXD.CommaExpr"), "CommaExpr")(p);
                memo[tuple(`CommaExpr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CommaExpr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing)), Spacing))), "OXD.CommaExpr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing)), Spacing))), "OXD.CommaExpr"), "CommaExpr")(TParseTree("", false,[], s));
        }
    }
    static string CommaExpr(GetName g)
    {
        return "OXD.CommaExpr";
    }

    static TParseTree Expr(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), "OXD.Expr")(p);
        }
        else
        {
            if (auto m = tuple(`Expr`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), "OXD.Expr"), "Expr")(p);
                memo[tuple(`Expr`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Expr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), "OXD.Expr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, CommaExpr, Spacing), "OXD.Expr"), "Expr")(TParseTree("", false,[], s));
        }
    }
    static string Expr(GetName g)
    {
        return "OXD.Expr";
    }

    static TParseTree Stmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IfStmt, Spacing), pegged.peg.wrapAround!(Spacing, BreakStmt, Spacing), pegged.peg.wrapAround!(Spacing, ForStmt, Spacing), pegged.peg.wrapAround!(Spacing, WhileStmt, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStmt, Spacing), pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStmt, Spacing), pegged.peg.wrapAround!(Spacing, ExprStmt, Spacing), pegged.peg.wrapAround!(Spacing, ScopeStmt, Spacing)), "OXD.Stmt")(p);
        }
        else
        {
            if (auto m = tuple(`Stmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IfStmt, Spacing), pegged.peg.wrapAround!(Spacing, BreakStmt, Spacing), pegged.peg.wrapAround!(Spacing, ForStmt, Spacing), pegged.peg.wrapAround!(Spacing, WhileStmt, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStmt, Spacing), pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStmt, Spacing), pegged.peg.wrapAround!(Spacing, ExprStmt, Spacing), pegged.peg.wrapAround!(Spacing, ScopeStmt, Spacing)), "OXD.Stmt"), "Stmt")(p);
                memo[tuple(`Stmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Stmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IfStmt, Spacing), pegged.peg.wrapAround!(Spacing, BreakStmt, Spacing), pegged.peg.wrapAround!(Spacing, ForStmt, Spacing), pegged.peg.wrapAround!(Spacing, WhileStmt, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStmt, Spacing), pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStmt, Spacing), pegged.peg.wrapAround!(Spacing, ExprStmt, Spacing), pegged.peg.wrapAround!(Spacing, ScopeStmt, Spacing)), "OXD.Stmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, IfStmt, Spacing), pegged.peg.wrapAround!(Spacing, BreakStmt, Spacing), pegged.peg.wrapAround!(Spacing, ForStmt, Spacing), pegged.peg.wrapAround!(Spacing, WhileStmt, Spacing), pegged.peg.wrapAround!(Spacing, ContinueStmt, Spacing), pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, ReturnStmt, Spacing), pegged.peg.wrapAround!(Spacing, ExprStmt, Spacing), pegged.peg.wrapAround!(Spacing, ScopeStmt, Spacing)), "OXD.Stmt"), "Stmt")(TParseTree("", false,[], s));
        }
    }
    static string Stmt(GetName g)
    {
        return "OXD.Stmt";
    }

    static TParseTree Arg(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing))), "OXD.Arg")(p);
        }
        else
        {
            if (auto m = tuple(`Arg`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing))), "OXD.Arg"), "Arg")(p);
                memo[tuple(`Arg`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Arg(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing))), "OXD.Arg")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing))), "OXD.Arg"), "Arg")(TParseTree("", false,[], s));
        }
    }
    static string Arg(GetName g)
    {
        return "OXD.Arg";
    }

    static TParseTree Args(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Arg, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), Spacing))), "OXD.Args")(p);
        }
        else
        {
            if (auto m = tuple(`Args`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Arg, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), Spacing))), "OXD.Args"), "Args")(p);
                memo[tuple(`Args`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Args(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Arg, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), Spacing))), "OXD.Args")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Arg, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("..."), Spacing)), Spacing))), "OXD.Args"), "Args")(TParseTree("", false,[], s));
        }
    }
    static string Args(GetName g)
    {
        return "OXD.Args";
    }

    static TParseTree FuncStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Args, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing)), "OXD.FuncStmt")(p);
        }
        else
        {
            if (auto m = tuple(`FuncStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Args, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing)), "OXD.FuncStmt"), "FuncStmt")(p);
                memo[tuple(`FuncStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FuncStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Args, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing)), "OXD.FuncStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Args, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing)), "OXD.FuncStmt"), "FuncStmt")(TParseTree("", false,[], s));
        }
    }
    static string FuncStmt(GetName g)
    {
        return "OXD.FuncStmt";
    }

    static TParseTree BreakStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.BreakStmt")(p);
        }
        else
        {
            if (auto m = tuple(`BreakStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.BreakStmt"), "BreakStmt")(p);
                memo[tuple(`BreakStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BreakStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.BreakStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("break"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.BreakStmt"), "BreakStmt")(TParseTree("", false,[], s));
        }
    }
    static string BreakStmt(GetName g)
    {
        return "OXD.BreakStmt";
    }

    static TParseTree ContinueStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ContinueStmt")(p);
        }
        else
        {
            if (auto m = tuple(`ContinueStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ContinueStmt"), "ContinueStmt")(p);
                memo[tuple(`ContinueStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ContinueStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ContinueStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("continue"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ContinueStmt"), "ContinueStmt")(TParseTree("", false,[], s));
        }
    }
    static string ContinueStmt(GetName g)
    {
        return "OXD.ContinueStmt";
    }

    static TParseTree IfStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), Spacing))), "OXD.IfStmt")(p);
        }
        else
        {
            if (auto m = tuple(`IfStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), Spacing))), "OXD.IfStmt"), "IfStmt")(p);
                memo[tuple(`IfStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IfStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), Spacing))), "OXD.IfStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("if"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("else"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), Spacing))), "OXD.IfStmt"), "IfStmt")(TParseTree("", false,[], s));
        }
    }
    static string IfStmt(GetName g)
    {
        return "OXD.IfStmt";
    }

    static TParseTree VarDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing)), Spacing))), "OXD.VarDecl")(p);
        }
        else
        {
            if (auto m = tuple(`VarDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing)), Spacing))), "OXD.VarDecl"), "VarDecl")(p);
                memo[tuple(`VarDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing)), Spacing))), "OXD.VarDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing)), Spacing))), "OXD.VarDecl"), "VarDecl")(TParseTree("", false,[], s));
        }
    }
    static string VarDecl(GetName g)
    {
        return "OXD.VarDecl";
    }

    static TParseTree VarStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.VarStmt")(p);
        }
        else
        {
            if (auto m = tuple(`VarStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.VarStmt"), "VarStmt")(p);
                memo[tuple(`VarStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree VarStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.VarStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, VarDecl, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.VarStmt"), "VarStmt")(TParseTree("", false,[], s));
        }
    }
    static string VarStmt(GetName g)
    {
        return "OXD.VarStmt";
    }

    static TParseTree TemplateArgs(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), "OXD.TemplateArgs")(p);
        }
        else
        {
            if (auto m = tuple(`TemplateArgs`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), "OXD.TemplateArgs"), "TemplateArgs")(p);
                memo[tuple(`TemplateArgs`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TemplateArgs(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), "OXD.TemplateArgs")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), Spacing), "OXD.TemplateArgs"), "TemplateArgs")(TParseTree("", false,[], s));
        }
    }
    static string TemplateArgs(GetName g)
    {
        return "OXD.TemplateArgs";
    }

    static TParseTree StructDecl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateArgs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, FuncStmt, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "OXD.StructDecl")(p);
        }
        else
        {
            if (auto m = tuple(`StructDecl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateArgs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, FuncStmt, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "OXD.StructDecl"), "StructDecl")(p);
                memo[tuple(`StructDecl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StructDecl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateArgs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, FuncStmt, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "OXD.StructDecl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("struct"), Spacing), pegged.peg.wrapAround!(Spacing, Identifier, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, TemplateArgs, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, FuncStmt, Spacing)), Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "OXD.StructDecl"), "StructDecl")(TParseTree("", false,[], s));
        }
    }
    static string StructDecl(GetName g)
    {
        return "OXD.StructDecl";
    }

    static TParseTree ScopeStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "OXD.ScopeStmt")(p);
        }
        else
        {
            if (auto m = tuple(`ScopeStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "OXD.ScopeStmt"), "ScopeStmt")(p);
                memo[tuple(`ScopeStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ScopeStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "OXD.ScopeStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing), pegged.peg.zeroOrMore!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing)), "OXD.ScopeStmt"), "ScopeStmt")(TParseTree("", false,[], s));
        }
    }
    static string ScopeStmt(GetName g)
    {
        return "OXD.ScopeStmt";
    }

    static TParseTree ReturnStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ReturnStmt")(p);
        }
        else
        {
            if (auto m = tuple(`ReturnStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ReturnStmt"), "ReturnStmt")(p);
                memo[tuple(`ReturnStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReturnStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ReturnStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("return"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ReturnStmt"), "ReturnStmt")(TParseTree("", false,[], s));
        }
    }
    static string ReturnStmt(GetName g)
    {
        return "OXD.ReturnStmt";
    }

    static TParseTree ForStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), "OXD.ForStmt")(p);
        }
        else
        {
            if (auto m = tuple(`ForStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), "OXD.ForStmt"), "ForStmt")(p);
                memo[tuple(`ForStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), "OXD.ForStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("for"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, VarStmt, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Expr, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), "OXD.ForStmt"), "ForStmt")(TParseTree("", false,[], s));
        }
    }
    static string ForStmt(GetName g)
    {
        return "OXD.ForStmt";
    }

    static TParseTree WhileStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), "OXD.WhileStmt")(p);
        }
        else
        {
            if (auto m = tuple(`WhileStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), "OXD.WhileStmt"), "WhileStmt")(p);
                memo[tuple(`WhileStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree WhileStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), "OXD.WhileStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("while"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, AsgExpr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Stmt, Spacing), extractChilds)), "OXD.WhileStmt"), "WhileStmt")(TParseTree("", false,[], s));
        }
    }
    static string WhileStmt(GetName g)
    {
        return "OXD.WhileStmt";
    }

    static TParseTree AliasStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.AliasStmt")(p);
        }
        else
        {
            if (auto m = tuple(`AliasStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.AliasStmt"), "AliasStmt")(p);
                memo[tuple(`AliasStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AliasStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.AliasStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("alias"), Spacing), pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("="), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.AliasStmt"), "AliasStmt")(TParseTree("", false,[], s));
        }
    }
    static string AliasStmt(GetName g)
    {
        return "OXD.AliasStmt";
    }

    static TParseTree ExprStmt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ExprStmt")(p);
        }
        else
        {
            if (auto m = tuple(`ExprStmt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ExprStmt"), "ExprStmt")(p);
                memo[tuple(`ExprStmt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExprStmt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ExprStmt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Expr, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), "OXD.ExprStmt"), "ExprStmt")(TParseTree("", false,[], s));
        }
    }
    static string ExprStmt(GetName g)
    {
        return "OXD.ExprStmt";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.action!(identifier, failOnKeyword)), "OXD.Identifier")(p);
        }
        else
        {
            if (auto m = tuple(`Identifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.action!(identifier, failOnKeyword)), "OXD.Identifier"), "Identifier")(p);
                memo[tuple(`Identifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.action!(identifier, failOnKeyword)), "OXD.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.action!(identifier, failOnKeyword)), "OXD.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "OXD.Identifier";
    }

    static TParseTree Type(TParseTree p)
    {
        if(__ctfe)
        {
            assert(false, "Type is left-recursive, which is not supported at compile-time. Consider using asModule().");
        }
        else
        {
            static TParseTree[size_t /*position*/] seed;
            if (auto s = p.end in seed)
                return *s;
            if (!blockMemoAtPos.canFind(p.end))
                if (auto m = tuple(`Type`, p.end) in memo)
                    return *m;
            auto current = fail(p);
            seed[p.end] = current;
            blockMemoAtPos ~= p.end;
            while (true)
            {
                auto result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, TypeBasic, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, TypeSpecial, Spacing)), "OXD.Type"), "Type")(p);
                if (result.end > current.end ||
                    (!current.successful && result.successful) /* null-match */)
                {
                    current = result;
                    seed[p.end] = current;
                } else {
                    seed.remove(p.end);
                    assert(blockMemoAtPos.canFind(p.end));
                    blockMemoAtPos = blockMemoAtPos.remove(countUntil(blockMemoAtPos, p.end));
                    memo[tuple(`Type`, p.end)] = current;
                    return current;
                }
            }
        }
    }

    static TParseTree Type(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, TypeBasic, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, TypeSpecial, Spacing)), "OXD.Type")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, TypeBasic, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, TypeSpecial, Spacing)), "OXD.Type"), "Type")(TParseTree("", false,[], s));
        }
    }
    static string Type(GetName g)
    {
        return "OXD.Type";
    }

    static TParseTree TypeSpecial(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), "OXD.TypeSpecial")(p);
        }
        else
        {
            if (auto m = tuple(`TypeSpecial`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), "OXD.TypeSpecial"), "TypeSpecial")(p);
                memo[tuple(`TypeSpecial`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeSpecial(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), "OXD.TypeSpecial")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("void"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("auto"), Spacing)), "OXD.TypeSpecial"), "TypeSpecial")(TParseTree("", false,[], s));
        }
    }
    static string TypeSpecial(GetName g)
    {
        return "OXD.TypeSpecial";
    }

    static TParseTree TypeBasic(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TypeConst, Spacing), pegged.peg.wrapAround!(Spacing, TypePtr, Spacing), pegged.peg.wrapAround!(Spacing, TypeSlice, Spacing), pegged.peg.wrapAround!(Spacing, TypeInt, Spacing), pegged.peg.wrapAround!(Spacing, TypeUser, Spacing)), "OXD.TypeBasic")(p);
        }
        else
        {
            if (blockMemoAtPos.canFind(p.end))
                return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TypeConst, Spacing), pegged.peg.wrapAround!(Spacing, TypePtr, Spacing), pegged.peg.wrapAround!(Spacing, TypeSlice, Spacing), pegged.peg.wrapAround!(Spacing, TypeInt, Spacing), pegged.peg.wrapAround!(Spacing, TypeUser, Spacing)), "OXD.TypeBasic"), "TypeBasic")(p);
            if (auto m = tuple(`TypeBasic`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TypeConst, Spacing), pegged.peg.wrapAround!(Spacing, TypePtr, Spacing), pegged.peg.wrapAround!(Spacing, TypeSlice, Spacing), pegged.peg.wrapAround!(Spacing, TypeInt, Spacing), pegged.peg.wrapAround!(Spacing, TypeUser, Spacing)), "OXD.TypeBasic"), "TypeBasic")(p);
                memo[tuple(`TypeBasic`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeBasic(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TypeConst, Spacing), pegged.peg.wrapAround!(Spacing, TypePtr, Spacing), pegged.peg.wrapAround!(Spacing, TypeSlice, Spacing), pegged.peg.wrapAround!(Spacing, TypeInt, Spacing), pegged.peg.wrapAround!(Spacing, TypeUser, Spacing)), "OXD.TypeBasic")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, TypeConst, Spacing), pegged.peg.wrapAround!(Spacing, TypePtr, Spacing), pegged.peg.wrapAround!(Spacing, TypeSlice, Spacing), pegged.peg.wrapAround!(Spacing, TypeInt, Spacing), pegged.peg.wrapAround!(Spacing, TypeUser, Spacing)), "OXD.TypeBasic"), "TypeBasic")(TParseTree("", false,[], s));
        }
    }
    static string TypeBasic(GetName g)
    {
        return "OXD.TypeBasic";
    }

    static TParseTree TypeUser(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), Spacing))), "OXD.TypeUser")(p);
        }
        else
        {
            if (auto m = tuple(`TypeUser`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), Spacing))), "OXD.TypeUser"), "TypeUser")(p);
                memo[tuple(`TypeUser`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeUser(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), Spacing))), "OXD.TypeUser")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.wrapAround!(Spacing, Identifier, Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("!"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, List!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(","), Spacing)), Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), Spacing)), Spacing))), "OXD.TypeUser"), "TypeUser")(TParseTree("", false,[], s));
        }
    }
    static string TypeUser(GetName g)
    {
        return "OXD.TypeUser";
    }

    static TParseTree TypeConst(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, TypeBasic, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.TypeConst")(p);
        }
        else
        {
            if (auto m = tuple(`TypeConst`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, TypeBasic, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.TypeConst"), "TypeConst")(p);
                memo[tuple(`TypeConst`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeConst(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, TypeBasic, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.TypeConst")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("const"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.action!(pegged.peg.wrapAround!(Spacing, TypeBasic, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing)), "OXD.TypeConst"), "TypeConst")(TParseTree("", false,[], s));
        }
    }
    static string TypeConst(GetName g)
    {
        return "OXD.TypeConst";
    }

    static TParseTree TypePtr(TParseTree p)
    {
        if(__ctfe)
        {
            assert(false, "TypePtr is left-recursive, which is not supported at compile-time. Consider using asModule().");
        }
        else
        {
            static TParseTree[size_t /*position*/] seed;
            if (auto s = p.end in seed)
                return *s;
            if (!blockMemoAtPos.canFind(p.end))
                if (auto m = tuple(`TypePtr`, p.end) in memo)
                    return *m;
            auto current = fail(p);
            seed[p.end] = current;
            blockMemoAtPos ~= p.end;
            while (true)
            {
                auto result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), "OXD.TypePtr"), "TypePtr")(p);
                if (result.end > current.end)
                {
                    current = result;
                    seed[p.end] = current;
                } else {
                    seed.remove(p.end);
                    assert(blockMemoAtPos.canFind(p.end));
                    blockMemoAtPos = blockMemoAtPos.remove(countUntil(blockMemoAtPos, p.end));
                    memo[tuple(`TypePtr`, p.end)] = current;
                    return current;
                }
            }
        }
    }

    static TParseTree TypePtr(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), "OXD.TypePtr")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("*"), Spacing)), "OXD.TypePtr"), "TypePtr")(TParseTree("", false,[], s));
        }
    }
    static string TypePtr(GetName g)
    {
        return "OXD.TypePtr";
    }

    static TParseTree TypeSlice(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("[]"), Spacing)), "OXD.TypeSlice")(p);
        }
        else
        {
            if (blockMemoAtPos.canFind(p.end))
                return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("[]"), Spacing)), "OXD.TypeSlice"), "TypeSlice")(p);
            if (auto m = tuple(`TypeSlice`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("[]"), Spacing)), "OXD.TypeSlice"), "TypeSlice")(p);
                memo[tuple(`TypeSlice`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeSlice(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("[]"), Spacing)), "OXD.TypeSlice")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.action!(pegged.peg.wrapAround!(Spacing, Type, Spacing), extractChilds), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("[]"), Spacing)), "OXD.TypeSlice"), "TypeSlice")(TParseTree("", false,[], s));
        }
    }
    static string TypeSlice(GetName g)
    {
        return "OXD.TypeSlice";
    }

    static TParseTree TypeInt(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__int"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "OXD.TypeInt")(p);
        }
        else
        {
            if (auto m = tuple(`TypeInt`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__int"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "OXD.TypeInt"), "TypeInt")(p);
                memo[tuple(`TypeInt`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TypeInt(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__int"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "OXD.TypeInt")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("bool"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("int"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("uint"), Spacing), pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__uint"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("__int"), Spacing)), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("("), Spacing), pegged.peg.wrapAround!(Spacing, Integer, Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(")"), Spacing))), "OXD.TypeInt"), "TypeInt")(TParseTree("", false,[], s));
        }
    }
    static string TypeInt(GetName g)
    {
        return "OXD.TypeInt";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(space, eol, Comment, CommentMulti)), "OXD.Spacing")(p);
        }
        else
        {
            if (auto m = tuple(`Spacing`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(space, eol, Comment, CommentMulti)), "OXD.Spacing"), "Spacing")(p);
                memo[tuple(`Spacing`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(space, eol, Comment, CommentMulti)), "OXD.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(pegged.peg.or!(space, eol, Comment, CommentMulti)), "OXD.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "OXD.Spacing";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol)), "OXD.Comment")(p);
        }
        else
        {
            if (auto m = tuple(`Comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol)), "OXD.Comment"), "Comment")(p);
                memo[tuple(`Comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol)), "OXD.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("//"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(eol), pegged.peg.any)), eol)), "OXD.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "OXD.Comment";
    }

    static TParseTree CommentMulti(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(CommentMulti, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any))), pegged.peg.literal!("*/"))), "OXD.CommentMulti")(p);
        }
        else
        {
            if (auto m = tuple(`CommentMulti`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(CommentMulti, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any))), pegged.peg.literal!("*/"))), "OXD.CommentMulti"), "CommentMulti")(p);
                memo[tuple(`CommentMulti`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CommentMulti(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(CommentMulti, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any))), pegged.peg.literal!("*/"))), "OXD.CommentMulti")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.or!(CommentMulti, pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any))), pegged.peg.literal!("*/"))), "OXD.CommentMulti"), "CommentMulti")(TParseTree("", false,[], s));
        }
    }
    static string CommentMulti(GetName g)
    {
        return "OXD.CommentMulti";
    }

    static TParseTree EscapeSequence(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backslash, pegged.peg.or!(doublequote, backslash))), "OXD.EscapeSequence")(p);
        }
        else
        {
            if (auto m = tuple(`EscapeSequence`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backslash, pegged.peg.or!(doublequote, backslash))), "OXD.EscapeSequence"), "EscapeSequence")(p);
                memo[tuple(`EscapeSequence`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EscapeSequence(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backslash, pegged.peg.or!(doublequote, backslash))), "OXD.EscapeSequence")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(backslash, pegged.peg.or!(doublequote, backslash))), "OXD.EscapeSequence"), "EscapeSequence")(TParseTree("", false,[], s));
        }
    }
    static string EscapeSequence(GetName g)
    {
        return "OXD.EscapeSequence";
    }

    static TParseTree StringLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), doublequote)), "OXD.StringLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`StringLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), doublequote)), "OXD.StringLiteral"), "StringLiteral")(p);
                memo[tuple(`StringLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), doublequote)), "OXD.StringLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(doublequote, pegged.peg.zeroOrMore!(pegged.peg.or!(EscapeSequence, pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.any))), doublequote)), "OXD.StringLiteral"), "StringLiteral")(TParseTree("", false,[], s));
        }
    }
    static string StringLiteral(GetName g)
    {
        return "OXD.StringLiteral";
    }

    static TParseTree NullLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), "OXD.NullLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`NullLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), "OXD.NullLiteral"), "NullLiteral")(p);
                memo[tuple(`NullLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NullLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), "OXD.NullLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("null"), Spacing), "OXD.NullLiteral"), "NullLiteral")(TParseTree("", false,[], s));
        }
    }
    static string NullLiteral(GetName g)
    {
        return "OXD.NullLiteral";
    }

    static TParseTree BoolLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), "OXD.BoolLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`BoolLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), "OXD.BoolLiteral"), "BoolLiteral")(p);
                memo[tuple(`BoolLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BoolLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), "OXD.BoolLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("true"), Spacing), pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("false"), Spacing)), "OXD.BoolLiteral"), "BoolLiteral")(TParseTree("", false,[], s));
        }
    }
    static string BoolLiteral(GetName g)
    {
        return "OXD.BoolLiteral";
    }

    static TParseTree IntegerLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer)), "OXD.IntegerLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`IntegerLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer)), "OXD.IntegerLiteral"), "IntegerLiteral")(p);
                memo[tuple(`IntegerLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IntegerLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer)), "OXD.IntegerLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer)), "OXD.IntegerLiteral"), "IntegerLiteral")(TParseTree("", false,[], s));
        }
    }
    static string IntegerLiteral(GetName g)
    {
        return "OXD.IntegerLiteral";
    }

    static TParseTree Integer(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(digit)), "OXD.Integer")(p);
        }
        else
        {
            if (auto m = tuple(`Integer`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(digit)), "OXD.Integer"), "Integer")(p);
                memo[tuple(`Integer`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Integer(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(digit)), "OXD.Integer")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(digit)), "OXD.Integer"), "Integer")(TParseTree("", false,[], s));
        }
    }
    static string Integer(GetName g)
    {
        return "OXD.Integer";
    }

    static TParseTree DoubleLiteral(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.caseInsensitiveLiteral!("e"), pegged.peg.option!(Sign), Integer)))), "OXD.DoubleLiteral")(p);
        }
        else
        {
            if (auto m = tuple(`DoubleLiteral`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.caseInsensitiveLiteral!("e"), pegged.peg.option!(Sign), Integer)))), "OXD.DoubleLiteral"), "DoubleLiteral")(p);
                memo[tuple(`DoubleLiteral`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DoubleLiteral(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.caseInsensitiveLiteral!("e"), pegged.peg.option!(Sign), Integer)))), "OXD.DoubleLiteral")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(Sign), Integer, pegged.peg.literal!("."), pegged.peg.option!(Integer), pegged.peg.option!(pegged.peg.and!(pegged.peg.caseInsensitiveLiteral!("e"), pegged.peg.option!(Sign), Integer)))), "OXD.DoubleLiteral"), "DoubleLiteral")(TParseTree("", false,[], s));
        }
    }
    static string DoubleLiteral(GetName g)
    {
        return "OXD.DoubleLiteral";
    }

    static TParseTree Sign(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing), "OXD.Sign")(p);
        }
        else
        {
            if (auto m = tuple(`Sign`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing), "OXD.Sign"), "Sign")(p);
                memo[tuple(`Sign`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Sign(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing), "OXD.Sign")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.literal!("-"), pegged.peg.literal!("+")), Spacing), "OXD.Sign"), "Sign")(TParseTree("", false,[], s));
        }
    }
    static string Sign(GetName g)
    {
        return "OXD.Sign";
    }

    template List(alias Elem, alias Sep)
    {
    static TParseTree List(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sep, Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "OXD.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sep, Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "OXD.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")"), "List_2")(p);
                memo[tuple("List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree List(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sep, Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "OXD.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Elem, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Sep, Spacing), pegged.peg.wrapAround!(Spacing, Elem, Spacing)), Spacing))), "OXD.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")"), "List_2")(TParseTree("", false,[], s));
        }
    }
    static string List(GetName g)
    {
        return "OXD.List!(" ~ pegged.peg.getName!(Elem)() ~ ", " ~ pegged.peg.getName!(Sep) ~ ")";
    }

    }
    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Main(p));
        result.children = [result];
        result.name = "OXD";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return OXD(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return OXD(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "OXD";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericOXD!(ParseTree).OXD OXD;

