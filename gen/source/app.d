import
		std.file,
		std.array,
		std.regex,
		std.stdio,
		std.format,

		pegged.grammar;


enum OXD = `
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
	FuncStmt			< Type{extractChilds} Identifier '(' Args ')' (';' / '{' Stmt{extractChilds}* '}')

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
`;

auto gen()
{
	string res;

	foreach(m; OXD.match(regex(`"(\w+)"`, `g`)))
	{
		res ~= format(`"%s",`, m.captures.back);
	}

	return res;
}

void main()
{
	chdir(`../source`);

	auto extra =
`
import
		std.array,
		std.algorithm;


auto extractChilds(ParseTree p)
{
	return p.successful ? p.children[0] : p;
}

auto failOnKeyword(ParseTree p)
{
	static immutable words = [ ` ~ gen ~ ` ];

	p.successful &= !words.canFind(p.matches.front);
	return p;
}
`;

	asModule(`oxd.grammar`, `oxd/grammar`, OXD, extra);
}
