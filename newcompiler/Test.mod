MODULE Test;
(*$CONSOLE*)

IMPORT
	Scanner, Console, Sys;
	
VAR
	srcfile: Sys.FileHandle;
	sym, prevSym: INTEGER;
	
	(* Symbols *)
(*    null = 0; times* = 1; rdiv* = 2; div* = 3; mod* = 4;
    and* = 5; plus* = 6; minus* = 7; or* = 8; eql* = 9;
    neq* = 10; lss* = 11; leq* = 12; gtr* = 13; geq* = 14;
    in* = 15; is* = 16; arrow* = 17; period* = 18;
    char* = 20; int* = 21; real* = 22; false* = 23; true* = 24;
    nil* = 25; string* = 26; not* = 27; lparen* = 28; lbrak* = 29;
    lbrace* = 30; ident* = 31;
    if* = 32; while* = 34; repeat* = 35; case* = 36; for* = 37;
    comma* = 40; colon* = 41; becomes* = 42; upto* = 43; rparen* = 44;
    rbrak* = 45; rbrace* = 46; then* = 47; of* = 48; do* = 49;
    to* = 50; by* = 51; semicolon* = 52; end* = 53; bar* = 54;
    else* = 55; elsif* = 56; until* = 57; return* = 58;
    array* = 60; record* = 61; pointer* = 62; address* = 63;
	const* = 70; type* = 71; var* = 72; procedure* = 73; begin* = 74;
	import* = 75; module* = 76; *)
	
PROCEDURE WriteLn (str: ARRAY OF CHAR);
BEGIN
	Console.WriteString (str);
	IF str[0] # ';' THEN Console.Write(' ') ELSE Console.WriteLn END
END WriteLn;
	
BEGIN
	Sys.Open (srcfile, 'Test.mod'); Scanner.Init (srcfile, 0); prevSym := 0;
	REPEAT Scanner.Get (sym);
		IF sym = Scanner.module THEN WriteLn ('MODULE')
		ELSIF sym = Scanner.import THEN WriteLn ('IMPORT')
		ELSIF sym = Scanner.begin THEN WriteLn ('BEGIN')
		ELSIF sym = Scanner.procedure THEN WriteLn ('PROCEDURE')
		ELSIF sym = Scanner.var THEN WriteLn ('VAR')
		ELSIF sym = Scanner.type THEN WriteLn ('TYPE')
		ELSIF sym = Scanner.const THEN WriteLn ('CONST')
		ELSIF sym = Scanner.address THEN WriteLn ('ADDRESS')
		ELSIF sym = Scanner.pointer THEN WriteLn ('POINTER')
		ELSIF sym = Scanner.record THEN WriteLn ('RECORD')
		ELSIF sym = Scanner.array THEN WriteLn ('ARRAY')
		ELSIF sym = Scanner.return THEN WriteLn ('RETURN')
		ELSIF sym = Scanner.until THEN WriteLn ('UNTIL')
		ELSIF sym = Scanner.elsif THEN WriteLn ('ELSIF')
		ELSIF sym = Scanner.else THEN WriteLn ('ELSE')
		ELSIF sym = Scanner.bar THEN WriteLn ('|')
		ELSIF sym = Scanner.end THEN WriteLn ('END')
		ELSIF sym = Scanner.semicolon THEN WriteLn (';')
		ELSIF sym = Scanner.by THEN WriteLn ('BY')
		ELSIF sym = Scanner.to THEN WriteLn ('TO')
		ELSIF sym = Scanner.do THEN WriteLn ('DO')
		ELSIF sym = Scanner.of THEN WriteLn ('OF')
		ELSIF sym = Scanner.then THEN WriteLn ('THEN')
		ELSIF sym = Scanner.rbrace THEN WriteLn ('}')
		ELSIF sym = Scanner.rbrak THEN WriteLn (']')
		ELSIF sym = Scanner.rparen THEN WriteLn (')')
		ELSIF sym = Scanner.upto THEN WriteLn ('..')
		ELSIF sym = Scanner.becomes THEN WriteLn (':=')
		ELSIF sym = Scanner.colon THEN WriteLn (':')
		ELSIF sym = Scanner.comma THEN WriteLn (',')
		ELSIF sym = Scanner.for THEN WriteLn ('FOR')
		ELSIF sym = Scanner.case THEN WriteLn ('CASE')
		ELSIF sym = Scanner.repeat THEN WriteLn ('REPEAT')
		ELSIF sym = Scanner.while THEN WriteLn ('WHILE')
		ELSIF sym = Scanner.if THEN WriteLn ('IF')
		ELSIF sym = Scanner.ident THEN WriteLn (Scanner.id)
		ELSIF sym = Scanner.lbrace THEN WriteLn ('{')
		ELSIF sym = Scanner.lbrak THEN WriteLn ('[')
		ELSIF sym = Scanner.lparen THEN WriteLn ('(')
		ELSIF sym = Scanner.not THEN WriteLn ('~')
		ELSIF sym = Scanner.string THEN WriteLn (Scanner.str)
		ELSIF sym = Scanner.nil THEN WriteLn ('NIL')
		ELSIF sym = Scanner.true THEN WriteLn ('TRUE')
		ELSIF sym = Scanner.false THEN WriteLn ('FALSE')
		ELSIF sym = Scanner.real THEN Console.WriteReal (Scanner.rval); Console.Write(' ')
		ELSIF sym = Scanner.int THEN Console.WriteInt (Scanner.ival); Console.Write(' ')
		ELSIF sym = Scanner.char THEN Console.Write (CHR(Scanner.ival)); Console.Write(' ')
		ELSIF sym = Scanner.period THEN WriteLn ('.')
		ELSIF sym = Scanner.arrow THEN WriteLn ('^')
		ELSIF sym = Scanner.is THEN WriteLn ('IS')
		ELSIF sym = Scanner.in THEN WriteLn ('IN')
		ELSIF sym = Scanner.geq THEN WriteLn ('>=')
		ELSIF sym = Scanner.gtr THEN WriteLn ('>')
		ELSIF sym = Scanner.leq THEN WriteLn ('<=')
		ELSIF sym = Scanner.lss THEN WriteLn ('<')
		ELSIF sym = Scanner.neq THEN WriteLn ('#')
		ELSIF sym = Scanner.eql THEN WriteLn ('=')
		ELSIF sym = Scanner.or THEN WriteLn ('OR')
		ELSIF sym = Scanner.minus THEN WriteLn ('-')
		ELSIF sym = Scanner.plus THEN WriteLn ('+')
		ELSIF sym = Scanner.and THEN WriteLn ('&')
		ELSIF sym = Scanner.mod THEN WriteLn ('MOD')
		ELSIF sym = Scanner.div THEN WriteLn ('DIV')
		ELSIF sym = Scanner.rdiv THEN WriteLn ('/')
		ELSIF sym = Scanner.times THEN WriteLn ('*')
		END;
		prevSym := sym
	UNTIL sym = Scanner.null;
	Sys.Close (srcfile)
END Test.