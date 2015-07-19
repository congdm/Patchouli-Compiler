MODULE Test;
(*$CONSOLE*)

IMPORT
	Scanner, Console, Sys;
	
VAR
	srcfile: Sys.FileHandle;
	sym: INTEGER;
	
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
	Console.WriteString (str); Console.WriteLn
END WriteLn;
	
BEGIN
	Sys.Open (srcfile, 'Test.mod'); Scanner.Init (srcfile, 0);
	REPEAT Scanner.Get (sym);
		IF sym = Scanner.module THEN WriteLn ('MODULE')
		END
	UNTIL sym = Scanner.null;
	Sys.Close (srcfile)
END Test.