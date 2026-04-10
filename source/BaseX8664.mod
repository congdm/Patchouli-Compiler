MODULE BaseX8664;

IMPORT
	Sys, B := Base;
	
CONST
	MaxSize = 80000000H; (* 2 GB limit *)
	MaxLocBlkSize = 100000H; (* 1 MB limit *)
	
TYPE
	Const* = POINTER TO RECORD (B.Const) ival*: Sys.Int; rval*: Sys.Real END ;
	Str* = POINTER TO RECORD (B.Str) pos*: INTEGER END ;
	
	Type* = POINTER TO RECORD (B.Type)
		align*: BYTE;
		size*: Sys.Int 
	END ;

	Module* = POINTER TO RECORD (B.Module)
		strbuf*: ARRAY 32768 OF BYTE;
		strbufSize*: INTEGER
	END ;

END BaseX8664.