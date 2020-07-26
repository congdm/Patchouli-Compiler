MODULE BaseX8664;

IMPORT
	B := Base;
	
TYPE
	Type* = POINTER TO RECORD (B.Type)
		untagged*, union*, unsafe*: BOOLEAN;
		len*, size*, align*, adr*: INTEGER;
		nPtr*, nProc*, nTraced*: INTEGER
	END ;

	Const* = POINTER TO RECORD (B.Const) value*: INTEGER END ;
	Var* = POINTER TO RECORD (B.Var) adr*: INTEGER END ;
	Par* = POINTER TO RECORD (Var) varpar*: BOOLEAN END ;
	Str* = POINTER TO RECORD (Var) len*: INTEGER END ;
	Field* = POINTER TO RECORD (B.Field) off*: INTEGER END ;

	StrBuf* = POINTER TO RECORD (B.StrBuf)
		size*: INTEGER;
		buf*: ARRAY 10000H OF CHAR
	END ;
	
	Module* = POINTER TO RECORD (B.Module)
		tdescTableSize*: INTEGER;
		strbuf*: B64.StrBuf
	END ;

END BaseX8664.