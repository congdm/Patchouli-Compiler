MODULE BaseX8664;

IMPORT
	B := Base;
	
CONST
	MaxSize = 80000000H; (* 2 GB limit *)
	MaxLocBlkSize = 100000H; (* 1 MB limit *)
	
TYPE
	Item* = RECORD
	END ;

	Object* = POINTER TO ObjDesc;
	Type* = POINTER TO TypeDesc;
	Node* = POINTER TO NodeDesc;
		
	ObjDesc* = RECORD (B.ObjDesc)
		val*, adr*, locblksize*: INTEGER
	END ;

	TypeDesc* = RECORD (B.TypeDesc)
		untagged*, union*, unsafe*: BOOLEAN;
		len*, size*, align*, adr*, parblksize*: INTEGER;
		nPtr*, nProc*, nTraced*: INTEGER
	END ;
	
	NodeDesc* = RECORD (B.NodeDesc)
		item*: Item
	END ;
	
	Module* = POINTER TO RECORD (B.Module)
		varSize*: INTEGER;
		tdescTableSize*: INTEGER;
		strbufsize*: INTEGER;
		strbuf*: ARRAY 10000H OF CHAR
	END ;

END BaseX8664.