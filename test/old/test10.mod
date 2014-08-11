MODULE Test10;

CONST
	max* = 100000;

TYPE
	MyInt* = INTEGER;
	Node* = POINTER TO NodeDesc;
	NodeDesc = RECORD
		flag : BOOLEAN;
		val* : INTEGER;
		next* : Node;
	END;

	TwoDim* = ARRAY 10 OF ARRAY 10 OF INT32;
	
VAR
	i* : INTEGER;
	n* : Node;

END Test10.
