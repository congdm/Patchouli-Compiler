MODULE Test3;

TYPE
	Node = POINTER TO RECORD
		x : INTEGER;
		next : Node
	END;

VAR
	node : Node;
	
PROCEDURE New() : Node;
BEGIN
	NEW (node)
	RETURN node
END New;

BEGIN
	node := New();
	node := New()
END Test3.
