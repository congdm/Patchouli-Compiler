MODULE Test8;

TYPE
	Node = POINTER TO NodeDesc;
	NodeDesc = RECORD
		val : INTEGER;
		next : Node
	END;
	
	List = POINTER TO RECORD
		node1 : Node;
		next : List;
	END;
	
	Node2 = POINTER TO Node2Desc;
	Node2Desc = RECORD (NodeDesc)
		val2 : INTEGER;
	END;
	
	Rec = RECORD
		a : ARRAY 1024 OF List
	END;

VAR
	n : Node;
	l : List;
	n2 : Node2;
	
PROCEDURE New1;
BEGIN
	l := NIL;
	IF l = NIL THEN
	END;
END New1;

PROCEDURE Proc2 (VAR a : NodeDesc; b : Node);
BEGIN
	a (Node2Desc).val2 := 0;
	b (Node2).val2 := 1;
	IF a IS Node2Desc THEN
	ELSIF b IS Node2Desc THEN
	END
END Proc2;

BEGIN
	n.val := 0;
	l.node1.val := 0;
	n2.val2 := 1;
	n2.val := 2;
	l.node1 (Node2).val2 := 6;
END Test8.
