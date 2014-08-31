MODULE Test4;
(*$CONSOLE*)

IMPORT
	Console;

TYPE
	String = ARRAY 32 OF CHAR;

	List = POINTER TO ListDesc;
	ListDesc = RECORD
		next : List; str : String
	END;
	
VAR
	list, sentinel : List;
	
PROCEDURE Add (str : String);
BEGIN
	NEW (list.next); list := list.next; list.str := str
END Add;
	
PROCEDURE Main;
	VAR str : String; p : List;
BEGIN
	NEW (sentinel); list := sentinel;
	str := 'This '; Add (str);
	str := 'is '; Add (str);
	str := 'a '; Add (str);
	str := 'test '; Add (str);
	str := 'example.'; Add (str);
	p := sentinel.next;
	WHILE p # NIL DO Console.WriteString (p.str); p := p.next END
END Main;

BEGIN
	Main
END Test4.