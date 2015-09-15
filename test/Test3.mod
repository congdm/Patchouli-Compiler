MODULE Test3;
(*$CONSOLE*)

IMPORT
	Console;
	
TYPE
	Shape = POINTER TO EXTENSIBLE RECORD
		x: INTEGER;
		Print: PROCEDURE (shape: Shape)
	END;
	
	Rectangle = POINTER TO RECORD (Shape)
	END;
	
	Circle = POINTER TO RECORD (Shape)
	END;
	
VAR
	s: Shape;
	r: Rectangle;
	c: Circle;
	
PROCEDURE PrintRect (rect: Shape);
BEGIN
	Console.WriteString ('This is a rectangle');
	Console.WriteLn
END PrintRect;

PROCEDURE PrintCircle (circle: Shape);
BEGIN
	Console.WriteString ('This is a circle');
	Console.WriteLn
END PrintCircle;
	
PROCEDURE NewRect (VAR rect: Rectangle);
BEGIN
	NEW (rect); rect.Print := PrintRect
END NewRect;

PROCEDURE NewCircle (VAR circle: Circle);
BEGIN
	NEW (circle); circle.Print := PrintCircle
END NewCircle;

PROCEDURE StaticPrint (shape: Shape);
BEGIN
	CASE shape OF
		Rectangle: Console.WriteString ('This is a rectangle') |
		Circle: Console.WriteString ('This is a circle')
	END;
	(*IF shape IS Rectangle THEN Console.WriteString ('This is a rectangle')
	ELSIF shape IS Circle THEN Console.WriteString ('This is a circle')
	END;*)
	Console.WriteLn
END StaticPrint;

BEGIN
	NewRect (r); s := r; s.Print (s); StaticPrint (s);
	NewCircle (c); s := c; s.Print (s); StaticPrint (s)
END Test3.