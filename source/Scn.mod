MODULE Scn;

IMPORT Files;
	
PROCEDURE Init*(fname: ARRAY OF CHAR; pos: INTEGER);
END Init;

PROCEDURE ReadCh;
END ReadCh;

PROCEDURE Get*(VAR sym: INTEGER);
END Get;

END Scn.