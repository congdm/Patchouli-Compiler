MODULE Test7;
IMPORT SYSTEM;
TYPE Block = RECORD tdAdr, mark: INTEGER END;
VAR x, i: INTEGER;
BEGIN
	i := SYSTEM.CAS(x{Block}.mark, 0, 0)
END Test7.