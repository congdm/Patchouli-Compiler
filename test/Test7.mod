MODULE Test7;
IMPORT SYSTEM;
TYPE Block = RECORD tdAdr, mark: INTEGER END;
VAR x, i: INTEGER; i32: SYSTEM.INT32; c32: SYSTEM.CARD32; i8: SYSTEM.INT8;
BEGIN
	i := SYSTEM.CAS(x{Block}.mark, 0, 0);
	i32 := c32; c32 := i32;
	IF c32 = i32 THEN
		x := i8; i8 := x
	END
END Test7.