MODULE A;
 (*$CONSOLE*)

IMPORT
   Console;

VAR
   R: REAL;
   
BEGIN
   R :=1.0*1.2;
   Console.WriteReal(R); Console.WriteString(' <- *'); Console.WriteLn;

   R :=1.0/1.2;
   Console.WriteReal(R); Console.WriteString(' <- /'); Console.WriteLn;

   R :=1.0+1.2;
   Console.WriteReal(R); Console.WriteString(' <- +'); Console.WriteLn;

   R :=1.0-1.2;
   Console.WriteReal(R); Console.WriteString(' <- -'); Console.WriteLn;
END A.