MODULE Generator;

IMPORT
	Base;

TYPE
	Instruction = POINTER TO InstructionDesc;
	InstructionDesc = ExTENSIBLE RECORD
		flag : SET;
		op : INTEGER;
		END;

VAR
	var_table : ARRAY 1024 OF Base.Object;
	codes : ARRAY 10000 OF Instruction;

PROCEDURE Module_init*;
	BEGIN
	END Module_init;

PROCEDURE End_module_init*;
	BEGIN
	END End_module_init;

END Generator.
