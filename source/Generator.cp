MODULE Generator;

IMPORT
	Base;
	
CONST
	form_nothing = 0; form_reg = 1; form_imm = 2; form_mem_reg = 3;
	form_mem_pc = 4; form_mem_scale = 5; form_proc_name = 6;
	
	op_NOP = 0; op_MOV = 1; op_MOVSX = 2; op_MOVZX = 3;
	
	reg_R8 = 0; reg_R9 = 1; reg_R10 = 2; reg_R11 = 3;
	reg_R12 = 4; reg_R13 = 5; reg_R14 = 6; reg_R15 = 7;
	reg_RCX = 8; reg_RDX = 9; reg_RSI = 10; reg_RDI = 11;
	reg_RAX = 12; reg_RBX = 13; reg_RBP = 14; reg_RSP = 15;

TYPE
	Operand = RECORD
		form, scale : BYTE;
		reg1, reg2, imm : INTEGER;
		obj : Base.Object
		END;
	
	Instruction = RECORD
		has_label : BOOLEAN;
		op : INTEGER;
		operands : ARRAY 3 OF Operand
		END;
	
VAR
	op_table : ARRAY 256, 10 OF CHAR;
	reg_table : ARRAY 128, 6 OF CHAR;

	codes : ARRAY 20000 OF Instruction;
	modid : Base.String;
	pc : INTEGER;
	out : Base.FileHandle;
	
PROCEDURE Write_scope_name (scope : Base.Object);
	BEGIN
	IF scope.dsc # NIL THEN
		Write_scope_name (scope.dsc);
		Base.Write_char (out, '@');
		Base.Write_string (out, scope.name.content)
	ELSE
		Base.Write_string (out, scope.name.content)
		END
	END Write_scope_name;
	
PROCEDURE Write_instruction (VAR inst : Instruction);

	PROCEDURE Write_operand (VAR op : Operand);
		BEGIN
		CASE op.form OF
			form_reg:
				Base.Write_string (out, reg_table [op.reg1]) |
			form_imm:
				Base.Write_number (out, op.imm) |
			form_mem_reg:
				Base.Write_char (out, '[');
				Base.Write_string (out, reg_table [op.reg1]);
				Base.Write_string (out, ' + ');
				Base.Write_number (out, op.imm);
				Base.Write_char (out, ']') |
			form_mem_pc:
				Base.Write_char (out, '[');
				Write_scope_name (Base.universe);
				Base.Write_string (out, '@VAR + ');
				Base.Write_number (out, op.imm);
				Base.Write_char (out, ']') |
			form_mem_scale:
				Base.Write_char (out, '[');
				Base.Write_string (out, reg_table [op.reg1]);
				Base.Write_string (out, ' + ');
				Base.Write_string (out, reg_table [op.reg2]);
				Base.Write_string (out, ' * ');
				Base.Write_number (out, op.scale);
				Base.Write_string (out, ' + ');
				Base.Write_number (out, op.imm);
				Base.Write_char (out, ']') |
			form_proc_name:
				Write_scope_name (op.obj.scope);
				Base.Write_char (out, '@');
				Base.Write_string (out, op.obj.name.content)
			END
		END Write_operand;

	BEGIN (* Write_instruction *)
	Base.Write_string (out, op_table [inst.op]);
	IF inst.operands [0].form # form_nothing THEN
		Base.Write_char (out, 9X);
		Write_operand (inst.operands [0]);
		IF inst.operands [1].form # form_nothing THEN
			Base.Write_string (out, ', ');
			Write_operand (inst.operands [1]);
			IF inst.operands [2].form # form_nothing THEN
				Base.Write_string (out, ', ');
				Write_operand (inst.operands [2])
				END
			END
		END;
	Base.Write_newline (out)
	END Write_instruction;
	
PROCEDURE Write_codes_to_file;
	VAR
		i : INTEGER;
	BEGIN
	Write_scope_name (Base.top_scope);
	Base.Write_char (out, ':');
	Base.Write_newline (out);
	FOR i := 0 TO pc - 1 DO
		IF codes [i].has_label THEN
			Write_scope_name (Base.top_scope);
			Base.Write_char (out, '@');
			Base.Write_number (out, i);
			Base.Write_char (out, ':');
			Base.Write_newline (out)
			END;
		Write_instruction (codes [i])
		END
	END Write_codes_to_file;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Module_init*;
	BEGIN
	END Module_init;

PROCEDURE End_module_init*;
	BEGIN
	END End_module_init;
	
PROCEDURE Enter* (proc : Base.Object; locblksize : INTEGER);
	BEGIN
	END Enter;
	
PROCEDURE Return_value* (VAR x : Base.Item);
	BEGIN
	END Return_value;
	
PROCEDURE Return* (proc : Base.Object; locblksize : INTEGER);
	BEGIN
	END Return;
	
PROCEDURE Init* (module_name : Base.String);
	BEGIN
	Base.Rewrite (out, modid.content);
	modid := module_name
	END Init;
	
PROCEDURE Finish*;
	BEGIN
	Base.Close (out)
	END Finish;
	
PROCEDURE Init_optable;
	BEGIN
	op_table [op_NOP] := 'nop';
	
	op_table [op_MOV] := 'mov';
	op_table [op_MOVSX] := 'movsx';
	op_table [op_MOVZX] := 'movzx';
	END Init_optable;
	
PROCEDURE Init_regtable;
	BEGIN
	reg_table [reg_RAX] := 'rax';
	reg_table [reg_RBX] := 'rbx';
	reg_table [reg_RCX] := 'rcx';
	reg_table [reg_RDX] := 'rdx';
	reg_table [reg_RSI] := 'rsi';
	reg_table [reg_RDI] := 'rdi';
	reg_table [reg_RBP] := 'rbp';
	reg_table [reg_RSP] := 'rsp';
	
	reg_table [reg_R8] := 'r8';
	reg_table [reg_R9] := 'r9';
	reg_table [reg_R10] := 'r10';
	reg_table [reg_R11] := 'r11';
	reg_table [reg_R12] := 'r12';
	reg_table [reg_R13] := 'r13';
	reg_table [reg_R14] := 'r14';
	reg_table [reg_R15] := 'r15';
	END Init_regtable;
	
BEGIN
Init_optable;
Init_regtable;
END Generator.
