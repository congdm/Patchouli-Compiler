MODULE GeneratorWin64;

IMPORT
	SYSTEM, Sys, Base, Scanner;
	
CONST
	MAX_INT32 = 2147483647;
	MIN_INT32 = -MAX_INT32 - 1;
	MAX_INT = 9223372036854775807;
	MIN_INT = -MAX_INT - 1;

	form_nothing = 0; form_reg = 1; form_imm = 2; form_mem_reg = 3;
	form_mem_pc = 4; form_mem_scale = 5; form_proc_name = 6; form_sym = 7;
	form_label = 8; form_mem_proc = 9; form_xreg = 10;
	
	forms_Memory = {form_mem_reg, form_mem_pc, form_mem_scale, form_mem_proc};
	
	flag_hasLabel = 0; flag_skipped = 1;
	
	op_NOP = 0;
	op_MOV = 1; op_MOVSX = 2; op_MOVZX = 3; op_LEA = 4; op_PUSH = 5; op_POP = 6;
	op_XCHG = 7; op_CQO = 8; op_REP_MOVSB = 9; op_REP_STOSQ = 10;
	op_REP_STOSB = 11;
	
	op_ADD = 20; op_SUB = 21; op_IMUL = 22; op_IDIV = 23; op_NEG = 24;
	op_AND = 25; op_OR = 26; op_XOR = 27; op_NOT = 28; op_CMP = 29;
	op_TEST = 30; op_SHL = 31; op_SHR = 32; op_SAR = 33; op_BT = 34;
	op_BTS = 35; op_BTR = 36; op_BTC = 37; op_INC = 38; op_DEC = 39;
	
	op_CALL = 40; op_RET = 41; op_LEAVE = 42;
	
	op_JMP = 50; vop_NJMP = 51; op_JO = 52; op_JNO = 53;
	op_JE = 54; op_JNE = 55; op_JL = 56; op_JGE = 57; op_JG = 58; op_JLE = 59;
	op_JA = 60; op_JBE = 61; op_JB = 62; op_JAE = 63; op_JC = 64; op_JNC = 65;
	
	op_MOVSS = 100; op_MOVSD = 101; op_MOVD = 102; op_MOVQ = 103;
	op_MOVAPS = 104;
	op_ADDSS = 110; op_SUBSS = 111; op_MULSS = 112; op_DIVSS = 113;
	op_ANDPS = 120; op_ORPS = 121; op_XORPS = 123;
	
	(* ====================================================================== *)
	
	(* RCX to R9 are registers for parameter passing *)
	reg_RCX = 0; reg_RDX = 1; reg_R8 = 2; reg_R9 = 3;
	
	reg_R10 = 4; reg_R11 = 5; (* R10, R11 are caller-save *)
	reg_R12 = 6; reg_R13 = 7; reg_R14 = 8; reg_R15 = 9; reg_TOP = 9;
	(* R12 to RDI are callee-save *)
	reg_RSI = 10; reg_RDI = 11;
	(* RSI, RDI, RAX, RBX, RBP, RSP are only used for special purpose *)
	reg_RAX = 12; reg_RBX = 13; reg_RBP = 14; reg_RSP = 15;
	reg_stackBase = reg_RBP;
	
	reg_EAX = reg_RAX + 16; reg_EBX = reg_RBX + 16; reg_ECX = reg_RCX + 16;
	reg_EDX = reg_RDX + 16; reg_ESI = reg_RSI + 16; reg_EDI = reg_RDI + 16;
	reg_EBP = reg_RBP + 16; reg_ESP = reg_RSP + 16; reg_R8D = reg_R8 + 16;
	reg_R9D = reg_R9 + 16; reg_R10D = reg_R10 + 16; reg_R11D = reg_R11 + 16;
	reg_R12D = reg_R12 + 16; reg_R13D = reg_R13 + 16; reg_R14D = reg_R14 + 16;
	reg_R15D = reg_R15 + 16;
	
	reg_AX = reg_RAX + 32; reg_BX = reg_RBX + 32; reg_CX = reg_RCX + 32;
	reg_DX = reg_RDX + 32; reg_SI = reg_RSI + 32; reg_DI = reg_RDI + 32;
	reg_BP = reg_RBP + 32; reg_SP = reg_RSP + 32; reg_R8W = reg_R8 + 32;
	reg_R9W = reg_R9 + 32; reg_R10W = reg_R10 + 32; reg_R11W = reg_R11 + 32;
	reg_R12W = reg_R12 + 32; reg_R13W = reg_R13 + 32; reg_R14W = reg_R14 + 32;
	reg_R15W = reg_R15 + 32;
	
	reg_AL = reg_RAX + 48; reg_BL = reg_RBX + 48; reg_CL = reg_RCX + 48;
	reg_DL = reg_RDX + 48; reg_SIL = reg_RSI + 48; reg_DIL = reg_RDI + 48;
	reg_BPL = reg_RBP + 48; reg_SPL = reg_RSP + 48; reg_R8L = reg_R8 + 48;
	reg_R9L = reg_R9 + 48; reg_R10L = reg_R10 + 48; reg_R11L = reg_R11 + 48;
	reg_R12L = reg_R12 + 48; reg_R13L = reg_R13 + 48; reg_R14L = reg_R14 + 48;
	reg_R15L = reg_R15 + 48;
	
	(* XMM0 to XMM3 are registers for parameter passing *)
	reg_XMM0 = 0; reg_XMM1 = 1; reg_XMM2 = 2; reg_XMM3 = 3;
	reg_XMM4 = 4; reg_XMM5 = 5; reg_XMM6 = 6; reg_XMM7 = 7; reg_XMM8 = 8;
	reg_XMM9 = 9; reg_XMM10 = 10; reg_XMM11 = 11; reg_XMM12 = 12;
	reg_XMM13 = 13; reg_XMM14 = 14; reg_XMM15 = 15;

TYPE
	Operand = RECORD
		form, scale, size : BYTE;
		reg1, reg2 : INTEGER;
		obj : Base.Object;
		sym : Base.String;
		imm : LONGINT
	END;
	
	Instruction = RECORD
		flag : SET;
		op : INTEGER;
		operands : ARRAY 3 OF Operand
	END;
	
	ProcedureInfo* = RECORD
		reg_stack, xreg_stack, mem_stack,
		stack_frame_usage, parblksize, param_adr : INTEGER;
		param_regs : ARRAY 4 OF BYTE
	END;
	
VAR
	op_table : ARRAY 256, 10 OF CHAR;
	reg_table : ARRAY 64, 5 OF CHAR;
	xreg_table : ARRAY 64, 6 OF CHAR;
	sym_array_index_trap, sym_integer_overflow_trap : Base.String;
	sym_invalid_divisor_trap, sym_type_check_trap : Base.String;
	sym_varbase, sym_stringbase, sym_tdbase : Base.String;

	out, datafile : Sys.FileHandle;
	codes : ARRAY 20000 OF Instruction;
	strings : ARRAY 131072 OF CHAR;
	type_desc_list : ARRAY Base.max_number_record_types OF Base.Type;
	modid : Base.String;
	
	pc* : INTEGER;
	str_offset, td_offset, record_no : INTEGER;
	reg_stack, mem_stack, xreg_stack : INTEGER;
	used_regs, used_xregs : SET;
	stack_frame_usage, stack_frame_size : INTEGER;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Write_scope_name (scope : Base.Object);
BEGIN
	IF scope.dsc # NIL THEN
		Write_scope_name (scope.dsc);
		Sys.Write_char (out, '@');
		Sys.Write_string (out, scope.name.content)
	ELSE
		Sys.Write_string (out, scope.name.content)
	END
END Write_scope_name;

PROCEDURE Write_mem_prefix (VAR mem : Operand);
BEGIN
	CASE mem.size OF
		0: (* Do nothing *) |
		1: Sys.Write_string (out, 'byte ') |
		2: Sys.Write_string (out, 'word ') |
		4: Sys.Write_string (out, 'dword ') |
		8: Sys.Write_string (out, 'qword ')
	END
END Write_mem_prefix;

PROCEDURE Write_operand (VAR op : Operand);
BEGIN
	IF op.form IN forms_Memory THEN
		Write_mem_prefix (op);
		Sys.Write_char (out, '[')
	END;
	CASE op.form OF
		form_reg:
			Sys.Write_string (out, reg_table [op.reg1]) |
		form_xreg:
			Sys.Write_string (out, xreg_table [op.reg1]) |
		form_imm:
			Sys.Write_number (out, op.imm) |
		form_mem_reg:
			Sys.Write_string (out, reg_table [op.reg1]);
			Sys.Write_string (out, ' + ');
			Sys.Write_number (out, op.imm) |
		form_mem_pc:
			Write_scope_name (Base.universe);
			Sys.Write_string (out, '@@');
			Sys.Write_string (out, op.sym.content);
			Sys.Write_string (out, ' + ');
			Sys.Write_number (out, op.imm) |
		form_mem_scale:
			Sys.Write_string (out, reg_table [op.reg1]);
			Sys.Write_string (out, ' + ');
			Sys.Write_string (out, reg_table [op.reg2]);
			Sys.Write_string (out, ' * ');
			Sys.Write_number (out, op.scale);
			Sys.Write_string (out, ' + ');
			Sys.Write_number (out, op.imm) |
		form_mem_proc:
			Write_scope_name (op.obj.scope);
			Sys.Write_char (out, '@');
			Sys.Write_string (out, op.obj.name.content) |
		form_proc_name:
			Write_scope_name (op.obj.scope);
			Sys.Write_char (out, '@');
			Sys.Write_string (out, op.obj.name.content) |
		form_label:
			Write_scope_name (Base.top_scope);
			Sys.Write_char (out, '@');
			Sys.Write_number (out, op.imm) |
		form_sym:
			Sys.Write_string (out, op.sym.content)
	END;
	IF op.form IN forms_Memory THEN Sys.Write_char (out, ']')
	END;
END Write_operand;
	
PROCEDURE Write_instruction (VAR inst : Instruction);
BEGIN
	Sys.Write_string (out, op_table [inst.op]);
	IF inst.operands [0].form # form_nothing THEN
		Sys.Write_char (out, 9X);
		Write_operand (inst.operands [0]);
		IF inst.operands [1].form # form_nothing THEN
			Sys.Write_string (out, ', ');
			Write_operand (inst.operands [1]);
			IF inst.operands [2].form # form_nothing THEN
				Sys.Write_string (out, ', ');
				Write_operand (inst.operands [2])
			END
		END
	END;
	Sys.Write_newline (out)
END Write_instruction;
	
PROCEDURE Inc_program_counter;
BEGIN
	INC (pc);
	codes [pc].op := op_NOP;
	codes [pc].flag := {};
	codes [pc].operands [0].form := form_nothing;
	codes [pc].operands [1].form := form_nothing;
	codes [pc].operands [2].form := form_nothing
END Inc_program_counter;
	
PROCEDURE Write_codes_to_file;
	VAR
		i : INTEGER;
BEGIN
	Write_scope_name (Base.top_scope);
	IF Base.top_scope = Base.universe THEN Sys.Write_string (out, '@@INIT')
	END;
	Sys.Write_char (out, ':');
	Sys.Write_newline (out);
	
	FOR i := 0 TO pc - 1 DO
		IF ~ (flag_skipped IN codes [i].flag) THEN
			IF flag_hasLabel IN codes [i].flag THEN
				Write_scope_name (Base.top_scope);
				Sys.Write_char (out, '@');
				Sys.Write_number (out, i);
				Sys.Write_char (out, ':');
				Sys.Write_newline (out)
			END;
			Write_instruction (codes [i])
		END
	END;
	
	Sys.Write_newline (out);
	pc := -1;
	Inc_program_counter
END Write_codes_to_file;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Small_reg (r, size : INTEGER) : INTEGER;
BEGIN
	CASE size OF
		1: INC (r, 48) |
		2: INC (r, 32) |
		4: INC (r, 16) |
		8: (* Do nothing *)
	END;
	RETURN r
END Small_reg;

PROCEDURE Set_mem_operand (VAR o : Operand; VAR mem : Base.Item);
BEGIN
	IF mem.mode = Base.mode_regI THEN
		o.form := form_mem_reg;
		o.reg1 := mem.r;
		o.imm := mem.a
	ELSIF mem.mode = Base.class_proc THEN
		o.form := form_mem_proc;
		o.obj := mem.proc
	ELSIF mem.lev > 0 THEN
		(* Local variable *)
		o.form := form_mem_reg;
		o.reg1 := reg_stackBase;
		IF Base.flag_param IN mem.flag THEN
			o.imm := 16 + mem.a
		ELSE
			o.imm := mem.a
		END
	ELSIF mem.lev = 0 THEN
		(* Global variable *)
		o.form := form_mem_pc;
		IF Base.flag_typeDesc IN mem.flag THEN
			o.sym := sym_tdbase
		ELSIF mem.type.form = Base.type_string THEN
			o.sym := sym_stringbase
		ELSE
			o.sym := sym_varbase
		END;
		o.imm := mem.a
	END;
		
	IF (mem.mode = Base.class_ref) OR (mem.type = NIL) THEN
		o.size := 0
	ELSE
		CASE mem.type.size OF
			1: o.size := 1 |
			2: o.size := 2 |
			4: o.size := 4 |
			8: o.size := 8
		ELSE o.size := 0
		END
	END
END Set_mem_operand;
	
PROCEDURE Set_memX_operand
(VAR o : Operand; bReg, iReg, scale : INTEGER; imm : LONGINT);
BEGIN
	o.form := form_mem_scale;
	o.reg1 := bReg;
	o.reg2 := iReg;
	o.scale := SHORT (SHORT (scale));
	o.imm := imm
END Set_memX_operand;
	
PROCEDURE Set_reg_operand (VAR o : Operand; r : INTEGER);
BEGIN
	o.form := form_reg;
	o.reg1 := r
END Set_reg_operand;
	
PROCEDURE Set_imm_operand (VAR o : Operand; imm : LONGINT);
BEGIN
	o.form := form_imm;
	o.imm := imm
END Set_imm_operand;
	
PROCEDURE Set_proc_operand (VAR o : Operand; proc : Base.Object);
BEGIN
	o.form := form_proc_name;
	o.obj := proc
END Set_proc_operand;
	
PROCEDURE Set_sym_operand (VAR o : Operand; sym : Base.String);
BEGIN
	o.form := form_sym;
	o.sym := sym
END Set_sym_operand;
	
PROCEDURE Set_label_operand (VAR o : Operand; label_num : INTEGER);
BEGIN
	o.form := form_label;
	o.imm := label_num;
END Set_label_operand;
	
PROCEDURE Set_xreg_operand (VAR o : Operand; xreg : INTEGER);
BEGIN
	o.form := form_xreg;
	o.reg1 := xreg
END Set_xreg_operand;
	
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Emit_op_bare (op : INTEGER);
BEGIN
	codes [pc].op := op;
	Inc_program_counter
END Emit_op_bare;
	
PROCEDURE Emit_op_reg (op, r : INTEGER);
BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r);
	Inc_program_counter
END Emit_op_reg;
	
PROCEDURE Emit_op_imm (op : INTEGER; imm : LONGINT);
BEGIN
	codes [pc].op := op;
	Set_imm_operand (codes [pc].operands [0], imm);
	Inc_program_counter
END Emit_op_imm;
	
PROCEDURE Emit_op_proc (op : INTEGER; proc : Base.Object);
BEGIN
	codes [pc].op := op;
	Set_proc_operand (codes [pc].operands [0], proc);
	Inc_program_counter
END Emit_op_proc;
	
PROCEDURE Emit_op_mem (op : INTEGER; mem : Base.Item);
BEGIN
	codes [pc].op := op;
	Set_mem_operand (codes [pc].operands [0], mem);
	Inc_program_counter
END Emit_op_mem;
	
PROCEDURE Emit_op_sym (op : INTEGER; sym : Base.String);
BEGIN
	codes [pc].op := op;
	Set_sym_operand (codes [pc].operands [0], sym);
	Inc_program_counter
END Emit_op_sym;
	
PROCEDURE Emit_op_label (op, label_num : INTEGER);
BEGIN
	codes [pc].op := op;
	Set_label_operand (codes [pc].operands [0], label_num);
	Inc_program_counter
END Emit_op_label;
	
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Emit_op_reg_reg (op, r1, r2 : INTEGER);
BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r1);
	Set_reg_operand (codes [pc].operands [1], r2);
	Inc_program_counter
END Emit_op_reg_reg;

PROCEDURE Emit_op_reg_mem (op, r : INTEGER; VAR mem : Base.Item);
BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r);
	Set_mem_operand (codes [pc].operands [1], mem);
	Inc_program_counter
END Emit_op_reg_mem;
	
PROCEDURE Emit_op_reg_memX (op, r, bReg, iReg, scale : INTEGER; imm : LONGINT);
BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r);
	Set_memX_operand (codes [pc].operands [1], bReg, iReg, scale, imm);
	Inc_program_counter
END Emit_op_reg_memX;
	
PROCEDURE Emit_op_reg_imm (op, r : INTEGER; imm : LONGINT);
BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r);
	Set_imm_operand (codes [pc].operands [1], imm);
	Inc_program_counter
END Emit_op_reg_imm;
	
PROCEDURE Emit_op_mem_reg (op : INTEGER; VAR mem : Base.Item; r : INTEGER);
BEGIN
	codes [pc].op := op;
	Set_mem_operand (codes [pc].operands [0], mem);
	Set_reg_operand (codes [pc].operands [1], r);
	Inc_program_counter
END Emit_op_mem_reg;
	
PROCEDURE Emit_op_mem_imm (op : INTEGER; VAR mem : Base.Item; imm : LONGINT);
BEGIN
	codes [pc].op := op;
	Set_mem_operand (codes [pc].operands [0], mem);
	Set_imm_operand (codes [pc].operands [1], imm);
	Inc_program_counter
END Emit_op_mem_imm;
	
PROCEDURE Emit_op_mem_xreg (op : INTEGER; VAR mem : Base.Item; xreg : INTEGER);
BEGIN
	codes[pc].op := op;
	Set_mem_operand (codes[pc].operands[0], mem);
	Set_xreg_operand (codes[pc].operands[1], xreg);
	Inc_program_counter
END Emit_op_mem_xreg;
	
PROCEDURE Emit_op_xreg_mem (op, xreg : INTEGER; VAR mem : Base.Item);
BEGIN
	codes[pc].op := op;
	Set_xreg_operand (codes[pc].operands[0], xreg);
	Set_mem_operand (codes[pc].operands[1], mem);
	Inc_program_counter
END Emit_op_xreg_mem;
	
PROCEDURE Emit_op_xreg_xreg (op, xreg1, xreg2 : INTEGER);
BEGIN
	codes[pc].op := op;
	Set_xreg_operand (codes[pc].operands[0], xreg1);
	Set_xreg_operand (codes[pc].operands[1], xreg2);
	Inc_program_counter
END Emit_op_xreg_xreg;
	
PROCEDURE Emit_op_xreg_reg (op, xreg, reg : INTEGER);
BEGIN
	codes[pc].op := op;
	Set_xreg_operand (codes[pc].operands[0], xreg);
	Set_reg_operand (codes[pc].operands[1], reg);
	Inc_program_counter
END Emit_op_xreg_reg;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Safe_to_add (x, y : LONGINT) : BOOLEAN;
	VAR
		result : BOOLEAN;
BEGIN
	result := TRUE;
	IF (x >= 0) & (y >= 0) THEN
		IF y > MAX_INT - x THEN result := FALSE
		END;
	ELSIF (x < 0) & (y < 0) THEN
		IF y < MIN_INT - x THEN result := FALSE
		END
	END;
	RETURN result
END Safe_to_add;
	
PROCEDURE Safe_to_subtract (x, y : LONGINT) : BOOLEAN;
	VAR
		result : BOOLEAN;
BEGIN
	result := TRUE;
	IF (x >= 0) & (y < 0) THEN
		IF x > MAX_INT + y THEN result := FALSE
		END;
	ELSIF (x < 0) & (y >= 0) THEN
		IF x < MIN_INT + y THEN result := FALSE
		END
	END;
	RETURN result
END Safe_to_subtract;
	
PROCEDURE Safe_to_multiply (x, y : LONGINT) : BOOLEAN;
	VAR
		result : BOOLEAN;
		q, r : LONGINT;
BEGIN
	result := TRUE;
	IF (x < 0) & (y >= 0) THEN
		q := x;	x := y; y := q (* swap *)
	ELSIF (x < 0) & (y < 0) THEN
		IF (x = MIN_INT) OR (y = MIN_INT) THEN result := FALSE
		ELSE x := -x; y := -y
		END
	END;
	IF x > 0 THEN
		IF y > 0 THEN
			IF x > MAX_INT / y THEN result := FALSE
			END
		ELSIF y < 0 THEN
			q := MIN_INT DIV y; r := MIN_INT MOD y;
			IF r = 0 THEN
				IF x > q THEN result := FALSE
				END
			ELSE
				IF x >= q THEN result := FALSE
				END
			END
		END
	END;
	RETURN result
END Safe_to_multiply;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Get_type_desc_size (typ : Base.Type) : INTEGER;
BEGIN
	RETURN 16 + 8 * (Base.type_extension_limit + 1) + 8 * typ.num_ptr
END Get_type_desc_size;

PROCEDURE Alloc_type_desc* (typ : Base.Type);
BEGIN
	IF record_no >= LEN (type_desc_list) THEN
		Scanner.Mark ('Too many record types (compiler limit)')
	ELSE
		type_desc_list [record_no] := typ;
		INC (record_no)
	END;
	typ.obj.val := td_offset;
	INC (td_offset, Get_type_desc_size (typ))
END Alloc_type_desc;

PROCEDURE Make_const* (VAR x : Base.Item; typ : Base.Type; val : LONGINT);
BEGIN
	x.flag := {};
	x.mode := Base.class_const;
	x.lev := Base.cur_lev;
	x.type := typ;
	x.a := val
END Make_const;
	
PROCEDURE Make_item* (VAR x : Base.Item; obj : Base.Object);
BEGIN
	x.flag := obj.flag;
	x.mode := obj.class;
	x.lev := obj.lev;
	x.type := obj.type;
	x.a := obj.val;
	x.b := 0;
	x.c := 0;
	x.proc := obj
END Make_item;
	
PROCEDURE Make_string* (VAR x : Base.Item; str : Base.String);
	VAR
		i : INTEGER;
BEGIN
	x.flag := {Base.flag_readOnly};
	x.mode := Base.class_var;
	x.lev := 0;
	
	Base.New_typ (x.type, Base.type_string);
	x.type.len := str.len + 1;
	x.type.size := (str.len + 1) * Base.char_type.size;
	x.type.base := Base.char_type;
	x.a := str_offset * Base.char_type.size;
	
	FOR i := 0 TO str.len DO
		strings [str_offset + i] := str.content [i]
	END;
	INC (str_offset, str.len + 1)
END Make_string;
	
PROCEDURE Make_type_desc_item (VAR x : Base.Item; typ : Base.Type);
BEGIN
	x.flag := {Base.flag_typeDesc};
	x.mode := Base.class_var;
	x.type := Base.int_type;
	x.lev := 0;
	x.a := typ.obj.val
END Make_type_desc_item;
	
PROCEDURE Make_len_item (VAR x : Base.Item; len_offset : INTEGER);
BEGIN
	x.flag := {Base.flag_param};
	x.mode := Base.class_var;
	x.type := Base.int_type;
	x.a := len_offset;
	x.lev := Base.cur_lev;
END Make_len_item;
	
PROCEDURE Make_mem_stack_item
(VAR x : Base.Item; typ : Base.Type; offset : INTEGER);
BEGIN
	x.flag := {};
	x.mode := Base.mode_regI;
	x.lev := 0;
	x.type := typ;
	x.a := offset;
	x.r := reg_RSP
END Make_mem_stack_item;
	
PROCEDURE Set_cond (VAR x : Base.Item; n : INTEGER);
BEGIN
	x.mode := Base.mode_cond;
	x.a := 0; x.b := 0; x.r := n
END Set_cond;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Push_reg (reg : INTEGER);
BEGIN
	Emit_op_reg (op_PUSH, reg);
	INC (mem_stack, 8)
END Push_reg;
	
PROCEDURE Pop_reg (reg : INTEGER);
BEGIN
	Emit_op_reg (op_POP, reg);
	DEC (mem_stack, 8)
END Pop_reg;

PROCEDURE Push_scalar_xmm (xreg : INTEGER);
	VAR
		temp : Base.Item;
BEGIN
	Emit_op_reg_imm (op_SUB, reg_RSP, 8);
	Make_mem_stack_item (temp, Base.int_type, 0);
	Emit_op_mem_xreg (op_MOVSD, temp, xreg);
	INC (mem_stack, 8)
END Push_scalar_xmm;

PROCEDURE Pop_scalar_xmm (xreg : INTEGER);
	VAR
		temp : Base.Item;
BEGIN
	Make_mem_stack_item (temp, Base.int_type, 0);
	Emit_op_mem_xreg (op_MOVSD, temp, xreg);
	Emit_op_reg_imm (op_ADD, reg_RSP, 8);
	DEC (mem_stack, 8)
END Pop_scalar_xmm;
	
PROCEDURE Push_const (const : LONGINT);
BEGIN
	IF (const < MIN_INT32) & (const > MAX_INT32) THEN
		Emit_op_reg_imm (op_MOV, reg_RAX, const);
		Emit_op_reg (op_PUSH, reg_RAX)
	ELSE
		Emit_op_imm (op_PUSH, const)
	END
END Push_const;
	
PROCEDURE Zero_clear_reg (reg : INTEGER);
BEGIN
	reg := Small_reg (reg, 4);
	Emit_op_reg_reg (op_XOR, reg, reg)
END Zero_clear_reg;
	
PROCEDURE Emit_op_reg_const (op, reg : INTEGER; const : LONGINT);
BEGIN
	IF (const > MAX_INT32) OR (const < MIN_INT32) THEN
		Emit_op_reg_imm (op_MOV, reg_RAX, const);
		Emit_op_reg_reg (op, reg, reg_RAX)
	ELSE
		Emit_op_reg_imm (op, reg, const)
	END
END Emit_op_reg_const;
	
PROCEDURE Emit_op_mem_const (op : INTEGER; mem : Base.Item; const : LONGINT);
BEGIN
	IF (const > MAX_INT32) OR (const < MIN_INT32) THEN
		Emit_op_reg_imm (op_MOV, reg_RAX, const);
		Emit_op_mem_reg (op, mem, reg_RAX)
	ELSE
		Emit_op_mem_imm (op, mem, const)
	END
END Emit_op_mem_const;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Inc_reg_stack;
BEGIN
	IF reg_stack = reg_TOP THEN
		Scanner.Mark ('COMPILER LIMIT: General register stack overflow')
	ELSE
		INCL (used_regs, reg_stack); INC (reg_stack)
	END
END Inc_reg_stack;
	
PROCEDURE Inc_xreg_stack;
BEGIN
	IF xreg_stack = reg_XMM15 THEN
		Scanner.Mark ('COMPILER LIMIT: XMM register stack overflow')
	ELSE
		INCL (used_xregs, xreg_stack); INC (xreg_stack)
	END
END Inc_xreg_stack;
	
PROCEDURE Free_reg;
BEGIN
	IF reg_stack = 0 THEN
		Scanner.Mark ('COMPILER FAULT: General register stack underflow')
	ELSE
		DEC (reg_stack)
	END
END Free_reg;
	
PROCEDURE Free_xreg;
BEGIN
	IF xreg_stack = 0 THEN
		Scanner.Mark ('COMPILER FAULT: XMM register stack underflow')
	ELSE
		DEC (xreg_stack)
	END
END Free_xreg;
	
PROCEDURE Free_item* (VAR x : Base.Item);
BEGIN
	IF x.mode IN Base.modes_UseReg THEN
		Free_reg
	ELSIF x.mode = Base.mode_xreg THEN
		Free_xreg
	END
END Free_item;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Ref_to_regI (VAR x : Base.Item);
BEGIN
	Inc_reg_stack;
	Emit_op_reg_mem (op_MOV, reg_stack - 1, x);
	x.mode := Base.mode_regI;
	x.r := reg_stack - 1;
	x.a := x.c
END Ref_to_regI;

(* Emit an load instruction without modifying item *)
PROCEDURE load_to_reg (reg : INTEGER; VAR x : Base.Item);
	VAR
		size : INTEGER;
BEGIN
	CASE x.mode OF
		Base.mode_reg: Emit_op_reg_reg (op_MOV, reg, x.r) |
		Base.class_proc: Emit_op_reg_mem (op_LEA, reg, x) |
		Base.class_const:
			IF x.a # 0 THEN
				IF (x.a > 0) & (x.a <= MAX_INT32) THEN reg := Small_reg (x.r, 4)
				END;
				Emit_op_reg_imm (op_MOV, reg, x.a)
			ELSE Zero_clear_reg (reg)
			END |
		(* Base.class_ref: Emit_op_reg_mem (op_MOV, reg, x) | *)
		Base.mode_regI, Base.class_var:
			IF x.type = NIL THEN size := 8
			ELSE size := x.type.size
			END;
			IF size IN {1, 2, 4} THEN
				reg := Small_reg (reg, 4);
				IF size = 4 THEN Emit_op_reg_mem (op_MOV, reg, x)
				ELSE Emit_op_reg_mem (op_MOVZX, reg, x)
				END
			ELSE Emit_op_reg_mem (op_MOV, reg, x)
			END
	END
END load_to_reg;
	
PROCEDURE load* (VAR x : Base.Item);
	VAR
		temp : Base.Item;
		r : INTEGER;
		
	PROCEDURE Load_real (VAR x : Base.Item);
	BEGIN
		IF x.mode # Base.mode_xreg THEN
			Inc_xreg_stack;
			IF x.mode IN Base.cls_Variable THEN
				Emit_op_xreg_mem (op_MOVSS, xreg_stack - 1, x)
			ELSIF x.mode = Base.class_const THEN
				IF x.a # 0 THEN
					Emit_op_reg_imm (op_MOV, reg_EAX, x.a);
					Emit_op_xreg_reg (op_MOVD, xreg_stack - 1, reg_EAX)
				ELSE
					Emit_op_xreg_xreg (op_XORPS, xreg_stack - 1, xreg_stack - 1)
				END
			END;
			
			IF x.mode IN Base.modes_UseReg THEN Free_reg
			END;
			
			x.mode := Base.mode_xreg;
			x.r := xreg_stack - 1
		END
	END Load_real;
	
BEGIN (* load *)
	IF x.mode = Base.class_ref THEN Ref_to_regI (x)
	END;

	IF x.type = Base.real_type THEN
		Load_real (x)
	ELSIF x.mode # Base.mode_reg THEN
		IF (x.type.size > 0) & (x.type.size <= 8)
		& (x.type.size IN {1, 2, 4, 8}) THEN
			IF x.mode # Base.mode_regI THEN
				Inc_reg_stack;
				x.r := reg_stack - 1
			END;
			IF x.mode IN Base.cls_Variable THEN
				IF x.type.size IN {1, 2, 4} THEN
					r := Small_reg (x.r, 4);
					IF x.type.size = 4 THEN Emit_op_reg_mem (op_MOV, r, x)
					ELSE Emit_op_reg_mem (op_MOVZX, r, x)
					END
				ELSE Emit_op_reg_mem (op_MOV, x.r, x)
				END
			ELSIF x.mode = Base.class_const THEN
				IF x.a # 0 THEN
					IF (x.a > 0) & (x.a <= MAX_INT32) THEN r := Small_reg (x.r, 4)
					ELSE r := x.r
					END;
					Emit_op_reg_imm (op_MOV, r, x.a)
				ELSE Zero_clear_reg (x.r)
				END
			ELSIF x.mode = Base.class_proc THEN
				Emit_op_reg_mem (op_LEA, x.r, x)
			END;
			x.mode := Base.mode_reg
		ELSE
			Scanner.Mark ('COMPILER FAULT: procedure Generator.load')
		END
	END
END load;
	
PROCEDURE Load_adr (VAR x : Base.Item);
BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x)
	END;
	IF x.mode = Base.mode_regI THEN
		IF x.a # 0 THEN Emit_op_reg_imm (op_ADD, x.r, x.a)
		END
	ELSIF x.mode = Base.class_var THEN
		Inc_reg_stack; x.r := reg_stack - 1;
		Emit_op_reg_mem (op_LEA, x.r, x)
	ELSE
		Scanner.Mark ('COMPILER FAULT: Invalid Load_adr')
	END;
	x.mode := Base.mode_reg; x.type := Base.int_type
END Load_adr;
	
PROCEDURE Convert_to_cond (VAR x : Base.Item);
BEGIN
	IF x.mode = Base.class_const THEN
		Set_cond (x, vop_NJMP - SHORT (x.a))
	ELSIF x.mode = Base.mode_reg THEN
		Emit_op_reg_reg (op_TEST, x.r, x.r);
		Free_reg;
		Set_cond (x, op_JNE)
	ELSE
		IF x.mode = Base.class_ref THEN Ref_to_regI (x)
		END;
		Emit_op_mem_imm (op_CMP, x, 0);
		IF x.mode = Base.mode_regI THEN Free_reg
		END;
		Set_cond (x, op_JNE)
	END
END Convert_to_cond;

PROCEDURE Store* (VAR x, y : Base.Item);
	VAR
		r : INTEGER;
BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x)
	END;
	IF y.mode = Base.class_ref THEN Ref_to_regI (y)
	END;
	
	IF (x.type = Base.char_type) & (y.type.form = Base.type_string) THEN
		y.mode := Base.class_const;
		y.type := Base.char_type;
		y.a := ORD (strings [y.a DIV Base.char_type.size]) 
	END;
	
	IF (x.type.size > 0) & (x.type.size <= 8)
	& (x.type.size IN {1, 2, 4, 8}) THEN
		IF y.mode = Base.class_const THEN
			IF (x.type = Base.byte_type) & (Base.Size_of_const (y.a) # 1) THEN
				Scanner.Mark ('Const value out of BYTE range');
				y.a := 0
			END;
			Emit_op_mem_const (op_MOV, x, y.a)
		ELSIF y.mode = Base.mode_xreg THEN
			IF x.type = Base.real_type THEN
				Emit_op_mem_xreg (op_MOVSS, x, y.r)
			END
		ELSE
			load (y);
			r := Small_reg (y.r, x.type.size);
			Emit_op_mem_reg (op_MOV, x, r)
		END
	ELSIF x.type.form IN {Base.type_array, Base.type_record} THEN
		Emit_op_reg_mem (op_LEA, reg_RSI, y);
		Emit_op_reg_mem (op_LEA, reg_RDI, x);
		
		IF (y.type.form = Base.type_string) & (y.type.len < x.type.len) THEN
			Emit_op_reg_imm (op_MOV, reg_RCX, y.type.size)
		ELSE
			Emit_op_reg_imm (op_MOV, reg_RCX, x.type.size)
		END;
		Emit_op_bare (op_REP_MOVSB);
		
		used_regs := used_regs + {reg_RSI, reg_RDI};
	END;
	
	Free_item (y);
	Free_item (x)
END Store;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Set1* (VAR x : Base.Item);
BEGIN
	IF x.mode = Base.mode_reg THEN
		IF x.a # 0 THEN Emit_op_reg_const (op_OR, x.r, x.a)
		END
	END
END Set1;
	
PROCEDURE Set2* (VAR x, y : Base.Item);
	VAR
		s : ARRAY 2 OF SET;
BEGIN
	IF y.mode = Base.class_const THEN
		(* Delay code generation *)
		s[0] := {}; s[1] := {};
		SYSTEM.PUT (SYSTEM.ADR (s), x.a);
		IF y.a > 31 THEN INCL (s[0], y.a) ELSE INCL (s[1], y.a - 32)
		END;
		SYSTEM.PUT (SYSTEM.ADR (x.a), s[0]);
		SYSTEM.PUT (SYSTEM.ADR (x.a) + 4, s[1])
	ELSIF x.mode = Base.class_const THEN
		load (y);
		Zero_clear_reg (reg_RAX);
		Emit_op_reg_reg (op_BTS, reg_RAX, y.r);
		Emit_op_reg_reg (op_MOV, y.r, reg_RAX);
		x.mode := Base.mode_reg;
		x.r := y.r
	ELSIF x.mode = Base.mode_reg THEN
		load (y);
		Emit_op_reg_reg (op_BTS, x.r, y.r);
		Free_reg
	ELSE
		Scanner.Mark ('FAULT: procedure Generator.Set2')
	END
END Set2;
	
PROCEDURE Set3* (VAR x, y, z : Base.Item);
	VAR
		s : ARRAY 2 OF SET;
		imm : LONGINT;
		
BEGIN (* Set3 *)
	IF z.mode = Base.class_ref THEN Ref_to_regI (z)
	END;
	IF (y.mode = Base.class_const) & (z.mode = Base.class_const) THEN
		IF y.a > z.a THEN
			Scanner.Mark ('The first element is greater than last element')
		ELSE
			(* Delay code generation *)
			s[0] := {}; s[1] := {};
			SYSTEM.PUT (SYSTEM.ADR (s), x.a);
			IF (y.a < 32) & (z.a < 32) THEN s[0] := s[0] + {y.a .. z.a}
			ELSIF (y.a > 31) & (z.a > 31) THEN
				s[1] := s[1] + {y.a - 32 .. z.a - 32}
			ELSE
				s[0] := s[0] + {y.a .. 31}; s[1] := s[1] + {32 .. z.a - 32}
			END;
			SYSTEM.PUT (SYSTEM.ADR (x.a), s[0]);
			SYSTEM.PUT (SYSTEM.ADR (x.a) + 4, s[1])
		END
	ELSIF ~ (y.mode IN {Base.mode_reg, Base.class_const})
	OR ~ (z.mode IN {Base.mode_reg, Base.class_const} + Base.cls_Variable) THEN
		Scanner.Mark ('FAULT: procedure Generator.Set3')
	ELSIF x.mode = Base.class_const THEN
		IF y.mode = Base.mode_reg THEN
			IF y.r > reg_RCX THEN Push_reg (reg_RCX);
				Emit_op_reg_reg (op_MOV, reg_RCX, y.r);
			END;
			Emit_op_reg_imm (op_MOV, reg_RAX, -1);
			Emit_op_reg_reg (op_SHL, reg_RAX, reg_CL);
			IF z.mode = Base.class_const THEN
				imm := -2; imm := ASH (imm, z.a);
				Emit_op_reg_imm (op_MOV, y.r, imm)
			ELSE
				load_to_reg (reg_RCX, z);
				Emit_op_reg_imm (op_MOV, y.r, -2);
				Emit_op_reg_reg (op_SHL, y.r, reg_CL)
			END;
			Emit_op_reg_reg (op_XOR, y.r, reg_RAX);
			IF z.mode IN Base.modes_UseReg THEN Free_reg
			END;
			IF y.r > reg_RCX THEN Pop_reg (reg_RCX)
			END
		ELSIF y.mode = Base.class_const THEN
			IF z.mode IN Base.modes_UseReg THEN
				IF z.r = reg_RCX THEN load (z)
				ELSE Push_reg (reg_RCX); load_to_reg (reg_RCX, z)
				END;
			ELSE
				IF reg_stack > reg_RCX THEN Push_reg (reg_RCX)
				END;
				load_to_reg (reg_RCX, z);
				Inc_reg_stack
			END;
			
			Emit_op_reg_imm (op_MOV, reg_RAX, -2);
			Emit_op_reg_reg (op_SHL, reg_RAX, reg_CL);	
			imm := -1; imm := ASH (imm, y.a);
			Emit_op_reg_imm (op_MOV, reg_stack - 1, imm);
			Emit_op_reg_reg (op_XOR, reg_stack - 1, reg_RAX);
			
			IF reg_stack > reg_RCX + 1 THEN Pop_reg (reg_RCX)
			END
		END;
		x.mode := Base.mode_reg; x.r := reg_stack - 1
	ELSIF x.mode = Base.mode_reg THEN
		Push_reg (reg_RCX);
		IF y.mode = Base.mode_reg THEN
			Emit_op_reg_reg (op_MOV, reg_RCX, y.r);
			Emit_op_reg_imm (op_MOV, reg_RAX, -1);
			Emit_op_reg_reg (op_SHL, reg_RAX, reg_CL);
			IF z.mode = Base.class_const THEN
				imm := -2; imm := ASH (imm, z.a);
				Emit_op_reg_imm (op_MOV, y.r, imm)
			ELSE
				load_to_reg (reg_RCX, z);
				Emit_op_reg_imm (op_MOV, y.r, -2);
				Emit_op_reg_reg (op_SHL, y.r, reg_CL)
			END;
			Emit_op_reg_reg (op_XOR, reg_RAX, y.r);
			Free_reg
		ELSIF y.mode = Base.class_const THEN
			load_to_reg (reg_RCX, z);	
			Emit_op_reg_imm (op_MOV, reg_RAX, -2);
			Emit_op_reg_reg (op_SHL, reg_RAX, reg_CL);	
			imm := -1; imm := ASH (imm, y.a);
			Emit_op_reg_imm (op_MOV, reg_RCX, imm);
			Emit_op_reg_reg (op_XOR, reg_RAX, reg_RCX)
		END;
		Pop_reg (reg_RCX);
		Emit_op_reg_reg (op_OR, x.r, reg_RAX);
		IF z.mode IN Base.modes_UseReg THEN Free_reg
		END
	ELSE
		Scanner.Mark ('FAULT: procedure Generator.Set3 error 2')
	END
END Set3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE negated (c : INTEGER) : INTEGER;
	VAR
		r : INTEGER;
BEGIN
	IF c MOD 2 = 0 THEN r := c + 1 ELSE r := c - 1
	END;
	RETURN r
END negated;
	
PROCEDURE merged (L0, L1 : INTEGER) : INTEGER;
	VAR
		L2, L3 : INTEGER;
BEGIN
    IF L0 # 0 THEN
		L3 := L0;
		REPEAT
			L2 := L3;
			L3 := SHORT (codes [L2].operands [0].imm);
		UNTIL L3 = 0;
		codes [L2].operands [0].imm := L1;
		L1 := L0
	END;
    RETURN L1
END merged;
	
PROCEDURE Fix_link* (L : INTEGER);
	VAR
		L1 : INTEGER;
BEGIN
	IF L # 0 THEN
		REPEAT
			L1 := SHORT (codes [L].operands [0].imm);
			codes [L].operands [0].imm := pc;
			L := L1
		UNTIL L = 0;
		INCL (codes [pc].flag, flag_hasLabel)
	END
END Fix_link;

PROCEDURE FJump* (VAR L : INTEGER);
BEGIN
	Emit_op_label (op_JMP, L);
	L := pc - 1
END FJump;

PROCEDURE CFJump* (VAR x : Base.Item);
	VAR
		op : INTEGER;
BEGIN
	IF x.mode # Base.mode_cond THEN Convert_to_cond (x)
	END;
	op := negated (x.r);
	IF op # vop_NJMP THEN
		Emit_op_label (op, SHORT (x.a));
		x.a := pc - 1
	END;
	Fix_link (x.b)
END CFJump;
	
PROCEDURE CBJump* (VAR x : Base.Item; L : INTEGER);
	VAR
		op : INTEGER;
BEGIN
	IF x.mode # Base.mode_cond THEN Convert_to_cond (x)
	END;
    op := negated (x.r);
	IF op # vop_NJMP THEN Emit_op_label (op, L)
	END;
	INCL (codes [L].flag, flag_hasLabel)
END CBJump;
  
PROCEDURE BJump* (L : INTEGER);
BEGIN
	Emit_op_label (op_JMP, L);
	INCL (codes [L].flag, flag_hasLabel)
END BJump;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Op1* (op : INTEGER; VAR x : Base.Item);
	VAR
		s : SET;
		t : INTEGER;
		float : SHORTREAL;
BEGIN
	IF op = Base.sym_not THEN
		IF x.mode = Base.class_const THEN
			x.a := (x.a + 1) MOD 2
		ELSE
			IF x.mode # Base.mode_cond THEN Convert_to_cond (x)
			END;
			x.r := negated (x.r);
			t := SHORT (x.a); x.a := x.b; x.b := t
		END
	ELSIF op = Base.sym_minus THEN
		IF x.mode = Base.class_const THEN
			IF x.type.form = Base.type_integer THEN
				IF x.a = Base.MIN_INT THEN
					Scanner.Mark ('Integer overflow detected')
				ELSE x.a := -x.a
				END
			ELSIF x.type.form = Base.type_set THEN
				s := {};
				SYSTEM.PUT (SYSTEM.ADR (s), x.a);
				x.a := ORD (-s)
			ELSIF x.type = Base.real_type THEN
				float := SHORT (0.0); t := SHORT (x.a);
				SYSTEM.PUT (SYSTEM.ADR (float), t);
				SYSTEM.PUT (SYSTEM.ADR (t), -float);
				x.a := t
			END
		ELSE
			load (x);
			IF x.type.form = Base.type_integer THEN
				Emit_op_reg (op_NEG, x.r);
				IF Base.integer_overflow_check IN Base.compiler_flag THEN
					Emit_op_sym (op_JO, sym_integer_overflow_trap)
				END
			ELSIF x.type.form = Base.type_set THEN
				Emit_op_reg (op_NOT, x.r)
			ELSIF x.type = Base.real_type THEN
				Emit_op_reg_imm (op_MOV, reg_EAX, -2147483648);
				Emit_op_xreg_reg (op_MOVD, reg_XMM0, reg_EAX);
				Emit_op_xreg_xreg (op_XORPS, x.r, reg_XMM0)
			END
		END
	ELSIF op = Base.sym_and THEN
		IF x.mode # Base.mode_cond THEN Convert_to_cond (x)
		END;
		op := negated (x.r);
		IF op # vop_NJMP THEN
			Emit_op_label (op, SHORT (x.a));
			x.a := pc - 1
		END;
		Fix_link (x.b); x.b := 0
	ELSIF op = Base.sym_or THEN
		IF x.mode # Base.mode_cond THEN Convert_to_cond (x)
		END;
		IF x.r # vop_NJMP THEN
			Emit_op_label (x.r, x.b);
			x.b := pc - 1
		END;
		Fix_link (SHORT (x.a)); x.a := 0
	END
END Op1;
	
PROCEDURE Commutative_op (op : INTEGER; VAR x, y : Base.Item);
BEGIN
	IF x.mode = Base.class_const THEN
		IF (x.a < MIN_INT32) OR (x.a > MAX_INT32)
		OR (y.mode = Base.class_var) THEN
			load (x)
		END
	END;
	IF x.mode = Base.class_const THEN
		load (y);
		Emit_op_reg_imm (op, y.r, x.a);
		x.mode := Base.mode_reg;
		x.r := y.r
	ELSIF x.mode = Base.mode_reg THEN
		IF y.mode = Base.class_ref THEN Ref_to_regI (y)
		END;
		CASE y.mode OF
			Base.class_const: Emit_op_reg_const (op, x.r, y.a) |
			Base.class_var, Base.mode_regI: Emit_op_reg_mem (op, x.r, y) |
			Base.mode_reg: Emit_op_reg_reg (op, x.r, y.r)
		END;
		IF y.mode IN Base.modes_UseReg THEN Free_reg
		END
	ELSE
		Scanner.Mark ('COMPILER FAULT: Commutative_op procedure')
	END;
	IF (x.type.form = Base.type_integer)
	& (Base.integer_overflow_check IN Base.compiler_flag) THEN
		Emit_op_sym (op_JO, sym_integer_overflow_trap)
	END
END Commutative_op;
	
PROCEDURE Subtract (VAR x, y : Base.Item);
BEGIN
	IF x.mode = Base.class_const THEN
		CASE y.mode OF
			Base.class_var:
				load (x);
				Emit_op_reg_mem (op_SUB, x.r, y) |
			Base.class_ref:
				load (x); Ref_to_regI (y);
				Emit_op_reg_mem (op_SUB, x.r, y);
				Free_reg |
			Base.mode_regI:
				Inc_reg_stack;
				Emit_op_reg_mem (op_MOV, reg_stack - 1, y);
				Emit_op_reg_imm (op_MOV, y.r, x.a);
				Emit_op_reg_reg (op_SUB, y.r, reg_stack - 1);
				Free_reg; x.mode := Base.mode_reg; x.r := y.r |
			Base.mode_reg:
				Inc_reg_stack;
				Emit_op_reg_reg (op_MOV, reg_stack - 1, y.r);
				Emit_op_reg_imm (op_MOV, y.r, x.a);
				Emit_op_reg_reg (op_SUB, y.r, reg_stack - 1);
				Free_reg; x.mode := Base.mode_reg; x.r := y.r
		END
	ELSIF x.mode = Base.mode_reg THEN
		CASE y.mode OF
			Base.class_const: Emit_op_reg_const (op_SUB, x.r, y.a) |
			Base.class_var: Emit_op_reg_mem (op_SUB, x.r, y) |
			Base.class_ref:
				Ref_to_regI (y); Emit_op_reg_mem (op_SUB, x.r, y); Free_reg |
			Base.mode_regI: Emit_op_reg_mem (op_SUB, x.r, y); Free_reg |
			Base.mode_reg: Emit_op_reg_reg (op_SUB, x.r, y.r); Free_reg
		END
	ELSE
		Scanner.Mark ('COMPILER FAULT: Subtract procedure')
	END;
	IF Base.integer_overflow_check IN Base.compiler_flag THEN
		Emit_op_sym (op_JO, sym_integer_overflow_trap)
	END
END Subtract;
	
PROCEDURE Multiply_by_const (VAR x : Base.Item; const : LONGINT);
BEGIN
	IF const = 0 THEN
		Zero_clear_reg (x.r)
	ELSIF const = 1 THEN
		(* Do nothing *)
	ELSIF const = 2 THEN
		Emit_op_reg_imm (op_SHL, x.r, 1);
		IF Base.integer_overflow_check IN Base.compiler_flag THEN
			Emit_op_sym (op_JO, sym_integer_overflow_trap)
		END
	ELSE
		Emit_op_reg_const (op_IMUL, x.r, const);
		IF Base.integer_overflow_check IN Base.compiler_flag THEN
			Emit_op_sym (op_JO, sym_integer_overflow_trap)
		END
	END
END Multiply_by_const;
	
PROCEDURE Multiply (VAR x, y : Base.Item);
BEGIN
	IF x.mode = Base.class_const THEN
		load (y);
		Multiply_by_const (y, x.a);
		x.mode := Base.mode_reg;
		x.r := y.r
	ELSIF x.mode = Base.mode_reg THEN
		IF y.mode = Base.class_const THEN
			Multiply_by_const (x, y.a)
		ELSE
			CASE y.mode OF
				Base.class_var: Emit_op_reg_mem (op_IMUL, x.r, y) |
				Base.class_ref:
					Ref_to_regI (y);
					Emit_op_reg_mem (op_IMUL, x.r, y);
					Free_reg |
				Base.mode_regI: Emit_op_reg_mem (op_IMUL, x.r, y); Free_reg |
				Base.mode_reg: Emit_op_reg_reg (op_IMUL, x.r, y.r); Free_reg
			END;
			IF Base.integer_overflow_check IN Base.compiler_flag THEN
				Emit_op_sym (op_JO, sym_integer_overflow_trap)
			END
		END
	ELSE
		Scanner.Mark ('COMPILER FAULT: Multiply procedure')
	END
END Multiply;
	
PROCEDURE Divide_by_const
(VAR x : Base.Item; const : LONGINT; is_modulo : BOOLEAN; VAR result : BOOLEAN);
	VAR
		e : INTEGER;
BEGIN
	IF const <= 0 THEN
		Scanner.Mark ('Division by non-positive number')
	ELSIF const = 1 THEN
		IF is_modulo THEN Zero_clear_reg (x.r)
		END
	ELSE
		e := Base.Integer_binary_logarithm (const);
		IF e > 0 THEN
			IF is_modulo THEN Emit_op_reg_imm (op_AND, x.r, const - 1)
			ELSE Emit_op_reg_imm (op_SAR, x.r, e)
			END
		ELSE result := FALSE
		END
	END
END Divide_by_const;

PROCEDURE Divide2 (VAR x : Base.Item; y : LONGINT; is_modulo : BOOLEAN);
	VAR
		i, divisor : INTEGER;
BEGIN
	IF reg_stack > reg_RDX + 1 THEN Push_reg (reg_RDX)
	END;
	
	Emit_op_reg_reg (op_MOV, reg_RAX, x.r);
	Emit_op_bare (op_CQO);
	
	IF x.r = reg_RDX THEN divisor := reg_RDX + 1
	ELSE divisor := x.r
	END;
	Emit_op_reg_imm (op_MOV, divisor, y);
	
	Emit_op_reg_reg (op_TEST, reg_RAX, reg_RAX);
	Emit_op_label (op_JL, pc + 4);
	
	Emit_op_reg (op_IDIV, divisor);
	IF is_modulo THEN
		Emit_op_reg_reg (op_MOV, x.r, reg_RDX);
		IF x.r = reg_RDX THEN codes [pc - 1].flag := {flag_skipped}
		END
	ELSE
		Emit_op_reg_reg (op_MOV, x.r, reg_RAX)
	END;
	i := pc;
	Emit_op_label (op_JMP, 0); (* fix up later *)
	
	INCL (codes [pc].flag, flag_hasLabel);
	Emit_op_reg (op_IDIV, divisor);
	IF is_modulo THEN
		IF divisor = x.r THEN
			Emit_op_reg_reg (op_ADD, divisor, reg_RDX)
		ELSE
			Emit_op_reg_reg (op_ADD, reg_RDX, divisor);
			Emit_op_reg_reg (op_MOV, x.r, reg_RDX)
		END
	ELSE
		Emit_op_reg_reg (op_TEST, reg_RDX, reg_RDX);
		Emit_op_label (op_JE, pc + 2);
		Emit_op_reg (op_DEC, reg_RAX);
		INCL (codes [pc].flag, flag_hasLabel);
		Emit_op_reg_reg (op_MOV, x.r, reg_RAX)
	END;
		
	INCL (codes [pc].flag, flag_hasLabel);
	codes [i].operands [0].imm := pc; (* fix up *)
	
	IF reg_stack > reg_RDX + 1 THEN Pop_reg (reg_RDX)
	END
END Divide2;

PROCEDURE Divide3 (x : LONGINT; VAR y : Base.Item; is_modulo : BOOLEAN);
	VAR
		divisor : Base.Item;
		saved_RDX, divisor_saved_to_stack : BOOLEAN;
BEGIN
	IF y.mode = Base.class_ref THEN Ref_to_regI (y)
	END;
	
	IF Base.integer_overflow_check IN Base.compiler_flag THEN
		IF y.mode = Base.mode_reg THEN Emit_op_reg_reg (op_TEST, y.r, y.r)
		ELSE Emit_op_mem_imm (op_CMP, y, 0)
		END;
		Emit_op_sym (op_JLE, sym_invalid_divisor_trap)
	END;
	
	IF x = 0 THEN
		IF y.mode IN Base.modes_UseReg THEN Zero_clear_reg (y.r)
		ELSE Inc_reg_stack; Zero_clear_reg (reg_stack - 1)
		END
	ELSE
		saved_RDX := FALSE; divisor_saved_to_stack := FALSE;
		IF y.mode IN Base.modes_UseReg THEN
			IF y.r = reg_RDX THEN
				IF y.mode = Base.mode_reg THEN Emit_op_reg (op_PUSH, reg_RDX)
				ELSE Emit_op_mem (op_PUSH, y)
				END;
				Make_mem_stack_item (divisor, Base.int_type, 0);
				divisor_saved_to_stack := TRUE
			ELSIF reg_stack > reg_RDX + 1 THEN
				Push_reg (reg_RDX);
				saved_RDX := TRUE
			END
		ELSIF reg_stack > reg_RDX THEN
			Push_reg (reg_RDX);
			saved_RDX := TRUE
		END;
		
		Emit_op_reg_imm (op_MOV, reg_RAX, x);
		Emit_op_bare (op_CQO);
		
		divisor := y;
		IF y.mode = Base.class_var THEN
			Emit_op_mem (op_IDIV, y);
			Inc_reg_stack
		ELSIF divisor_saved_to_stack THEN
			Make_mem_stack_item (divisor, Base.int_type, 0);
			Emit_op_mem (op_IDIV, divisor)
		ELSIF y.mode = Base.mode_regI THEN Emit_op_mem (op_IDIV, y)
		ELSE Emit_op_reg (op_IDIV, y.r)
		END;
		
		IF x > 0 THEN
			IF is_modulo & (reg_stack - 1 # reg_RDX) THEN
				Emit_op_reg_reg (op_MOV, reg_stack - 1, reg_RDX)
			ELSE
				Emit_op_reg_reg (op_MOV, reg_stack - 1, reg_RAX)
			END
		ELSE
			IF is_modulo THEN
				IF divisor.mode = Base.mode_reg THEN
					IF divisor.r = reg_stack - 1 THEN
						Emit_op_reg_reg (op_ADD, divisor.r, reg_RDX)
					ELSE
						Emit_op_reg_reg (op_ADD, reg_RDX, divisor.r);
						Emit_op_reg_reg (op_MOV, reg_stack - 1, reg_RDX)
					END
				ELSE
					Emit_op_reg_mem (op_ADD, reg_RDX, divisor);
					Emit_op_reg_reg (op_MOV, reg_stack - 1, reg_RDX)
				END
			ELSE
				Emit_op_reg_reg (op_TEST, reg_RDX, reg_RDX);
				Emit_op_label (op_JE, pc + 2);
				Emit_op_reg (op_DEC, reg_RAX);
				INCL (codes [pc].flag, flag_hasLabel);
				Emit_op_reg_reg (op_MOV, reg_stack - 1, reg_RAX)
			END
		END;
		
		IF saved_RDX THEN Pop_reg (reg_RDX)
		ELSIF divisor_saved_to_stack THEN Emit_op_reg_imm (op_ADD, reg_RSP, 8)
		END
	END
END Divide3;

PROCEDURE Divide4 (VAR x, y : Base.Item; is_modulo : BOOLEAN);
	VAR
		i : INTEGER;
		divisor : Base.Item;
		saved_RDX, divisor_saved_to_stack : BOOLEAN;
BEGIN
	IF y.mode = Base.class_ref THEN Ref_to_regI (y)
	END;
	
	IF Base.integer_overflow_check IN Base.compiler_flag THEN
		IF y.mode = Base.mode_reg THEN Emit_op_reg_reg (op_TEST, y.r, y.r)
		ELSE Emit_op_mem_imm (op_CMP, y, 0)
		END;
		Emit_op_sym (op_JLE, sym_invalid_divisor_trap)
	END;

	saved_RDX := FALSE; divisor_saved_to_stack := FALSE;
	IF y.mode IN Base.modes_UseReg THEN
		IF y.r = reg_RDX THEN
			IF y.mode = Base.mode_reg THEN Emit_op_reg (op_PUSH, reg_RDX)
			ELSE Emit_op_mem (op_PUSH, y)
			END;
			Make_mem_stack_item (divisor, Base.int_type, 0);
			divisor_saved_to_stack := TRUE
		ELSIF reg_stack > reg_RDX + 2 THEN
			Push_reg (reg_RDX);
			saved_RDX := TRUE
		END
	ELSIF reg_stack > reg_RDX + 1 THEN
		Push_reg (reg_RDX);
		saved_RDX := TRUE
	END;
	
	Emit_op_reg_reg (op_MOV, reg_RAX, x.r);
	Emit_op_bare (op_CQO);
	
	IF divisor_saved_to_stack THEN
		Make_mem_stack_item (divisor, Base.int_type, 0)
	ELSE divisor := y
	END;
	
	Emit_op_reg_reg (op_TEST, reg_RAX, reg_RAX);
	Emit_op_label (op_JL, pc + 4);
	
	IF divisor.mode IN Base.cls_Variable THEN Emit_op_mem (op_IDIV, divisor)
	ELSE Emit_op_reg (op_IDIV, divisor.r)
	END;
	
	IF is_modulo THEN
		Emit_op_reg_reg (op_MOV, x.r, reg_RDX);
		IF x.r = reg_RDX THEN codes [pc - 1].flag := {flag_skipped}
		END
	ELSE
		Emit_op_reg_reg (op_MOV, x.r, reg_RAX)
	END;
	i := pc;
	Emit_op_label (op_JMP, 0); (* fix up later *)
	
	INCL (codes [pc].flag, flag_hasLabel);
	IF divisor.mode IN Base.cls_Variable THEN Emit_op_mem (op_IDIV, divisor)
	ELSE Emit_op_reg (op_IDIV, divisor.r)
	END;
	
	IF is_modulo THEN
		IF divisor.mode = Base.mode_reg THEN
			Emit_op_reg_reg (op_ADD, reg_RDX, divisor.r)
		ELSE Emit_op_reg_mem (op_ADD, reg_RDX, divisor)
		END;	
		IF x.r # reg_RDX THEN Emit_op_reg_reg (op_MOV, x.r, reg_RDX)
		END
	ELSE
		Emit_op_reg_reg (op_TEST, reg_RDX, reg_RDX);
		Emit_op_label (op_JE, pc + 2);
		Emit_op_reg (op_DEC, reg_RAX);
		INCL (codes [pc].flag, flag_hasLabel);
		Emit_op_reg_reg (op_MOV, x.r, reg_RAX)
	END;
	
	IF y.mode IN Base.modes_UseReg THEN Free_reg
	END;
	
	IF saved_RDX THEN Pop_reg (reg_RDX)
	ELSIF divisor_saved_to_stack THEN Emit_op_reg_imm (op_ADD, reg_RSP, 8)
	END
END Divide4;

PROCEDURE Divide (VAR x, y : Base.Item; is_modulo : BOOLEAN);
	VAR
		div_const : BOOLEAN;
BEGIN
	IF y.mode = Base.class_const THEN
		div_const := TRUE;
		Divide_by_const (x, y.a, is_modulo, div_const)
	ELSE
		div_const := FALSE
	END;
	
	IF ~ div_const THEN
		IF y.mode = Base.class_const THEN
			Divide2 (x, y.a, is_modulo)
		ELSIF x.mode = Base.class_const THEN
			Divide3 (x.a, y, is_modulo);
			x.r := reg_stack - 1; x.mode := Base.mode_reg
		ELSE
			Divide4 (x, y, is_modulo)
		END
	END
END Divide;
	
PROCEDURE Difference (VAR x, y : Base.Item);
BEGIN
	IF x.mode = Base.class_const THEN
		load (y);
		Emit_op_reg (op_NOT, y.r);
		Emit_op_reg_imm (op_AND, y.r, x.a);
		x.mode := Base.mode_reg;
		x.r := y.r
	ELSIF x.mode = Base.mode_reg THEN
		load (y);
		Emit_op_reg (op_NOT, y.r);
		Emit_op_reg_reg (op_AND, x.r, y.r);
		Free_reg
	END
END Difference;
	
PROCEDURE Op2_real (op : INTEGER; VAR x, y : Base.Item);
BEGIN
	load (x);
	
	IF y.mode = Base.class_const THEN load (y)
	ELSIF y.mode = Base.class_ref THEN Ref_to_regI (y)
	END;
	
	IF y.mode IN Base.cls_Variable THEN
		Emit_op_xreg_mem (op, x.r, y);
		IF y.mode = Base.mode_regI THEN Free_reg
		END
	ELSIF y.mode = Base.mode_xreg THEN
		Emit_op_xreg_xreg (op, x.r, y.r);
		Free_xreg
	ELSE
		Scanner.Mark ('COMPILER FAULT: Op2_real procedure')
	END
END Op2_real;

PROCEDURE Op2_int_const (op : INTEGER; x, y : LONGINT) : LONGINT;
BEGIN
	CASE op OF
		Base.sym_plus:
			IF Safe_to_add (x, y) THEN INC (x, y)
			ELSE Scanner.Mark ('Integer overflow detected')
			END |
		Base.sym_minus:
			IF Safe_to_subtract (x, y) THEN DEC (x, y)
			ELSE Scanner.Mark ('Integer overflow detected')
			END |
		Base.sym_times:
			IF Safe_to_multiply (x, y) THEN x := x * y
			ELSE Scanner.Mark ('Integer overflow detected')
			END |
		Base.sym_div:
			IF y > 0 THEN x := x DIV y
			ELSE Scanner.Mark ('Division by non-positive number')
			END |
		Base.sym_mod:
			IF y > 0 THEN x := x MOD y
			ELSE Scanner.Mark ('Division by non-positive number')
			END
	END;
	RETURN x
END Op2_int_const;

PROCEDURE Op2_set_const (op : INTEGER; x, y : LONGINT) : LONGINT;
	VAR
		s1, s2 : ARRAY 2 OF SET;
BEGIN
	s1[0] := {}; s1[1] := {};
	s2[0] := {}; s2[1] := {};
	SYSTEM.PUT (SYSTEM.ADR (s1), x);
	SYSTEM.PUT (SYSTEM.ADR (s2), y);
	
	CASE op OF
		Base.sym_plus:
			s1[0] := s1[0] + s2[0];
			s1[1] := s1[1] + s2[1] |
		Base.sym_minus:
			s1[0] := s1[0] - s2[0];
			s1[1] := s1[1] - s2[1] |
		Base.sym_times:
			s1[0] := s1[0] * s2[0];
			s1[1] := s1[1] * s2[1] |
		Base.sym_slash:
			s1[0] := s1[0] / s2[0];
			s1[1] := s1[1] / s2[1] |
	END;
	
	SYSTEM.PUT (SYSTEM.ADR (x), s1[0]);
	SYSTEM.PUT (SYSTEM.ADR (x) + 4, s1[1]);
	RETURN x
END Op2_set_const;
	
PROCEDURE Op2* (op : INTEGER; VAR x, y : Base.Item);
	VAR
		real1, real2 : SHORTREAL;
BEGIN
	IF (x.mode = Base.class_const) & (y.mode = Base.class_const) THEN
		IF x.type.form = Base.type_integer THEN
			x.a := Op2_int_const (op, x.a, y.a)
		ELSIF x.type = Base.set_type THEN
			x.a := Op2_set_const (op, x.a, y.a)
		ELSIF x.type = Base.real_type THEN
			real1 := SHORT(0.0);
			real2 := SHORT(0.0);
			SYSTEM.PUT (SYSTEM.ADR (real1), x.a);
			SYSTEM.PUT (SYSTEM.ADR (real2), y.a);
			x.a := 0;
			CASE op OF
				Base.sym_plus: SYSTEM.PUT (SYSTEM.ADR (x.a), real1 + real2) |
				Base.sym_minus: SYSTEM.PUT (SYSTEM.ADR (x.a), real1 - real2) |
				Base.sym_times: SYSTEM.PUT (SYSTEM.ADR (x.a), real1 * real2) |
				Base.sym_slash: SYSTEM.PUT (SYSTEM.ADR (x.a), real1 / real2) |
			END
		END
	ELSE
		IF x.type.form = Base.type_integer THEN
			CASE op OF
				Base.sym_plus: Commutative_op (op_ADD, x, y) |
				Base.sym_minus: Subtract (x, y) |
				Base.sym_times: Multiply (x, y) |
				Base.sym_div: Divide (x, y, FALSE) |
				Base.sym_mod: Divide (x, y, TRUE)
			END
		ELSIF x.type = Base.set_type THEN
			CASE op OF
				Base.sym_plus: Commutative_op (op_OR, x, y) |
				Base.sym_minus: Difference (x, y) |
				Base.sym_times: Commutative_op (op_AND, x, y) |
				Base.sym_slash: Commutative_op (op_XOR, x, y) |
			END
		ELSIF x.type = Base.real_type THEN
			CASE op OF
				Base.sym_plus: Op2_real (op_ADDSS, x, y) |
				Base.sym_minus: Op2_real (op_SUBSS, x, y) |
				Base.sym_times: Op2_real (op_MULSS, x, y) |
				Base.sym_slash: Op2_real (op_DIVSS, x, y) |
			END
		ELSIF x.type = Base.bool_type THEN
			IF y.mode # Base.mode_cond THEN Convert_to_cond (y)
			END;
			IF op = Base.sym_and THEN
				x.a := merged (SHORT (y.a), SHORT (x.a));
				x.b := y.b
			ELSIF op = Base.sym_or THEN
				x.b := merged (SHORT (y.b), SHORT (x.b));
				x.a := y.a
			END;
			x.r := y.r
		END
	END
END Op2;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Selector operation *)
	
PROCEDURE Field* (VAR x : Base.Item; field : Base.Object);
BEGIN
	IF x.mode = Base.class_ref THEN INC (x.c, field.val)
	ELSE INC (x.a, field.val)
	END;
	x.type := field.type;
	x.flag := x.flag - {Base.flag_varParam, Base.flag_param}
END Field;
	
PROCEDURE Deref* (VAR x : Base.Item);
BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x)
	END;
	IF ~ (x.mode IN {Base.mode_regI, Base.mode_reg}) THEN
		Inc_reg_stack;
		x.r := reg_stack - 1
	END;
	IF x.mode IN Base.cls_Variable THEN
		Emit_op_reg_mem (op_MOV, x.r, x)
	END;
	x.mode := Base.mode_regI; x.a := 0;
	
	x.type := x.type.base;
	EXCL (x.flag, Base.flag_readOnly)
END Deref;
	
PROCEDURE Open_array_index (VAR x, y : Base.Item);
	VAR
		flag : BOOLEAN;
		size, scale, e : INTEGER;

	PROCEDURE Calculate_offset
	(VAR y : Base.Item; typ : Base.Type; len_offset : INTEGER);
		VAR
			size, e : INTEGER;
			len : Base.Item;
	BEGIN
		Make_len_item (len, len_offset);
		Emit_op_reg_mem (op_IMUL, y.r, len);
		
		size := typ.base.size;
		IF size > 0 THEN
			e := Base.Integer_binary_logarithm (size);
			IF e = 0 THEN
				(* Do nothing *)
			ELSIF e > 0 THEN
				Emit_op_reg_imm (op_SHL, y.r, e)
			ELSE
				Emit_op_reg_imm (op_IMUL, y.r, size)
			END
		ELSIF typ.base.form = Base.type_array THEN
			Calculate_offset (y, typ.base, len_offset + 8)
		ELSE
			Scanner.Mark ('COMPILER FAULT: procedure Open_array_index, Calculate_offset')
		END
	END Calculate_offset;
		
	PROCEDURE Check_array_index (VAR x, y : Base.Item);
		VAR
			len : Base.Item;
	BEGIN
		IF Base.array_bound_check IN Base.compiler_flag THEN
			Make_len_item (len, x.b);
			IF y.mode = Base.class_const THEN
				Emit_op_mem_imm (op_CMP, len, y.a);
				Emit_op_sym (op_JBE, sym_array_index_trap)
			ELSE
				Emit_op_reg_mem (op_CMP, y.r, len);
				Emit_op_sym (op_JAE, sym_array_index_trap)
			END
		END
	END Check_array_index;

BEGIN (* Open_array_index *)
	flag := FALSE;
	IF y.mode # Base.class_const THEN load (y); flag := TRUE
	ELSIF y.a < 0 THEN Scanner.Mark ('Array index out of bound')
	ELSIF y.a > 0 THEN flag := TRUE
	END;
	
	IF flag THEN
		IF x.mode = Base.class_ref THEN Ref_to_regI (x)
		END;
		Check_array_index (x, y);
		size := x.type.base.size;
		IF size > 0 THEN
			IF y.mode = Base.class_const THEN
				INC (x.a, y.a * size)
			ELSE
				e := Base.Integer_binary_logarithm (size);
				IF e < 0 THEN
					Emit_op_reg_imm (op_IMUL, y.r, size); scale := 0
				ELSIF e > 3 THEN
					Emit_op_reg_imm (op_SHL, y.r, e); scale := 0
				ELSE
					scale := size
				END;
				Emit_op_reg_memX (op_LEA, reg_stack - 2, x.r, y.r, scale, x.a);
				Free_reg; x.r := reg_stack - 1; x.a := 0
			END
		ELSIF x.type.base.form = Base.type_array THEN
			load (y);
			Calculate_offset (y, x.type.base, x.b + 8);
			Emit_op_reg_memX (op_LEA, reg_stack - 2, x.r, y.r, 0, x.a);
			Free_reg; x.r := reg_stack - 1; x.a := 0
		ELSE
			Scanner.Mark ('COMPILER FAULT: procedure Open_array_index')
		END
	END;
	INC (x.b, 8)
END Open_array_index;
	
PROCEDURE Index* (VAR x, y : Base.Item);
	VAR
		scale, e, size : INTEGER;
BEGIN
	IF x.type.len < 0 THEN
		IF x.b = 0 THEN x.b := SHORT (x.a) + 8
		END;
		Open_array_index (x, y)
	ELSE
		IF x.mode = Base.class_ref THEN Ref_to_regI (x)
		END;
		IF y.mode = Base.class_const THEN
			IF (y.a < 0) OR (y.a >= x.type.len) THEN
				Scanner.Mark ('Array index out of bound')
			ELSE
				INC (x.a, y.a * x.type.base.size)
			END
		ELSE
			load (y);
			IF Base.array_bound_check IN Base.compiler_flag THEN
				Emit_op_reg_imm (op_CMP, y.r, x.type.len);
				Emit_op_sym (op_JAE, sym_array_index_trap)
			END;
				
			size := x.type.base.size;
			e := Base.Integer_binary_logarithm (size);
			IF e < 0 THEN
				Emit_op_reg_imm (op_IMUL, y.r, size); scale := 0
			ELSIF e > 3 THEN
				Emit_op_reg_imm (op_SHL, y.r, e); scale := 0
			ELSE
				scale := size
			END;
				
			IF x.mode = Base.mode_regI THEN
				Emit_op_reg_memX (op_LEA, reg_stack - 2, x.r, y.r, scale, x.a);
				Free_reg
			ELSIF x.mode = Base.class_var THEN
				Emit_op_reg_memX (op_LEA, y.r, reg_stackBase, y.r, scale, x.a);
				x.mode := Base.mode_regI
			ELSE
				Scanner.Mark ('COMPILER_FAULT: procedure Index')
			END;
			x.r := reg_stack - 1; x.a := 0
		END
	END;
	x.type := x.type.base
END Index;
	
PROCEDURE Type_guard* (VAR x : Base.Item; typ : Base.Type);
	VAR
		tag, td : Base.Item;
BEGIN
	IF x.type # typ THEN
		IF x.type.form = Base.type_pointer THEN
			load (x);
			
			tag.flag := {};
			tag.mode := Base.mode_regI;
			tag.type := Base.nil_type;
			tag.r := x.r;
			tag.a := -8;
			
			Inc_reg_stack;
			Emit_op_reg_mem (op_MOV, reg_stack - 1, tag);
			tag.r := reg_stack - 1;
			tag.a := 8 + typ.base.len * 8;
			
			Make_type_desc_item (td, typ.base)
		ELSE
			(* x is record variable parameter *)
			tag.flag := {Base.flag_param};
			tag.mode := Base.class_ref;
			tag.type := Base.nil_type;
			tag.lev := x.lev;
			tag.a := x.a + 8;
			
			Ref_to_regI (tag);
			tag.a := 8 + typ.len * 8;
			
			Make_type_desc_item (td, typ)
		END;
		Emit_op_reg_mem (op_LEA, reg_RAX, td);
		Emit_op_reg_mem (op_CMP, reg_RAX, tag);
		Emit_op_sym (op_JNE, sym_type_check_trap);
		Free_reg; x.type := typ
	END
END Type_guard;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Scalar_comparison (op : INTEGER; VAR x, y : Base.Item);
	VAR
		r1, r2 : INTEGER;
BEGIN
	IF x.mode = Base.class_proc THEN load (x)
	END;
	
	IF y.mode = Base.class_ref THEN Ref_to_regI (y)
	ELSIF y.mode IN {Base.class_proc, Base.mode_cond} THEN load (y)
	END;
	
	IF x.mode = Base.mode_reg THEN
		IF y.mode = Base.class_const THEN
			Emit_op_reg_const (op_CMP, x.r, y.a)
		ELSIF y.mode IN Base.cls_Variable THEN
			r1 := Small_reg (x.r, x.type.size);
			Emit_op_reg_mem (op_CMP, r1, y)
		ELSIF y.mode = Base.mode_reg THEN
			Emit_op_reg_reg (op_CMP, x.r, y.r)
		END;
		Free_reg
	ELSIF x.mode = Base.class_const THEN
		IF y.mode = Base.mode_reg THEN
			Emit_op_reg_const (op_CMP, y.r, x.a)
		ELSIF y.mode IN Base.cls_Variable THEN
			Emit_op_mem_const (op_CMP, y, x.a)
		END;
		IF op > Base.sym_not_equal THEN
			IF op = Base.sym_less THEN op := Base.sym_greater
			ELSIF op = Base.sym_less_equal THEN op := Base.sym_greater_equal
			ELSIF op = Base.sym_greater THEN op := Base.sym_less
			ELSIF op = Base.sym_greater_equal THEN op := Base.sym_less_equal
			END
		END
	END;
	IF y.mode IN Base.modes_UseReg THEN Free_reg
	END
END Scalar_comparison;
	
PROCEDURE String_comparison (op : INTEGER; VAR x, y : Base.Item);
BEGIN
	(* Implement later *)
END String_comparison;
	
PROCEDURE Comparison* (op : INTEGER; VAR x, y : Base.Item);
BEGIN
	IF (x.type = Base.char_type) & (y.type.form = Base.type_string) THEN
		y.mode := Base.class_const;
		y.type := Base.char_type;
		y.a := ORD (strings [y.a DIV Base.char_type.size])
	ELSIF (y.type = Base.char_type) & (x.type.form = Base.type_string) THEN
		x.mode := Base.class_const;
		x.type := Base.char_type;
		x.a := ORD (strings [x.a DIV Base.char_type.size])
	END;
	
	IF (x.mode = Base.class_const) & (y.mode = Base.class_const) THEN
		IF op = Base.sym_equal THEN
			IF x.a = y.a THEN x.a := 1 ELSE x.a := 0
			END
		ELSIF op = Base.sym_not_equal THEN
			IF x.a # y.a THEN x.a := 1 ELSE x.a := 0
			END
		ELSIF op = Base.sym_greater THEN
			IF x.a > y.a THEN x.a := 1 ELSE x.a := 0
			END
		ELSIF op = Base.sym_greater_equal THEN
			IF x.a >= y.a THEN x.a := 1 ELSE x.a := 0
			END
		ELSIF op = Base.sym_less THEN
			IF x.a < y.a THEN x.a := 1 ELSE x.a := 0
			END
		ELSIF op = Base.sym_less_equal THEN
			IF x.a <= y.a THEN x.a := 1 ELSE x.a := 0
			END
		END;
	ELSIF (x.mode = Base.class_proc) OR (x.type.form IN Base.types_Scalar) THEN
		Scalar_comparison (op, x, y);
		Set_cond (x, op - Base.sym_equal + op_JE)
	ELSE
		String_comparison (op, x, y);
		Set_cond (x, op - Base.sym_equal + op_JE)
	END;
	x.type := Base.bool_type
END Comparison;
	
PROCEDURE Membership* (VAR x, y : Base.Item);
	VAR
		s : ARRAY 2 OF SET;
BEGIN
	IF (x.mode = Base.class_const) & (y.mode = Base.class_const) THEN
		s[0] := {}; s[1] := {};
		SYSTEM.PUT (SYSTEM.ADR (s), y.a);
		IF (x.a >= 0) & (x.a < 32) & (x.a IN s[0]) 
		OR (x.a >= 32) & (x.a < 64) & (x.a IN s[1]) THEN
			x.a := 1
		ELSE
			x.a := 0
		END
	ELSE
		IF y.mode = Base.class_const THEN load (y)
		ELSIF y.mode = Base.class_ref THEN Ref_to_regI (y)
		END;
		
		IF x.mode = Base.mode_reg THEN
			IF y.mode = Base.mode_reg THEN Emit_op_reg_reg (op_BT, y.r, x.r)
			ELSE Emit_op_mem_reg (op_BT, y, x.r)
			END;
			Free_reg
		ELSIF x.mode = Base.class_const THEN
			IF y.mode = Base.mode_reg THEN Emit_op_reg_const (op_BT, y.r, x.a)
			ELSE Emit_op_mem_const (op_BT, y, x.a)
			END
		END;
		
		IF y.mode IN Base.modes_UseReg THEN Free_reg
		END;
		Set_cond (x, op_JC)
	END;
	x.type := Base.bool_type
END Membership;
	
PROCEDURE Inclusion* (op : INTEGER; VAR x, y : Base.Item);
	VAR
		s1, s2 : ARRAY 2 OF SET;
BEGIN
	IF (x.mode = Base.class_const) & (y.mode = Base.class_const) THEN
		s1[0] := {}; s1[1] := {};
		SYSTEM.PUT (SYSTEM.ADR (s1), x.a);
		s2[0] := {}; s2[1] := {};
		SYSTEM.PUT (SYSTEM.ADR (s2), y.a);
		IF op = Base.sym_less_equal THEN
			IF (s1[0] - s2[0] = {}) & (s1[1] - s2[1] = {}) THEN x.a := 1
			ELSE x.a := 0
			END
		ELSE
			IF (s2[0] - s1[0] = {}) & (s2[1] - s1[1] = {}) THEN x.a := 1
			ELSE x.a := 0
			END
		END
	ELSE
		IF op = Base.sym_less_equal THEN
			load (y); Emit_op_reg (op_NOT, y.r);
			IF x.mode = Base.class_const THEN
				Emit_op_reg_const (op_AND, y.r, x.a)
			ELSE
				Emit_op_reg_reg (op_AND, y.r, x.r); Free_reg
			END;
			Free_reg
		ELSE
			load (x); Emit_op_reg (op_NOT, x.r);
			CASE y.mode OF
				Base.mode_reg: Emit_op_reg_reg (op_AND, x.r, y.r); Free_reg |
				Base.class_const: Emit_op_reg_const (op_AND, x.r, y.a) |
				Base.class_var: Emit_op_reg_mem (op_AND, x.r, y) |
				Base.class_ref:
					Ref_to_regI (y);
					Emit_op_reg_mem (op_AND, x.r, y);
					Free_reg |
				Base.mode_regI: Emit_op_reg_mem (op_AND, x.r, y); Free_reg
			END
		END;
		Set_cond (x, op_JE)
	END;
	x.type := Base.bool_type
END Inclusion;
	
PROCEDURE Type_test* (VAR x : Base.Item; typ : Base.Type);
	VAR
		td : Base.Item;
BEGIN
	IF x.type # typ THEN
		IF x.type.form = Base.type_pointer THEN
			load (x);
			x.mode := Base.mode_regI;
			x.a := -8;
			load (x);
			x.mode := Base.mode_regI;
			x.a := 8 + typ.base.len * 8;
			Make_type_desc_item (td, typ.base)
		ELSE
			(* x is record variable parameter *)
			INC (x.a, 8);
			Ref_to_regI (x);
			x.type := Base.nil_type;
			x.a := 8 + typ.len * 8;
			Make_type_desc_item (td, typ)
		END;
		Emit_op_reg_mem (op_LEA, reg_RAX, td);
		Emit_op_reg_mem (op_CMP, reg_RAX, x);
		Free_reg; Set_cond (x, op_JE); x.type := Base.bool_type
	ELSE
		Make_const (x, Base.bool_type, 1)
	END
END Type_test;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Inc_stack_frame_usage (amount : INTEGER);
BEGIN
	INC (stack_frame_usage, amount);
	IF stack_frame_usage > stack_frame_size THEN
		stack_frame_size := stack_frame_usage
	END
END Inc_stack_frame_usage;
	
PROCEDURE Init_local_variable (tp : Base.Type; adr : INTEGER);
	VAR
		par : Base.Item;
		field : Base.Object;
		i : INTEGER;
BEGIN
	IF tp.form IN {Base.type_pointer, Base.type_procedure} THEN
		par.mode := Base.class_var;
		par.type := Base.nil_type; par.flag := {}; par.lev := 1; par.a := adr;
		Emit_op_mem_imm (op_MOV, par, 0)
	ELSIF tp.form = Base.type_record THEN
		field := tp.fields;
		WHILE field # Base.guard DO
			Init_local_variable (field.type, adr + SHORT (field.val));
			field := field.next
		END
	ELSIF tp.form = Base.type_array THEN
		IF tp.base.form IN {Base.type_pointer, Base.type_procedure} THEN
			par.mode := Base.class_var;
			par.type := NIL; par.flag := {}; par.lev := 1; par.a := adr;
			Emit_op_reg_mem (op_LEA, -reg_RSI, par);
			Emit_op_reg_imm (op_MOV, -reg_RCX, par.type.len);
			Emit_op_reg_reg (op_XOR, -reg_EAX, -reg_EAX);
			Emit_op_bare (op_REP_STOSQ)
		ELSIF tp.base.form IN {Base.type_record, Base.type_array} THEN
			FOR i := 0 TO tp.len - 1 DO
				Init_local_variable (tp.base, adr + i * tp.base.size)
			END
		END
	END
END Init_local_variable;

PROCEDURE Enter* (proc : Base.Object; locblksize : INTEGER);
	VAR
		r, k : INTEGER;
		par, temp : Base.Item;
		obj : Base.Object;
		
	PROCEDURE Get_param_size (obj : Base.Object) : INTEGER;
		VAR
			result : INTEGER;
	BEGIN
		IF (obj.type.form = Base.type_array) & (obj.type.len < 0) THEN
			result := obj.type.size
		ELSIF (Base.flag_varParam IN obj.flag)
		& (obj.type.form = Base.type_record) THEN
			result := 16
		ELSE result := 8
		END;
		RETURN result
	END Get_param_size;
		
BEGIN (* Enter *)
	Emit_op_reg (op_PUSH, reg_RBP); (* Instruction no. 0 *)
	Emit_op_reg_reg (op_MOV, reg_RBP, reg_RSP); (* No. 1 *)
	Emit_op_reg_imm (op_SUB, reg_RSP, 0); (* No. 2 *)
	
	(* This compiler is single-pass, so how can we know the right amount to
	   decrease RSP?
	   The trick is just generating a placeholder instruction and fixing it
	   later.
	   And after the whole procedure was compiled and RSP did't need to be
	   modified, we can just mark the above intruction so the compiler will not
	   write it to the output file.
	*)
	
	stack_frame_usage := locblksize;
	stack_frame_size := locblksize;
	
	IF proc.parblksize <= 32 THEN Base.top_scope.parblksize := 32
	ELSE Base.top_scope.parblksize := proc.parblksize
	END;
		
	par.mode := Base.class_var;
	par.type := NIL;
	par.lev := Base.cur_lev;
	par.flag := {Base.flag_param};
	par.a := 0;
	
	obj := Base.top_scope.next;
	IF obj # Base.guard THEN k := Get_param_size (obj)
	END;
	(* Instruction no. 3 to 6 *)
	
	FOR r := 0 TO 3 DO
		IF (obj # Base.guard) & (Base.flag_param IN obj.flag) THEN
			IF obj.type.form = Base.type_real THEN
				Emit_op_mem_xreg (op_MOVSD, par, r)
			ELSE Emit_op_mem_reg (op_MOV, par, r)
			END;
			INC (par.a, 8); DEC (k, 8);
			IF k = 0 THEN obj := obj.next;
				IF obj # Base.guard THEN k := Get_param_size (obj)
				END
			END
		ELSE
			Emit_op_bare (op_NOP);
			codes [pc - 1].flag := {flag_skipped};
		END
	END;
	
	(* Why we need to emit superflous instructions and then mark them as
	   skipped?
	   Reason: The number of starting intructions is unchanged for every
	   procedure. And that makes fixing simpler. =)
	*)
	
	(* Saving non-volatile registers, also using fix-later trick *)
	Make_mem_stack_item (temp, NIL, 0);
	FOR r := reg_XMM6 TO reg_XMM15 DO
		Emit_op_mem_xreg (op_MOVAPS, temp, r);	(* No. 7 to 16 *)
		INC (temp.a, 16)
	END;
	FOR r := reg_R12 TO reg_RDI DO
		Emit_op_reg (op_PUSH, r)	(* No. 17 to 22 *)
	END;

	(*
	(* Init pointers value to 0 *)
	obj := Base.top_scope.next;
	WHILE obj # Base.guard DO
		IF (obj.class = Base.class_var) & ~ (Base.flag_param IN obj.flag) THEN
			Init_local_variable (obj.type, SHORT (obj.val))
		END;
		obj := obj.next
	END;
	*)
	(* The Oberon-07 report didn't require this feature, so the above piece of
	   code is intended for future usage *)
	
	reg_stack := 0; xreg_stack := 0; mem_stack := 0;
	used_regs := {}; used_xregs := {}
END Enter;
	
PROCEDURE Return_value* (VAR x : Base.Item);
BEGIN
	CASE x.mode OF
		Base.mode_reg: Emit_op_reg_reg (op_MOV, reg_RAX, x.r); Free_reg |
		Base.class_const: Emit_op_reg_imm (op_MOV, reg_RAX, x.a) |
		Base.class_var: Emit_op_reg_mem (op_MOV, reg_RAX, x) |
		Base.class_ref:
			Ref_to_regI (x);
			Emit_op_reg_mem (op_MOV, reg_RAX, x);
			Free_reg |
		Base.mode_regI: Emit_op_reg_mem (op_MOV, reg_RAX, x); Free_reg
	END
END Return_value;
	
PROCEDURE Return* (proc : Base.Object; locblksize : INTEGER);
	VAR
		i, r, nRegs, nXregs : INTEGER;
		temp : Base.Item;
		need_fix : BOOLEAN;
BEGIN
	nRegs := 0;
	FOR r := reg_RDI TO reg_R12 BY -1 DO
		IF r IN used_regs THEN Emit_op_reg (op_POP, r); INC (nRegs)
		ELSE codes [22 - reg_RDI + r].flag := {flag_skipped}
		END
	END;
	
	r := reg_XMM6; nXregs := 0;
	WHILE (r <= reg_XMM15) & (r IN used_xregs) DO INC (nXregs); INC (r)
	END;
	
	IF nXregs > 0 THEN
		IF stack_frame_size MOD 16 # 0 THEN
			stack_frame_size := (stack_frame_size DIV 16 + 1) * 16
		END;
		INC (stack_frame_size, nXregs * 16);
		
		Make_mem_stack_item (temp, NIL, 0);
		
		IF nRegs MOD 2 # 0 THEN
			INC (stack_frame_size, 8); INC (temp.a, 8); need_fix := TRUE
		ELSE need_fix := FALSE
		END;
		
		FOR r := 0 TO nXregs - 1 DO
			IF need_fix THEN INC (codes [7 + r].operands [0].imm, 8)
			END;
			Emit_op_xreg_mem (op_MOVAPS, r + reg_XMM6, temp);
			INC (temp.a, 16)
		END
	ELSE
		i := (stack_frame_size + nRegs * 8) MOD 16;
		IF i > 0 THEN INC (stack_frame_size, 16 - i)
		END
	END;
	
	WHILE nXregs <= reg_XMM15 - reg_XMM6 DO
		codes [7 + nXregs].flag := {flag_skipped};
		INC (nXregs)
	END;
	
	IF stack_frame_size = 0 THEN codes [2].flag := {flag_skipped}
	ELSE codes [2].operands [1].imm := stack_frame_size
	END;
	
	Emit_op_bare (op_LEAVE);
	Emit_op_bare (op_RET);
	Write_codes_to_file
END Return;

(* -------------------------------------------------------------------------- *)

PROCEDURE Save_reg_stacks (VAR procinfo : ProcedureInfo);
	VAR
		i : INTEGER;
BEGIN
	procinfo.reg_stack := reg_stack;
	procinfo.xreg_stack := xreg_stack;
	i := 0;
	WHILE i < reg_stack DO Push_reg (i); INC (i)
	END;
	i := 0;
	WHILE i < xreg_stack DO Push_scalar_xmm (i); INC (i)
	END;
	reg_stack := 0;
	xreg_stack := 0
END Save_reg_stacks;

PROCEDURE Restore_reg_stacks (VAR procinfo : ProcedureInfo);
	VAR
		i : INTEGER;
BEGIN
	reg_stack := procinfo.reg_stack;
	xreg_stack := procinfo.xreg_stack;
	i := xreg_stack - 1;
	WHILE i >= 0 DO Pop_scalar_xmm (i); DEC (i)
	END;
	i := reg_stack - 1;
	WHILE i >= 0 DO Pop_reg (i); DEC (i)
	END
END Restore_reg_stacks;
	
PROCEDURE Prepare_to_call* (VAR x : Base.Item; VAR procinfo : ProcedureInfo);
BEGIN
	IF x.mode # Base.class_proc THEN
		IF x.mode = Base.class_ref THEN Ref_to_regI (x)
		END;
		IF x.mode IN Base.cls_Variable THEN
			Emit_op_reg_mem (op_MOV, reg_RAX, x)
		ELSIF x.mode = Base.mode_reg THEN
			Emit_op_reg_reg (op_MOV, reg_RAX, x.r)
		END;
		IF x.mode IN Base.modes_UseReg THEN Free_reg
		END
	END;
	
	Save_reg_stacks (procinfo);
	
	procinfo.stack_frame_usage := 0;
	
	IF x.mode = Base.class_proc THEN procinfo.parblksize := x.proc.parblksize 
	ELSE procinfo.parblksize := x.type.len
	END;
	
	IF procinfo.parblksize < 32 THEN procinfo.parblksize := 32
	END;
	INC (mem_stack, procinfo.parblksize);
	IF mem_stack MOD 16 = 8 THEN
		INC (procinfo.parblksize, 8); INC (mem_stack, 8)
	END;	
	Emit_op_reg_imm (op_SUB, reg_RSP, procinfo.parblksize);
	procinfo.mem_stack := mem_stack;
	
	procinfo.param_adr := 0;
	
	IF (x.mode # Base.class_proc) & (x.type.len # 0) THEN
		Push_reg (reg_RAX)
	END
END Prepare_to_call;

PROCEDURE Correct_param_regs (VAR procinfo : ProcedureInfo);
	VAR
		i, j, k, n : INTEGER;
BEGIN
	n := procinfo.parblksize DIV 8;
	IF n > 4 THEN n := 4
	END;
	IF n > 1 THEN
		i := n;
		WHILE i > 1 DO
			k := 0; j := 0;
			WHILE j < i DO
				IF procinfo.param_regs[j] = procinfo.param_regs[i - 1] THEN
					INC (k)
				END;
				INC (j)
			END;
			IF k # i THEN
				IF procinfo.param_regs[i - 1] = 0 THEN
					Emit_op_reg_reg (op_MOV, i, k)
				ELSE
					Emit_op_xreg_xreg (op_MOV, i, k)
				END
			END;
			DEC (i)
		END
	END
END Correct_param_regs;
	
PROCEDURE Call* (VAR x : Base.Item; VAR procinfo : ProcedureInfo);
BEGIN
	Correct_param_regs (procinfo);
	IF x.mode = Base.class_proc THEN
		Emit_op_proc (op_CALL, x.proc)
	ELSE
		IF x.type.len # 0 THEN
			IF procinfo.mem_stack = mem_stack - 8 THEN Pop_reg (reg_RAX)
			ELSE Scanner.Mark ('COMPILER FAULT: procedure Generator.Call')
			END
		END;
		Emit_op_reg (op_CALL, reg_RAX)
	END;
	
	(* Release the stack area used for parameters *)
	Emit_op_reg_imm (op_ADD, reg_RSP, procinfo.parblksize);
	DEC (mem_stack, procinfo.parblksize);
	
	(* Free temporary stack frame usage *)
	DEC (stack_frame_usage, procinfo.stack_frame_usage);
	
	Restore_reg_stacks (procinfo);
	
	(* Get return value *)
	IF x.mode = Base.class_proc THEN
		IF x.type # NIL THEN
			x.r := reg_stack; Inc_reg_stack;
			x.mode := Base.mode_reg; x.flag := {};
			Emit_op_reg_reg (op_MOV, x.r, reg_RAX)
		END
	ELSIF (x.type # NIL) & (x.type.form = Base.type_procedure) THEN
		IF x.type.base # NIL THEN
			Inc_reg_stack; x.r := reg_stack - 1; 
			x.mode := Base.mode_reg; x.type := x.type.base; x.flag := {};
			Emit_op_reg_reg (op_MOV, x.r, reg_RAX)
		END
	ELSE
		Scanner.Mark ('COMPILER FAULT: procedure Generator.Call error 2')
	END
END Call;
	
PROCEDURE Normal_parameter* (VAR x : Base.Item; VAR procinfo : ProcedureInfo);
	VAR
		param : Base.Item;
		adr : INTEGER;
BEGIN
	load (x);
	IF procinfo.param_adr >= 32 THEN
		adr := mem_stack - procinfo.mem_stack + procinfo.param_adr;
		Make_mem_stack_item (param, x.type, adr);
		IF x.mode = Base.mode_reg THEN
			Emit_op_mem_reg (op_MOV, param, x.r); Free_reg
		ELSIF x.mode = Base.mode_xreg THEN
			Emit_op_mem_xreg (op_MOVSD, param, x.r); Free_xreg
		END
	ELSIF x.mode = Base.mode_xreg THEN
		procinfo.param_regs [procinfo.param_adr DIV 8] := 1
	ELSE
		procinfo.param_regs [procinfo.param_adr DIV 8] := 0
	END;
	INC (procinfo.param_adr, 8)
END Normal_parameter;
	
PROCEDURE Reference_parameter*(VAR x : Base.Item; VAR procinfo : ProcedureInfo);
	VAR
		param : Base.Item;
		adr : INTEGER;
BEGIN
	Load_adr (x);
	IF procinfo.param_adr >= 32 THEN
		adr := mem_stack - procinfo.mem_stack + procinfo.param_adr;
		Make_mem_stack_item (param, Base.int_type, adr);
		Emit_op_mem_reg (op_MOV, param, x.r);
		Free_reg
	ELSE
		procinfo.param_regs [procinfo.param_adr DIV 8] := 0
	END;
	INC (procinfo.param_adr, 8)
END Reference_parameter;
	
PROCEDURE Record_variable_parameter*
(VAR x : Base.Item; VAR procinfo : ProcedureInfo);
	VAR
		tag : Base.Item;
BEGIN
	IF Base.flag_varParam IN x.flag THEN
		tag.flag := {Base.flag_param};
		tag.mode := Base.class_ref;
		tag.lev := x.lev;
		tag.a := x.a + 8
	ELSE
		Make_type_desc_item (tag, x.type)
	END;
	Reference_parameter (x, procinfo);
	Reference_parameter (tag, procinfo)
END Record_variable_parameter;
	
PROCEDURE Open_array_parameter*
(VAR x : Base.Item; VAR procinfo : ProcedureInfo; formal_type : Base.Type);
	VAR
		array, len : Base.Item;
		tp : Base.Type;
BEGIN
	array := x;
	Reference_parameter (array, procinfo);
	
	tp := x.type;
	IF x.b = 0 THEN x.b := SHORT(x.a) + 8
	END;
	
	WHILE (formal_type.form = Base.type_array) & (formal_type.len < 0) DO
		IF tp.form IN {Base.type_array, Base.type_string} THEN
			IF tp.len < 0 THEN
				Make_len_item (len, x.b);
				INC (x.b, 8)
			ELSE
				len.mode := Base.class_const;
				len.type := Base.int_type;
				len.a := tp.len;
			END
		ELSE
			Scanner.Mark ('COMPILER FAULT: procedure Generator.Open_array_parameter')
		END;
		
		Normal_parameter (len, procinfo);		
		formal_type := formal_type.base;
		tp := tp.base
	END
END Open_array_parameter;
	
PROCEDURE String_parameter*
(VAR x : Base.Item; VAR procinfo : ProcedureInfo; formal_type : Base.Type);
	VAR
		array : Base.Item;
BEGIN
	array.flag := {};
	array.mode := Base.class_var;
	array.lev := 1;
	array.type := formal_type;
	array.a := stack_frame_usage;
	
	INC (procinfo.stack_frame_usage, array.type.size);
	Inc_stack_frame_usage (array.type.size);
	
	Store (array, x);
	Reference_parameter (array, procinfo)
END String_parameter;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Begin_FOR*
(VAR x, beg, end, rel : Base.Item; VAR L : INTEGER; inc : INTEGER);
	VAR
		t : Base.Item;
		i : INTEGER;
BEGIN
	t := x; Store (t, beg);
	
	i := stack_frame_usage; Inc_stack_frame_usage (8);
	t.mode := Base.class_var; t.lev := 1; t.type := Base.int_type; t.a := i;
	Store (t, end);
	
	end.mode := Base.class_var; end.lev := 1; end.type := Base.int_type;
	end.a := i;
	
	L := pc; rel := x;
	t.mode := Base.class_var; t.lev := 1; t.type := Base.int_type; t.a := i;
	IF inc >= 0 THEN Comparison (Base.sym_less_equal, rel, t)
	ELSE Comparison (Base.sym_greater_equal, rel, t)
	END;
	CFJump (rel)
END Begin_FOR;
	
PROCEDURE End_FOR* (VAR x, rel, inc : Base.Item; L : INTEGER);
BEGIN
	IF inc.mode = Base.mode_reg THEN
		Emit_op_mem_reg (op_ADD, x, inc.r); Free_reg
	ELSE
		Emit_op_mem_const (op_ADD, x, inc.a)
	END;
	IF (x.type.form = Base.type_integer)
	& (Base.integer_overflow_check IN Base.compiler_flag) THEN
		Emit_op_sym (op_JO, sym_integer_overflow_trap)
	END;
	BJump (L); Fix_link (SHORT (rel.a))
END End_FOR;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init_type_desc;
	VAR
		i, j : INTEGER;
		tp : Base.Type;
		basetps : ARRAY Base.type_extension_limit + 1 OF Base.Type;
		dtag, tag : Base.Item;
BEGIN
	FOR i := 0 TO record_no - 1 DO
		tp := type_desc_list [i];
		Make_type_desc_item (dtag, type_desc_list [i]);
		INC (dtag.a, 8);
		basetps [tp.len] := tp;
		
		WHILE tp.base # NIL DO
			tp := tp.base;
			basetps [tp.len] := tp
		END;
		j := 0;
		WHILE j <= Base.type_extension_limit DO
			IF basetps [j] # NIL THEN
				Make_type_desc_item (tag, basetps [j]);
				Emit_op_reg_mem (op_LEA, reg_RAX, tag);
				Emit_op_mem_reg (op_MOV, dtag, reg_RAX);
				INC (dtag.a, 8);
				basetps [j] := NIL;
				INC (j)
			ELSE
				j := Base.type_extension_limit + 1 (* exit loop *)
			END
		END
	END
END Init_type_desc;

PROCEDURE Module_init*;
BEGIN
	Emit_op_reg (op_PUSH, reg_RBP);
	Emit_op_reg_reg (op_MOV, reg_RBP, reg_RSP);
	Emit_op_reg_imm (op_SUB, reg_RSP, 0);
	
	(* Init_type_desc; *)
	
	stack_frame_usage := 0;
	stack_frame_size := 0;
	reg_stack := 0;
	mem_stack := 0;
	used_regs := {}
END Module_init;

PROCEDURE End_module_init*;
BEGIN
	Emit_op_reg_imm (op_SUB, reg_RSP, 32);
	Emit_op_reg_imm (op_MOV, reg_ECX, 0);
	Emit_op_sym (op_CALL, Base.Make_string ('[@ExitProcess]'));
	
	(* Perform fixup *)
	IF stack_frame_size = 0 THEN
		codes [2].flag := {flag_skipped}
	ELSE
		IF stack_frame_size MOD 16 # 0 THEN
			stack_frame_size := (stack_frame_size DIV 16 + 1) * 16
		END;
		codes [2].operands [1].imm := stack_frame_size;
	END;
	
	Write_codes_to_file
END End_module_init;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE SFunc_ABS* (VAR y : Base.Item);
	BEGIN
	IF y.mode = Base.class_const THEN
		IF y.a = MIN_INT THEN Scanner.Mark ('Integer overflow detected')
		ELSE y.a := ABS (y.a) END
	ELSE
		load (y);
		Emit_op_reg_reg (op_TEST, y.r, y.r);
		Emit_op_label (op_JGE, pc + 2);
		Emit_op_reg (op_NEG, y.r);
		INCL (codes [pc].flag, flag_hasLabel)
		END
	END SFunc_ABS;
	
PROCEDURE SFunc_ODD* (VAR y : Base.Item);
	BEGIN
	IF y.mode = Base.class_const THEN
		y.a := y.a MOD 2
	ELSE
		IF y.mode = Base.mode_reg THEN
			Emit_op_reg_imm (op_SHR, y.r, 1);
			Free_reg
		ELSE
			Emit_op_mem_imm (op_BT, y, 0);
			IF y.mode IN Base.modes_UseReg THEN Free_reg END
			END;
		Set_cond (y, op_JC)
		END
	END SFunc_ODD;
	
PROCEDURE SFunc_LEN* (VAR y : Base.Item);
	BEGIN
	IF y.type.len < 0 THEN
		IF y.b = 0 THEN y.b := SHORT (y.a) + 8 END;
		Make_len_item (y, y.b)
	ELSE
		Make_const (y, Base.int_type, y.type.len)
		END;
	IF y.mode IN Base.modes_UseReg THEN Free_reg END
	END SFunc_LEN;
	
PROCEDURE SFunc_ORD* (VAR y : Base.Item);
BEGIN
	IF y.mode IN Base.cls_Variable THEN
		IF y.type.form = Base.type_string THEN
			y.mode := Base.class_const;
			y.a := ORD (strings [y.a DIV Base.char_type.size])
		ELSIF y.type.size < 8 THEN
			load (y)
		END
	END
END SFunc_ORD;
	
PROCEDURE SFunc_CHR* (VAR y : Base.Item);
	BEGIN
	IF y.mode = Base.class_const THEN
		IF (y.a < 0) OR (y.a > Base.MAX_CHAR) THEN
			Scanner.Mark ('Value out of characters range')
			END
	ELSIF y.mode IN Base.cls_Variable THEN
		IF y.type.size < Base.char_type.size THEN load (y) END
		END
	END SFunc_CHR;
	
PROCEDURE SFunc_ADR* (VAR y : Base.Item);
	BEGIN
	Load_adr (y)
	END SFunc_ADR;
	
PROCEDURE SFunc_VAL* (VAR y, z : Base.Item);
BEGIN
	IF z.mode = Base.class_const THEN
		IF Base.Size_of_const (z.a) > y.type.size THEN
			Scanner.Mark ('Const too big for casting')
		END;
		IF (z.a < MIN_INT32) & (z.a > MAX_INT32) THEN load (z)
		END
	END;
	IF (z.mode IN Base.cls_Variable) & (z.type.size < y.type.size) THEN
		load (z)
	END;
	z.type := y.type;
	y := z
END SFunc_VAL;

PROCEDURE SProc_LoadLibrary* (VAR x : Base.Item);
	VAR
		i : INTEGER;
BEGIN
	IF mem_stack MOD 16 # 0 THEN i := 40 ELSE i := 32
	END;
	Emit_op_reg_imm (op_SUB, reg_RSP, i);
	
	Load_adr (x);
	Emit_op_sym (op_CALL, Base.Make_string ('[@LoadLibrary]'));
	
	Emit_op_reg_imm (op_ADD, reg_RSP, i);
	Emit_op_reg_reg (op_MOV, x.r, reg_RAX)
END SProc_LoadLibrary;
	
PROCEDURE SProc_GetProcAddress* (VAR x, y : Base.Item);
	VAR
		i : INTEGER;
BEGIN
	IF mem_stack MOD 16 # 0 THEN i := 40 ELSE i := 32
	END;
	Emit_op_reg_imm (op_SUB, reg_RSP, i);
	
	Emit_op_sym (op_CALL, Base.Make_string ('[@GetProcAddress]'));
	
	Emit_op_reg_imm (op_ADD, reg_RSP, i);
	Emit_op_reg_reg (op_MOV, x.r, reg_RAX);
	Free_reg
END SProc_GetProcAddress;
	
PROCEDURE SProc_GET* (VAR x, y : Base.Item);
	VAR
		i : INTEGER;
	BEGIN
	load (x);
	x.mode := Base.mode_regI; x.a := 0;
	load (x);
	Emit_op_mem_reg (op_MOV, y, Small_reg (x.r, y.type.size));
	Free_reg;
	IF y.mode IN Base.modes_UseReg THEN Free_reg END
	END SProc_GET;
	
PROCEDURE SProc_PUT* (VAR x, y : Base.Item);
	VAR
		i : INTEGER;
BEGIN
	load (x); x.mode := Base.mode_regI; x.a := 0; x.type := y.type;
	IF y.mode = Base.class_const THEN
		Emit_op_mem_const (op_MOV, x, y.a)
	ELSE
		load (y);
		Emit_op_mem_reg (op_MOV, x, Small_reg (y.r, y.type.size));
		Free_reg
	END;
	Free_reg
END SProc_PUT;
	
PROCEDURE SProc_COPY* (VAR x, y, z : Base.Item);
	VAR
		i : INTEGER;
	BEGIN
	IF (z.mode = Base.class_const)
	& ((z.a = 1) OR (z.a = 2) OR (z.a = 4) OR (z.a = 8)) THEN
		load (x); load (y);
		x.mode := Base.mode_regI; x.a := 0;
		y.mode := Base.mode_regI; y.a := 0;
		load (x); y.type := NIL;
		Emit_op_mem_reg (op_MOV, y, Small_reg (x.r, SHORT (z.a)));
		Free_reg; Free_reg
	ELSE
		load (x); Emit_op_reg_reg (op_MOV, -reg_RSI, x.r); Free_reg;
		load (y); Emit_op_reg_reg (op_MOV, -reg_RDI, y.r); Free_reg;
		load (z); Emit_op_reg_reg (op_MOV, -reg_RCX, z.r); Free_reg;
		Emit_op_bare (op_REP_MOVSB);
		used_regs := used_regs + {reg_RSI, reg_RDI}
		END
	END SProc_COPY;
	
PROCEDURE SProc_NEW* (VAR x : Base.Item);
	VAR
		td : Base.Item;
	BEGIN
	IF x.mode = Base.class_ref THEN Emit_op_reg_mem (op_MOV, reg_RCX, x)
	ELSE Emit_op_reg_mem (op_LEA, reg_RCX, x) END;
	Make_type_desc_item (td, x.type);
	Emit_op_reg_mem (op_LEA, reg_RDX, td);
	Emit_op_sym (op_CALL, Base.Make_string ('[@NEW]'));
	IF x.mode IN Base.modes_UseReg THEN Free_reg END
	END SProc_NEW;
	
PROCEDURE SProc_DISPOSE* (VAR x : Base.Item);
	BEGIN
	IF x.mode = Base.class_ref THEN Emit_op_reg_mem (op_MOV, reg_RCX, x)
	ELSE Emit_op_reg_mem (op_LEA, reg_RCX, x) END;
	Emit_op_sym (op_CALL, Base.Make_string ('[@DISPOSE]'));
	IF x.mode IN Base.modes_UseReg THEN Free_reg END
	END SProc_DISPOSE;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init* (module_name : Base.String);
	VAR
		i : INTEGER;
		filename : ARRAY 512 OF CHAR;
		path : ARRAY 1024 OF CHAR;
BEGIN
	i := 0; Sys.Get_executable_path (path, i);
	path [i] := 'd'; path [i + 1] := 'a'; path [i + 2] := 't';
	path [i + 3] := 'a'; path [i + 4] := '.'; path [i + 5] := 'd';
	path [i + 6] := 'a'; path [i + 7] := 't';
	Sys.Open (datafile, path);
	
	i := 0;
	WHILE i < module_name.len DO
		filename [i] := module_name.content [i];
		INC (i)
	END;
	filename [i] := '.';
	filename [i + 1] := 'a';
	filename [i + 2] := 's';
	filename [i + 3] := 'm';
	Sys.Rewrite (out, filename);
	
	modid := module_name;

	Sys.Write_string (out, 'format PE64');
	Sys.Write_newline (out);
	Sys.Write_string (out, 'entry ');
	Sys.Write_string (out, module_name.content);
	Sys.Write_string (out, '@@INIT');
	Sys.Write_newline (out);
	Sys.Write_newline (out);
	Sys.Write_string (out, "section '.text' code readable executable");
	Sys.Write_newline (out);
	Sys.Write_newline (out)
END Init;
	
PROCEDURE Generate_trap_section;
	VAR
		i : INTEGER;
BEGIN
	FOR i := 1 TO 4 DO
		CASE i OF
			1: Sys.Write_string (out, sym_array_index_trap.content) |
			2: Sys.Write_string (out, sym_integer_overflow_trap.content) |
			3: Sys.Write_string (out, sym_invalid_divisor_trap.content) |
			4: Sys.Write_string (out, sym_type_check_trap.content)
		END;
		Sys.Write_char (out, ':');
		Sys.Write_newline (out);
		Sys.Write_string (out, 'and	rsp, -16');
		Sys.Write_newline (out);
		Sys.Write_string (out, 'sub	rsp, 32');
		Sys.Write_newline (out);
		Sys.Write_string (out, 'mov	ecx, -'); Sys.Write_number (out, i);
		Sys.Write_newline (out);
		Sys.Write_string (out, 'call	[@ExitProcess]');
		Sys.Write_newline (out);
	END;
	Sys.Write_newline (out)
END Generate_trap_section;
	
PROCEDURE Generate_type_desc_section;
	VAR
		i : INTEGER;
		
	PROCEDURE Emit_record_tdesc (typ : Base.Type);
		VAR
			i : INTEGER;
			
		PROCEDURE Emit_pointer_offset (typ : Base.Type; offset : INTEGER);
			VAR
				field : Base.Object;
				n : INTEGER;
		BEGIN
			IF typ.form = Base.type_pointer THEN
				Sys.Write_char (out, ',');
				Sys.Write_number (out, offset)
			ELSIF typ.form = Base.type_record THEN
				n := typ.num_ptr;
				IF (typ.base # NIL) & (typ.base.num_ptr > 0) THEN
					Emit_pointer_offset (typ.base, offset);
					DEC (n, typ.base.num_ptr);
					IF n > 0 THEN Sys.Write_char (out, ',')
					END
				END;
				field := typ.fields;
				WHILE n > 0 DO
					IF field.type.num_ptr > 0 THEN
						Emit_pointer_offset
							(field.type, offset + SHORT(field.val));
						DEC (n, field.type.num_ptr);
						IF n > 0 THEN Sys.Write_char (out, ',')
						END
					END;
					field := field.next
				END
			ELSIF typ.form = Base.type_array THEN
				FOR n := 0 TO typ.len - 1 DO
					Emit_pointer_offset (typ.base, offset + n * typ.base.size);
					IF n # typ.len - 1 THEN Sys.Write_char (out, ',')
					END
				END
			END
		END Emit_pointer_offset;	
			
	BEGIN (* Emit_record_tdesc *)
		Sys.Write_number (out, typ.size);
		FOR i := 0 TO Base.type_extension_limit DO Sys.Write_string (out, ',0')
		END;
		IF typ.num_ptr > 0 THEN
			Sys.Write_char (out, ',');
			Emit_pointer_offset (typ, 0)
		END;
		Sys.Write_string (out, ',-1')
	END Emit_record_tdesc;
		
BEGIN (* Generate_type_desc_section *)
	Sys.Write_string (out, modid.content);
	Sys.Write_string (out, '@@');
	Sys.Write_string (out, sym_tdbase.content);
	Sys.Write_char (out, ':');
	Sys.Write_newline (out);
	
	FOR i := 0 TO record_no - 1 DO
		Sys.Write_string (out, '	dq ');
		Emit_record_tdesc (type_desc_list [i]);
		Sys.Write_newline (out)
	END
END Generate_type_desc_section;
	
PROCEDURE Generate_string_section;
	VAR
		i : INTEGER;
BEGIN
	Sys.Write_string (out, modid.content);
	Sys.Write_string (out, '@@');
	Sys.Write_string (out, sym_stringbase.content);
	Sys.Write_string (out, ' dw ');
	Sys.Write_number (out, ORD (strings [0]));
	i := 1;
	WHILE i < str_offset DO
		Sys.Write_char (out, ',');
		Sys.Write_number (out, ORD (strings [i]));
		INC (i)
	END;
	Sys.Write_newline (out)
END Generate_string_section;
	
PROCEDURE Generate_variable_section (vars_size : INTEGER);
BEGIN
	Sys.Write_string (out, modid.content);
	Sys.Write_string (out, '@@');
	Sys.Write_string (out, sym_varbase.content);
	Sys.Write_string (out, ' db ');
	Sys.Write_number (out, vars_size);
	Sys.Write_string (out, ' dup ?');
	Sys.Write_newline (out)
END Generate_variable_section;
	
PROCEDURE Finish* (vars_size : INTEGER);
	VAR
		i : INTEGER;
BEGIN
	Generate_trap_section;	
	Sys.Write_string (out, "section '.data' data readable writable");
	Sys.Write_newline (out);

	IF td_offset > 0 THEN Generate_type_desc_section
	END;
	IF str_offset > 0 THEN Generate_string_section
	END;
	IF vars_size > 0 THEN Generate_variable_section (vars_size)
	END;
	Sys.Write_newline (out);
	
	Sys.Copy_file (out, datafile, 781);
	
	Sys.Close (datafile);
	Sys.Close (out)
END Finish;
	
PROCEDURE Init_optable;
BEGIN
	op_table [op_NOP] := 'nop';
	
	op_table [op_MOV] := 'mov';
	op_table [op_MOVSX] := 'movsx';
	op_table [op_MOVZX] := 'movzx';
	op_table [op_LEA] := 'lea';
	op_table [op_PUSH] := 'push';
	op_table [op_POP] := 'pop';
	op_table [op_XCHG] := 'xchg';
	op_table [op_CQO] := 'cqo';
	op_table [op_REP_MOVSB] := 'rep movsb';
	op_table [op_REP_STOSQ] := 'rep stosq';
	
	op_table [op_ADD] := 'add';
	op_table [op_SUB] := 'sub';
	op_table [op_IMUL] := 'imul';
	op_table [op_IDIV] := 'idiv';
	op_table [op_NEG] := 'neg';
	op_table [op_AND] := 'and';
	op_table [op_OR] := 'or';
	op_table [op_XOR] := 'xor';
	op_table [op_NOT] := 'not';
	op_table [op_CMP] := 'cmp';
	op_table [op_TEST] := 'test';
	op_table [op_SHL] := 'shl';
	op_table [op_SHR] := 'shr';
	op_table [op_SAR] := 'sar';
	op_table [op_BT] := 'bt';
	op_table [op_BTS] := 'bts';
	op_table [op_BTR] := 'btr';
	op_table [op_BTC] := 'btc';
	op_table [op_INC] := 'inc';
	op_table [op_DEC] := 'dec';
	
	op_table [op_CALL] := 'call';
	op_table [op_RET] := 'ret';
	op_table [op_LEAVE] := 'leave';
	
	op_table [op_JMP] := 'jmp';
	op_table [op_JO] := 'jo';
	op_table [op_JNO] := 'jno';
	op_table [op_JB] := 'jb';
	op_table [op_JAE] := 'jae';
	op_table [op_JE] := 'je';
	op_table [op_JNE] := 'jne';
	op_table [op_JL] := 'jl';
	op_table [op_JGE] := 'jge';
	op_table [op_JG] := 'jg';
	op_table [op_JLE] := 'jle';
	op_table [op_JA] := 'ja';
	op_table [op_JBE] := 'jbe';
	op_table [op_JC] := 'jc';
	op_table [op_JNC] := 'jnc';
	
	op_table [op_MOVSS] := 'movss';
	op_table [op_MOVSD] := 'movsd';
	op_table [op_MOVD] := 'movd';
	op_table [op_MOVQ] := 'movq';
	op_table [op_MOVAPS] := 'movaps';
	
	op_table [op_ADDSS] := 'addss';
	op_table [op_SUBSS] := 'subss';
	op_table [op_MULSS] := 'mulss';
	op_table [op_DIVSS] := 'divss';
	op_table [op_ANDPS] := 'andps';
	op_table [op_ORPS] := 'orps';
	op_table [op_XORPS] := 'xorps'
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
	
	reg_table [reg_EAX] := 'eax';
	reg_table [reg_EBX] := 'ebx';
	reg_table [reg_ECX] := 'ecx';
	reg_table [reg_EDX] := 'edx';
	reg_table [reg_ESI] := 'esi';
	reg_table [reg_EDI] := 'edi';
	reg_table [reg_EBP] := 'ebp';
	reg_table [reg_ESP] := 'esp';
	reg_table [reg_R8D] := 'r8d';
	reg_table [reg_R9D] := 'r9d';
	reg_table [reg_R10D] := 'r10d';
	reg_table [reg_R11D] := 'r11d';
	reg_table [reg_R12D] := 'r12d';
	reg_table [reg_R13D] := 'r13d';
	reg_table [reg_R14D] := 'r14d';
	reg_table [reg_R15D] := 'r15d';
	
	reg_table [reg_AX] := 'ax';
	reg_table [reg_BX] := 'bx';
	reg_table [reg_CX] := 'cx';
	reg_table [reg_DX] := 'dx';
	reg_table [reg_SI] := 'si';
	reg_table [reg_DI] := 'di';
	reg_table [reg_BP] := 'bp';
	reg_table [reg_SP] := 'sp';
	reg_table [reg_R8W] := 'r8w';
	reg_table [reg_R9W] := 'r9w';
	reg_table [reg_R10W] := 'r10w';
	reg_table [reg_R11W] := 'r11w';
	reg_table [reg_R12W] := 'r12w';
	reg_table [reg_R13W] := 'r13w';
	reg_table [reg_R14W] := 'r14w';
	reg_table [reg_R15W] := 'r15w';
	
	reg_table [reg_AL] := 'al';
	reg_table [reg_BL] := 'bl';
	reg_table [reg_CL] := 'cl';
	reg_table [reg_DL] := 'dl';
	reg_table [reg_SIL] := 'sil';
	reg_table [reg_DIL] := 'dil';
	reg_table [reg_BPL] := 'bpl';
	reg_table [reg_SPL] := 'spl';
	reg_table [reg_R8L] := 'r8l';
	reg_table [reg_R9L] := 'r9l';
	reg_table [reg_R10L] := 'r10l';
	reg_table [reg_R11L] := 'r11l';
	reg_table [reg_R12L] := 'r12l';
	reg_table [reg_R13L] := 'r13l';
	reg_table [reg_R14L] := 'r14l';
	reg_table [reg_R15L] := 'r15l';
	
	xreg_table [reg_XMM0] := 'xmm0';
	xreg_table [reg_XMM1] := 'xmm1';
	xreg_table [reg_XMM2] := 'xmm2';
	xreg_table [reg_XMM3] := 'xmm3';
	xreg_table [reg_XMM4] := 'xmm4';
	xreg_table [reg_XMM5] := 'xmm5';
	xreg_table [reg_XMM6] := 'xmm6';
	xreg_table [reg_XMM7] := 'xmm7';
	xreg_table [reg_XMM8] := 'xmm8';
	xreg_table [reg_XMM9] := 'xmm9';
	xreg_table [reg_XMM10] := 'xmm10';
	xreg_table [reg_XMM11] := 'xmm11';
	xreg_table [reg_XMM12] := 'xmm12';
	xreg_table [reg_XMM13] := 'xmm13';
	xreg_table [reg_XMM14] := 'xmm14';
	xreg_table [reg_XMM15] := 'xmm15'
END Init_regtable;
	
PROCEDURE Init_symbols;
BEGIN
	sym_array_index_trap := Base.Make_string ('INVALID_ARRAY_INDEX_TRAP');
	sym_integer_overflow_trap := Base.Make_string ('INTEGER_OVERFLOW_TRAP');
	sym_invalid_divisor_trap := Base.Make_string ('INVALID_DIVISOR_TRAP');
	sym_type_check_trap := Base.Make_string ('TYPE_CHECK_TRAP');
	
	sym_varbase := Base.Make_string ('VAR');
	sym_stringbase := Base.Make_string ('STRING');
	sym_tdbase := Base.Make_string ('TYPEDESC')
END Init_symbols;
	
BEGIN
	Init_optable;
	Init_regtable;
	Init_symbols
END GeneratorWin64.
