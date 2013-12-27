MODULE Generator;

IMPORT
	SYSTEM, Base, Scanner;
	
CONST
	MAX_INT32 = 2147483647;
	MIN_INT32 = -MAX_INT32 - 1;

	form_nothing = 0; form_reg = 1; form_imm = 2; form_mem_reg = 3;
	form_mem_pc = 4; form_mem_scale = 5; form_proc_name = 6; form_sym = 7;
	form_label = 8;
	
	flag_haslabel = 0; flag_skiped = 1;
	
	op_NOP = 0;
	op_MOV = 1; op_MOVSX = 2; op_MOVZX = 3; op_LEA = 4; op_PUSH = 5; op_POP = 6;
	op_XCHG = 7; op_CQO = 8; op_REP_MOVSB = 9;
	
	op_ADD = 20; op_SUB = 21; op_IMUL = 22; op_IDIV = 23; op_NEG = 24;
	op_AND = 25; op_OR = 26; op_XOR = 27; op_NOT = 28; op_CMP = 29;
	op_TEST = 30; op_SHL = 31; op_SHR = 32; op_SAR = 33; op_BT = 34;
	op_BTS = 35; op_BTR = 36;
	
	op_CALL = 40; op_RET = 41; op_LEAVE = 42;
	
	op_JMP = 50; vop_NJMP = 51; op_JO = 52; op_JNO = 53;
	op_JB = 54; op_JAE = 55; op_JE = 56; op_JNE = 57; op_JL = 58; op_JGE = 59;
	op_JG = 60; op_JLE = 61; op_JA = 62; op_JBE = 63;
	
	(* ====================================================================== *)
	
	reg_R10 = 0; reg_R11 = 1; (* R10, R11 are caller-save *)
	reg_R12 = 2; reg_R13 = 3; reg_R14 = 4; reg_R15 = 5; reg_TOP = 5;
	(* R12 to RDI are callee-save *)
	reg_RSI = 6; reg_RDI = 7;
	(* RSI, RDI are only used for special purpose *)
	
	(* RCX to R9 are registers for parameter passing *)
	(* RCX, RDX also are special purpose *)
	reg_RCX = 8; reg_RDX = 9; reg_R8 = 10; reg_R9 = 11;
	
	(* RAX, RBX, RBP, RSP are use for special purpose *)
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

TYPE
	Operand = RECORD
		form, scale, size : BYTE;
		reg1, reg2, imm : INTEGER;
		obj : Base.Object;
		sym : Base.String
		END;
	
	Instruction = RECORD
		flag : SET;
		op : INTEGER;
		operands : ARRAY 3 OF Operand
		END;
	
VAR
	op_table : ARRAY 256, 10 OF CHAR;
	reg_table : ARRAY 256, 6 OF CHAR;
	sym_array_index_trap, sym_integer_overflow_trap : Base.String;
	sym_invalid_divisor_trap, sym_type_check_trap : Base.String;
	sym_varbase, sym_stringbase, sym_tagbase : Base.String;

	out : Base.FileHandle;
	codes : ARRAY 20000 OF Instruction;
	strings : ARRAY 131072 OF CHAR;
	record_type_list : ARRAY Base.max_number_record_types OF Base.Type;
	modid : Base.String;
	
	pc : INTEGER;
	str_offset, tag_offset, record_no : INTEGER;
	reg_stack, mem_stack, param_regs_usage : INTEGER;
	used_regs : SET;
	stack_frame_usage, stack_frame_size : INTEGER;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
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

	PROCEDURE Write_mem_prefix (VAR mem : Operand);
		BEGIN
		CASE mem.size OF
			0: (* Do nothing *) |
			1: Base.Write_string (out, 'byte ') |
			2: Base.Write_string (out, 'word ') |
			4: Base.Write_string (out, 'dword ') |
			8: Base.Write_string (out, 'qword ')
			END
		END Write_mem_prefix;

	PROCEDURE Write_operand (VAR op : Operand);
		BEGIN
		CASE op.form OF
			form_reg:
				Base.Write_string (out, reg_table [op.reg1]) |
			form_imm:
				Base.Write_number (out, op.imm) |
			form_mem_reg:
				Write_mem_prefix (op);
				Base.Write_char (out, '[');
				Base.Write_string (out, reg_table [op.reg1]);
				Base.Write_string (out, ' + ');
				Base.Write_number (out, op.imm);
				Base.Write_char (out, ']') |
			form_mem_pc:
				Write_mem_prefix (op);
				Base.Write_char (out, '[');
				Write_scope_name (Base.universe);
				Base.Write_string (out, '@@');
				Base.Write_string (out, op.sym.content);
				Base.Write_string (out, ' + ');
				Base.Write_number (out, op.imm);
				Base.Write_char (out, ']') |
			form_mem_scale:
				Write_mem_prefix (op);
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
				Base.Write_string (out, op.obj.name.content) |
			form_label:
				Write_scope_name (Base.top_scope);
				Base.Write_char (out, '@');
				Base.Write_number (out, op.imm) |
			form_sym:
				Base.Write_string (out, op.sym.content)
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
	IF Base.top_scope = Base.universe THEN
		Base.Write_string (out, '@@INIT')
		END;
	Base.Write_char (out, ':');
	Base.Write_newline (out);
	FOR i := 0 TO pc - 1 DO
		IF ~ (flag_skiped IN codes [i].flag) THEN
			IF flag_haslabel IN codes [i].flag THEN
				Write_scope_name (Base.top_scope);
				Base.Write_char (out, '@');
				Base.Write_number (out, i);
				Base.Write_char (out, ':');
				Base.Write_newline (out)
				END;
			Write_instruction (codes [i])
			END
		END;
	Base.Write_newline (out);
	
	pc := -1;
	Inc_program_counter
	END Write_codes_to_file;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Is_big_immediate (imm : INTEGER) : BOOLEAN;
	VAR
		r : BOOLEAN;
	BEGIN
	IF (imm < MIN_INT32) OR (imm > MAX_INT32) THEN r := TRUE
	ELSE r := FALSE END; RETURN r
	END Is_big_immediate;
	
PROCEDURE Decode_reg (r : INTEGER) : INTEGER;
	BEGIN
	IF r < 0 THEN
		r := -r
	ELSIF r > reg_TOP THEN
		r := r MOD (reg_TOP + 1)
		END;
	RETURN r
	END Decode_reg;
	
PROCEDURE Small_reg (r, size : INTEGER) : INTEGER;
	BEGIN
	r := Decode_reg (r) MOD 64;
	CASE size OF
		1: INC (r, 48) |
		2: INC (r, 32) |
		4: INC (r, 16) |
		8: (* Do nothing *)
		END;
	RETURN -r
	END Small_reg;

PROCEDURE Set_mem_operand (VAR o : Operand; VAR mem : Base.Item);
	BEGIN
	IF mem.mode = Base.mode_regI THEN
		o.form := form_mem_reg;
		o.reg1 := Decode_reg (mem.r);
		o.imm := mem.a
	ELSIF Base.flag_param IN mem.flag THEN
		o.form := form_mem_reg;
		o.reg1 := reg_stackBase;
		IF mem.a < 32 THEN
			o.imm := 16 + Base.top_scope.parblksize - 32 + mem.a
		ELSE
			o.imm := 16 + mem.a - 32
			END
	ELSIF mem.lev > 0 THEN
		o.form := form_mem_reg;
		o.reg1 := reg_stackBase;
		o.imm := mem.a
	ELSE
		o.form := form_mem_pc;
		IF mem.lev = -1 THEN
			o.sym := sym_tagbase
		ELSIF mem.type.form = Base.type_string THEN
			o.sym := sym_stringbase
		ELSIF mem.lev = 0 THEN
			o.sym := sym_varbase
			END;
		o.imm := mem.a
		END;
		
	IF (mem.mode = Base.class_ref) OR (mem.type = NIL) THEN
		o.size := 0
	ELSIF mem.type.form IN Base.types_Scalar THEN
		o.size := SHORT (SHORT (mem.type.size))
	ELSE
		o.size := 0
		END
	END Set_mem_operand;
	
PROCEDURE Set_memX_operand (VAR o : Operand; bReg, iReg, scale, imm : INTEGER);
	BEGIN
	o.form := form_mem_scale;
	o.reg1 := Decode_reg (bReg);
	o.reg2 := Decode_reg (iReg);
	o.scale := SHORT (SHORT (scale));
	o.imm := imm
	END Set_memX_operand;
	
PROCEDURE Set_reg_operand (VAR o : Operand; r : INTEGER);
	BEGIN
	o.form := form_reg;
	o.reg1 := Decode_reg (r)
	END Set_reg_operand;
	
PROCEDURE Set_imm_operand (VAR o : Operand; imm : INTEGER);
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
	
PROCEDURE Emit_op_reg_memX (op, r, bReg, iReg, scale, imm : INTEGER);
	BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r);
	Set_memX_operand (codes [pc].operands [1], bReg, iReg, scale, imm);
	Inc_program_counter
	END Emit_op_reg_memX;
	
PROCEDURE Emit_op_reg_imm (op, r, imm : INTEGER);
	BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r);
	Set_imm_operand (codes [pc].operands [1], imm);
	Inc_program_counter
	END Emit_op_reg_imm;
	
PROCEDURE Emit_op_reg_proc (op, r : INTEGER; proc : Base.Object);
	BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r);
	Set_proc_operand (codes [pc].operands [1], proc);
	Inc_program_counter
	END Emit_op_reg_proc;
	
PROCEDURE Emit_op_mem_reg (op : INTEGER; VAR mem : Base.Item; r : INTEGER);
	BEGIN
	codes [pc].op := op;
	Set_mem_operand (codes [pc].operands [0], mem);
	Set_reg_operand (codes [pc].operands [1], r);
	Inc_program_counter
	END Emit_op_mem_reg;
	
PROCEDURE Emit_op_mem_imm (op : INTEGER; VAR mem : Base.Item; imm : INTEGER);
	BEGIN
	codes [pc].op := op;
	Set_mem_operand (codes [pc].operands [0], mem);
	Set_imm_operand (codes [pc].operands [1], imm);
	Inc_program_counter
	END Emit_op_mem_imm;
	
PROCEDURE Emit_op_reg_reg_imm (op, r1, r2, imm : INTEGER);
	BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r1);
	Set_reg_operand (codes [pc].operands [1], r2);
	Set_imm_operand (codes [pc].operands [2], imm);
	Inc_program_counter
	END Emit_op_reg_reg_imm;
	
PROCEDURE Emit_op_reg_mem_imm
(op, r : INTEGER; VAR mem : Base.Item; imm : INTEGER);
	BEGIN
	codes [pc].op := op;
	Set_reg_operand (codes [pc].operands [0], r);
	Set_mem_operand (codes [pc].operands [1], mem);
	Set_imm_operand (codes [pc].operands [2], imm);
	Inc_program_counter
	END Emit_op_reg_mem_imm;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Make_const* (VAR x : Base.Item; typ : Base.Type; val : INTEGER);
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
	
PROCEDURE Make_tag_item (VAR x : Base.Item; typ : Base.Type);
	BEGIN
	x.flag := {};
	x.mode := Base.class_var;
	x.lev := -1;
	x.a := typ.tag
	END Make_tag_item;
	
PROCEDURE Alloc_type_tag* (typ : Base.Type);
	BEGIN
	IF record_no >= LEN (record_type_list) THEN
		Scanner.Mark ('Too many record types (compiler limit)')
	ELSE
		record_type_list [record_no] := typ;
		INC (record_no)
		END;
	typ.tag := tag_offset;
	INC (tag_offset, 16 + 8 * (Base.type_extension_limit + 1) + 8 * typ.num_ptr)
	END Alloc_type_tag;
	
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
	
PROCEDURE Zero_clear_reg (reg : INTEGER);
	BEGIN
	reg := Small_reg (reg, 4);
	Emit_op_reg_reg (op_XOR, reg, reg)
	END Zero_clear_reg;

PROCEDURE Inc_reg_stack;
	BEGIN
	IF reg_stack > reg_TOP THEN
		Push_reg (reg_stack)
	ELSE
		used_regs := used_regs + {Decode_reg (reg_stack)}
		END;
	INC (reg_stack);
	END Inc_reg_stack;
	
PROCEDURE Free_reg;
	BEGIN
	DEC (reg_stack);
	IF reg_stack > reg_TOP THEN Pop_reg (reg_stack) END
	END Free_reg;
	
PROCEDURE Use_register (x : Base.Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF x.mode IN {Base.mode_reg, Base.mode_regI} THEN
		result := TRUE
	ELSE
		result := FALSE
		END;
	RETURN result
	END Use_register;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Ref_to_regI (VAR x : Base.Item);
	VAR
		reg : INTEGER;
	BEGIN
	IF ~ Use_register (x) THEN
		x.r := reg_stack;
		Inc_reg_stack
		END;
	IF x.mode IN Base.cls_Variable THEN Emit_op_reg_mem (op_MOV, x.r, x) END;
	x.mode := Base.mode_regI; x.a := 0;
	END Ref_to_regI;
	
PROCEDURE load* (VAR x : Base.Item);
	BEGIN
	IF x.mode # Base.mode_reg THEN
		IF x.mode = Base.class_const THEN
			x.r := reg_stack;
			Inc_reg_stack;
			Emit_op_reg_imm (op_MOV, x.r, x.a)
		ELSE
			IF x.mode = Base.class_var THEN
				x.r := reg_stack;
				Inc_reg_stack
			ELSIF x.mode = Base.class_ref THEN
				Ref_to_regI (x)
				END;
			CASE x.type.size OF
				1, 2: Emit_op_reg_mem (op_MOVZX, x.r, x) |
				4: Emit_op_reg_mem (op_MOV, Small_reg (x.r, 4), x)
				ELSE Emit_op_reg_mem (op_MOV, x.r, x)
				END
			END;
		x.mode := Base.mode_reg;
		END
	END load;
	
PROCEDURE Load_adr (VAR x : Base.Item);
	BEGIN
	IF x.mode = Base.class_proc THEN
		Emit_op_reg_proc (op_LEA, x.r, x.proc);
		x.type := Base.nil_type
	ELSIF x.mode = Base.mode_regI THEN
		Emit_op_reg_mem (op_LEA, x.r, x)
	ELSE
		x.r := reg_stack;
		Inc_reg_stack;
		IF x.mode = Base.class_ref THEN
			Emit_op_reg_mem (op_MOV, x.r, x)
		ELSE
			Emit_op_reg_mem (op_LEA, x.r, x)
			END
		END;
	x.mode := Base.mode_reg;
	END Load_adr;	

PROCEDURE Store* (VAR x, y : Base.Item);
	VAR
		i : INTEGER;
	BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x) END;
	IF y.mode = Base.class_ref THEN Ref_to_regI (y) END;
	IF x.type.form IN Base.types_Scalar THEN
		IF y.type.form = Base.type_string THEN
			i := y.a DIV Base.char_type.size;
			Emit_op_mem_imm (op_MOV, x, ORD (strings [i]))
		ELSIF (y.mode = Base.class_const) & ~ Is_big_immediate (y.a) THEN
			Emit_op_mem_imm (op_MOV, x, y.a)
		ELSE
			load (y);
			IF x.type.size = 8 THEN
				Emit_op_mem_reg (op_MOV, x, y.r)
			ELSE
				Emit_op_mem_reg (op_MOV, x, Small_reg (y.r, x.type.size))
				END
			END
	ELSIF x.type.form = Base.type_array THEN
		Emit_op_reg_mem (op_LEA, -reg_RSI, y);
		Emit_op_reg_mem (op_LEA, -reg_RDI, x);
		IF param_regs_usage > 0 THEN Emit_op_reg (op_PUSH, -reg_RCX) END;
		IF (y.type.form = Base.type_string) & (y.type.len < x.type.len) THEN
			Emit_op_reg_imm (op_MOV, -reg_RCX, y.type.size)
		ELSE
			Emit_op_reg_imm (op_MOV, -reg_RCX, x.type.size);
			END;
		Emit_op_bare (op_REP_MOVSB);
		IF param_regs_usage > 0 THEN Emit_op_reg (op_POP, -reg_RCX) END;
		used_regs := used_regs + {reg_RSI, reg_RDI}
	ELSIF x.type.form = Base.type_record THEN
		Emit_op_reg_mem (op_LEA, -reg_RSI, y);
		Emit_op_reg_mem (op_LEA, -reg_RDI, x);
		IF param_regs_usage > 0 THEN Emit_op_reg (op_PUSH, -reg_RCX) END;
		Emit_op_reg_imm (op_MOV, -reg_RCX, x.type.size);
		Emit_op_bare (op_REP_MOVSB);
		IF param_regs_usage > 0 THEN Emit_op_reg (op_POP, -reg_RCX) END;
		used_regs := used_regs + {reg_RSI, reg_RDI}
		END;
	IF Use_register (y) THEN Free_reg END;
	IF Use_register (x) THEN Free_reg END
	END Store;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Set1* (VAR x : Base.Item);
	BEGIN
	IF x.mode = Base.mode_reg THEN
		IF x.a # 0 THEN
			IF Is_big_immediate (x.a) THEN
				Emit_op_reg_imm (op_MOV, -reg_RAX, x.a);
				Emit_op_reg_reg (op_OR, x.r, -reg_RAX)
			ELSE
				Emit_op_reg_imm (op_OR, x.r, x.a)
				END
			END
		END
	END Set1;
	
PROCEDURE Set2* (VAR x, y : Base.Item);
	VAR
		s : SET;
	BEGIN
	IF y.mode = Base.class_const THEN
		(* Delay code generation *)
		s := {};
		SYSTEM.PUT (SYSTEM.ADR (s), x.a);
		x.a := ORD (s + {y.a})
	ELSIF x.mode = Base.class_const THEN
		load (y);
		Zero_clear_reg (reg_RAX);
		Emit_op_reg_reg (op_BTS, -reg_RAX, y.r);
		Emit_op_reg_reg (op_MOV, y.r, -reg_RAX);
		x.mode := Base.mode_reg;
		x.r := y.r
	ELSE
		ASSERT (x.mode = Base.mode_reg);
		load (y);
		Emit_op_reg_reg (op_BTS, x.r, y.r);
		Free_reg
		END
	END Set2;
	
PROCEDURE Set3* (VAR x, y, z : Base.Item);
	VAR
		s : SET;
		x_is_const, y_use_reg, z_use_reg : BOOLEAN;
		
	PROCEDURE P1 (VAR e : Base.Item; dr, i : INTEGER);
		BEGIN
		IF e.mode = Base.mode_reg THEN
			Emit_op_reg_reg (op_MOV, -reg_CL, Small_reg (e.r, 1));
		ELSE
			e.type := Base.byte_type;
			Emit_op_reg_mem (op_MOV, -reg_CL, e)
			END;
		Emit_op_reg_imm (op_MOV, dr, i);
		Emit_op_reg_reg (op_SHL, dr, -reg_CL)
		END P1;
		
	PROCEDURE P2 (imm, dr, i, val : INTEGER);
		BEGIN
		IF imm < 32 THEN
			Emit_op_reg_imm (op_XOR, dr, val)
		ELSE
			Emit_op_reg_imm (op_MOV, -reg_CL, imm);
			Emit_op_reg_imm (op_MOV, -reg_RAX, i);
			Emit_op_reg_reg (op_SHL, -reg_RAX, -reg_CL);
			Emit_op_reg_reg (op_XOR, dr, -reg_RAX)
			END
		END P2;
		
	PROCEDURE Pattern (dr : INTEGER; VAR y, z : Base.Item);
		BEGIN
		IF y.mode = Base.class_const THEN
			P1 (z, dr, -2); P2 (y.a, dr, -1, ORD ({y.a .. 63}))
		ELSIF z.mode = Base.class_const THEN
			P1 (y, dr, -1); P2 (z.a, dr, -2, ORD ({z.a + 1 .. 63}))
		ELSE 
			P1 (y, -reg_RAX, -1); P1 (z, dr, -2);
			Emit_op_reg_reg (op_XOR, dr, -reg_RAX);
			END
		END Pattern;
		
	BEGIN (* Set3 *)
	IF (y.mode = Base.class_const) & (z.mode = Base.class_const) THEN
		(* Delay code generation *)
		s := {};
		SYSTEM.PUT (SYSTEM.ADR (s), x.a);
		x.a := ORD (s + {y.a .. z.a})
	ELSE
		IF y.mode = Base.class_ref THEN Ref_to_regI (y) END;
		IF z.mode = Base.class_ref THEN Ref_to_regI (z) END;
		
		x_is_const := x.mode = Base.class_const;
		y_use_reg := Use_register (y); z_use_reg := Use_register (z);
		IF x_is_const & ~ y_use_reg & ~ z_use_reg THEN Inc_reg_stack END;
		
		IF param_regs_usage > 0 THEN Push_reg (-reg_RCX) END;
			
		IF x_is_const THEN
			IF y_use_reg & z_use_reg THEN x.r := reg_stack - 2
			ELSE x.r := reg_stack - 1 END;
			x.mode := Base.mode_reg;
			Pattern (x.r, y, z)
		ELSE
			IF param_regs_usage > 1 THEN Push_reg (-reg_RDX) END;
			Pattern (-reg_RDX, y, z);
			Emit_op_reg_reg (op_OR, x.r, -reg_RDX);
			IF param_regs_usage > 1 THEN Pop_reg (-reg_RDX) END;
			END;
		
		IF param_regs_usage > 0 THEN Pop_reg (-reg_RCX) END;
		
		IF x.r = reg_stack - 3 THEN Free_reg; Free_reg
		ELSIF x.r = reg_stack - 2 THEN Free_reg
		ELSIF x.r # reg_stack - 1 THEN
			Scanner.Mark ('COMPILER FAULT: PROCEDURE Set3')
			END
		END
	END Set3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Op1* (op : INTEGER; VAR x : Base.Item);
	VAR
		s : SET;
	BEGIN
	IF op = Base.sym_not THEN
		(* Implement later *)
	ELSIF op = Base.sym_minus THEN
		IF x.mode = Base.class_const THEN
			IF x.type.form = Base.type_integer THEN
				IF x.a = Base.MIN_INT THEN
					Scanner.Mark ('Integer overflow detected')
				ELSE
					x.a := -x.a
					END
			ELSIF x.type.form = Base.type_set THEN
				s := {};
				SYSTEM.PUT (SYSTEM.ADR (s), x.a);
				SYSTEM.PUT (SYSTEM.ADR (x.a), -s)
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
				END;
			END
	ELSIF op = Base.sym_and THEN
		(* Implement later *)
	ELSIF op = Base.sym_or THEN
		(* Implement later *)
		END
	END Op1;
	
PROCEDURE Commutative_op (op : INTEGER; VAR x, y : Base.Item);
	BEGIN
	IF x.mode = Base.class_const THEN
		CASE y.mode OF
			Base.class_var:
				load (x);
				Emit_op_reg_mem (op, x.r, y) |
			Base.class_ref, Base.mode_regI:
				load (y);
				Emit_op_reg_imm (op, y.r, x.a);
				x.mode := Base.mode_reg;
				x.r := y.r |
			Base.mode_reg:
				Emit_op_reg_imm (op, y.r, x.a);
				x.mode := Base.mode_reg;
				x.r := y.r
			END
	ELSE
		ASSERT (x.mode = Base.mode_reg);
		CASE y.mode OF
			Base.class_const: Emit_op_reg_imm (op, x.r, y.a) |
			Base.class_var: Emit_op_reg_mem (op, x.r, y) |
			Base.class_ref:
				Ref_to_regI (y);
				Emit_op_reg_mem (op, x.r, y);
				Free_reg |
			Base.mode_regI:
				Emit_op_reg_mem (op, x.r, y);
				Free_reg |
			Base.mode_reg:
				Emit_op_reg_reg (op, x.r, y.r);
				Free_reg
			END
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
				load (x);
				load (y);
				Emit_op_reg_reg (op_SUB, x.r, y.r);
				Free_reg |
			Base.mode_regI:
				Emit_op_reg_mem (op_MOV, reg_stack - 1, y);
				Emit_op_reg_imm (op_MOV, y.r, x.a);
				Emit_op_reg_reg (op_SUB, y.r, reg_stack - 1);
				Free_reg;
				x.mode := Base.mode_reg;
				x.r := y.r |
			Base.mode_reg:
				Inc_reg_stack;
				Emit_op_reg_reg (op_MOV, reg_stack - 1, y.r);
				Emit_op_reg_imm (op_MOV, y.r, x.a);
				Emit_op_reg_reg (op_SUB, y.r, reg_stack - 1);
				Free_reg;
				x.mode := Base.mode_reg;
				x.r := y.r
			END
	ELSE
		ASSERT (x.mode = Base.mode_reg);
		CASE y.mode OF
			Base.class_const: Emit_op_reg_imm (op_SUB, x.r, y.a) |
			Base.class_var: Emit_op_reg_mem (op_SUB, x.r, y) |
			Base.class_ref:
				Ref_to_regI (y);
				Emit_op_reg_mem (op_SUB, x.r, y);
				Free_reg |
			Base.mode_regI:
				Emit_op_reg_mem (op_SUB, x.r, y);
				Free_reg |
			Base.mode_reg:
				Emit_op_reg_reg (op_SUB, x.r, y.r);
				Free_reg
			END
		END;
	IF Base.integer_overflow_check IN Base.compiler_flag THEN
		Emit_op_sym (op_JO, sym_integer_overflow_trap)
		END
	END Subtract;
	
PROCEDURE Multiply_by_const (VAR x : Base.Item; const : INTEGER);
	VAR
		y : Base.Item;
	BEGIN
	ASSERT (x.mode = Base.mode_reg);
	IF const = 0 THEN
		Zero_clear_reg (x.r)
	ELSIF const = 1 THEN
		(* Do nothing *)
	ELSIF const = 2 THEN
		Emit_op_reg (op_SHL, x.r);
		IF Base.integer_overflow_check IN Base.compiler_flag THEN
			Emit_op_sym (op_JO, sym_integer_overflow_trap)
			END
	ELSE
		Emit_op_reg_reg_imm (op_IMUL, x.r, x.r, const);
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
	ELSIF y.mode = Base.class_const THEN
		Multiply_by_const (x, y.a)
	ELSE
		ASSERT (x.mode = Base.mode_reg);
		CASE y.mode OF
			Base.class_var: Emit_op_reg_mem (op_IMUL, x.r, y) |
			Base.class_ref:
				Ref_to_regI (y);
				Emit_op_reg_mem (op_IMUL, x.r, y);
				Free_reg |
			Base.mode_regI:
				Emit_op_reg_mem (op_IMUL, x.r, y);
				Free_reg |
			Base.mode_reg:
				Emit_op_reg_reg (op_IMUL, x.r, y.r);
				Free_reg
			END;
		IF Base.integer_overflow_check IN Base.compiler_flag THEN
			Emit_op_sym (op_JO, sym_integer_overflow_trap)
			END
		END
	END Multiply;
	
PROCEDURE Divide_by_const
(VAR x : Base.Item; const : INTEGER; is_modulo : BOOLEAN) : BOOLEAN;
	VAR
		e : INTEGER;
		result : BOOLEAN;
	BEGIN
	ASSERT (x.mode = Base.mode_reg);
	result := TRUE;
	IF const <= 0 THEN
		Scanner.Mark ('Division by non-positive number')
	ELSIF const = 1 THEN
		IF is_modulo THEN Zero_clear_reg (x.r) END
	ELSE
		e := Base.Integer_binary_logarithm (const);
		IF e > 0 THEN
			IF is_modulo THEN
				Emit_op_reg_imm (op_AND, x.r, const - 1);
			ELSE
				Emit_op_reg_imm (op_SAR, x.r, e)
				END
		ELSE
			result := FALSE
			END
		END;
	RETURN result
	END Divide_by_const;
	
PROCEDURE Divide (VAR x, y : Base.Item; is_modulo : BOOLEAN);
	VAR
		i : INTEGER;
		flag : BOOLEAN;
	BEGIN
	load (x);
	
	IF y.mode = Base.class_const THEN
		flag := Divide_by_const (x, y.a, is_modulo);
		IF ~ flag THEN load (y) END
	ELSE
		load (y);
		IF Base.integer_overflow_check IN Base.compiler_flag THEN
			Emit_op_reg_reg (op_TEST, y.r, y.r);
			Emit_op_sym (op_JLE, sym_invalid_divisor_trap)
			END;
		flag := FALSE
		END;
		
	IF ~ flag THEN
		IF param_regs_usage > 1 THEN
			Emit_op_reg (op_PUSH, -reg_RDX)
			END;
			
		Emit_op_reg_reg (op_MOV, -reg_RAX, x.r);
		Emit_op_bare (op_CQO);
		Emit_op_reg (op_IDIV, y.r);
		Emit_op_reg_reg (op_TEST, x.r, x.r);
		Emit_op_label (op_JL, pc + 3);
		
		IF is_modulo THEN
			Emit_op_reg_reg (op_MOV, reg_stack - 2, -reg_RDX)
		ELSE
			Emit_op_reg_reg (op_MOV, reg_stack - 2, -reg_RAX)
			END;
		i := pc;
		Emit_op_label (op_JMP, 0); (* fix up later *)
		
		INCL (codes [pc].flag, flag_haslabel);
		IF is_modulo THEN
			Emit_op_reg_reg (op_ADD, -reg_RDX, y.r);
			Emit_op_reg_reg (op_MOV, reg_stack - 2, -reg_RDX)
		ELSE
			Emit_op_reg_reg (op_TEST, -reg_RDX, -reg_RDX);
			Emit_op_label (op_JE, pc + 2);
			Emit_op_reg_imm (op_SUB, -reg_RAX, 1);
			INCL (codes [pc].flag, flag_haslabel);
			Emit_op_reg_reg (op_MOV, reg_stack - 2, -reg_RAX)
			END;
			
		INCL (codes [pc].flag, flag_haslabel);
		codes [i].operands [0].imm := pc;
		
		IF param_regs_usage > 1 THEN
			Emit_op_reg (op_POP, -reg_RDX)
			END;
			
		x.r := reg_stack - 2;
		Free_reg
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
	ELSE
		ASSERT (x.mode = Base.mode_reg);
		load (y);
		Emit_op_reg (op_NOT, y.r);
		Emit_op_reg_reg (op_AND, x.r, y.r);
		Free_reg
		END
	END Difference;
	
PROCEDURE Op2* (op : INTEGER; VAR x, y : Base.Item);
	VAR
		s1, s2 : SET;
	BEGIN
	IF (x.mode = Base.class_const) & (y.mode = Base.class_const) THEN
		IF x.type.form = Base.type_integer THEN
			CASE op OF
				Base.sym_plus:
					IF Base.Is_safe_addition (x.a, y.a) THEN INC (x.a, y.a)
					ELSE Scanner.Mark ('Integer overflow detected') END |
				Base.sym_minus:
					IF Base.Is_safe_subtraction (x.a, y.a) THEN DEC (x.a, y.a)
					ELSE Scanner.Mark ('Integer overflow detected') END |
				Base.sym_times:
					IF Base.Is_safe_multiplication (x.a, y.a) THEN
						x.a := x.a * y.a
					ELSE
						Scanner.Mark ('Integer overflow detected')
						END	|
				Base.sym_div:
					IF y.a > 0 THEN x.a := x.a DIV y.a
					ELSE Scanner.Mark ('Division by non-positive number') END |
				Base.sym_mod:
					IF y.a > 0 THEN x.a := x.a MOD y.a
					ELSE Scanner.Mark ('Division by non-positive number') END
				END
		ELSIF x.type.form = Base.type_set THEN
			s1 := {};
			s2 := {};
			SYSTEM.PUT (SYSTEM.ADR (s1), x.a);
			SYSTEM.PUT (SYSTEM.ADR (s2), y.a);
			CASE op OF
				Base.sym_plus: x.a := ORD (s1 + s2) |
				Base.sym_minus: x.a := ORD (s1 - s2) |
				Base.sym_times: x.a := ORD (s1 * s2) |
				Base.sym_slash: x.a := ORD (s1 / s2) |
				END
		ELSIF x.type.form = Base.type_boolean THEN
			CASE op OF
				Base.sym_and:
					IF (x.a # 0) & (y.a # 0) THEN x.a := 1
					ELSE x.a := 0 END |
				Base.sym_or:
					IF (x.a # 0) OR (y.a # 0) THEN x.a := 1
					ELSE x.a := 0 END
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
		ELSIF x.type.form = Base.type_set THEN
			CASE op OF
				Base.sym_plus: Commutative_op (op_OR, x, y) |
				Base.sym_minus: Difference (x, y) |
				Base.sym_times: Commutative_op (op_AND, x, y) |
				Base.sym_slash: Commutative_op (op_XOR, x, y) |
				END
			END
		END
	END Op2;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Selector operation *)
	
PROCEDURE Field* (VAR x : Base.Item; field : Base.Object);
	BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x) END;
	INC (x.a, field.val);
	x.flag := x.flag - {Base.flag_varParam, Base.flag_param}
	END Field;
	
PROCEDURE Deref* (VAR x : Base.Item);
	BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x) END;
	Ref_to_regI (x);
	x.type := x.type.base;
	EXCL (x.flag, Base.flag_readOnly)
	END Deref;
	
PROCEDURE Open_array_index (VAR x, y : Base.Item);
	VAR
		size, scale, e : INTEGER;

	PROCEDURE Calculate_offset
	(VAR y : Base.Item; typ : Base.Type; len_offset : INTEGER);
		VAR
			size, e : INTEGER;
			len : Base.Item;
		BEGIN
		len.flag := {Base.flag_param};
		len.mode := Base.class_var;
		len.a := len_offset;
		len.lev := Base.cur_lev;
		
		Emit_op_reg_mem (op_IMUL, y.r, len);
		
		size := typ.base.size;
		IF size > 0 THEN
			e := Base.Integer_binary_logarithm (size);
			IF e = 0 THEN
				(* Do nothing *)
			ELSIF e > 0 THEN
				Emit_op_reg_imm (op_SHL, y.r, e)
			ELSE
				Emit_op_reg_reg_imm (op_IMUL, y.r, y.r, size)
				END;
		ELSE
			ASSERT (typ.base.form = Base.type_array);
			Calculate_offset (y, typ.base, len_offset + 8)
			END
		END Calculate_offset;
		
	PROCEDURE Check_array_index (VAR x, y : Base.Item);
		VAR
			len : Base.Item;
		BEGIN
		IF Base.array_bound_check IN Base.compiler_flag THEN
			len.flag := {Base.flag_param};
			len.mode := Base.class_var;
			len.a := x.b;
			len.lev := Base.cur_lev;
			
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
	ASSERT (x.mode = Base.mode_regI);
	IF y.mode # Base.class_const THEN load (y)
	ELSIF y.a < 0 THEN Scanner.Mark ('Array index out of bound') END;
	Check_array_index (x, y);
	
	size := x.type.base.size;
	IF size > 0 THEN
		IF y.mode = Base.class_const THEN
			INC (x.a, y.a * size)
		ELSE
			e := Base.Integer_binary_logarithm (size);
			IF e < 0 THEN
				Emit_op_reg_reg_imm (op_IMUL, y.r, y.r, size);
				scale := 0
			ELSIF e > 3 THEN
				Emit_op_reg_imm (op_SHL, y.r, e);
				scale := 0
			ELSE
				scale := size
				END;
			Emit_op_reg_memX (op_LEA, reg_stack - 2, x.r, y.r, scale, x.a);
			Free_reg;
			x.r := reg_stack - 1;
			x.a := 0
			END
	ELSE
		ASSERT (x.type.base.form = Base.type_array);
		load (y);
		Calculate_offset (y, x.type.base, x.b + 8);
		Emit_op_reg_memX (op_LEA, reg_stack - 2, x.r, y.r, 0, x.a);
		Free_reg;
		x.r := reg_stack - 1;
		x.a := 0
		END;
	INC (x.b, 8)
	END Open_array_index;
	
PROCEDURE Index* (VAR x, y : Base.Item);
	VAR
		scale, e, size : INTEGER;
	BEGIN
	IF x.type.len < 0 THEN
		IF x.mode = Base.class_ref THEN
			x.b := x.a + 8;
			Ref_to_regI (x)
			END;
		Open_array_index (x, y)
	ELSE
		IF x.mode = Base.class_ref THEN	Ref_to_regI (x) END;
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
				Emit_op_reg_reg_imm (op_IMUL, y.r, y.r, size);
				scale := 0
			ELSIF e > 3 THEN
				Emit_op_reg_imm (op_SHL, y.r, e);
				scale := 0
			ELSE
				scale := size
				END;
				
			IF x.mode = Base.mode_regI THEN
				Emit_op_reg_memX (op_LEA, reg_stack - 2, x.r, y.r, scale, x.a);
				Free_reg
			ELSE
				ASSERT (x.mode = Base.class_var);
				Emit_op_reg_memX (op_LEA, y.r, reg_stackBase, y.r, scale, x.a);
				x.mode := Base.mode_regI
				END;
			x.r := reg_stack - 1;
			x.a := 0
			END
		END;
	x.type := x.type.base;
	END Index;
	
PROCEDURE Type_guard* (VAR x : Base.Item; typ : Base.Type);
	VAR
		tag1, tag2 : Base.Item;
	BEGIN
	IF x.type # typ THEN
		Make_tag_item (tag2, typ);
		Emit_op_reg_mem (op_LEA, -reg_RAX, tag2);
		
		IF x.type.form = Base.type_pointer THEN
			load (x);
			tag1.flag := {};
			tag1.mode := Base.mode_regI;
			tag1.r := x.r;
			tag1.a := -8;
			Inc_reg_stack;
			Emit_op_reg_mem (op_MOV, reg_stack - 1, tag1);
			tag1.r := reg_stack - 1;
		ELSE
			(* x is record variable parameter *)
			tag1.flag := {Base.flag_param};
			tag1.mode := Base.class_ref;
			tag1.lev := x.lev;
			tag1.a := x.a + 8;
			Ref_to_regI (tag1)
			END;
			
		tag1.a := 8 + typ.len * 8;
		Emit_op_reg_mem (op_CMP, -reg_RAX, tag1);
		Emit_op_sym (op_JNE, sym_type_check_trap);
		Free_reg;
		x.type := typ;
		END;
	END Type_guard;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Scalar_comparison (op : INTEGER; VAR x, y : Base.Item);
	VAR
		r1, r2 : INTEGER;
	BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x)
	ELSIF x.mode = Base.class_proc THEN Load_adr (x)
	ELSE Handle_big_const (x) END;
	
	IF y.mode = Base.class_ref THEN Ref_to_regI (y)
	ELSIF y.mode = Base.class_proc THEN Load_adr (x)
	ELSIF y.mode = Base.mode_cond THEN load (x)
	ELSE Handle_big_const (x) END;
	
	IF (x.mode IN Base.cls_Variable) & (y.mode IN Base.cls_Variable) THEN
		load (x)
		END;
	
	IF x.mode = Base.mode_reg THEN
		r1 := Small_reg (x.r, x.type.size);
		IF y.mode = Base.class_const THEN
			Emit_op_reg_imm (op_CMP, r1, y.a)
		ELSIF y.mode IN Base.cls_Variable THEN
			Emit_op_reg_mem (op_CMP, r1, y)
		ELSIF y.mode = Base.mode_reg THEN
			r2 := Small_reg (y.r, y.type.size);
			Emit_op_reg_reg (op_CMP, r1, r2)
			END
	ELSIF x.mode IN Base.cls_Variable THEN
		IF y.mode = Base.class_const THEN
			Emit_op_mem_imm (op_CMP, x, y.a)
		ELSIF y.mode = Base.mode_reg THEN
			r2 := Small_reg (y.r, y.type.size);
			Emit_op_mem_imm (op_CMP, x, r2)
			END
	ELSIF x.mode = Base.class_const THEN
		IF y.mode = Base.mode_reg THEN
			r2 := Small_reg (y.r, y.type.size);
			Emit_op_reg_imm (op_CMP, r2, x.a)
		ELSIF y.mode IN Base.cls_Variable THEN
			Emit_op_mem_imm (op_CMP, y, x.a)
			END;
		IF (op = Base.sym_equal) OR (op = Base.sym_not_equal) THEN
		ELSIF 
		END;
	END Scalar_comparison;
	
PROCEDURE Relation* (op : INTEGER; VAR x, y : Base.Item);
	BEGIN
	(* Implement later *)
	IF (x.mode = Base.class_const) & (y.mode = Base.class_const) THEN
		IF op = Base.sym_equal THEN IF x.a = y.a THEN x.a = 1 ELSE x.a = 0 END
		ELSIF op = Base.sym_not_equal THEN IF x.a # y.a THEN x.a = 1 ELSE x.a = 0 END
		ELSIF op = Base.sym_greater THEN IF x.a > y.a THEN x.a = 1 ELSE x.a = 0 END
		ELSIF op = Base.sym_greater_equal THEN IF x.a >= y.a THEN x.a = 1 ELSE x.a = 0 END
		ELSIF op = Base.sym_less THEN IF x.a < y.a THEN x.a = 1 ELSE x.a = 0 END
		ELSIF op = Base.sym_less_equal THEN IF x.a <= y.a THEN x.a = 1 ELSE x.a = 0 END END;
	IF (x.type.form IN Base.types_Scalar)
	& (y.type.form IN Base.types_Scalar) THEN
		
		ELSIF x.mode = Base.class_const
				
				
		IF x.mode IN Base.cls_Variable THEN
	x.type := Base.bool_type
	END Relation;
	
PROCEDURE Membership* (VAR x, y : Base.Item);
	BEGIN
	(* Implement later *)
	END Membership;
	
PROCEDURE Inclusion* (VAR x, y : Base.Item);
	BEGIN
	(* Implement later *)
	END Inclusion;
	
PROCEDURE Type_test* (VAR x : Base.Item; typ : Base.Type);
	BEGIN
	(* Implement later *)
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

PROCEDURE Enter* (proc : Base.Object; locblksize : INTEGER);
	VAR
		i, k : INTEGER;
		par : Base.Item;
	BEGIN
	Emit_op_reg (op_PUSH, -reg_RBP); (* Instruction no. 0 *)
	Emit_op_reg_reg (op_MOV, -reg_RBP, -reg_RSP); (* No. 1 *)
	Emit_op_reg_imm (op_SUB, -reg_RSP, 0); (* No. 2 *)
	
	stack_frame_usage := locblksize;
	stack_frame_size := locblksize;
	
	k := proc.parblksize DIV 8;
	IF k <= 4 THEN
		Base.top_scope.parblksize := 32
	ELSE
		Base.top_scope.parblksize := proc.parblksize;
		k := 4;
		END;
		
	par.mode := Base.class_var;
	par.lev := Base.cur_lev;
	par.flag := {Base.flag_param};
	par.a := 0;
	
	FOR i := 0 TO 3 DO
		Emit_op_mem_reg (op_MOV, par, -reg_RCX - i); (* No. 3 to 6 *)
		IF i >= k THEN codes [pc - 1].flag := {flag_skiped} END;
		INC (par.a, 8)
		END;
	
	FOR i := reg_R12 TO reg_RDI DO
		Emit_op_reg (op_PUSH, -i)	(* No. 7 to 12 *)
		END;
	
	reg_stack := 0;
	mem_stack := 0;
	used_regs := {}
	END Enter;
	
PROCEDURE Return_value* (VAR x : Base.Item);
	BEGIN
	CASE x.mode OF
		Base.mode_reg:
			Emit_op_reg_reg (op_MOV, -reg_RAX, x.r);
			Free_reg |
		Base.class_const:
			Emit_op_reg_imm (op_MOV, -reg_RAX, x.a) |
		Base.class_var:
			Emit_op_reg_mem (op_MOV, -reg_RAX, x) |
		Base.class_ref:
			Ref_to_regI (x);
			Emit_op_reg_mem (op_MOV, -reg_RAX, x);
			Free_reg |
		Base.mode_regI:
			Emit_op_reg_mem (op_MOV, -reg_RAX, x);
			Free_reg
		END
	END Return_value;
	
PROCEDURE Return* (proc : Base.Object; locblksize : INTEGER);
	VAR
		num_of_used_regs, i : INTEGER;
	BEGIN
	num_of_used_regs := 6;
	FOR i := reg_RDI TO reg_R12 BY -1 DO
		IF i IN used_regs THEN
			Emit_op_reg (op_POP, -i)
		ELSE
			codes [12 - reg_RDI + i].flag := {flag_skiped};
			DEC (num_of_used_regs)
			END;
		END;
	
	Emit_op_bare (op_LEAVE);
	Emit_op_bare (op_RET);
	
	(* Perform fixup *)
	IF stack_frame_size = 0 THEN
		IF num_of_used_regs MOD 2 # 0 THEN codes [2].operands [1].imm := 8
		ELSE codes [2].flag := {flag_skiped} END
	ELSE
		INC (stack_frame_size, num_of_used_regs * 8);
		IF stack_frame_size MOD 16 # 0 THEN
			stack_frame_size := (stack_frame_size DIV 16 + 1) * 16
			END;
		codes [2].operands [1].imm := stack_frame_size - num_of_used_regs * 8;
		END;
	Write_codes_to_file
	END Return;
	
PROCEDURE Prepare_to_call* (VAR x : Base.Item);
	VAR
		i, top : INTEGER;
	BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x) END;
	
	x.d := param_regs_usage; (* Use for remembering previous param regs usage *)
	IF param_regs_usage > 0 THEN
		FOR i := 0 TO param_regs_usage - 1 DO Push_reg (-reg_RCX - i) END;
		param_regs_usage := 0
		END;
	
	x.c := 0; (* Use for tracking temporary stack space usage *)
	
	IF x.mode = Base.class_proc THEN x.b := x.proc.parblksize 
	ELSE x.b := x.type.len END; (* Store the size of parameter block *)
	IF x.b < 32 THEN x.b := 32 END;
	INC (mem_stack, x.b);
	IF mem_stack MOD 16 = 8 THEN INC (x.b, 8); INC (mem_stack, 8) END;
	
	Emit_op_reg_imm (op_SUB, -reg_RSP, x.b);
	x.e := mem_stack (* Store the location of parameter block *)
	END Prepare_to_call;
	
PROCEDURE Call* (VAR x : Base.Item);
	VAR
		temp1, temp2 : Base.Item;
		i : INTEGER;

	PROCEDURE Need_saving (reg : INTEGER) : BOOLEAN;
		VAR
			result : BOOLEAN;
		BEGIN
		IF (reg >= 0) & (reg <= reg_TOP) & (reg_stack > reg) THEN
			result := TRUE
		ELSE
			result := FALSE
			END;
		RETURN result
		END Need_saving;
		
	BEGIN (* Call *)
	IF Need_saving (reg_R10) THEN
		temp1.mode := Base.class_var;
		temp1.lev := 1;
		temp1.a := stack_frame_usage;
		Inc_stack_frame_usage (8);
		Emit_op_mem_reg (op_MOV, temp1, -reg_R10)
		END;
	IF Need_saving (reg_R11) THEN
		temp2.mode := Base.class_var;
		temp2.lev := 1;
		temp2.a := stack_frame_usage;
		Inc_stack_frame_usage (8);
		Emit_op_mem_reg (op_MOV, temp2, -reg_R11)
		END;
	
	IF x.mode = Base.class_proc THEN Emit_op_proc (op_CALL, x.proc)
	ELSIF x.mode = Base.mode_reg THEN Emit_op_reg (op_CALL, x.r)
	ELSE Emit_op_mem (op_CALL, x) END;
		
	IF Need_saving (reg_R11) THEN
		Emit_op_reg_mem (op_MOV, -reg_R11, temp2);
		DEC (stack_frame_usage, 8)
		END;
	IF Need_saving (reg_R10) THEN
		Emit_op_reg_mem (op_MOV, -reg_R10, temp1);
		DEC (stack_frame_usage, 8)
		END;
	
	(* Unwind the stack area used for parameters *)
	Emit_op_reg_imm (op_ADD, -reg_RSP, x.b);
	DEC (mem_stack, x.b);
	
	(* Release temporary stack frame, if there are any *)
	DEC (stack_frame_usage, x.c);
	
	(* Restore the previous parameter registers, if there are any *)
	param_regs_usage := x.d;
	IF param_regs_usage > 0 THEN
		FOR i := param_regs_usage - 1 TO 0 BY -1 DO
			Pop_reg (-reg_RCX - i)
			END
		END;
	
	(* Return value *)
	IF x.mode = Base.class_proc THEN
		IF x.type # NIL THEN
			x.mode := Base.mode_reg;
			x.r := reg_stack;
			Inc_reg_stack;
			Emit_op_reg_reg (op_MOV, x.r, -reg_RAX)
			END;
		x.flag := {}
	ELSE (* x is procedure variable *)
		IF x.type.base # NIL THEN
			IF ~ Use_register (x) THEN
				x.r := reg_stack;
				Inc_reg_stack
				END;
			x.mode := Base.mode_reg;
			Emit_op_reg_reg (op_MOV, x.r, -reg_RAX)
		ELSIF Use_register (x) THEN
			Free_reg
			END;
		x.flag := {}
		END
	END Call;
	
PROCEDURE Normal_parameter* (VAR x, proc : Base.Item; adr : INTEGER);
	VAR
		param_reg : INTEGER;
		param : Base.Item;
	BEGIN
	IF adr < 32 THEN
		param_reg := reg_RCX + (adr DIV 8);
		CASE x.mode OF
			Base.mode_reg:
				Emit_op_reg_reg (op_MOV, -param_reg, x.r);
				Free_reg |
			Base.class_const:
				Emit_op_reg_imm (op_MOV, -param_reg, x.a) |
			Base.class_ref:
				Emit_op_reg_mem (op_MOV, -param_reg, x) |
			Base.class_proc:
				Emit_op_reg_proc (op_LEA, -param_reg, x.proc) |
			Base.class_var, Base.mode_regI:
				IF (x.type = NIL) OR ~ (x.type.form IN Base.types_Scalar)
				OR (x.type.size = 8) THEN
					Emit_op_reg_mem (op_MOV, -param_reg, x)
				ELSE
					Emit_op_reg_mem
					(op_MOV, Small_reg (param_reg, x.type.size), x)
					END;
				IF x.mode = Base.mode_regI THEN Free_reg END
			END;
		INC (param_regs_usage)
	ELSE
		load (x);
		param.mode := Base.mode_regI;
		param.r := -reg_RSP;
		param.a := mem_stack - x.e + adr - 32;
		Emit_op_mem_reg (op_MOV, param, x.r);
		Free_reg
		END
	END Normal_parameter;
	
PROCEDURE Reference_parameter* (VAR x, proc : Base.Item; adr : INTEGER);
	VAR
		param_reg : INTEGER;
	BEGIN
	IF x.mode = Base.class_ref THEN
		Normal_parameter (x, proc, adr)
	ELSIF adr < 32 THEN
		param_reg := reg_RCX + (adr DIV 8);
		Emit_op_reg_mem (op_LEA, -param_reg, x);
		IF Use_register (x) THEN Free_reg END;
		INC (param_regs_usage)
	ELSE
		Load_adr (x);
		Normal_parameter (x, proc, adr)
		END
	END Reference_parameter;
	
PROCEDURE Record_variable_parameter* (VAR x, proc : Base.Item; adr : INTEGER);
	VAR
		tag : Base.Item;
	BEGIN
	IF Base.flag_varParam IN x.flag THEN
		tag.flag := {Base.flag_param};
		tag.mode := Base.class_ref;
		tag.lev := x.lev;
		tag.a := x.a + 8
	ELSE
		Make_tag_item (tag, x.type)
		END;
	Reference_parameter (x, proc, adr);
	Reference_parameter (tag, proc, adr + 8)
	END Record_variable_parameter;
	
PROCEDURE Open_array_parameter* (VAR x, proc : Base.Item; par : Base.Object);
	VAR
		len : Base.Item;
		tp, formal_type : Base.Type;
		adr : INTEGER;
	BEGIN
	adr := par.val;
	Reference_parameter (x, proc, adr);
	
	formal_type := par.type;
	tp := x.type;
	WHILE (formal_type.form = Base.type_array) & (formal_type.len < 0) DO
		INC (adr, 8);
		IF (tp.form = Base.type_array) & (tp.len < 0) THEN
			len.mode := Base.class_var;
			len.lev := x.lev;
			len.a := x.b;
			INC (x.b, 8)
		ELSE
			len.mode := Base.class_const;
			len.a := tp.len;
			END;
		Normal_parameter (len, proc, adr);
		formal_type := formal_type.base;
		tp := tp.base
		END
	END Open_array_parameter;
	
PROCEDURE String_parameter* (VAR x, proc : Base.Item; par : Base.Object);
	VAR
		temp_array : Base.Item;
	BEGIN
	temp_array.mode := Base.class_var;
	temp_array.lev := 1;
	temp_array.type := par.type;
	temp_array.a := stack_frame_usage;
	
	INC (proc.c, temp_array.type.size);
	Inc_stack_frame_usage (temp_array.type.size);
	
	Store (temp_array, x);
	Reference_parameter (temp_array, proc, par.val)
	END String_parameter;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init_type_tag;
	VAR
		i, j : INTEGER;
		tp : Base.Type;
		basetps : ARRAY Base.type_extension_limit + 1 OF Base.Type;
		dtag, tag : Base.Item;
	BEGIN
	FOR i := 0 TO record_no - 1 DO
		tp := record_type_list [i];
		Make_tag_item (dtag, tp);
		INC (dtag.a, 8);
		basetps [tp.len] := tp;
		
		WHILE tp.base # NIL DO
			tp := tp.base;
			basetps [tp.len] := tp
			END;
		j := 0;
		WHILE j <= Base.type_extension_limit DO
			IF basetps [j] # NIL THEN
				Make_tag_item (tag, basetps [j]);
				Emit_op_reg_mem (op_LEA, -reg_RAX, tag);
				Emit_op_mem_reg (op_MOV, dtag, -reg_RAX);
				INC (dtag.a, 8);
				basetps [j] := NIL;
				INC (j)
			ELSE
				j := Base.type_extension_limit + 1
				END
			END
		END
	END Init_type_tag;

PROCEDURE Module_init*;
	BEGIN
	Emit_op_reg (op_PUSH, -reg_RBP);
	Emit_op_reg_reg (op_MOV, -reg_RBP, -reg_RSP);
	Emit_op_reg_imm (op_SUB, -reg_RSP, 0);
	
	Init_type_tag;
	
	stack_frame_usage := 0;
	stack_frame_size := 0;
	reg_stack := 0;
	mem_stack := 0;
	used_regs := {}
	END Module_init;

PROCEDURE End_module_init*;
	BEGIN
	Emit_op_reg_imm (op_SUB, -reg_RSP, 32);
	Emit_op_reg_imm (op_MOV, -reg_ECX, 0);
	Emit_op_sym (op_CALL, Base.Make_string ('[@ExitProcess]'));
	
	(* Perform fixup *)
	IF stack_frame_size = 0 THEN
		codes [2].flag := {flag_skiped}
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

PROCEDURE SProc_LoadLibrary* (VAR x, y : Base.Item);
	VAR
		i : INTEGER;
	BEGIN
	IF Use_register (x) & (x.r IN {reg_R10, reg_R11}) THEN
		Push_reg (-x.r)
		END;
	IF mem_stack MOD 16 # 0 THEN
		i := 40
	ELSE
		i := 32
		END;
	Emit_op_reg_imm (op_SUB, -reg_RSP, i);
	
	Load_adr (y);
	Emit_op_reg_reg (op_MOV, -reg_RCX, y.r);
	Emit_op_sym (op_CALL, Base.Make_string ('[@LoadLibrary]'));
	
	Emit_op_reg_imm (op_ADD, -reg_RSP, i);
	IF Use_register (x) & (x.r IN {reg_R10, reg_R11}) THEN
		Pop_reg (-x.r)
		END;
	Emit_op_mem_reg (op_MOV, x, -reg_RAX);
	
	Free_reg;
	IF Use_register (x) THEN Free_reg END
	END SProc_LoadLibrary;
	
PROCEDURE SProc_GetProcAddress* (VAR x, y, z : Base.Item);
	VAR
		i : INTEGER;
	BEGIN
	IF Use_register (x) & (x.r IN {reg_R10, reg_R11}) THEN
		Push_reg (-x.r)
		END;
	IF mem_stack MOD 16 # 0 THEN
		i := 40
	ELSE
		i := 32
		END;
	Emit_op_reg_imm (op_SUB, -reg_RSP, i);
	
	load (y);
	Emit_op_reg_reg (op_MOV, -reg_RCX, y.r);
	Load_adr (z);
	Emit_op_reg_reg (op_MOV, -reg_RDX, z.r);
	Emit_op_sym (op_CALL, Base.Make_string ('[@GetProcAddress]'));
	
	Emit_op_reg_imm (op_ADD, -reg_RSP, i);
	IF Use_register (x) & (x.r IN {reg_R10, reg_R11}) THEN
		Pop_reg (-x.r)
		END;
	Emit_op_mem_reg (op_MOV, x, -reg_RAX);
	
	Free_reg;
	Free_reg;
	IF Use_register (x) THEN Free_reg END
	END SProc_GetProcAddress;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init* (module_name : Base.String);
	VAR
		i : INTEGER;
		filename : ARRAY 256 OF CHAR;
	BEGIN
	FOR i := 0 TO module_name.len DO
		filename [i] := module_name.content [i]
		END;
	i := module_name.len;
	filename [i] := '.';
	filename [i + 1] := 'a';
	filename [i + 2] := 's';
	filename [i + 3] := 'm';
	Base.Rewrite (out, filename);
	
	modid := module_name;

	Base.Write_string (out, 'format PE64');
	Base.Write_newline (out);
	Base.Write_string (out, 'entry ');
	Base.Write_string (out, module_name.content);
	Base.Write_string (out, '@@INIT');
	Base.Write_newline (out);
	Base.Write_newline (out);
	Base.Write_string (out, "section '.text' code readable executable");
	Base.Write_newline (out);
	Base.Write_newline (out)
	END Init;
	
PROCEDURE Generate_trap_section;
	VAR
		i : INTEGER;
	BEGIN
	FOR i := 1 TO 4 DO
		CASE i OF
			1: Base.Write_string (out, sym_array_index_trap.content) |
			2: Base.Write_string (out, sym_integer_overflow_trap.content) |
			3: Base.Write_string (out, sym_invalid_divisor_trap.content) |
			4: Base.Write_string (out, sym_type_check_trap.content)
			END;
		Base.Write_char (out, ':');
		Base.Write_newline (out);
		Base.Write_string (out, 'AND	rsp, -16');
		Base.Write_newline (out);
		Base.Write_string (out, 'SUB	rsp, 32');
		Base.Write_newline (out);
		Base.Write_string (out, 'MOV	ecx, -');
		Base.Write_number (out, i);
		Base.Write_newline (out);
		Base.Write_string (out, 'CALL	[@ExitProcess]');
		Base.Write_newline (out);
		END;
	Base.Write_newline (out)
	END Generate_trap_section;
	
PROCEDURE Generate_tag_section;
	VAR
		i : INTEGER;
		
	PROCEDURE Emit_pointer_offset (typ : Base.Type; offset : INTEGER);
		VAR
			field : Base.Object;
			n : INTEGER;
		BEGIN
		IF typ.form = Base.type_record THEN
			n := typ.num_ptr;
			IF (typ.base # NIL) & (typ.base.num_ptr > 0) THEN
				Emit_pointer_offset (typ.base, offset);
				DEC (n, typ.base.num_ptr)
				END;
			IF n > 0 THEN
				field := typ.fields;
				WHILE n > 0 DO
					IF field.type.num_ptr > 0 THEN
						IF field.type.form = Base.type_pointer THEN
							Base.Write_char (out, ',');
							Base.Write_number (out, offset + field.val);
							DEC (n)
						ELSE
							Emit_pointer_offset (typ.base, offset + field.val);
							DEC (n, typ.base.num_ptr)
							END
						END;
					field := field.next
					END
				END
		ELSIF typ.form = Base.type_array THEN
			IF typ.base.form = Base.type_pointer THEN
				FOR n := 0 TO typ.len - 1 DO
					Base.Write_char (out, ',');
					Base.Write_number (out, offset + n * 8)
					END
			ELSE
				FOR n := 0 TO typ.len - 1 DO
					Emit_pointer_offset (typ.base, offset + n * 8)
					END
				END
			END
		END Emit_pointer_offset;
		
	PROCEDURE Emit_record_tag (typ : Base.Type);
		VAR
			i : INTEGER;
		BEGIN
		Base.Write_number (out, typ.size);
		FOR i := 0 TO Base.type_extension_limit DO
			Base.Write_string (out, ',0')
			END;
		IF typ.num_ptr > 0 THEN
			Emit_pointer_offset (typ, 0)
			END;
		Base.Write_string (out, ',-1')
		END Emit_record_tag;
		
	BEGIN (* Generate_tag_section *)
	Base.Write_string (out, modid.content);
	Base.Write_string (out, '@@');
	Base.Write_string (out, sym_tagbase.content);
	Base.Write_string (out, ' dq ');
	Emit_record_tag (record_type_list [0]);
	i := 1;
	WHILE i < record_no DO
		Base.Write_char (out, ',');
		Emit_record_tag (record_type_list [i]);
		INC (i)
		END;
	Base.Write_newline (out)
	END Generate_tag_section;
	
PROCEDURE Generate_string_section;
	VAR
		i : INTEGER;
	BEGIN
	Base.Write_string (out, modid.content);
	Base.Write_string (out, '@@');
	Base.Write_string (out, sym_stringbase.content);
	Base.Write_string (out, ' db ');
	Base.Write_number (out, ORD (strings [0]));
	i := 1;
	WHILE i < str_offset DO
		Base.Write_char (out, ',');
		Base.Write_number (out, ORD (strings [i]));
		INC (i)
		END;
	Base.Write_newline (out)
	END Generate_string_section;
	
PROCEDURE Generate_variable_section (vars_size : INTEGER);
	BEGIN
	Base.Write_string (out, modid.content);
	Base.Write_string (out, '@@');
	Base.Write_string (out, sym_varbase.content);
	Base.Write_string (out, ' db ');
	Base.Write_number (out, vars_size);
	Base.Write_string (out, ' dup ?');
	Base.Write_newline (out)
	END Generate_variable_section;
	
PROCEDURE Finish* (vars_size : INTEGER);
	VAR
		i : INTEGER;
	BEGIN
	Generate_trap_section;	
	Base.Write_string (out, "section '.data' data readable writable");
	Base.Write_newline (out);
	IF tag_offset > 0 THEN Generate_tag_section END;
	IF str_offset > 0 THEN Generate_string_section END;
	IF vars_size > 0 THEN Generate_variable_section (vars_size) END;
	Base.Write_newline (out);
	
	Base.Write_string (out, "section '.idata' import data readable writeable");
	Base.Write_newline (out);
	Base.Write_string (out, 'dd 0,0,0,RVA @kernel_name,RVA @kernel_table');
	Base.Write_newline (out);
	Base.Write_string (out, 'dd 0,0,0,0,0');
	Base.Write_newline (out);
	Base.Write_string (out, '@kernel_table:');
	Base.Write_newline (out);
	Base.Write_string (out, '@ExitProcess dq RVA @nameofExitProcess');
	Base.Write_newline (out);
	Base.Write_string (out, '@LoadLibrary dq RVA @nameofLoadLibrary');
	Base.Write_newline (out);
	Base.Write_string (out, '@GetProcAddress dq RVA @nameofGetProcAddress');
	Base.Write_newline (out);
	Base.Write_string (out, 'dq 0');
	Base.Write_newline (out);
	Base.Write_string (out, "@kernel_name db 'KERNEL32.DLL',0");
	Base.Write_newline (out);
	Base.Write_string (out, '@nameofExitProcess dw 0');
	Base.Write_newline (out);
	Base.Write_string (out, "db 'ExitProcess',0");
	Base.Write_newline (out);
	Base.Write_string (out, '@nameofLoadLibrary dw 0');
	Base.Write_newline (out);
	Base.Write_string (out, "db 'LoadLibraryA',0");
	Base.Write_newline (out);
	Base.Write_string (out, '@nameofGetProcAddress dw 0');
	Base.Write_newline (out);
	Base.Write_string (out, "db 'GetProcAddress',0");
	Base.Write_newline (out);
	
	Base.Close (out)
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
	op_table [op_JBE] := 'jbe'
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
	END Init_regtable;
	
PROCEDURE Init_symbols;
	BEGIN
	sym_array_index_trap := Base.Make_string ('INVALID_ARRAY_INDEX_TRAP');
	sym_integer_overflow_trap := Base.Make_string ('INTEGER_OVERFLOW_TRAP');
	sym_invalid_divisor_trap := Base.Make_string ('INVALID_DIVISOR_TRAP');
	sym_type_check_trap := Base.Make_string ('TYPE_CHECK_TRAP');
	
	sym_varbase := Base.Make_string ('VAR');
	sym_stringbase := Base.Make_string ('STRING');
	sym_tagbase := Base.Make_string ('TYPETAG')
	END Init_symbols;
	
BEGIN
Init_optable;
Init_regtable;
Init_symbols
END Generator.
