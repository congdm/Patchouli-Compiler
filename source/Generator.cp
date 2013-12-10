MODULE Generator;

IMPORT
	SYSTEM, Base, Scanner;
	
CONST
	form_nothing = 0; form_reg = 1; form_imm = 2; form_mem_reg = 3;
	form_mem_pc = 4; form_mem_scale = 5; form_proc_name = 6; form_sym = 7;
	form_label = 8;
	
	flag_haslabel = 0; flag_skiped = 1;
	
	op_NOP = 0;
	op_MOV = 1; op_MOVSX = 2; op_MOVZX = 3; op_LEA = 4; op_PUSH = 5; op_POP = 6;
	op_XCHG = 7; op_CQO = 8; op_REP_MOVSB = 9;
	
	op_ADD = 20; op_SUB = 21; op_IMUL = 22; op_IDIV = 23; op_NEG = 24;
	op_AND = 25; op_OR = 26; op_XOR = 27; op_NOT = 28;
	
	op_CALL = 40; op_RET = 41; op_LEAVE = 42;
	
	reg_R12 = 0; reg_R13 = 1; reg_R14 = 2; reg_R15 = 3;
	reg_R10 = 4; reg_R11 = 5; reg_TOP = 5;
	
	reg_RCX = 6; reg_RDX = 7; reg_R8 = 8; reg_R9 = 9;
	reg_RSI = 10; reg_RDI = 11; reg_RAX = 12;
	reg_RBX = 13; reg_RBP = 14; reg_RSP = 15;
	
	reg_EAX = reg_RAX + 64; reg_EBX = reg_RBX + 64; reg_ECX = reg_RCX + 64;
	reg_EDX = reg_RDX + 64; reg_ESI = reg_RSI + 64; reg_EDI = reg_RDI + 64;
	reg_EBP = reg_RBP + 64; reg_ESP = reg_RSP + 64; reg_R8D = reg_R8 + 64;
	reg_R9D = reg_R9 + 64; reg_R10D = reg_R10 + 64; reg_R11D = reg_R11 + 64;
	reg_R12D = reg_R12 + 64; reg_R13D = reg_R13 + 64; reg_R14D = reg_R14 + 64;
	reg_R15D = reg_R15 + 64;
	
	reg_AX = reg_RAX + 128; reg_BX = reg_RBX + 128; reg_CX = reg_RCX + 128;
	reg_DX = reg_RDX + 128; reg_SI = reg_RSI + 128; reg_DI = reg_RDI + 128;
	reg_BP = reg_RBP + 128; reg_SP = reg_RSP + 128; reg_R8W = reg_R8 + 128;
	reg_R9W = reg_R9 + 128; reg_R10W = reg_R10 + 128; reg_R11W = reg_R11 + 128;
	reg_R12W = reg_R12 + 128; reg_R13W = reg_R13 + 128; reg_R14W = reg_R14 + 128;
	reg_R15W = reg_R15 + 128;
	
	reg_AL = reg_RAX + 192; reg_BL = reg_RBX + 192; reg_CL = reg_RCX + 192;
	reg_DL = reg_RDX + 192; reg_SIL = reg_RSI + 192; reg_DIL = reg_RDI + 192;
	reg_BPL = reg_RBP + 192; reg_SPL = reg_RSP + 192; reg_R8L = reg_R8 + 192;
	reg_R9L = reg_R9 + 192; reg_R10L = reg_R10 + 192; reg_R11L = reg_R11 + 192;
	reg_R12L = reg_R12 + 192; reg_R13L = reg_R13 + 192; reg_R14L = reg_R14 + 192;
	reg_R15L = reg_R15 + 192;

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
	sym_array_index_trap : Base.String;
	sym_varbase, sym_stringbase : Base.String;

	out : Base.FileHandle;
	codes : ARRAY 20000 OF Instruction;
	strings : ARRAY 131072 OF CHAR;
	modid : Base.String;
	
	pc : INTEGER;
	str_offset : INTEGER;
	reg_stack, mem_stack : INTEGER;
	used_regs : SET;
	stack_frame, stack_frame_size : INTEGER;
	
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
				Base.Write_char (out, '@');
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
	
PROCEDURE Decode_reg (r : INTEGER) : INTEGER;
	BEGIN
	IF r < 0 THEN
		r := -r
	ELSIF r >= 6 THEN
		r := r MOD 6
		END;
	RETURN r
	END Decode_reg;
	
PROCEDURE Small_reg (r, size : INTEGER) : INTEGER;
	BEGIN
	r := Decode_reg (r) MOD 64;
	CASE size OF
		1: INC (r, 192) |
		2: INC (r, 128) |
		4: INC (r, 64)
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
		o.reg1 := reg_RBP;
		IF mem.a < 32 THEN
			o.imm := 16 + Base.top_scope.parblksize - 32 + mem.a
		ELSE
			o.imm := 16 + mem.a - 32
			END
	ELSIF mem.lev > 0 THEN
		o.form := form_mem_reg;
		o.reg1 := reg_RBP;
		o.imm := -mem.a - mem.type.size
	ELSE
		o.form := form_mem_pc;
		IF mem.type.form = Base.type_string THEN
			o.sym := sym_stringbase
		ELSE
			o.sym := sym_varbase
			END;
		o.imm := mem.a
		END;
		
	IF (mem.mode = Base.class_ref) OR (mem.type = NIL) THEN
		o.size := 0
	ELSIF Base.Is_scalar_type (mem.type) THEN
		o.size := SHORT (SHORT (mem.type.size))
	ELSE
		o.size := 0
		END
	END Set_mem_operand;
	
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
	
PROCEDURE Emit_op_reg_imm (op, r, imm : INTEGER);
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
	
PROCEDURE Emit_op_mem_imm (op : INTEGER; VAR mem : Base.Item; imm : INTEGER);
	BEGIN
	codes [pc].op := op;
	Set_mem_operand (codes [pc].operands [0], mem);
	Set_imm_operand (codes [pc].operands [1], imm);
	Inc_program_counter
	END Emit_op_mem_imm;
	
PROCEDURE Emit_op_mem_proc
(op : INTEGER; VAR mem : Base.Item; proc : Base.Object);
	BEGIN
	codes [pc].op := op;
	Set_mem_operand (codes [pc].operands [0], mem);
	Set_proc_operand (codes [pc].operands [1], proc);
	Inc_program_counter
	END Emit_op_mem_proc;
	
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
	x.flag := {Base.flag_readonly};
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

PROCEDURE Inc_reg_stack;
	BEGIN
	IF reg_stack >= 6 THEN
		Push_reg (reg_stack)
	ELSE
		used_regs := used_regs + {Decode_reg (reg_stack)}
		END;
	INC (reg_stack);
	END Inc_reg_stack;
	
PROCEDURE Free_reg;
	BEGIN
	DEC (reg_stack);
	IF reg_stack >= 6 THEN Pop_reg (reg_stack) END
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
	IF Base.Is_variable (x) THEN Emit_op_reg_mem (op_MOV, x.r, x) END;
	x.mode := Base.mode_regI;
	x.a := 0
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
		x.mode := Base.mode_reg
		END
	END load;
	
PROCEDURE Load_adr (VAR x : Base.Item);
	BEGIN
	IF x.mode = Base.mode_regI THEN
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
	x.mode := Base.mode_reg
	END Load_adr;
	
PROCEDURE Finish_cond* (VAR x : Base.Item);
	BEGIN
	(* Implement later *)
	END Finish_cond;
	
PROCEDURE Store* (VAR x, y : Base.Item);
	VAR
		i : INTEGER;
	BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x) END;
	IF Base.Is_scalar_type (x.type) THEN
		IF y.type.form = Base.type_string THEN
			i := y.a DIV Base.char_type.size;
			Emit_op_mem_imm (op_MOV, x, ORD (strings [i]))
		ELSIF y.mode = Base.class_const THEN
			Emit_op_mem_imm (op_MOV, x, y.a)
		ELSIF y.mode = Base.class_proc THEN
			Emit_op_mem_proc (op_MOV, x, y.proc)
		ELSE
			load (y);
			IF x.type.size = 8 THEN
				Emit_op_mem_reg (op_MOV, x, y.r)
			ELSE
				Emit_op_mem_reg (op_MOV, x, Small_reg (y.r, x.type.size))
				END
			END;
		IF Use_register (y) THEN Free_reg END
	ELSIF x.type.form = Base.type_array THEN
		Emit_op_reg_mem (op_LEA, -reg_RSI, y);
		Emit_op_reg_mem (op_LEA, -reg_RDI, x);
		IF (y.type.form = Base.type_string) & (y.type.len < x.type.len) THEN
			Emit_op_reg_imm (op_MOV, -reg_RCX, y.type.size)
		ELSE
			Emit_op_reg_imm (op_MOV, -reg_RCX, x.type.size);
			END;
		Emit_op_bare (op_REP_MOVSB);
		IF Use_register (y) THEN Free_reg END;
		used_regs := used_regs + {reg_RSI, reg_RDI}
	ELSIF x.type.form = Base.type_record THEN
		(* Implement later *)
		END;
	IF Use_register (x) THEN Free_reg END
	END Store;
	
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
				(* NOTICE: need to check overflow *)
				x.a := -x.a
			ELSIF x.type.form = Base.type_set THEN
				s := {};
				SYSTEM.PUT (SYSTEM.ADR (s), x.a);
				SYSTEM.PUT (SYSTEM.ADR (x.a), -s)
				END
		ELSE
			load (x);
			IF x.type.form = Base.type_integer THEN
				Emit_op_reg (op_NEG, x.r)
			ELSIF x.type.form = Base.type_set THEN
				Emit_op_reg (op_NOT, x.r)
				END
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
	ELSE (* x.mode = mode_reg *)
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
	ELSE (* x.mode = mode_reg *)
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
		END
	END Subtract;
	
PROCEDURE Multiply (VAR x, y : Base.Item);
	BEGIN
	IF x.mode = Base.class_const THEN
		CASE y.mode OF
			Base.class_var:
				load (x);
				Emit_op_reg_mem (op_IMUL, x.r, y) |
			Base.class_ref:
				Ref_to_regI (y);
				Emit_op_reg_mem_imm (op_IMUL, y.r, y, x.a);
				x.r := y.r |
			Base.mode_regI:
				Emit_op_reg_mem_imm (op_IMUL, y.r, y, x.a);
				x.r := y.r |
			Base.mode_reg:
				Emit_op_reg_reg_imm (op_IMUL, y.r, y.r, x.a);
				x.r := y.r
			END;
		x.mode := Base.mode_reg
	ELSE (* x.mode = mode_reg *)
		CASE y.mode OF
			Base.class_const: Emit_op_reg_reg_imm (op_IMUL, x.r, x.r, y.a) |
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
			END
		END
	END Multiply;
	
PROCEDURE Divide (VAR x, y : Base.Item; is_modulo : BOOLEAN);
	BEGIN
	IF x.mode = Base.class_const THEN
		Emit_op_reg_imm (op_MOV, -reg_RAX, x.a);
		Emit_op_bare (op_CQO);
		CASE y.mode OF
			Base.class_var:
				Emit_op_mem (op_IDIV, y);
				x.r := reg_stack;
				Inc_reg_stack |
			Base.class_ref:
				Ref_to_regI (y);
				Emit_op_mem (op_IDIV, y);
				x.r := y.r |
			Base.mode_regI:
				Emit_op_mem (op_IDIV, y);
				x.r := y.r |
			Base.mode_reg:
				Emit_op_reg (op_IDIV, y.r);
				x.r := y.r
			END;
		x.mode := Base.mode_reg;
	ELSE (* x.mode = mode_reg *)
		Emit_op_reg_imm (op_MOV, -reg_RAX, x.r);
		Emit_op_bare (op_CQO);
		CASE y.mode OF
			Base.class_const:
				Emit_op_reg_imm (op_MOV, x.r, y.a);
				Emit_op_reg (op_IDIV, x.r) |
			Base.class_var: Emit_op_mem (op_IDIV, y) |
			Base.class_ref:
				Ref_to_regI (y);
				Emit_op_mem (op_IDIV, y);
				Free_reg |
			Base.mode_regI:
				Emit_op_mem (op_IDIV, y);
				Free_reg |
			Base.mode_reg:
				Emit_op_reg (op_IDIV, y.r);
				Free_reg
			END;
		END;
	IF is_modulo THEN
		Emit_op_reg_reg (op_MOV, x.r, -reg_RDX)
	ELSE
		Emit_op_reg_reg (op_MOV, x.r, -reg_RAX)
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
	ELSE (* x.mode = mode_reg *)
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
				Base.sym_plus: INC (x.a, y.a) |
				Base.sym_minus: DEC (x.a, y.a) |
				Base.sym_times: x.a := x.a * y.a |
				Base.sym_div: x.a := x.a DIV y.a |
				Base.sym_mod: x.a := x.a MOD y.a
				END
		ELSIF x.type.form = Base.type_set THEN
			s1 := {};
			s2 := {};
			SYSTEM.PUT (SYSTEM.ADR (s1), x.a);
			SYSTEM.PUT (SYSTEM.ADR (s2), y.a);
			CASE op OF
				Base.sym_plus: SYSTEM.PUT (SYSTEM.ADR (x.a), s1 + s2) |
				Base.sym_minus: SYSTEM.PUT (SYSTEM.ADR (x.a), s1 - s2) |
				Base.sym_times: SYSTEM.PUT (SYSTEM.ADR (x.a), s1 * s2) |
				Base.sym_slash: SYSTEM.PUT (SYSTEM.ADR (x.a), s1 / s2) |
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

PROCEDURE Module_init*;
	BEGIN
	END Module_init;

PROCEDURE End_module_init*;
	BEGIN
	Write_codes_to_file
	END End_module_init;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Enter* (proc : Base.Object; locblksize : INTEGER);
	VAR
		i, k : INTEGER;
		par : Base.Item;
	BEGIN
	Emit_op_reg (op_PUSH, -reg_RBP); (* Instruction no. 0 *)
	Emit_op_reg_reg (op_MOV, -reg_RBP, -reg_RSP); (* No. 1 *)
	Emit_op_reg_imm (op_SUB, -reg_RSP, 0); (* No. 2 *)
	
	stack_frame := locblksize;
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
	
	Emit_op_reg (op_PUSH, -reg_R12); (* No. 7 *)
	Emit_op_reg (op_PUSH, -reg_R13); (* No. 8 *)
	Emit_op_reg (op_PUSH, -reg_R14); (* No. 9 *)
	Emit_op_reg (op_PUSH, -reg_R15); (* No. 10 *)
	Emit_op_reg (op_PUSH, -reg_RSI); (* No. 11 *)
	Emit_op_reg (op_PUSH, -reg_RDI); (* No. 12 *)
	
	reg_stack := 0;
	mem_stack := 0;
	used_regs := {}
	END Enter;
	
PROCEDURE Return_value* (VAR x : Base.Item);
	BEGIN
	load (x);
	Emit_op_reg_reg (op_MOV, -reg_RAX, x.r);
	Free_reg
	END Return_value;
	
PROCEDURE Return* (proc : Base.Object; locblksize : INTEGER);
	VAR
		num_of_used_regs : INTEGER;
	BEGIN
	num_of_used_regs := 6;
	IF reg_RDI IN used_regs THEN Emit_op_reg (op_POP, -reg_RDI)
	ELSE codes [12].flag := {flag_skiped}; DEC (num_of_used_regs) END;
	IF reg_RSI IN used_regs THEN Emit_op_reg (op_POP, -reg_RSI)
	ELSE codes [11].flag := {flag_skiped}; DEC (num_of_used_regs) END;
	IF reg_R15 IN used_regs THEN Emit_op_reg (op_POP, -reg_R15)
	ELSE codes [10].flag := {flag_skiped}; DEC (num_of_used_regs) END;
	IF reg_R14 IN used_regs THEN Emit_op_reg (op_POP, -reg_R14)
	ELSE codes [9].flag := {flag_skiped}; DEC (num_of_used_regs) END;
	IF reg_R13 IN used_regs THEN Emit_op_reg (op_POP, -reg_R13)
	ELSE codes [8].flag := {flag_skiped}; DEC (num_of_used_regs) END;
	IF reg_R12 IN used_regs THEN Emit_op_reg (op_POP, -reg_R12)
	ELSE codes [7].flag := {flag_skiped}; DEC (num_of_used_regs) END;

	Emit_op_bare (op_LEAVE);
	Emit_op_bare (op_RET);
	
	(* Perform fixup *)
	IF stack_frame_size = 0 THEN
		IF num_of_used_regs MOD 2 # 0 THEN
			codes [2].operands [1].imm := 8
		ELSE
			codes [2].flag := {flag_skiped}
			END
	ELSE
		INC (stack_frame_size, num_of_used_regs * 8);
		IF stack_frame_size MOD 16 # 0 THEN
			stack_frame_size := (stack_frame_size DIV 16 + 1) * 16
			END;
		codes [2].operands [1].imm := stack_frame_size - num_of_used_regs * 8;
		END;
	
	Write_codes_to_file
	END Return;
	
PROCEDURE Need_saving (reg : INTEGER) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF reg < reg_TOP THEN
		IF Decode_reg (reg_stack) > reg THEN
			result := TRUE
		ELSE
			result := FALSE
			END
	ELSIF reg = reg_TOP THEN
		IF (reg_stack > reg_TOP) & (Decode_reg (reg_stack) = 0) THEN
			result := TRUE
		ELSE
			result := FALSE
			END
	ELSE
		result := FALSE
		END;
	RETURN result
	END Need_saving;
	
PROCEDURE Prepare_to_call* (VAR x : Base.Item);
	VAR
		i, top : INTEGER;
	BEGIN
	IF Need_saving (reg_R10) THEN Push_reg (reg_R10) END;
	IF Need_saving (reg_R11) THEN Push_reg (reg_R11) END;
	
	x.c := 0; (* Use for tracking temporary stack space usage *)
	IF x.mode = Base.class_proc THEN
		x.b := x.proc.parblksize (* Use for tracking parameter block size *)
	ELSE
		x.b := x.type.len
		END;
	IF x.b < 32 THEN x.b := 32 END;

	INC (mem_stack, x.b);
	IF mem_stack MOD 16 = 8 THEN
		INC (x.b, 8);
		INC (mem_stack, 8)
		END;
	Emit_op_reg_imm (op_SUB, -reg_RSP, x.b)
	END Prepare_to_call;
	
PROCEDURE Cleanup_after_call* (VAR x : Base.Item);
	BEGIN
	Emit_op_reg_imm (op_ADD, -reg_RSP, x.b);
	DEC (mem_stack, x.b);
	DEC (stack_frame, x.c);
	
	IF Need_saving (reg_R11) THEN Pop_reg (reg_R11) END;
	IF Need_saving (reg_R10) THEN Pop_reg (reg_R10) END
	END Cleanup_after_call;
	
PROCEDURE Normal_parameter* (VAR x : Base.Item; adr : INTEGER);
	VAR
		param_reg : INTEGER;
		param : Base.Item;
	BEGIN
	IF adr < 32 THEN
		param_reg := reg_RCX + (adr DIV 8);
		IF x.mode = Base.mode_reg THEN
			Emit_op_reg_reg (op_MOV, -param_reg, x.r)
		ELSIF x.mode = Base.class_const THEN
			Emit_op_reg_imm (op_MOV, -param_reg, x.a)
		ELSE
			Emit_op_reg_mem (op_MOV, -param_reg, x)
			END;
		IF Use_register (x) THEN Free_reg END
	ELSE
		load (x);
		param.mode := Base.mode_regI;
		param.r := -reg_RSP;
		param.a := adr - 32;
		Emit_op_mem_reg (op_MOV, param, x.r);
		Free_reg
		END
	END Normal_parameter;
	
PROCEDURE Reference_parameter* (VAR x : Base.Item; adr : INTEGER);
	BEGIN
	IF x.mode = Base.class_ref THEN
		Normal_parameter (x, adr)
	ELSE
		Load_adr (x);
		Normal_parameter (x, adr);
		Free_reg
		END
	END Reference_parameter;
	
PROCEDURE Record_variable_parameter* (VAR x : Base.Item; adr : INTEGER);
	VAR
		tag : Base.Item;
	BEGIN
	IF (x.mode = Base.class_ref) & ~ (Base.flag_readonly IN x.flag) THEN
		tag := x;
		INC (tag.a, 8)
	ELSE
		tag.mode := Base.class_var;
		tag.lev := 0;
		tag.a := x.type.tag
		END;
	Reference_parameter (x, adr);
	Normal_parameter (x, adr + 8)
	END Record_variable_parameter;
	
PROCEDURE Open_array_parameter* (VAR x : Base.Item; par : Base.Object);
	VAR
		len : Base.Item;
		tp, formal_type : Base.Type;
		adr : INTEGER;
	BEGIN
	adr := par.val;
	Reference_parameter (x, adr);
	
	formal_type := par.type;
	tp := x.type;
	WHILE Base.Is_open_array (formal_type) DO
		INC (adr, 8);
		IF Base.Is_open_array (tp) THEN
			len.mode := Base.class_var;
			len.lev := x.lev;
			len.a := x.b;
			INC (x.b, 8)
		ELSE
			len.mode := Base.class_const;
			len.a := tp.len;
			END;
		Normal_parameter (len, adr);
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
	temp_array.a := stack_frame;
	
	INC (proc.c, temp_array.type.size);
	INC (stack_frame, proc.c);
	IF stack_frame > stack_frame_size THEN stack_frame_size := stack_frame END;
	
	Store (temp_array, x);
	Reference_parameter (temp_array, par.val)
	END String_parameter;
	
PROCEDURE Call* (VAR x : Base.Item);
	BEGIN
	IF x.mode = Base.class_proc THEN
		Emit_op_proc (op_CALL, x.proc)
	ELSE
		IF x.mode = Base.mode_reg THEN
			Emit_op_reg (op_CALL, x.r)
		ELSE
			Emit_op_mem (op_CALL, x)
			END;
		IF Use_register (x) THEN Free_reg END
		END
	END Call;
	
PROCEDURE Get_return_value* (VAR x : Base.Item);
	BEGIN
	IF x.mode # Base.class_proc THEN x.type := x.type.base END;
	x.mode := Base.mode_reg;
	x.r := reg_stack;
	Inc_reg_stack;
	Emit_op_reg_reg (op_MOV, x.r, -reg_RAX)
	END Get_return_value;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Field* (VAR x : Base.Item; field : Base.Object);
	BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x) END;
	INC (x.a, field.val);
	x.type := field.type
	END Field;
	
PROCEDURE Deref* (VAR x : Base.Item);
	BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x) END;
	Ref_to_regI (x);
	x.type := x.type.base;
	x.flag := x.flag - {Base.flag_readonly}
	END Deref;
	
PROCEDURE Open_array_index (VAR x, y : Base.Item);

	PROCEDURE Calculate_offset
	(VAR y : Base.Item; typ : Base.Type; len_offset : INTEGER);
		VAR
			len : Base.Item;
		BEGIN
		IF typ.base.size >= 0 THEN
			Emit_op_reg_reg_imm (op_IMUL, y.r, y.r, typ.base.size)
		ELSE
			INC (len_offset, 8);
			Calculate_offset (y, typ.base, len_offset);
			
			len.mode := Base.class_var;
			len.a := len_offset;
			len.lev := Base.cur_lev;
			len.type := Base.int_type;
			
			Emit_op_reg_mem (op_IMUL, y.r, len)
			END
		END Calculate_offset;

	BEGIN (* Open array index *)
	IF (y.mode = Base.class_const) & (y.a < 0) THEN
		Scanner.Mark ('Array index out of bound');
		Free_reg
	ELSE
		load (y);
		IF Base.array_check_flag THEN
			END;
		Calculate_offset (y, x.type, x.b);

		Emit_op_reg_reg (op_ADD, reg_stack - 2, reg_stack - 1);
		x.r := reg_stack - 2;
		Free_reg;
		
		INC (x.b, 8)
		END
	END Open_array_index;
	
PROCEDURE Index* (VAR x, y : Base.Item);
	BEGIN
	IF x.mode = Base.class_ref THEN	Ref_to_regI (x) END;
	IF x.type.len < 0 THEN
		Open_array_index (x, y)
	ELSE
		IF y.mode = Base.class_const THEN
			IF (y.a < 0) OR (y.a >= x.type.len) THEN
				Scanner.Mark ('Array index out of bound');
				IF Use_register (x) THEN Free_reg END
			ELSE
				INC (x.a, y.a * x.type.base.size)
				END
		ELSE
			load (y);
			IF Base.array_check_flag THEN
				END;
			Emit_op_reg_reg_imm (op_IMUL, y.r, y.r, x.type.base.size);
			IF x.mode = Base.mode_regI THEN
				Emit_op_reg_reg (op_ADD, reg_stack - 2, reg_stack - 1);
				x.r := reg_stack - 2;
			ELSE
				Inc_reg_stack;
				Emit_op_reg_mem (op_LEA, reg_stack - 1, x);
				Emit_op_reg_reg (op_ADD, y.r, reg_stack - 1);
				x.mode := Base.mode_regI;
				x.r := y.r;
				x.a := 0
				END;
			Free_reg
			END
		END;
	x.type := x.type.base
	END Index;
	
PROCEDURE Type_guard* (VAR x : Base.Item; typ : Base.Type);
	BEGIN
	END Type_guard;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Relation* (op : INTEGER; VAR x, y : Base.Item);
	BEGIN
	END Relation;
	
PROCEDURE Membership_test* (VAR x, y : Base.Item);
	BEGIN
	END Membership_test;
	
PROCEDURE Type_test* (VAR x : Base.Item; typ : Base.Type);
	BEGIN
	END Type_test;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init* (module_name : Base.String);
	BEGIN
	Base.Rewrite (out, module_name.content);
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
	
	op_table [op_CALL] := 'call';
	op_table [op_RET] := 'ret';
	op_table [op_LEAVE] := 'leave'
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
	sym_varbase := Base.Make_string ('VAR');
	sym_stringbase := Base.Make_string ('STRING')
	END Init_symbols;
	
BEGIN
Init_optable;
Init_regtable;
Init_symbols
END Generator.
