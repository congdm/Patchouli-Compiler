unit Generator;

interface
	uses
		Scanner;
	
	const
		class_head = 0; class_var = 1; class_par = 2; class_const = 3; class_field = 4; class_typ = 5;
		class_proc = 6; class_sproc = 7; class_func = 8; class_ind = 9;
		mode_reg = 10; mode_regI = 11; mode_cond = 12;
		type_boolean = 0; type_integer = 1; type_array = 2; type_record = 3; type_set = 4;
		type_dynArray = 5; type_pointer = 10;
		
		MAX_INT = 9223372036854775807;
		MIN_INT = -9223372036854775808;
		
		Type_tag_offset = -16;
		
		Array_desc_Signature = 0;
		Array_desc_Length = 8;
		Array_desc_Base = 16;

	type
		Type_ = ^Type_desc;
		Object_ = ^Obj_desc;
		
		Type_desc = record
			form : Integer;
			fields : Object_;
			base : Type_;
			size, len : Int64;
			tag : Integer;
			end;
		
		Obj_desc = record
			name : AnsiString;
			class_, lev : Integer;
			is_param : Boolean;
			typ : Type_;
			next, dsc : Object_;
			val : Int64;
			read_only : Boolean;
			end;
			
		Item = record
			mode, lev : Integer;
			typ : Type_;
			a, b, c, r : Int64;
			read_only : Boolean;
			end;
			
	var
		int_type, bool_type, int8_type, int16_type, int32_type : Type_;
		dummy_record_type, nil_type : Type_;
		set_type : Type_;
		cur_lev, line_num : Integer;
		
		range_check_flag, overflow_check_flag : Boolean;
		
	procedure Write_asm_output_to_file (var f : Text);
	procedure Emit_blank_line;
	
	procedure Make_item (var x : Item; y : Object_);
	procedure Make_const (var x : Item; typ : Type_; val : Int64);
	procedure Make_clean_const (var x : Item; typ : Type_; val : Int64);
	procedure Make_function_result_item (var x : Item);
	
	procedure Store (var x, y : Item);
	procedure Copy (var x, y : Item; count : Int64);
	procedure Copy2 (var x, y : Item);
	
	procedure Op1 (op : Integer; var x : Item);
	procedure Op2 (op : Integer; var x, y : Item);
	
	procedure Set1 (var x : Item);
	procedure Set2 (var x, y : Item);
	procedure Set3 (var x, y, z : Item);
	
	procedure Index (var x, y : Item);
	procedure Dyn_array_Index (var x, y : Item);
	procedure Open_array_Index (var x, y : Item);
	procedure Field (var x : Item; y : Object_);
	procedure Deref (var x : Item);
	procedure Type_guard (var x : Item; typ : Type_);
	
	procedure Relation (op : Integer; var x, y : Item);
	procedure Inclusion_test (op : Integer; var x, y : Item);
	procedure Membership_test (var x, y : Item);
	procedure Type_test (var x : Item; typ : Type_);
	
	procedure Cond_jump (var x : Item);
	procedure Emit_label (lb : AnsiString);
	procedure Jump (lb : AnsiString);
	procedure Fix_link (L0 : Int64; lb : AnsiString);
	procedure Finish_cond (var x : Item);
	
	procedure Inc_level (i : Integer);
	procedure Enter (parblksize, locblksize : Int64);
	procedure Return (parblksize : Int64);
	procedure Call (var x : Item);
	procedure Set_Function_result (var x : Item);
	procedure Save_registers;
	procedure Restore_registers;
	procedure Parameter (var x : Item; fp : Object_);
	procedure Open_array_Param1 (var x : Item; fp : Object_; formal_typ : Type_; dim : Integer);
	procedure Open_array_Param2 (var x : Item; fp : Object_);
	procedure Record_variable_parameter (var x : Item);
	
	procedure SFunc_ORD (var x : Item);
	procedure SFunc_ODD (var x : Item);
	procedure SFunc_GET (var x, y : Item);
	procedure SFunc_PUT (var x, y : Item);
	procedure SFunc_TOINT8 (var x : Item);
	procedure SFunc_TOINT16 (var x : Item);
	procedure SFunc_TOINT32 (var x : Item);
	procedure SFunc_LEN (var x : Item);
	
	function Get_result_int_type (var x, y : Item) : Type_;
	procedure Check_reg_stack;
	procedure Begin_Main;
	procedure End_Main;
	procedure Global_var_decl (var obj : Object_);
	procedure Emit_type_tag (name : AnsiString; var typ : Type_);

implementation
	uses
		Sysutils;
		
	const
		new_line = #13#10;

		(* This compiler only use registers for intermediate values *)
		(* RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP is reserved for special purposes *)
		
		reg_R8 = 0; reg_R9 = 1; reg_R10 = 2; reg_R11 = 3;
		reg_R12 = 4; reg_R13 = 5; reg_R14 = 6; reg_R15 = 7;
		reg_RBX = 8; reg_RBP = 9; reg_RSP = 10; reg_RAX = 11; reg_RDX = 12;
		reg_RCX = 13; reg_RSI = 14; reg_RDI = 15;
		
		reg_R8D  = reg_R8  + 32;   reg_R9D  = reg_R9  + 32;   reg_R10D = reg_R10 + 32;   reg_R11D = reg_R11 + 32;
		reg_R12D = reg_R12 + 32;   reg_R13D = reg_R13 + 32;   reg_R14D = reg_R14 + 32;   reg_R15D = reg_R15 + 32;
		reg_EBX  = reg_RBX + 32;   reg_EBP  = reg_RBP + 32;   reg_ESP  = reg_RSP + 32;   reg_EAX  = reg_RAX + 32;
		reg_EDX  = reg_RDX + 32;   reg_ECX  = reg_RCX + 32;   reg_ESI  = reg_RSI + 32;   reg_EDI  = reg_RDI + 32;
				
		reg_R8W  = reg_R8  + 64;   reg_R9W  = reg_R9  + 64;   reg_R10W = reg_R10 + 64;   reg_R11W = reg_R11 + 64;
		reg_R12W = reg_R12 + 64;   reg_R13W = reg_R13 + 64;   reg_R14W = reg_R14 + 64;   reg_R15W = reg_R15 + 64;
		reg_BX   = reg_RBX + 64;   reg_BP   = reg_RBP + 64;   reg_SP   = reg_RSP + 64;   reg_AX   = reg_RAX + 64;
		reg_DX   = reg_RDX + 64;   reg_CX   = reg_RCX + 64;   reg_SI   = reg_RSI + 64;   reg_DI   = reg_RDI + 64;
		
		reg_R8L  = reg_R8  + 96;   reg_R9L  = reg_R9  + 96;   reg_R10L = reg_R10 + 96;   reg_R11L = reg_R11 + 96;
		reg_R12L = reg_R12 + 96;   reg_R13L = reg_R13 + 96;   reg_R14L = reg_R14 + 96;   reg_R15L = reg_R15 + 96;
		reg_BL   = reg_RBX + 96;   reg_BPL  = reg_RBP + 96;   reg_SPL  = reg_RSP + 96;   reg_AL   = reg_RAX + 96;
		reg_DL   = reg_RDX + 96;   reg_CL   = reg_RCX + 96;   reg_SIL  = reg_RSI + 96;   reg_DIL  = reg_RDI + 96;
		
		op_ADD = 0; op_SUB = 1; op_IMUL = 2; op_IDIV = 3; op_NEG = 4; vop_div = $10000; vop_mod = $10001;
		op_AND = 5; op_OR = 6; op_NOT = 7; op_XOR = 8; op_CMP = 9; op_TEST = 10;
		op_SHL = 11; op_SHR = 12; op_BTS = 13; op_BTR = 14; op_BTC = 15; op_BT = 16;

		op_MOV = 21; op_XCHG = 22; op_PUSH = 23; op_POP = 24; op_LEA = 25;
		op_MOVSX = 26; op_MOVSXD = 27; op_CQO = 28;
		
		op_JE = 30; op_JNE = 31; op_JMP = 42; vop_NJMP = 43;
		op_JL = 32; op_JGE = 33; op_JG = 34; op_JLE = 35;
		op_JB = 36; op_JAE = 37; op_JA = 38; op_JBE = 39;
		op_JC = 40; op_JNC = 41; op_JO = 44; op_JNO = 45;
		
		op_LEAVE = 60; op_RET = 61; op_CALL = 62;
		
		op_REP_MOVSB = 70; op_REP_MOVSW = 71;
		op_REP_MOVSD = 72; op_REP_MOVSQ = 73;
		
	type
		Code_line = record
			inst : Integer;
			lb, o1, o2, o3, comment : AnsiString;
			end;
			
		Data_line = record
			lb : AnsiString;
			data_size, tag_size : Int64;
			is_tag : Boolean;
			ptr_table : Array of Int64;
			end;
		
	var
		reg_stack : Set of 0..31;
		data_line_num : Integer;
		code_output : Array of Code_line;
		data_output : Array of Data_line;
		
		reg_table : Array [0..128] of AnsiString;
		rel_table : Array [0..31] of Int64;
		op_table : Array [0..255] of AnsiString;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)
		
	procedure Write_asm_output_to_file (var f : Text);
		var
			i, j : Integer;
			line : Code_line;
			dline : Data_line;
		begin
		for i := 1 to Length (code_output) - 1 do
			begin
			line := code_output [i];
			if line.lb <> '' then Write (f, line.lb + ':');
			if line.inst >= 0 then Write (f, #9 + op_table [line.inst]);
			if line.o1 <> '' then Write (f, ' ' + line.o1);
			if line.o2 <> '' then Write (f, ', ' + line.o2);
			if line.o3 <> '' then Write (f, ', ' + line.o3);
			if line.comment <> '' then Write (f, '; ' + line.comment);
			Writeln (f);
			end;
			
		Writeln (f, '; GLOBAL VARIABLES DECLARATION');
		Writeln (f, 'Global:');
		for i := 1 to Length (data_output) - 1 do
			begin
			dline := data_output [i];
			if not dline.is_tag then
				Writeln (f, dline.lb, #9'db ' , dline.data_size, ' dup 0')
			else
				begin
				Write (f, dline.lb, #9'dq "ReCoRd@#", ');
				Write (f, dline.tag_size, ', ', dline.data_size);
				for j := 0 to Length (dline.ptr_table) - 1 do 
					Write (f, ', ', dline.ptr_table [j]);
				Writeln (f)
				end
			end;
		end;
		
	procedure Write_line (lb : AnsiString; inst : Integer; o1, o2, o3, comment : AnsiString);
		begin
		SetLength (code_output, line_num + 1);
		code_output [line_num].lb := lb;
		code_output [line_num].inst := inst;
		code_output [line_num].o1 := o1;
		code_output [line_num].o2 := o2;
		code_output [line_num].o3 := o3;
		code_output [line_num].comment := comment;
		line_num := line_num + 1;
		end;
		
	procedure Write_op (inst : Integer; o1, o2, o3 : AnsiString);
		begin
		Write_line ('', inst, o1, o2, o3, '');
		end;
		
	procedure Emit_blank_line;
		begin
		Write_line ('', -1, '', '', '', '');
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)
		
	function Mem_op (var x : Item) : AnsiString;
		var
			mem_prefix : AnsiString;
		begin
		if x.typ^.size = 4 then mem_prefix := 'dword '
		else if x.typ^.size = 2 then mem_prefix := 'word '
		else if x.typ^.size = 1 then mem_prefix := 'byte '
		else mem_prefix := 'qword ';
		
		if (x.lev = 0) and (x.mode = class_var) then
			Result := mem_prefix + '[' + data_output [x.r].lb + ' + ' + IntToStr (x.a) + ']'
		else
			Result := mem_prefix + '[' + reg_table [x.r] + ' + ' + IntToStr (x.a) + ']';
		end;
		
	function Mem_op2 (base, index, scale, disp : Int64) : AnsiString;
		begin
		Result := '[' + reg_table [base] + ' + ';
		Result := Result + reg_table [index] + ' * ' + IntToStr (scale) + ' + ';
		Result := Result + IntToStr (disp) + ']';
		end;
		
	function Reg32 (reg : Int64) : Int64;
		begin
		Result := (reg mod 32) + 32;
		end;
		
	function Reg16 (reg : Int64) : Int64;
		begin
		Result := (reg mod 32) + 64;
		end;
		
	function Reg8 (reg : Int64) : Int64;
		begin
		Result := (reg mod 32) + 96;
		end;
		
	procedure Put_op_reg (op, reg : Int64);
		begin
		Write_op (op, reg_table [reg], '', '');
		end;
		
	procedure Put_op_reg_reg (op, dest, src : Int64);
		begin
		Write_op (op, reg_table [dest], reg_table [src], '');
		end;
		
	procedure Put_op_reg_imm (op, reg, imm : Int64);
		begin
		Write_op (op, reg_table [reg], IntToStr (imm), '');
		end;
		
	procedure Put_op_reg_reg_imm (op, dest, src, imm : Int64);
		begin
		Write_op (op, reg_table [dest], reg_table [src], IntToStr (imm));
		end;
		
	procedure Put_op_mem (op : Int64; var x : Item);
		begin
		Write_op (op, Mem_op (x), '', '');
		end;
		
	procedure Put_op_mem_imm (op : Int64; var x : Item; imm : Int64);
		begin
		Write_op (op, Mem_op (x), IntToStr (imm), '');
		end;
		
	procedure Put_op_reg_mem (op, dest : Int64; var x : Item);
		begin
		Write_op (op, reg_table [dest], Mem_op (x), '');
		end;
		
	procedure Put_op_mem_reg (op : Int64; var x : Item; src : Int64);
		begin
		Write_op (op, Mem_op (x), reg_table [src], '');
		end;
		
	procedure Put_op_imm (op : Int64; imm : Int64);
		begin
		Write_op (op, IntToStr (imm), '', '');
		end;
		
	procedure Put_op_sym (op : Int64; sym : AnsiString);
		begin
		Write_op (op, sym, '', '');
		end;
		
	procedure Put_op_bare (op : Int64);
		begin
		Write_op (op, '', '', '');
		end;
		
	procedure Put_op_sym_reg (op : Int64; sym : AnsiString; reg : Int64);
		begin
		Write_op (op, sym, reg_table [reg], '');
		end;
		
	procedure Put_op_sym_imm (op : Int64; sym : AnsiString; imm : Int64);
		begin
		Write_op (op, sym, IntToStr (imm), '');
		end;
		
	procedure Put_op_reg_sym (op : Int64; reg : Int64; sym : AnsiString);
		begin
		Write_op (op, reg_table [reg], sym, '');
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	function Is_huge_const (x : Int64) : Boolean;
		begin
		if (x >= $80000000) or (x < -$80000000) then Is_huge_const := True
		else Is_huge_const := False;
		end;
		
	function Get_power_of_2 (x : Qword) : Integer;
		begin
		Result := 0;
		while x > 1 do
			begin
			if (x mod 2) = 1 then begin Result := -1; exit; end;
			x := x div 2;
			Inc (Result);
			end;
		end;
		
	procedure Check_overflow (op : Integer; x, y : Int64);
		begin
		if op = Scanner.sym_plus then
			begin
			if (x > 0) and (y > MAX_INT - x) then
				Scanner.Mark ('Const evaluation overflow')
			else if (x < 0) and (y < MIN_INT - x) then
				Scanner.Mark ('Const evaluation overflow');
			end
		else if op = Scanner.sym_minus then
			begin
			if (x >= 0) and (y <= MIN_INT + x) then
				Scanner.Mark ('Const evaluation overflow')
			else if (x < 0) and (y > MAX_INT + x + 1) then
				Scanner.Mark ('Const evaluation overflow');
			end;
		end;
		
	function Alloc_reg : Int64;
		var
			i : Integer;
		begin
		Result := -1; i := 0;
		while (i < 8) and (Result < 0) do
			begin
			if not (i in reg_stack) then
				begin
				reg_stack := reg_stack + [i];
				Result := i;
				end;
			Inc (i)
			end;
		if Result < 0 then
			begin
			Scanner.Mark ('Register stack overflow');
			Result := 0;
			end
		end;
		
	procedure Free_reg (var x : Item);
		begin
		reg_stack := reg_stack - [x.r];
		end;
		
	function negate_cond (cond : Int64) : Int64;
		begin
		if cond mod 2 = 0 then Inc (cond) else Dec (cond);
		Result := cond;
		end;
		
	function Chg_cond_direction (op : Int64) : Int64;
		begin
		if op = op_JLE then Result := op_JGE
		else if op = op_JGE then Result := op_JLE
		else if op = op_JL then Result := op_JG
		else if op = op_JG then Result := op_JL;
		end;
		
	function merged (L0, L1 : Int64) : Int64;
		var
			L2, L3 : Int64;
		begin
		if L0 <> 0 then
			begin
			L2 := L0;
			while true do
				begin
				L3 := StrToInt (code_output [L2].o1);
				if L3 = 0 then break;
				L2 := L3;
				end;
			code_output [L2].o1 := IntToStr (L1);
			L1 := L0;
			end;
		Result := L1;	
		end;
		
	procedure Fix_link (L0 : Int64; lb : AnsiString);
		var L1 : Int64;
		begin
		while L0 <> 0 do
			begin
			L1 := StrToInt (code_output [L0].o1);
			code_output [L0].o1 := lb;
			L0 := L1;
			end;
		end;
		
	function Calc_reg_off (var x : Item) : Int64;
		begin
		if x.typ^.size = 4 then Result := 32
		else if x.typ^.size = 2 then Result := 64
		else if x.typ^.size = 1 then Result := 96
		else Result := 0;
		end;
		
	function Get_const_size (var x : Item) : Int64;
		begin
		if (x.a <= 127) and (x.a >= -128) then Result := 1
		else if (x.a <= 32767) and (x.a >= -32768) then Result := 2
		else if (x.a <= 2147483647) and (x.a >= -2147483648) then Result := 4
		else Result := 8;
		end;
		
	function Get_result_int_type (var x, y : Item) : Type_;
		var
			xsize, ysize : Integer;
		begin
		if x.mode = class_const then xsize := Get_const_size (x) else xsize := x.typ^.size;
		if y.mode = class_const then ysize := Get_const_size (y) else ysize := y.typ^.size;
		if xsize >= ysize then Result := x.typ else Result := y.typ;
		end;
		
	function Use_register (var x : Item) : Boolean;
		begin
		if (x.mode = mode_reg) or (x.mode = mode_regI) then Result := True
		else Result := False;
		end;
		
	procedure Clear_reg (reg : Int64);
		begin
		Put_op_reg_reg (op_XOR, Reg32 (reg), Reg32 (reg))
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Ref_to_regI (var x : Item);
		var
			reg : Int64;
		begin
		if x.mode = mode_regI then
			Put_op_reg_mem (op_MOV, x.r, x)
		else
			begin
			reg := Alloc_reg;
			Put_op_reg_mem (op_MOV, reg, x);
			x.r := reg; x.mode := mode_regI;
			end;		
		x.a := 0
		end;
		
	procedure Put_load_mem_op (reg : Int64; var x : Item);
		begin	
		if x.typ^.size = 4 then
			Put_op_reg_mem (op_MOVSXD, reg, x)
		else if (x.typ^.size = 2) or (x.typ^.size = 1) then
			Put_op_reg_mem (op_MOVSX, reg, x)
		else
			Put_op_reg_mem (op_MOV, reg, x);
		end;

	procedure load (var x : Item);
		var
			reg : Int64;
		begin
		if x.mode <> mode_reg then
			begin
			if x.mode = class_par then Ref_to_regI (x);
			if x.mode = class_var then
				begin
				reg := Alloc_reg;
				Put_load_mem_op (reg, x);
				x.r := reg;
				end
			else if (x.mode = class_const) and (x.a <> 0) then
				begin
				x.r := Alloc_reg;
				Put_op_reg_imm (op_MOV, x.r, x.a);
				end
			else if (x.mode = class_const) and (x.a = 0) then
				begin
				x.r := Alloc_reg;
				Clear_reg (x.r)
				end
			else if x.mode = mode_regI then
				begin Put_load_mem_op (x.r, x); end;
			x.mode := mode_reg;
			end;
		end;
	
	procedure Store_cond (var x, y : Item);
		var
			lb1, lb2 : AnsiString;
		begin
		if (x.mode = class_var) or (x.mode = mode_regI) then
			begin
			lb1 := 'STORE_' + IntToStr (line_num) + '_END';
			if (y.a = 0) and (y.b = 0) then
				Cond_jump (y);
			if y.a <> 0 then
				begin
				Put_op_mem_imm (op_MOV, x, 1);
				Put_op_sym (op_JMP, lb1);
				lb2 := 'LINE_' + IntToStr (line_num);
				Emit_label (lb2); Fix_link (y.a, lb2);
				Put_op_mem_imm (op_MOV, x, 0);
				Emit_label (lb1);
				end
			else if y.b <> 0 then
				begin
				Put_op_mem_imm (op_MOV, x, 0);
				Put_op_sym (op_JMP, lb1);
				lb2 := 'LINE_' + IntToStr (line_num);
				Emit_label (lb2); Fix_link (y.b, lb2);
				Put_op_mem_imm (op_MOV, x, 1);
				Emit_label (lb1);
				end;
			if Use_register (x) then Free_reg (x)
			end
		else
			Scanner.Mark ('Invalid assignment');
		end;
	
	procedure Store (var x, y : Item);
		var
			reg_off : Int64;
		begin
		if x.read_only then Scanner.Mark ('Assignment to read-only variable');
		if x.mode = class_par then Ref_to_regI (x);
		if (y.mode = class_const) and Is_huge_const (y.a) then load (y);
		
		if y.mode = mode_cond then
			Store_cond (x, y)
		else if y.mode = class_const then
			begin
			if x.mode = class_var then
				Put_op_mem_imm (op_MOV, x, y.a)
			else if x.mode = mode_regI then
				begin Put_op_mem_imm (op_MOV, x, y.a); Free_reg (x) end
			else
				begin Scanner.Mark ('Invalid assignment') end;
			end
		else
			begin
			if y.mode <> mode_reg then load (y);
			reg_off := Calc_reg_off (x);
			if x.mode = class_var then
				Put_op_mem_reg (op_MOV, x, y.r + reg_off)
			else if x.mode = mode_regI then
				begin Put_op_mem_reg (op_MOV, x, y.r + reg_off); Free_reg (x) end
			else
				begin Scanner.Mark ('Invalid assignment') end;
			Free_reg (y);
			end;
		end;
		
	procedure Load_cond (var x : Item);
		begin
		if x.typ^.form = type_boolean then
			begin
			if x.mode <> mode_cond then
				begin
				if x.mode = class_par then Ref_to_regI (x);
				if x.mode = class_const then
					begin if x.a = 0 then x.c := vop_NJMP else x.c := op_JMP end
				else if x.mode = class_var then
					begin Put_op_mem_imm (op_CMP, x, 0); x.c := op_JNE; end
				else if x.mode = mode_regI then
					begin Put_op_mem_imm (op_CMP, x, 0); x.c := op_JNE; Free_reg (x) end;
				x.mode := mode_cond; x.a := 0; x.b := 0;
				end;
			end
		else
			Scanner.Mark ('Boolean type?');
		end;
		
	procedure Load_adr (var x : Item);
		var
			reg : Int64;
		begin
		if x.mode = class_var then
			begin
			reg := Alloc_reg;
			Put_op_reg_mem (op_LEA, reg, x);
			x.r := reg;
			end
		else if x.mode = class_par then
			begin
			reg := Alloc_reg;
			Put_op_reg_mem (op_MOV, reg, x);
			x.r := reg;
			end
		else if (x.mode = mode_regI) and (x.a <> 0) then
			begin Put_op_reg_mem (op_LEA, x.r, x) end
		else
			Scanner.Mark ('Load address error, not a variable?');
		x.mode := mode_reg;
		end;
		
	(* Load_to_reg is different from load that *)
	(* it does not modify the mode of Item and *)
	(* does not use registers stack            *)
	procedure Load_to_reg (reg : Int64; var x : Item);
		var
			y : Item;
		begin
		if x.mode = class_par then
			begin
			Put_op_reg_mem (op_MOV, reg, x);
			y.mode := mode_regI; y.r := reg; y.a := 0; y.typ := x.typ;
			Put_load_mem_op (reg, y);
			end
		else if x.mode = mode_reg then
			begin Put_op_reg_reg (op_MOV, reg, x.r); end
		else if (x.mode = class_var) or (x.mode = mode_regI) then
			begin Put_load_mem_op (reg, x); end
		else if (x.mode = class_const) and (x.a <> 0) then
			begin Put_op_reg_imm (op_MOV, reg, x.a); end
		else if (x.mode = class_const) and (x.a = 0) then
			begin Clear_reg (reg) end;
		end;
	
	(* For static structures with known size *)	
	procedure Copy (var x, y : Item; count : Int64);
		begin
		if (x.mode = class_const) or (x.mode = mode_reg)
		or (y.mode = class_const) or (y.mode = mode_reg) then
			Scanner.Mark ('Wrong Item mode')
		else
			begin
			if x.mode = class_par then Put_op_reg_mem (op_MOV, reg_RDI, x)
			else Put_op_reg_mem (op_LEA, reg_RDI, x);
			if y.mode = class_par then Put_op_reg_mem (op_MOV, reg_RSI, y)
			else Put_op_reg_mem (op_LEA, reg_RSI, y);
			
			if count mod 8 = 0 then
				begin
				Put_op_reg_imm (op_MOV, reg_RCX, count div 8);
				Put_op_bare (op_REP_MOVSQ)
				end
			else if count mod 4 = 0 then
				begin
				Put_op_reg_imm (op_MOV, reg_RCX, count div 4);
				Put_op_bare (op_REP_MOVSD)
				end
			else if count mod 2 = 0 then
				begin
				Put_op_reg_imm (op_MOV, reg_RCX, count div 2);
				Put_op_bare (op_REP_MOVSW)
				end
			else
				begin
				Put_op_reg_imm (op_MOV, reg_RCX, count);
				Put_op_bare (op_REP_MOVSB)
				end;
				
			if Use_register (x) then Free_reg (x);
			if Use_register (y) then Free_reg (y)
			end
		end;
	
	(* For open arrays *)
	procedure Copy2 (var x, y : Item);
		var
			lenx, leny : Item;
		begin
		if (x.mode = class_const) or (x.mode = mode_reg)
		or (y.mode = class_const) or (y.mode = mode_reg) then
			Scanner.Mark ('Wrong Item mode')
		else
			begin
			if y.typ^.len = 0 then
				begin
				leny := y; leny.r := reg_RBP; leny.a := y.b + y.c * 8;
				leny.mode := class_var; leny.typ := int_type
				end
			else Make_const (leny, int_type, y.typ^.len);
			Load_to_reg (reg_RCX, leny);
			
			if x.typ^.len = 0 then
				begin
				lenx := x; lenx.r := reg_RBP; lenx.a := x.b + x.c * 8;
				lenx.mode := class_var; lenx.typ := int_type;
				Put_op_reg_mem (op_CMP, reg_RCX, lenx)
				end
			else Put_op_reg_imm (op_CMP, reg_RCX, x.typ^.len);
			
			if range_check_flag then Put_op_sym (op_JA, 'RANGE_CHECK_TRAP');
			
			if x.mode = class_par then Put_op_reg_mem (op_MOV, reg_RDI, x)
			else Put_op_reg_mem (op_LEA, reg_RDI, x);
			if y.mode = class_par then Put_op_reg_mem (op_MOV, reg_RSI, y)
			else Put_op_reg_mem (op_LEA, reg_RSI, y);
			Put_op_bare (op_REP_MOVSB);
							
			if Use_register (x) then Free_reg (x);
			if Use_register (y) then Free_reg (y)
			end
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)
	
	(* In Oberon, divisor must be positive and *)
	(* negative result is rounded toward negative infinity. *)
	(* That is Euclidean division *)
	procedure Put_division_op (op : Int64; var x, y : Item);
		var
			r : Int64;
			i : Integer;
		begin
		(* IDIV only accept dividend in RDX:RAX and divisor in reg/mem *)
		if (y.mode = class_const) and (y.a <= 0) then
			begin
			Scanner.Mark ('Divisor must be positive');
			end
		else
			begin
			i := line_num;
			
			if y.mode <> class_const then
				begin
				load (y);
				Put_op_reg_reg (op_TEST, y.r, y.r);
				Put_op_sym (op_JLE, 'NOT_POSITIVE_DIVISOR_TRAP')
				end
			else
				load (y);
			
			Load_to_reg (reg_RAX, x);
			Put_op_bare (op_CQO);
			Put_op_reg_reg (op_TEST, reg_RAX, reg_RAX);
			Put_op_sym (op_JLE, 'NEGATIVE_DIVIDEND_' + IntToStr (i));
				
			Put_op_reg (op_IDIV, y.r);
			Put_op_sym (op_JMP, 'END_DIVISION_' + IntToStr (i));
			
			Emit_label ('NEGATIVE_DIVIDEND_' + IntToStr (i));
			Put_op_reg (op_IDIV, y.r);
			Put_op_reg_reg (op_TEST, reg_RDX, reg_RDX);
			Put_op_sym (op_JE, 'END_DIVISION_' + IntToStr (i));
			if op = vop_div then
				Put_op_reg_imm (op_SUB, reg_RAX, 1)
			else
				Put_op_reg_reg (op_ADD, reg_RDX, y.r);
			
			
			Emit_label ('END_DIVISION_' + IntToStr (i));
			if op = vop_div then r := reg_RAX else r := reg_RDX;
			if Use_register (x) then
				begin
				Put_op_reg_reg (op_MOV, x.r, r);
				Free_reg (y)
				end
			else
				begin
				Put_op_reg_reg (op_MOV, y.r, r);
				x.r := y.r; x.mode := mode_reg
				end
			end
		end;
		
	procedure Put_subtract_op (var x, y : Item);
		begin
		(* ADD, SUB, IMUL,... can not work directly with huge const (64 bits) *)
		load (x);
		
		if (y.mode = class_const) and Is_huge_const (y.a) then load (y)
		else if y.mode = class_par then Ref_to_regI (y);
		if y.typ^.size < 8 then load (y);
		
		if y.mode = mode_reg then
			begin
			Put_op_reg_reg (op_SUB, x.r, y.r);
			Free_reg (y)
			end
		else if y.mode = class_const then
			begin Put_op_reg_imm (op_SUB, x.r, y.a) end
		else if y.mode = mode_regI then
			begin
			Put_op_reg_mem (op_SUB, x.r, y);
			Free_reg (y)
			end
		else if y.mode = class_var then
			begin Put_op_reg_mem (op_SUB, x.r, y); end;
		end;
		
	procedure Put_op (op : Int64; var x, y : Item);
		var
			t : Item;
		begin
		(* ADD, SUB, IMUL,... can not work directly with huge const (64 bits) *)
		if (x.mode = class_const) or (x.mode = class_var) or (x.mode = mode_regI) or (x.mode = class_par) then
			begin
			y.read_only := x.read_only;
			t := y; y := x; x := t;
			end;
			
		load (x);
		
		if (y.mode = class_const) and Is_huge_const (y.a) then load (y)
		else if y.mode = class_par then Ref_to_regI (y);
		if y.typ^.size < 8 then load (y);
			
		if y.mode = mode_reg then
			begin
			Put_op_reg_reg (op, x.r, y.r);
			Free_reg (y)
			end
		else if y.mode = class_const then
			begin
			if op <> op_IMUL then
				Put_op_reg_imm (op, x.r, y.a)
			else if op = op_IMUL then
				Put_op_reg_reg_imm (op, x.r, x.r, y.a);
			end
		else if y.mode = class_var then
			begin
			Put_op_reg_mem (op, x.r, y);
			end
		else if y.mode = mode_regI then
			begin
			Put_op_reg_mem (op, x.r, y);
			Free_reg (y)
			end
		end;

	function Put_compare_op (var x, y : Item) : Boolean;
		begin
		(* ADD, SUB, IMUL,... can not work directly with huge const (64 bits) *)
		if (x.mode = class_const) and Is_huge_const (x.a) then load (x)
		else if x.mode = class_par then Ref_to_regI (x);
		
		if (y.mode = class_const) and Is_huge_const (y.a) then load (y)
		else if y.mode = class_par then Ref_to_regI (y);
		
		if ((x.mode = class_var) or (x.mode = mode_regI))
				and ((y.mode = class_var) or (y.mode = mode_regI)) then
			load (x);
		
		Result := False;	
		if x.mode = mode_reg then
			begin
			if y.typ^.size < 8 then load (y);
			if y.mode = mode_reg then begin Put_op_reg_reg (op_CMP, x.r, y.r); Free_reg (y) end
			else if y.mode = class_var then begin Put_op_reg_mem (op_CMP, x.r, y); end
			else if y.mode = mode_regI then begin Put_op_reg_mem (op_CMP, x.r, y); Free_reg (y) end
			else if y.mode = class_const then begin Put_op_reg_imm (op_CMP, x.r, y.a); end;
			Free_reg (x)
			end
		else if (x.mode = class_var) or (x.mode = mode_regI) then
			begin
			if y.mode = mode_reg then
				begin
				if x.typ^.size < 8 then
					begin load (x); Put_op_reg_reg (op_CMP, x.r, y.r); end
				else
					Put_op_mem_reg (op_CMP, x, y.r);
				Free_reg (y)
				end
			else if y.mode = class_const then
				Put_op_mem_imm (op_CMP, x, y.a);			
			if Use_register (x) then Free_reg (x)
			end
		else if x.mode = class_const then
			begin
			Result := True;
			if y.mode = mode_reg then begin Put_op_reg_imm (op_CMP, y.r, x.a); Free_reg (y) end
			else if y.mode = class_var then begin Put_op_mem_imm (op_CMP, y, x.a); end
			else if y.mode = mode_regI then begin Put_op_mem_imm (op_CMP, y, x.a); Free_reg (y) end;
			end
		end;
		
	procedure Put_difference_op (var x, y : Item);
		begin
		load (y);
		Put_op_reg (op_NOT, y.r);
		Put_op (op_AND, x, y);
		end;
		
	procedure Op1 (op : Integer; var x : Item);
		var
			t : Int64; lb : AnsiString;
		begin
		if op = Scanner.sym_minus then
			begin
			if x.typ = set_type then
				begin
				if x.mode = class_const then x.a := not x.a
				else
					begin
					if x.mode <> mode_reg then load (x);
					Put_op_reg (op_NOT, x.r);
					end;
				end
			else (* x.typ is integer *)
				begin
				if x.mode = class_const then x.a := -x.a
				else
					begin
					if x.mode <> mode_reg then load (x);
					Put_op_reg (op_NEG, x.r);
					if overflow_check_flag then
						begin
						Put_op_reg_imm (op_MOV, reg_RAX, MIN_INT);
						Put_op_reg_reg (op_CMP, reg_RAX, x.r);
						Put_op_sym (op_JE, 'INTEGER_OVERFLOW_TRAP');
						end;
					end;
				end;
			end
		else if op = Scanner.sym_not then
			begin
			if x.mode <> mode_cond then Load_cond (x);
			x.c := negate_cond (x.c); t := x.a; x.a := x.b; x.b := t;
			end
		else if op = Scanner.sym_and then
			begin
			if x.mode <> mode_cond then Load_cond (x);
			if negate_cond (x.c) <> vop_NJMP then
				begin
				Put_op_sym (negate_cond (x.c), IntToStr (x.a));
				x.a := line_num - 1;
				end;
			if x.b <> 0 then
				begin
				lb := 'LINE_' + IntToStr (line_num);
				Fix_link (x.b, lb); Emit_label (lb); x.b := 0;
				end;
			end
		else if op = Scanner.sym_or then
			begin
			if x.mode <> mode_cond then Load_cond (x);
			if x.c <> vop_NJMP then
				begin
				Put_op_sym (x.c, IntToStr (x.b));
				x.b := line_num - 1;
				end;
			if x.a <> 0 then
				begin
				lb := 'LINE_' + IntToStr (line_num);
				Fix_link (x.a, lb); Emit_label (lb); x.a := 0;
				end;
			end
		end;
		
	procedure Op2 (op : Integer; var x, y : Item);
		var
			result_type : Type_;
		begin
		if x.typ^.form = type_set then
			begin
			if (x.mode = class_const) and (y.mode = class_const) then
				begin
				if op = Scanner.sym_plus then x.a := x.a or y.a
				else if op = Scanner.sym_minus then x.a := x.a and (not y.a)
				else if op = Scanner.sym_times then x.a := x.a and y.a
				else if op = Scanner.sym_slash then x.a := x.a xor y.a
				else Scanner.Mark ('Wrong operator');
				end
			else
				begin
				if op = Scanner.sym_plus then Put_op (op_OR, x, y)
				else if op = Scanner.sym_minus then Put_difference_op (x, y)
				else if op = Scanner.sym_times then Put_op (op_AND, x, y)
				else if op = Scanner.sym_slash then Put_op (op_XOR, x, y)
				else Scanner.Mark ('Wrong operator');
				end
			end
		else if x.typ^.form = type_integer then
			begin
			if (x.mode = class_const) and (y.mode = class_const) then
				begin
				Check_overflow (op, x.a, y.a);
				if op = Scanner.sym_plus then x.a := x.a + y.a
				else if op = Scanner.sym_minus then x.a := x.a - y.a
				else if op = Scanner.sym_times then x.a := x.a * y.a
				else if op = Scanner.sym_div then
					begin
					if (x.a < 0) and (x.a mod y.a <> 0) then x.a := x.a div y.a - 1
					else x.a := x.a div y.a
					end
				else if op = Scanner.sym_mod then
					begin
					if (x.a < 0) then
						begin
						x.a := x.a mod y.a;
						if x.a < 0 then x.a := y.a + x.a
						end
					else x.a := x.a mod y.a
					end
				else Scanner.Mark ('Wrong operator');
				end
			else
				begin
				result_type := Get_result_int_type (x, y);
				
				if op = Scanner.sym_plus then Put_op (op_ADD, x, y)
				else if op = Scanner.sym_minus then Put_subtract_op (x, y)
				else if op = Scanner.sym_times then Put_op (op_IMUL, x, y)
				else if op = Scanner.sym_div then Put_division_op (vop_div, x, y)
				else if op = Scanner.sym_mod then Put_division_op (vop_mod, x, y)
				else Scanner.Mark ('Wrong operator');
				
				if (op = Scanner.sym_plus) or (op = Scanner.sym_minus) or (op = Scanner.sym_times) then
					if overflow_check_flag then Put_op_sym (op_JO, 'INTEGER_OVERFLOW_TRAP');
				
				x.typ := result_type;
				end
			end
		else if x.typ^.form = type_boolean then
			begin
			if y.mode <> mode_cond then Load_cond (y);
			if op = Scanner.sym_or then
				begin x.a := y.a; x.b := merged (y.b, x.b); x.c := y.c; end
			else if op = Scanner.sym_and then
				begin x.b := y.b; x.a := merged (y.a, x.a); x.c := y.c; end
			end;
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	function Shift_left (x : Int64; s : Integer) : Int64;
		begin
		Result := x;
		while s > 0 do begin Result := Result * 2; Dec (s); end;
		end;
		
	function Shift_right (x : Int64; s : Integer) : Int64;
		begin
		Result := x;
		while s > 0 do begin Result := Result div 2; Dec (s); end;
		end;
		
	function Bit_test (x : Int64; s : Integer) : Boolean;
		begin
		x := Shift_right (x, s);
		if (x mod 2) = 1 then Result := True else Result := False;
		end;

	(* procedure Set2 may use RCX during calculation *)
	procedure Set2 (var x, y : Item);
		var
			lb : AnsiString;
		begin
		lb := 'END_SET_' + IntToStr (line_num);
		if y.mode = class_const then
			begin
			if (y.a <= 63) and (y.a >= 0) then
				begin
				(* Delay code generation - all const are evaluated in x.a *)
				(* and added to register later *)
				x.a := x.a or Shift_left (1, y.a);
				end;
			end
		else
			begin
			Load_to_reg (reg_RCX, y);
			if x.mode = class_const then
				begin
				if Use_register (y) then begin x.mode := mode_reg; x.r := y.r; Clear_reg (x.r) end
				else load (x);
				x.a := 0;
				end;
			Put_op_reg_imm (op_CMP, reg_RCX, 63);
			Put_op_sym (op_JA, lb);
			Put_op_reg_reg (op_BTS, x.r, reg_RCX);
			if Use_register (y) and (x.r <> y.r) then Free_reg (y);
			Emit_label (lb);
			end;
		end;
	
	(* procedure Set3 may use RAX, RCX during calculation *)
	(* Mode of x is only reg or const *)
	procedure Set3 (var x, y, z : Item);
		var
			lb : AnsiString;
			i : Integer;
			r : Int64;
		begin
		lb := 'END_SET_' + IntToStr (line_num);
		(* Case 1: Both y and z are consts *)
		if (y.mode = class_const) and (z.mode = class_const) then
			begin
			if (y.a >= 0) and (z.a <= 63) and (z.a >= 0) and (y.a <= z.a) then
				begin
				if y.a = z.a then
					x.a := x.a or Shift_left (1, y.a)
				else
					begin
					r := 0;
					for i := y.a to z.a do r := r + Shift_left (1, i);
					x.a := x.a or r;
					end;
				end;
			end
			
		(* Case 2: Only y is const *)
		else if y.mode = class_const then
			begin
			if (y.a <= 63) and (y.a >= 0) then
				begin
				Load_to_reg (reg_RCX, z);
				if x.mode = class_const then
					begin
					if Use_register (z) then begin x.mode := mode_reg; x.r := z.r; Clear_reg (x.r) end
					else load (x);
					x.a := 0;
					end;
				Put_op_reg_imm (op_CMP, reg_RCX, 63);
				Put_op_sym (op_JA, lb);
				Put_op_reg_imm (op_CMP, reg_CL, y.a);
				Put_op_sym (op_JB, lb);
				Put_op_reg_reg (op_MOV, reg_RAX, -2);
				Put_op_reg_reg (op_SHL, reg_RAX, reg_CL);
				Put_op_reg (op_NOT, reg_RAX);
				if y.a <> 0 then
					begin
					Put_op_reg_imm (op_SHR, reg_RAX, y.a);
					Put_op_reg_imm (op_SHL, reg_RAX, y.a);
					end;
				Put_op_reg_reg (op_OR, x.r, reg_RAX);
				Emit_label (lb);
				if Use_register (z) and (x.r <> z.r) then Free_reg (z)
				end;
			end
			
		(* Case 3: Only z is const *)
		else if z.mode = class_const then
			begin
			if (z.a <= 63) and (z.a >= 0) then
				begin
				Load_to_reg (reg_RCX, y);
				if x.mode = class_const then
					begin
					if Use_register (y) then begin x.mode := mode_reg; x.r := y.r; Clear_reg (x.r) end
					else load (x);
					x.a := 0;
					end;
				Put_op_reg_imm (op_CMP, reg_RCX, z.a);
				Put_op_sym (op_JA, lb);
				Put_op_reg_imm (op_MOV, reg_RAX, not Shift_left (-2, z.a));
				Put_op_reg_reg (op_SHR, reg_RAX, reg_CL);
				Put_op_reg_reg (op_SHL, reg_RAX, reg_CL);
				Put_op_reg_reg (op_OR, x.r, reg_RAX);
				Emit_label (lb);
				if Use_register (y) and (y.r <> x.r) then Free_reg (y)
				end;
			end
			
		(* Case 4: Both y and z are not consts *) (* FINAL *)
		else
			begin
			Load_to_reg (reg_RCX, z);
			if x.mode = class_const then
				begin
				if Use_register (z) then begin x.mode := mode_reg; x.r := z.r; Clear_reg (x.r) end
				else load (x);
				x.a := 0;
				end;
				
			load (y);
			
			Put_op_reg_imm (op_CMP, reg_RCX, 63);
			Put_op_sym (op_JA, lb);
			Put_op_reg_reg (op_CMP, reg_RCX, y.r);
			Put_op_sym (op_JB, lb);
			
			Put_op_reg_reg (op_MOV, reg_RAX, -2);
			Put_op_reg_reg (op_SHL, reg_RAX, reg_CL);
			Put_op_reg (op_NOT, reg_RAX);
			
			Put_op_reg_reg (op_MOV, reg_CL, Reg8 (y.r));
			Put_op_reg_reg (op_SHR, reg_RAX, reg_CL);
			Put_op_reg_reg (op_SHL, reg_RAX, reg_CL);
			
			Put_op_reg_reg (op_OR, x.r, reg_RAX);
			Emit_label (lb);
			
			if Use_register (z) and (z.r <> x.r) then Free_reg (z);
			Free_reg (y);
			end;
		end;
		
	procedure Set1 (var x : Item);
		var
			y : Item;
			i : Integer;
		begin
		if (x.mode = mode_reg) and (x.a <> 0) then
			begin
			i := Get_power_of_2 (x.a);
			if i < 0 then
				begin
				Make_const (y, set_type, x.a);
				Put_op (op_OR, x, y);
				end
			else
				begin Put_op_reg_imm (op_BTS, x.r, i); end;
			x.a := 0;
			end;
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)
	
	(* Procedure Index may use RAX during calculation *)
	procedure Index (var x, y : Item);
		var
			elem_size, scale : Integer;
		begin
		if y.mode = class_const then
			begin
			if (y.a < 0) or (y.a >= x.typ^.len) then
				begin
				Scanner.Mark ('Index out of range');
				y.a := 0
				end;
			x.a := x.a + y.a * x.typ^.base^.size;
			end
		else
			begin
			load (y);
			if range_check_flag then
				begin
				Put_op_reg_imm (op_CMP, y.r, x.typ^.len);
				Put_op_sym (op_JAE, 'RANGE_CHECK_TRAP');
				end;
				
			elem_size := x.typ^.base^.size;
			if (elem_size = 1) or (elem_size = 2) or (elem_size = 4) or (elem_size = 8) then
				begin scale := elem_size end
			else
				begin
				Put_op_reg_reg_imm (op_IMUL, y.r, y.r, elem_size);
				scale := 1;
				end;
			
			if x.mode = class_par then Ref_to_regI (x);
			if x.mode = class_var then
				begin
				if x.lev = 0 then
					begin
					Put_op_reg_mem (op_LEA, reg_RAX, x);
					Put_op_reg_sym (op_LEA, y.r, Mem_op2 (reg_RAX, y.r, scale, 0));
					end
				else
					Put_op_reg_sym (op_LEA, y.r, Mem_op2 (x.r, y.r, scale, x.a));
				x.mode := mode_regI;
				x.r := y.r; x.a := 0;
				end
			else if x.mode = mode_regI then
				begin
				Put_op_reg_sym (op_LEA, x.r, Mem_op2 (x.r, y.r, scale, x.a));
				x.a := 0;
				Free_reg (y)
				end;
			end;
		end;
		
	procedure Open_array_Index (var x, y : Item);
		var
			elem_size, scale : Integer;
			len : Item;
			base_tp : Type_;
		begin
		if (y.mode = class_const) and (y.a < 0) then
			begin
			Scanner.Mark ('Index out of range');
			y.a := 0;
			end;
		if (y.mode = class_const) and (y.a = 0) then
			Inc (x.c)
		else if (y.mode = class_const) and (x.typ^.base^.size <> 0) then
			begin
			if range_check_flag then
				begin
				len := x; len.r := reg_RBP; len.a := x.b + x.c * 8;
				len.mode := class_var; len.typ := int_type;
				Put_op_mem_imm (op_CMP, len, y.a);
				Put_op_sym (op_JBE, 'RANGE_CHECK_TRAP');
				end;
			if x.mode = class_par then Ref_to_regI (x);
			x.a := x.a + y.a * x.typ^.base^.size;
			Inc (x.c)
			end
		else
			begin
			load (y);
			len := x; len.r := reg_RBP; len.a := x.b + x.c * 8;
			len.mode := class_var; len.typ := int_type;
			
			if range_check_flag then
				begin
				Put_op_reg_mem (op_CMP, y.r, len);
				Put_op_sym (op_JAE, 'RANGE_CHECK_TRAP');
				end;
				
			base_tp := x.typ^.base;
			while (base_tp^.form = type_array) and (base_tp^.len = 0) do
				begin
				len.a := len.a + 8;
				Put_op_reg_mem (op_IMUL, y.r, len);
				base_tp := base_tp^.base;
				end;
				
			elem_size := base_tp^.size;
			if (elem_size = 1) or (elem_size = 2) or (elem_size = 4) or (elem_size = 8) then
				begin scale := elem_size end
			else
				begin
				Put_op_reg_reg_imm (op_IMUL, y.r, y.r, elem_size);
				scale := 1;
				end;
				
			if x.mode = class_par then Ref_to_regI (x);
			Put_op_reg_sym (op_LEA, x.r, Mem_op2 (x.r, y.r, scale, x.a));
			Free_reg (y);
			x.a := 0;
			Inc (x.c)
			end
		end;
		
	procedure Dyn_array_Index (var x, y : Item);
		var
			elem_size, scale : Integer;
			len : Item;
		begin
		if (y.mode = class_const) and (y.a < 0) then
			begin
			Scanner.Mark ('Index out of range');
			y.a := 0
			end;
		if (y.mode = class_const) and (y.a = 0) then
			begin
			load (x);
			x.a := 0;
			x.mode := mode_regI
			end
		else if y.mode = class_const then
			begin
			if x.mode = class_par then Ref_to_regI (x);
			if range_check_flag then
				begin
				len := x; len.a := len.a + 8 + Array_desc_Length;
				len.typ := int_type;
				Put_op_mem_imm (op_CMP, len, y.a);
				Put_op_sym (op_JBE, 'RANGE_CHECK_TRAP');
				end;
			load (x);
			x.a := y.a * x.typ^.base^.size;
			x.mode := mode_regI
			end
		else
			begin
			load (y);
			if x.mode = class_par then Ref_to_regI (x);
			if range_check_flag then
				begin
				len := x; len.a := len.a + 8 + Array_desc_Length;
				len.typ := int_type;
				Put_op_reg_mem (op_CMP, y.r, len);
				Put_op_sym (op_JAE, 'RANGE_CHECK_TRAP');
				end;
			
			elem_size := x.typ^.base^.size;
			if (elem_size = 1) or (elem_size = 2) or (elem_size = 4) or (elem_size = 8) then
				begin scale := elem_size end
			else
				begin
				Put_op_reg_reg_imm (op_IMUL, y.r, y.r, elem_size);
				scale := 1;
				end;
				
			load (x);
			Put_op_reg_sym (op_LEA, x.r, Mem_op2 (x.r, y.r, scale, 0));
			x.a := 0; x.mode := mode_regI;
			Free_reg (y)
			end;
		x.read_only := False;
		end;
	
	procedure Field (var x : Item; y : Object_);
		begin
		if x.mode = class_par then Ref_to_regI (x);
		x.a := x.a + y^.val; x.typ := y^.typ;
		end;
		
	procedure Deref (var x : Item);
		begin
		load (x);
		x.mode := mode_regI;
		x.a := 0;
		x.read_only := False
		end;
	
	(* Procedure Type_guard use RAX, RCX *)
	procedure Type_guard (var x : Item; typ : Type_);
		var
			tag : Item;
		begin
		if x.typ^.form = type_pointer then
			begin
			Load_to_reg (reg_RCX, x);
			tag.lev := 0; tag.mode := class_var; tag.r := typ^.base^.tag; tag.a := 0;
			Put_op_reg_mem (op_LEA, reg_RAX, tag);
			tag.lev := x.lev; tag.mode := mode_regI; tag.r := reg_RCX;
			tag.a := type_tag_offset;
			Put_op_reg_mem (op_CMP, reg_RAX, tag);
			Put_op_sym (op_JNE, 'TYPE_GUARD_TRAP')
			end
		else
			begin
			tag.lev := 0; tag.mode := class_var; tag.r := typ^.tag; tag.a := 0;
			Put_op_reg_mem (op_LEA, reg_RAX, tag);
			tag.lev := x.lev; tag.mode := class_var; tag.r := reg_RBP;
			tag.a := x.a + 8;
			Put_op_reg_mem (op_CMP, reg_RAX, tag);
			Put_op_sym (op_JNE, 'TYPE_GUARD_TRAP')
			end
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)
		
	procedure Relation (op : Integer; var x, y : Item);
		begin
		if y.mode = mode_cond then Finish_cond (y);
		if (x.mode = class_const) and (y.mode = class_const) then
			begin
			if x.a = y.a       then if op = Scanner.sym_equal         then x.a := 1 else x.a := 0
			else if x.a <> y.a then if op = Scanner.sym_not_equal     then x.a := 1 else x.a := 0
			else if x.a > y.a  then if op = Scanner.sym_greater       then x.a := 1 else x.a := 0
			else if x.a >= y.a then if op = Scanner.sym_greater_equal then x.a := 1 else x.a := 0
			else if x.a < y.a  then if op = Scanner.sym_less          then x.a := 1 else x.a := 0
			else if x.a <= y.a then if op = Scanner.sym_less_equal    then x.a := 1 else x.a := 0;
			end
		else if (x.mode = class_const) and (Get_const_size (x) > y.typ^.size) then
			begin
			if x.a > 0 then
				begin if op = Scanner.sym_greater then x.a := 1 else x.a := 0; end
			else
				begin if op = Scanner.sym_less then x.a := 1 else x.a := 0; end;
			end
		else if (y.mode = class_const) and (Get_const_size (y) > x.typ^.size) then
			begin
			if y.a > 0 then
				begin if op = Scanner.sym_less then x.a := 1 else x.a := 0; end
			else
				begin if op = Scanner.sym_greater then x.a := 1 else x.a := 0; end;
			end
		else
			begin
			if Put_compare_op (x, y) then
				x.c := Chg_cond_direction (rel_table [op])
			else
				x.c := rel_table [op];
			x.mode := mode_cond;
			x.a := 0; x.b := 0;
			end;
		end;
		
	procedure Inclusion_test (op : Integer; var x, y : Item);
		begin
		if (x.mode = class_const) and (y.mode = class_const) then
			begin
			if op = Scanner.sym_greater_equal then
				if (y.a and not x.a) = 0 then x.a := 1 else x.a := 0
			else
				if (x.a and not y.a) = 0 then x.a := 1 else x.a := 0;
			end
		else
			begin
			load (x); load (y);
			if op = Scanner.sym_greater_equal then Put_op_reg (op_NOT, x.r)
			else Put_op_reg (op_NOT, y.r);
			Put_op_reg_reg (op_AND, x.r, y.r);
			x.c := op_JE; x.a := 0; x.b := 0;
			Free_reg (x); Free_reg (y)
			end;
		end;
		
	procedure Membership_test (var x, y : Item);
		begin
		if (x.mode = class_const) and (y.mode = class_const) then
			begin
			if (x.a < 0) or (x.a > 63) then x.a := 0
			else if Bit_test (y.a, x.a) then x.a := 1
			else x.a := 0;
			end
		else if x.mode = class_const then
			begin
			if (x.a < 0) or (x.a > 63) then
				x.a := 0
			else
				begin
				load (y);
				Put_op_reg_imm (op_BT, y.r, x.a);
				x.mode := mode_cond;
				x.a := 0; x.b := 0; x.c := op_JC;
				Free_reg (y)
				end;
			end
		else
			begin
			load (x); load (y);
			Put_op_reg_imm (op_CMP, x.r, 63);
			Put_op_sym (op_JA, '0');
			x.a := line_num - 1;
			Put_op_reg_reg (op_BT, y.r, x.r);
			x.mode := mode_cond;
			x.b := 0; x.c := op_JC;
			Free_reg (x); Free_reg (y)
			end;
		end;
		
	(* Procedure Type_test use RAX *)
	procedure Type_test (var x : Item; typ : Type_);
		var
			tag : Item;
		begin
		if x.typ^.form = type_pointer then
			begin
			load (x);
			tag.lev := 0; tag.mode := class_var; tag.r := typ^.base^.tag; tag.a := 0;
			Put_op_reg_mem (op_LEA, reg_RAX, tag);
			tag.lev := x.lev; tag.mode := mode_regI; tag.r := x.r;
			tag.a := type_tag_offset;
			Put_op_reg_mem (op_CMP, reg_RAX, tag);
			x.mode := mode_cond;
			x.a := 0; x.b := 0; x.c := op_JE;
			Free_reg (x)
			end
		else
			begin
			tag.lev := 0; tag.mode := class_var; tag.r := typ^.tag; tag.a := 0;
			Put_op_reg_mem (op_LEA, reg_RAX, tag);
			tag.lev := x.lev; tag.mode := class_var; tag.r := reg_RBP;
			tag.a := x.a + 8;
			Put_op_reg_mem (op_CMP, reg_RAX, tag);
			x.mode := mode_cond;
			x.a := 0; x.b := 0; x.c := op_JE;
			end
		end;
		
	procedure Cond_jump (var x : Item);
		var
			lb : AnsiString;
		begin
		if x.mode <> mode_cond then Load_cond (x);
		if negate_cond (x.c) <> vop_NJMP then
			begin
			Put_op_sym (negate_cond (x.c), IntToStr (x.a));
			x.a := line_num - 1;
			end;
		if x.b <> 0 then
			begin
			lb := 'LINE_' + IntToStr (line_num);
			Fix_link (x.b, lb); Emit_label (lb); x.b := 0;
			end;
		end;
		
	procedure Jump (lb : AnsiString);
		begin
		Put_op_sym (op_JMP, lb);
		end;
		
	procedure Finish_cond (var x : Item);
		var
			lb1, lb2 : AnsiString;
			reg : Int64;
		begin
		if x.mode = mode_cond then
			begin
			reg := Alloc_reg;
			lb1 := 'FINISH_COND_' + IntToStr (line_num) + '_END';
			if (x.a = 0) and (x.b = 0) then
				Cond_jump (x);
			if x.a <> 0 then
				begin
				Put_op_reg_imm (op_MOV, reg, 1);
				Put_op_sym (op_JMP, lb1);
				lb2 := 'LINE_' + IntToStr (line_num);
				Emit_label (lb2); Fix_link (x.a, lb2);
				Put_op_reg_imm (op_MOV, reg, 0);
				Emit_label (lb1);
				end
			else if x.b <> 0 then
				begin
				Put_op_reg_imm (op_MOV, reg, 0);
				Put_op_sym (op_JMP, lb1);
				lb2 := 'LINE_' + IntToStr (line_num);
				Emit_label (lb2); Fix_link (x.b, lb2);
				Put_op_reg_imm (op_MOV, reg, 1);
				Emit_label (lb1);
				end;
			x.mode := mode_reg; x.r := reg;	
			end
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Inc_level (i : Integer);
		begin cur_lev := cur_lev + i; end;
		
	procedure Enter (parblksize, locblksize : Int64);
		begin
		Put_op_reg (op_PUSH, reg_RBP);
		Put_op_reg_reg (op_MOV, reg_RBP, reg_RSP);
		if locblksize > 0 then Put_op_reg_imm (op_SUB, reg_RSP, locblksize);
		end;
		
	procedure Return (parblksize : Int64);
		begin
		Put_op_bare (op_LEAVE);
		Put_op_imm (op_RET, parblksize);
		Emit_blank_line;
		end;
		
	procedure Set_Function_result (var x : Item);
		begin
		Load_to_reg (reg_RAX, x);
		if Use_register (x) then Free_reg (x)
		end;
		
	procedure Parameter (var x : Item; fp : Object_);
		begin
		if fp^.class_ = class_par then
			begin
			if x.read_only and not fp^.read_only then
				Scanner.Mark ('Can not pass read-only var as var parameter')
			else if x.mode = class_par then
				Put_op_mem (op_PUSH, x)
			else
				begin
				Load_adr (x);
				Put_op_reg (op_PUSH, x.r);
				Free_reg (x)
				end;
			end
		else
			begin
			if x.mode = class_par then Ref_to_regI (x)
			else if (x.mode = class_const) and Is_huge_const (x.a) then load (x);
			if ((x.mode = class_var) or (x.mode = mode_regI)) and (x.typ^.size = 1) then
				load (x);
			
			if x.mode = mode_reg then
				begin Put_op_reg (op_PUSH, x.r); Free_reg (x) end
			else if x.mode = mode_regI then
				begin Put_op_mem (op_PUSH, x); Free_reg (x) end
			else if x.mode = class_var then
				begin Put_op_mem (op_PUSH, x); end
			else if x.mode = class_const then
				begin Put_op_imm (op_PUSH, x.a); end;
			end;
		end;
		
	procedure Open_array_Param1 (var x : Item; fp : Object_; formal_typ : Type_; dim : Integer);
		var
			y, len : Item;
			base_tp : Type_;
		begin
		y := x;
		y.typ := x.typ^.base;
		y.c := x.c + 1;
		
		base_tp := formal_typ^.base;
		
		if base_tp^.form = type_array then (* Multi-dimension array case *)
			begin
			if y.typ^.form = type_array then
				Open_array_Param1 (y, fp, base_tp, dim + 1)
			else
				Scanner.Mark ('Array''s dimension not matching');
			end
		else if base_tp <> y.typ then
			Scanner.Mark ('Array''s base type incompatible');
		
		if x.typ^.len = 0 then (* x is open array *)
			begin
			len := x; len.r := reg_RBP; len.a := x.b + x.c * 8;
			len.mode := class_var; len.typ := int_type;
			Put_op_mem (op_PUSH, len)
			end
		else (* x is static array *)
			Put_op_imm (op_PUSH, x.typ^.len);
			
		if dim = 1 then
			begin
			if x.read_only and not fp^.read_only then
				Scanner.Mark ('Can not pass read-only var as var parameter');
			if x.mode = class_par then
				Put_op_mem (op_PUSH, x)
			else
				begin
				Load_adr (x);
				Put_op_reg (op_PUSH, x.r);
				Free_reg (x)
				end;
			end;
		end;
		
	procedure Open_array_Param2 (var x : Item; fp : Object_);
		var
			len : Item;
			base_tp : Type_;
		begin
		base_tp := fp^.typ^.base;
		
		if base_tp^.form = type_array then
			Scanner.Mark ('Array''s dimension not matching')
		else if base_tp <> x.typ^.base then
			Scanner.Mark ('Array''s base type incompatible');
		
		if x.mode = class_par then Ref_to_regI (x);
		len := x; len.a := len.a + 8 + Array_desc_Length;
		len.typ := int_type;
		Put_op_mem (op_PUSH, len);

		if x.read_only and not fp^.read_only then
			Scanner.Mark ('Can not pass read-only var as var parameter');
			
		Put_op_mem (op_PUSH, x);
		if Use_register (x) then Free_reg (x)
		end;
	
	(* Procedure Record_variable_parameter will use RAX *)
	procedure Record_variable_parameter (var x : Item);
		var
			tag : Item;
		begin
		if x.read_only then
			Scanner.Mark ('Can not pass read-only var as var parameter')
		else if x.mode = class_par then
			begin
			x.a := x.a + 8;
			Put_op_mem (op_PUSH, x);
			x.a := x.a - 8;
			Put_op_mem (op_PUSH, x)
			end
		else
			begin
			tag.lev := 0; tag.r := x.typ^.tag; tag.a := 0;
			tag.mode := class_var;
			Put_op_reg_mem (op_LEA, reg_RAX, tag);
			Put_op_reg (op_PUSH, reg_RAX);
			Load_adr (x);
			Put_op_reg (op_PUSH, x.r);
			Free_reg (x)
			end
		end;
		
	procedure Call (var x : Item);
		begin
		Put_op_sym (op_CALL, code_output [x.a].lb);
		end;
		
	procedure Save_registers;
		var
			i : Integer;
		begin
		for i := 0 to 7 do
			if i in reg_stack then
				Put_op_reg (op_PUSH, i);
		end;
		
	procedure Restore_registers;
		var i : Integer;
		begin
		for i := 7 downto 0 do
			if i in reg_stack then
				Put_op_reg (op_POP, i);
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure SFunc_ORD (var x : Item);
		var
			lb1, lb2 : AnsiString;
			reg : Int64;
		begin
		if x.mode = mode_cond then
			begin
			lb1 := 'END_SFUNC_ORD_' + IntToStr (line_num);
			reg := Alloc_reg;
			if (x.a = 0) and (x.b = 0) then Cond_jump (x);
			if x.a <> 0 then
				begin
				Put_op_reg_imm (op_MOV, reg, 1);
				Put_op_sym (op_JMP, lb1);
				lb2 := 'LINE_' + IntToStr (line_num);
				Emit_label (lb2); Fix_link (x.a, lb2);
				Put_op_reg_imm (op_MOV, reg, 0);
				Emit_label (lb1);
				end
			else if x.b <> 0 then
				begin
				Put_op_reg_imm (op_MOV, reg, 0);
				Put_op_sym (op_JMP, lb1);
				lb2 := 'LINE_' + IntToStr (line_num);
				Emit_label (lb2); Fix_link (x.b, lb2);
				Put_op_reg_imm (op_MOV, reg, 1);
				Emit_label (lb1);
				end;
			x.mode := mode_reg; x.r := reg;
			end
		else if (x.mode = class_const) and ((x.typ^.form = type_boolean) or (x.typ^.form = type_set)) then
			begin (* do nothing *) end
		else if (x.typ^.form = type_boolean) or (x.typ^.form = type_set) then
			begin load (x) end
		else
			begin
			Scanner.Mark ('Function ORD input type must be BOOLEAN or SET');
			Make_clean_const (x, int_type, 0);
			end;
		x.typ := int_type;
		end;
		
	procedure SFunc_ODD (var x : Item);
		begin
		if x.typ^.form <> type_integer then
			begin
			Scanner.Mark ('Function ODD input must be an INTEGER');
			Make_clean_const (x, bool_type, 0);
			end
		else if x.mode = class_const then
			begin x.typ := bool_type; x.a := abs (x.a mod 2); end
		else
			begin
			load (x);
			Put_op_reg_imm (op_SHR, x.r, 1);
			x.mode := mode_cond; x.a := 0; x.b := 0; x.c := op_JC;
			Free_reg (x)
			end;
		end;
		
	procedure SFunc_GET (var x, y : Item);
		begin
		if (x.typ^.form <> type_integer) then
			begin
			Scanner.Mark ('Function GET: address must be an INTEGER');
			end
		else if (y.mode = mode_reg) or (y.mode = class_const) then
			begin
			Scanner.Mark ('Function GET: destination must be a variable');
			end
		else if (y.typ^.form = type_array) or (y.typ^.form = type_dynArray) or (y.typ^.form = type_record) then
			begin
			Scanner.Mark ('Function GET: type of destination variable must be basic type');
			end
		else
			begin
			if y.mode = class_par then Ref_to_regI (y);
			load (x); x.mode := mode_regI; x.a := 0;
			load (x);
			Put_op_mem_reg (op_MOV, y, x.r);
			end;
		if Use_register (x) then Free_reg (x);
		if Use_register (y) then Free_reg (y);
		end;
	
	procedure SFunc_PUT (var x, y : Item);
		begin
		if (x.typ^.form <> type_integer) then
			begin
			Scanner.Mark ('Function PUT: address must be an INTEGER');
			end
		else if (y.typ^.form = type_array) or (y.typ^.form = type_dynArray) or (y.typ^.form = type_record) then
			begin
			Scanner.Mark ('Function PUT: type of source must be basic type');
			end
		else
			begin
			load (y);
			load (x); x.mode := mode_regI; x.a := 0; x.typ := y.typ;
			Put_op_mem_reg (op_MOV, x, y.r);
			end;
		if Use_register (x) then Free_reg (x);
		if Use_register (y) then Free_reg (y);
		end;
		
	procedure SFunc_TOINT8 (var x : Item);
		begin
		if x.typ^.form <> type_integer then
			begin
			Scanner.Mark ('Function TOINT8 input must be an INTEGER');
			end
		else
			begin
			load (x);
			if overflow_check_flag then
				begin
				Put_op_reg_imm (op_CMP, x.r, -128);
				Put_op_sym (op_JL, 'INTEGER_OVERFLOW_TRAP');
				Put_op_reg_imm (op_CMP, x.r, 127);
				Put_op_sym (op_JG, 'INTEGER_OVERFLOW_TRAP');
				end;
			end;
		end;
		
	procedure SFunc_TOINT16 (var x : Item);
		begin
		if x.typ^.form <> type_integer then
			begin
			Scanner.Mark ('Function TOINT16 input must be an INTEGER');
			end
		else
			begin
			load (x);
			if overflow_check_flag then
				begin
				Put_op_reg_imm (op_CMP, x.r, -32768);
				Put_op_sym (op_JL, 'INTEGER_OVERFLOW_TRAP');
				Put_op_reg_imm (op_CMP, x.r, 32767);
				Put_op_sym (op_JG, 'INTEGER_OVERFLOW_TRAP');
				end;
			end;
		end;
		
	procedure SFunc_TOINT32 (var x : Item);
		begin
		if x.typ^.form <> type_integer then
			begin
			Scanner.Mark ('Function TOINT32 input must be an INTEGER');
			end
		else
			begin
			load (x);
			if overflow_check_flag then
				begin
				Put_op_reg_imm (op_CMP, x.r, -2147483648);
				Put_op_sym (op_JL, 'INTEGER_OVERFLOW_TRAP');
				Put_op_reg_imm (op_CMP, x.r, 2147483647);
				Put_op_sym (op_JG, 'INTEGER_OVERFLOW_TRAP');
				end;
			end;
		end;
		
	procedure SFunc_LEN (var x : Item);
		var
			reg : Int64;
		begin
		if x.typ^.form = type_array then
			begin
			if x.typ^.len = 0 then (* Open array *)
				begin
				if Use_register (x) then reg := x.r
				else reg := Alloc_reg;
				x.r := reg_RBP; x.a := x.b + x.c * 8;
				x.mode := class_var; x.typ := int_type;
				Put_op_reg_mem (op_MOV, reg, x);
				x.r := reg; x.mode := mode_reg
				end
			else (* Static array *)
				Make_clean_const (x, int_type, x.typ^.len)
			end
		else if x.typ^.form = type_dynArray then (* Dynamic array *)
			begin
			if x.mode = class_par then Ref_to_regI (x);
			x.a := x.a + 8 + Array_desc_Length;
			x.typ := int_type;
			load (x)
			end
		else
			begin
			Scanner.Mark ('Function LEN input must be an ARRAY');
			Make_clean_const (x, int_type, 0)
			end;
		end;

(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Emit_label (lb : AnsiString);
		begin
		Write_line (lb, -1, '', '', '', '');
		end;

	procedure Make_item (var x : Item; y : Object_);
		begin
		if (y^.class_ = class_var) or (y^.class_ = class_const) or (y^.class_ = class_par) then
			begin
			x.mode := y^.class_; x.typ := y^.typ; x.lev := y^.lev;
			x.a := y^.val; x.b := 0; x.c := 0;
			x.read_only := y^.read_only;
			
			if y^.lev = 0 then
				begin
				if y^.class_ = class_var then begin x.r := x.a; x.a := 0; end
				end
			else if y^.lev = cur_lev then 
				x.r := reg_RBP
			else
				begin Scanner.Mark ('Level!?'); x.r := 0; end;
				
			if (x.mode = class_par) and (x.typ^.form = type_array) and (x.typ^.len = 0) then
				begin x.b := x.a; x.c := 1 end
			end
		else
			Scanner.Mark ('Expected a variable, parameter or const')
		end;
		
	procedure Make_const (var x : Item; typ : Type_; val : Int64);
		begin
		x.mode := class_const; x.typ := typ; x.a := val;
		end;
		
	procedure Make_clean_const (var x : Item; typ : Type_; val : Int64);
		begin
		if Use_register (x) then Free_reg (x);
		x.mode := class_const; x.typ := typ; x.a := val;
		end;
		
	procedure Make_function_result_item (var x : Item);
		begin
		if not Use_register (x) then x.r := Alloc_reg;
		x.mode := mode_reg;
		Put_op_reg_reg (op_MOV, x.r, reg_RAX);
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)
	
	procedure Begin_Main;
		begin
		Emit_label ('MAIN');
		Put_op_reg (op_PUSH, reg_RBP);
		Put_op_reg_reg (op_MOV, reg_RBP, reg_RSP);
		end;
	
	procedure End_Main;
		begin
		Put_op_bare (op_LEAVE);
		Put_op_bare (op_RET);
		Emit_blank_line;
		Emit_label ('RANGE_CHECK_TRAP');
		Emit_blank_line;
		Emit_label ('INTEGER_OVERFLOW_TRAP');
		Emit_blank_line;
		Emit_label ('NOT_POSITIVE_DIVISOR_TRAP');
		Emit_blank_line;
		Emit_label ('TYPE_GUARD_TRAP');
		Emit_blank_line;
		end;
		
	procedure Check_reg_stack;
		begin
		if reg_stack <> [] then
			begin
			Scanner.Mark ('Reg stack error');
			reg_stack := [];
			end;
		end;
		
	procedure Global_var_decl (var obj : Object_);
		var
			i : Integer;
		begin
		i := data_line_num; Inc (data_line_num);
		SetLength (data_output, data_line_num);
		
		data_output [i].lb := obj^.name + '_';
		data_output [i].data_size := obj^.typ^.size;
		data_output [i].is_tag := False;
		obj^.val := i;
		end;
		
	procedure Emit_type_tag (name : AnsiString; var typ : Type_);
		var
			obj : Object_;
			i, k, j, len, len2 : Integer;
			
		procedure Append_array (var dest : Data_line; src : Type_; offset : Int64);
			var
				i, k, len, len2, j : Integer;
			begin
			len := Length (dest.ptr_table);
			if (src^.base^.form = type_pointer) or (src^.base^.form = type_dynArray) then
				begin
				SetLength (dest.ptr_table, len + src^.len);
				for i := 0 to src^.len - 1 do
					dest.ptr_table [len + i] := offset + i * 8
				end
			else if src^.base^.form = type_record then
				begin
				k := src^.base^.tag;
				len2 := Length (data_output [k].ptr_table);
				if len2 > 0 then
					begin
					SetLength (dest.ptr_table, len + len2 * src^.len);
					for i := 0 to src^.len - 1 do for j := 0 to len2 - 1 do
						dest.ptr_table [len + i * len2 + j] :=
							offset + i * src^.base^.size
							+ data_output [k].ptr_table [j]
					end
				end
			else if src^.base^.form = type_array then
				for i := 0 to src^.len - 1 do
					Append_array (dest, src^.base, offset + i * src^.base^.size)
			end;
			
		begin (* Procedure Emit_type_tag *)
		i := data_line_num; Inc (data_line_num);
		SetLength (data_output, data_line_num);
		
		data_output [i].lb := 'TYPE_TAG.' + name + '_' + IntToStr (i);
		data_output [i].is_tag := True;
		data_output [i].data_size := typ^.size;
		len := 0;
		if typ^.base <> nil then
			begin
			k := typ^.base.tag;
			len := Length (data_output [k].ptr_table);
			data_output [i].ptr_table := System.Copy (data_output [k].ptr_table, 0, len);
			end;
			
		obj := typ^.fields; 
		while obj^.class_ = class_field do
			begin
			if (obj^.typ^.form = type_pointer) or (obj^.typ.form = type_dynArray) then
				begin
				Inc (len);
				SetLength (data_output [i].ptr_table, len);
				data_output [i].ptr_table [len-1] := obj^.val
				end
			else if obj^.typ^.form = type_record then
				begin
				k := obj^.typ^.tag;
				len2 := Length (data_output [k].ptr_table);
				if len2 > 0 then
					begin
					SetLength (data_output [i].ptr_table, len + len2);
					for j := 0 to len2 - 1 do
						data_output [i].ptr_table [len + j] := obj^.val + data_output [k].ptr_table [j];
					len := len + len2;
					end
				end
			else if obj^.typ^.form = type_array then
				begin
				Append_array (data_output [i], obj^.typ, obj^.val);
				len := Length (data_output [i].ptr_table)
				end;
			obj := obj^.next
			end;
		data_output [i].tag_size := 24 + len * 8;
		typ^.tag := i;
		end; (* Procedure Emit_type_tag *)
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Init_mnemonic_table;
		begin
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
		
		op_table [op_ADD] := 'ADD';
		op_table [op_SUB] := 'SUB';
		op_table [op_IMUL] := 'IMUL';
		op_table [op_IDIV] := 'IDIV';
		op_table [op_NEG] := 'NEG';
		
		op_table [op_AND] := 'AND';
		op_table [op_OR] := 'OR';
		op_table [op_NOT] := 'NOT';
		op_table [op_XOR] := 'XOR';
		op_table [op_CMP] := 'CMP';
		op_table [op_TEST] := 'TEST';
		op_table [op_SHL] := 'SHL';
		op_table [op_SHR] := 'SHR';
		op_table [op_BTS] := 'BTS';
		op_table [op_BTR] := 'BTR';
		op_table [op_BTC] := 'BTC';
		op_table [op_BT] := 'BT';
		
		op_table [op_MOV] := 'MOV';
		op_table [op_XCHG] := 'XCHG';
		op_table [op_PUSH] := 'PUSH';
		op_table [op_XCHG] := 'POP';
		op_table [op_LEA] := 'LEA';
		op_table [op_MOVSX] := 'MOVSX';
		op_table [op_MOVSXD] := 'MOVSXD';
		op_table [op_CQO] := 'CQO';
		
		op_table [op_JMP] := 'JMP';
		op_table [op_JE] := 'JE';
		op_table [op_JNE] := 'JNE';
		op_table [op_JL] := 'JL';
		op_table [op_JG] := 'JG';
		op_table [op_JLE] := 'JLE';
		op_table [op_JGE] := 'JGE';
		op_table [op_JB] := 'JB';
		op_table [op_JA] := 'JA';
		op_table [op_JBE] := 'JBE';
		op_table [op_JAE] := 'JAE';
		op_table [op_JC] := 'JC';
		op_table [op_JNC] := 'JNC';
		op_table [op_JO] := 'JO';
		op_table [op_JNO] := 'JNO';
		
		op_table [op_LEAVE] := 'LEAVE';
		op_table [op_RET] := 'RET';
		op_table [op_CALL] := 'CALL';
		
		op_table [op_REP_MOVSB] := 'REP MOVSB';
		op_table [op_REP_MOVSW] := 'REP MOVSW';
		op_table [op_REP_MOVSD] := 'REP MOVSD';
		op_table [op_REP_MOVSQ] := 'REP MOVSQ';
		end;
		
	procedure Init_relation_table;
		begin
		rel_table [Scanner.sym_equal] := op_JE;
		rel_table [Scanner.sym_not_equal] := op_JNE;
		rel_table [Scanner.sym_less] := op_JL;
		rel_table [Scanner.sym_greater] := op_JG;
		rel_table [Scanner.sym_less_equal] := op_JLE;
		rel_table [Scanner.sym_greater_equal] := op_JGE;
		end;
		
initialization
	cur_lev := 0;
	line_num := 1;
	data_line_num := 1;
	range_check_flag := True;
	overflow_check_flag := True;

	New (bool_type);  bool_type^.form  := type_boolean;  bool_type^.size  := 1;
	New (int_type);   int_type^.form   := type_integer;  int_type^.size   := 8;
	New (int8_type);  int8_type^.form  := type_integer;  int8_type^.size  := 1;
	New (int16_type); int16_type^.form := type_integer;  int16_type^.size := 2;
	New (int32_type); int32_type^.form := type_integer;  int32_type^.size := 4;
	New (set_type);   set_type^.form   := type_set;      set_type^.size   := 8;
	
	New (dummy_record_type);
	dummy_record_type^.form := type_integer;
	dummy_record_type^.size := 8;
	dummy_record_type^.base := nil;
	
	New (nil_type);
	nil_type^.form := type_pointer;
	nil_type^.size := 8;
	nil_type^.base := dummy_record_type;

	Init_mnemonic_table;
	Init_relation_table;
	
	reg_stack := [];
end.
