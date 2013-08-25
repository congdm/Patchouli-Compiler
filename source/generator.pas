unit Generator;

interface
	uses
		Scanner;
	
	const
		class_head = 0; class_var = 1; class_par = 2; class_const = 3; class_field = 4; class_typ = 5;
		class_proc = 6; class_sproc = 7;
		mode_reg = 10; mode_regI = 11; mode_cond = 12;
		type_boolean = 0; type_integer = 1; type_array = 2; type_record = 3;
		
		reg_RCX = 2; reg_R8 = 3; reg_R9 = 4; reg_R10 = 5; reg_R11 = 6;
		reg_RSI = 7; reg_RDI = 8; reg_R12 = 9; reg_R13 = 10; reg_R14 = 11; reg_R15 = 12; reg_RBX = 13;
		reg_RBP = 14; reg_RSP = 15; reg_RAX = 16; reg_RDX = 17;

	type
		Type_ = ^Type_desc;
		Object_ = ^Obj_desc;
		
		Type_desc = record
			form : Integer;
			fields : Object_;
			base : Type_;
			size, len : Int64;
			end;
		
		Obj_desc = record
			name : String;
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
		int_type, bool_type : Type_;
		cur_lev, line_num : Integer;
		
		range_check_flag : Boolean;
		
	procedure Write_asm_output_to_file (var f : Text);
	procedure Emit_blank_line;
	
	procedure Make_item (var x : Item; y : Object_);
	procedure Make_const (var x : Item; typ : Type_; val : Int64);
	procedure Make_clean_const (var x : Item; typ : Type_; val : Int64);
	
	procedure Store (var x, y : Item);	
	procedure Op1 (op : Integer; var x : Item);
	procedure Op2 (op : Integer; var x, y : Item);
	
	procedure Index (var x, y : Item);
	procedure Field (var x : Item; y : Object_);
	
	procedure Relation (op : Integer; var x, y : Item);
	procedure Cond_jump (var x : Item);
	procedure Emit_label (lb : AnsiString);
	procedure Jump (lb : AnsiString);
	procedure Fix_link (L0 : Int64; lb : AnsiString);
	
	procedure Inc_level (i : Integer);
	procedure Enter (parblksize, locblksize : Int64);
	procedure Return (parblksize : Int64);
	procedure Parameter (var x : Item; cls : Integer);
	procedure Call (var x : Item);
	
	procedure SFunc_ORD (var x : Item);
	procedure SFunc_ODD (var x : Item);
	procedure SFunc_BIT (var x, y : Item);

implementation
	uses
		Sysutils;
		
	const
		new_line = #13#10;
		
		op_ADD = 0; op_SUB = 1; op_IMUL = 2; op_IDIV = 3; op_NEG = 4; vop_div = $10000; vop_mod = $10001;
		op_AND = 5; op_OR = 6; op_NOT = 7; op_XOR = 8; op_CMP = 9; op_TEST = 10;
		op_SHL = 11; op_SHR = 12;

		op_MOV = 21; op_XCHG = 22; op_PUSH = 23; op_POP = 24; op_LEA = 25;
		
		op_JE = 30; op_JNE = 31; op_JMP = 42; vop_NJMP = 43;
		op_JL = 32; op_JGE = 33; op_JG = 34; op_JLE = 35; 
		op_JB = 36; op_JAE = 37; op_JA = 38; op_JBE = 39; 
		op_JC = 40; op_JNC = 41;
		
		op_LEAVE = 60; op_RET = 61; op_CALL = 62;
		
	type
		Asm_line = record
			lb, inst, o1, o2, o3, comment : AnsiString;
			end;
		
	var
		cur_reg : Int64;
		asm_output : Array of Asm_line;
		reg_table : Array [0..31] of AnsiString;
		rel_table : Array [0..31] of Int64;
		op_table : Array [0..255] of AnsiString;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)
		
	procedure Write_asm_output_to_file (var f : Text);
		var
			i : Integer;
			line : Asm_line;
		begin
		for i := 1 to Length (asm_output) - 1 do
			begin
			line := asm_output [i];
			if line.lb <> '' then Write (f, line.lb + ':');
			if line.inst <> '' then Write (f, ' ' + line.inst);
			if line.o1 <> '' then Write (f, ' ' + line.o1);
			if line.o2 <> '' then Write (f, ', ' + line.o2);
			if line.o3 <> '' then Write (f, ', ' + line.o3);
			if line.comment <> '' then Write (f, '; ' + line.comment);
			Writeln (f);
			end;
		end;
		
	procedure Write_line (lb, inst, o1, o2, o3, comment : AnsiString);
		begin
		SetLength (asm_output, line_num + 1);
		asm_output [line_num].lb := lb;
		asm_output [line_num].inst := inst;
		asm_output [line_num].o1 := o1;
		asm_output [line_num].o2 := o2;
		asm_output [line_num].o3 := o3;
		asm_output [line_num].comment := comment;
		line_num := line_num + 1;
		end;
		
	procedure Write_op (inst, o1, o2, o3 : AnsiString);
		begin
		Write_line ('', inst, o1, o2, o3, '');
		end;
		
	procedure Emit_blank_line;
		begin
		Write_line ('', '', '', '', '', '');
		end;
		
	function Mem_op (var x : Item; need_size_prefix : Boolean = False) : AnsiString;
		begin
		if need_size_prefix then
			Result := 'qword [' + reg_table [x.r] + ' + ' + IntToStr (x.a) + ']'
		else
			Result := '[' + reg_table [x.r] + ' + ' + IntToStr (x.a) + ']';
		end;
		
	procedure Put_op_reg (op, reg : Int64);
		begin
		Write_op (op_table [op], reg_table [reg], '', '');
		end;
		
	procedure Put_op_reg_reg (op, dest, src : Int64);
		begin
		Write_op (op_table [op], reg_table [dest], reg_table [src], '');
		end;
		
	procedure Put_op_reg_imm (op, reg, imm : Int64);
		begin
		Write_op (op_table [op], reg_table [reg], IntToStr (imm), '');
		end;
		
	procedure Put_op_reg_reg_imm (op, dest, src, imm : Int64);
		begin
		Write_op (op_table [op], reg_table [dest], reg_table [src], IntToStr (imm));
		end;
		
	procedure Put_op_mem (op : Int64; var x : Item);
		begin
		Write_op (op_table [op], Mem_op (x, True), '', '');
		end;
		
	procedure Put_op_mem_imm (op : Int64; var x : Item; imm : Int64);
		begin
		Write_op (op_table [op], Mem_op (x, True), IntToStr (imm), '');
		end;
		
	procedure Put_op_reg_mem (op, dest : Int64; var x : Item);
		begin
		Write_op (op_table [op], reg_table [dest], Mem_op (x), '');
		end;
		
	procedure Put_op_mem_reg (op : Int64; var x : Item; src : Int64);
		begin
		Write_op (op_table [op], Mem_op (x), reg_table [src], '');
		end;
		
	procedure Put_op_imm (op : Int64; imm : Int64);
		begin
		Write_op (op_table [op], IntToStr (imm), '', '');
		end;
		
	procedure Put_op_sym (op : Int64; sym : AnsiString);
		begin
		Write_op (op_table [op], sym, '', '');
		end;
		
	procedure Put_op_bare (op : Int64);
		begin
		Write_op (op_table [op], '', '', '');
		end;
		
	procedure Put_op_sym_reg (op : Int64; sym : AnsiString; reg : Int64);
		begin
		Write_op (op_table [op], sym, reg_table [reg], '');
		end;
		
	procedure Put_op_sym_imm (op : Int64; sym : AnsiString; imm : Int64);
		begin
		Write_op (op_table [op], sym, IntToStr (imm), '');
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	function Is_huge_const (x : Int64) : Boolean;
		begin
		if (x > $80000000) or (x <= -$80000000) then Is_huge_const := True 
		else Is_huge_const := False;
		end;
		
	procedure Check_overflow (op : Integer; x, y : Int64);
		begin
		end;
		
	procedure Inc_reg;
		begin
		if cur_reg < reg_RBX then Inc (cur_reg) else Scanner.Mark ('Register stack overflow');
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
				L3 := StrToInt (asm_output [L2].o1);
				if L3 = 0 then break;
				L2 := L3;
				end;
			asm_output [L2].o1 := IntToStr (L1);
			L1 := L0;
			end;
		Result := L1;	
		end;
		
	procedure Fix_link (L0 : Int64; lb : AnsiString);
		var L1 : Int64;
		begin
		while L0 <> 0 do
			begin
			L1 := StrToInt (asm_output [L0].o1);
			asm_output [L0].o1 := lb;
			L0 := L1;
			end;
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Ref_to_regI (var x : Item);
		begin
		Put_op_reg_mem (op_MOV, cur_reg, x); Inc_reg;
		x.r := cur_reg - 1; x.a := 0; x.mode := mode_regI;
		end;

	procedure load (var x : Item);
		begin
		if x.mode <> mode_reg then
			begin
			if x.mode = class_par then Ref_to_regI (x);
			if x.mode = class_var then
				begin	Inc_reg;	Put_op_reg_mem (op_MOV, cur_reg - 1, x);	end
			else if x.mode = class_const then
				begin Inc_reg; Put_op_reg_imm (op_MOV, cur_reg - 1, x.a); end
			else if x.mode = mode_regI then
				begin Put_op_reg_mem (op_MOV, x.r, x) end;
			x.mode := mode_reg; x.r := cur_reg - 1;
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
				end
			end
		else
			Scanner.Mark ('Invalid assignment');
		end;
	
	procedure Store (var x, y : Item);
		begin
		if x.read_only then Scanner.Mark ('Assignment to read-only variable');
		if x.mode = class_par then Ref_to_regI (x);
		if y.mode = mode_cond then
			Store_cond (x, y)
		else if (y.mode = class_const) and (not Is_huge_const (y.a)) then
			begin
			if x.mode = class_var then
				Put_op_mem_imm (op_MOV, x, y.a)
			else if x.mode = mode_regI then
				begin Put_op_mem_imm (op_MOV, x, y.a); Dec (cur_reg); end
			else
				Scanner.Mark ('Invalid assignment');
			end
		else
			begin
			if y.mode <> mode_reg then load (y);
			if x.mode = class_var then
				Put_op_mem_reg (op_MOV, x, y.r)
			else if x.mode = mode_regI then
				begin Put_op_mem_reg (op_MOV, x, y.r); Dec (cur_reg); end
			else
				Scanner.Mark ('Invalid assignment');
			Dec (cur_reg);
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
					begin	Put_op_mem_imm (op_CMP, x, 0); x.c := op_JNE; Dec (cur_reg) end;
				x.mode := mode_cond; x.a := 0; x.b := 0;
				end;
			end
		else
			Scanner.Mark ('Boolean type?');
		end;
		
	procedure Load_adr (var x : Item);
		begin
		if x.mode = class_var then
			begin Put_op_reg_mem (op_LEA, cur_reg, x); Inc_reg; end
		else if x.mode = class_par then
			begin Put_op_reg_mem (op_MOV, cur_reg, x); Inc_reg; end
		else if x.mode = mode_regI then
			begin Put_op_reg_mem (op_LEA, x.r, x); end
		else
			Scanner.Mark ('Load address error, not a variable?');
		x.mode := mode_reg;
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)
		
	procedure Put_division_op (op : Int64; var x, y : Item);
		var
			r : Int64;
		begin
		(* IDIV only accept dividend in RDX:RAX and divisor in reg/mem *)
		Put_op_reg_reg (op_XOR, reg_RDX, reg_RDX);
		
		if y.mode = class_const then load (y)
		else if y.mode = class_par then Ref_to_regI (y);
		
		if x.mode = class_par then Ref_to_regI (x);
		if x.mode = class_const then
			Put_op_reg_imm (op_MOV, reg_RAX, x.a)
		else if x.mode = mode_reg then
			Put_op_reg_reg (op_MOV, reg_RAX, x.r)
		else
			Put_op_reg_mem (op_MOV, reg_RAX, x);
			
		if y.mode = mode_reg then Put_op_reg (op_IDIV, y.r)
		else Put_op_mem (op_IDIV, y);
		
		if op = vop_div then r := reg_RAX else r := reg_RDX;
		
		if (x.mode = mode_reg) or (x.mode = mode_regI) then
			begin
			if (y.mode = mode_reg) or (y.mode = mode_regI) then Dec (cur_reg);
			end
		else
			begin
			if (y.mode <> mode_reg) and (y.mode <> mode_regI) then Inc_reg;
			end;
		
		x.r := cur_reg - 1; x.mode := mode_reg;
		Put_op_reg_reg (op_MOV, x.r, r);
		end;
		
	procedure Put_subtract_op (var x, y : Item);
		begin
		(* ADD, SUB, IMUL,... can not work directly with huge const (64 bits) *)
		load (x);
		
		if (y.mode = class_const) and Is_huge_const (y.a) then load (y)
		else if y.mode = class_par then Ref_to_regI (y);
		
		if y.mode = mode_reg then
			begin
			Put_op_reg_reg (op_SUB, x.r, y.r);
			if y.r < x.r then begin Put_op_reg_reg (op_MOV, y.r, x.r); x.r := y.r end;
			Dec (cur_reg);
			end
		else if y.mode = class_const then
			begin	Put_op_reg_imm (op_SUB, x.r, y.a)	end
		else if y.mode = mode_regI then
			begin
			Put_op_reg_mem (op_SUB, x.r, y);
			if y.r < x.r then begin Put_op_reg_reg (op_MOV, y.r, x.r); x.r := y.r end;
			Dec (cur_reg);
			end
		else if y.mode = class_var then
			begin	Put_op_reg_mem (op_SUB, x.r, y);	end;
		end;
	
	procedure Put_op (op : Int64; var x, y : Item);
		var
			t : Item;
		begin
		(* ADD, SUB, IMUL,... can not work directly with huge const (64 bits) *)
		if (x.mode = class_const) or (x.mode = class_var) then
			begin
			y.read_only := x.read_only; 
			t := y; y := x; x := t; 
			end;
			
		load (x);
		
		if (y.mode = class_const) and Is_huge_const (y.a) then
			load (y)
		else if y.mode = class_par then Ref_to_regI (y);
			
		if y.mode = mode_reg then
			begin
			Put_op_reg_reg (op, x.r, y.r);
			Dec (cur_reg);
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
			Dec (cur_reg);
			end
		end;

	function Put_compare_op (var x, y : Item) : Boolean;
		begin
		if (x.mode = class_const) and Is_huge_const (x.a) then load (x)
		else if x.mode = class_par then Ref_to_regI (x);
		
		if (y.mode = class_const) and Is_huge_const (y.a) then load (y)
		else if y.mode = class_par then Ref_to_regI (y);
		
		if ((x.mode = class_var) or (x.mode = mode_regI)) and ((y.mode = class_var) or (y.mode = mode_regI)) then
			load (x);
		
		Result := False;	
		if x.mode = mode_reg then
			begin
			if y.mode = mode_reg then begin Put_op_reg_reg (op_CMP, x.r, y.r); Dec (cur_reg); end
			else if y.mode = class_var then begin Put_op_reg_mem (op_CMP, x.r, y); end
			else if y.mode = mode_regI then begin Put_op_reg_mem (op_CMP, x.r, y); Dec (cur_reg); end
			else if y.mode = class_const then begin Put_op_reg_imm (op_CMP, x.r, y.a); end;
			Dec (cur_reg);
			end
		else if (x.mode = class_var) or (x.mode = mode_regI) then
			begin
			if y.mode = mode_reg then begin Put_op_mem_reg (op_CMP, x, y.r); Dec (cur_reg); end
			else if y.mode = class_const then begin Put_op_mem_imm (op_CMP, x, y.a); end;
			if x.mode = mode_regI then Dec (cur_reg);
			end
		else if x.mode = class_const then
			begin
			Result := True;
			if y.mode = mode_reg then begin Put_op_reg_imm (op_CMP, y.r, x.a); Dec (cur_reg); end
			else if y.mode = class_var then begin Put_op_mem_imm (op_CMP, y, x.a); end
			else if y.mode = mode_regI then begin Put_op_mem_imm (op_CMP, y, x.a); Dec (cur_reg); end
			end
		end;
		
	procedure Op1 (op : Integer; var x : Item);
		var
			t : Int64; lb : AnsiString;
		begin
		if op = Scanner.sym_minus then
			begin
			if x.mode = class_const then x.a := -x.a
			else
				begin
				if x.mode <> mode_reg then load (x);
				Put_op_reg (op_NEG, x.r);
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
		begin
		if x.typ^.form = type_integer then
			begin
			if (x.mode = class_const) and (y.mode = class_const) then
				begin
				Check_overflow (op, x.a, y.a);
				if op = Scanner.sym_plus then x.a := x.a + y.a
				else if op = Scanner.sym_minus then x.a := x.a - y.a
				else if op = Scanner.sym_times then x.a := x.a * y.a
				else if op = Scanner.sym_div then x.a := x.a div y.a
				else if op = Scanner.sym_mod then x.a := x.a mod y.a
				else Scanner.Mark ('Wrong operator');
				end
			else
				begin
				if op = Scanner.sym_plus then Put_op (op_ADD, x, y)
				else if op = Scanner.sym_minus then Put_subtract_op (x, y)
				else if op = Scanner.sym_times then Put_op (op_IMUL, x, y)
				else if op = Scanner.sym_div then Put_division_op (vop_div, x, y)
				else if op = Scanner.sym_mod then Put_division_op (vop_mod, x, y)
				else Scanner.Mark ('Wrong operator');
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
		
	procedure Index (var x, y : Item);
		begin
		if y.mode = class_const then
			begin
			if (y.a < 0) or (y.a >= x.typ^.len) then Scanner.Mark ('Index out of range');
			x.a := x.a + y.a * x.typ^.base^.size;
			end
		else
			begin
			if y.mode <> mode_reg then load (y);
			if range_check_flag then
				begin
				Put_op_reg_imm (op_CMP, y.r, x.typ^.len);
				Put_op_sym (op_JAE, 'RANGE_CHECK_TRAP');
				end;
			Put_op_reg_reg_imm (op_IMUL, y.r, y.r, x.typ^.base^.size);
			
			if x.mode = class_par then Ref_to_regI (x);
			if x.mode = class_var then
				begin
				Put_op_reg_reg (op_ADD, y.r, x.r);
				x.r := y.r; x.mode := mode_regI;
				end
			else if x.mode = mode_regI then
				begin
				Put_op_reg_reg (op_ADD, x.r, y.r);
				Dec (cur_reg);
				end;
			end;
		end;
	
	procedure Field (var x : Item; y : Object_);
		begin 
		if x.mode = class_par then Ref_to_regI (x);
		x.a := x.a + y^.val; x.typ := y^.typ;
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)
		
	procedure Relation (op : Integer; var x, y : Item);
		begin
		if (x.mode = class_const) and (y.mode = class_const) then
			begin
			if x.a = y.a then if op = Scanner.sym_equal then x.a := 1 else x.a := 0
			else if x.a <> y.a then if op = Scanner.sym_not_equal then x.a := 1 else x.a := 0
			else if x.a > y.a then if op = Scanner.sym_greater then x.a := 1 else x.a := 0
			else if x.a >= y.a then if op = Scanner.sym_greater_equal then x.a := 1 else x.a := 0
			else if x.a < y.a then if op = Scanner.sym_less then x.a := 1 else x.a := 0
			else if x.a <= y.a then if op = Scanner.sym_less_equal then x.a := 1 else x.a := 0;
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
		end;
		
	procedure Parameter (var x : Item; cls : Integer);
		begin
		if cls = class_par then
			if not x.read_only then
				begin	Load_adr (x); Put_op_reg (op_PUSH, cur_reg - 1); Dec (cur_reg); end
			else
				begin Scanner.Mark ('Can not pass read-only var as var parameter'); end
		else
			begin
			if x.mode = class_par then Ref_to_regI (x)
			else if (x.mode = class_const) and Is_huge_const (x.a) then load (x);
			
			if x.mode = mode_reg then
				begin Put_op_reg (op_PUSH, x.r); Dec (cur_reg); end
			else if x.mode = mode_regI then
				begin Put_op_mem (op_PUSH, x); Dec (cur_reg); end
			else if x.mode = class_var then
				begin Put_op_mem (op_PUSH, x); end
			else if x.mode = class_const then
				begin Put_op_imm (op_PUSH, x.a); end;
			end;
		end;
		
	procedure Call (var x : Item);
		begin
		Put_op_sym (op_CALL, 'PROC_' + IntToStr (x.a));
		end;
		
(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure SFunc_ORD (var x : Item);
		var
			lb1, lb2 : AnsiString;
		begin
		if x.mode = mode_cond then
			begin
			lb1 := 'END_SFUNC_ORD_' + IntToStr (line_num);
			Inc_reg;
			if (x.a = 0) and (x.b = 0) then Cond_jump (x);
			if x.a <> 0 then
				begin
				Put_op_reg_imm (op_MOV, cur_reg - 1, 1);
				Put_op_sym (op_JMP, lb1);
				lb2 := 'LINE_' + IntToStr (line_num);
				Emit_label (lb2); Fix_link (x.a, lb2);
				Put_op_reg_imm (op_MOV, cur_reg - 1, 0);
				Emit_label (lb1);
				end
			else if x.b <> 0 then
				begin
				Put_op_reg_imm (op_MOV, cur_reg - 1, 0);
				Put_op_sym (op_JMP, lb1);
				lb2 := 'LINE_' + IntToStr (line_num);
				Emit_label (lb2); Fix_link (x.b, lb2);
				Put_op_reg_imm (op_MOV, cur_reg - 1, 1);
				Emit_label (lb1);
				end;
			x.mode := mode_reg; x.r := cur_reg - 1;
			end
		else if (x.mode = class_const) and (x.typ^.form = type_boolean) then
			begin (* do nothing *) end
		else if x.typ^.form = type_boolean then
			begin load (x); end
		else
			begin
			Scanner.Mark ('Function ORD input must be BOOLEAN type');
			Make_clean_const (x, int_type, 0);
			end;
		x.typ := int_type;
		end;
		
	procedure SFunc_ODD (var x : Item);
		var
			lb1, lb2 : AnsiString;
		begin
		if x.typ^.form <> type_integer then
			begin
			Scanner.Mark ('Function ODD input must be INTEGER type');
			Make_clean_const (x, bool_type, 0);
			end
		else if x.mode = class_const then
			begin	x.typ := bool_type; x.a := abs (x.a mod 2); end
		else
			begin
			load (x);
			Put_op_reg_imm (op_SHR, cur_reg - 1, 1);
			x.mode := mode_cond; x.a := 0; x.b := 0; x.c := op_JC;
			end;
		end;
		
	procedure SFunc_BIT (var x, y : Item);
		begin
		end;

(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Emit_label (lb : AnsiString);
		begin
		Write_line (lb, '', '', '', '', '');
		end;

	procedure Make_item (var x : Item; y : Object_);
		begin
		x.mode := y^.class_; x.typ := y^.typ; x.lev := y^.lev; x.a := y^.val; x.b := 0;
		x.read_only := y^.read_only;
		if y^.lev = 0 then x.r := reg_RBX
		else if y^.lev = cur_lev then x.r := reg_RBP
		else begin Scanner.Mark ('Level!?'); x.r := 0; end;
		end;
		
	procedure Make_const (var x : Item; typ : Type_; val : Int64);
		begin
		x.mode := class_const; x.typ := typ; x.a := val;
		end;
		
	procedure Make_clean_const (var x : Item; typ : Type_; val : Int64);
		begin
		if (x.mode = mode_reg) or (x.mode = mode_regI) then Dec (cur_reg);
		x.mode := class_const; x.typ := typ; x.a := val;
		end;
		
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
		
		op_table [op_MOV] := 'MOV';
		op_table [op_XCHG] := 'XCHG';
		op_table [op_PUSH] := 'PUSH';
		op_table [op_XCHG] := 'POP';
		op_table [op_LEA] := 'LEA';
		
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
		
		op_table [op_LEAVE] := 'LEAVE';
		op_table [op_RET] := 'RET';
		op_table [op_CALL] := 'CALL';
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
	range_check_flag := True;

	New (bool_type); bool_type^.form := type_boolean; bool_type^.size := 8;
	New (int_type); int_type^.form := type_integer; int_type^.size := 8;

	Init_mnemonic_table;
	Init_relation_table;
	
	cur_reg := reg_RCX;
end.