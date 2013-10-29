unit Parser;

interface
	uses
		Scanner, Base, Generator;

	var
		sym : Integer;

	procedure Declarations (var var_base : Base.MachineInteger);
	procedure ProcedureDecl;
	procedure StatementSequence;
	procedure Module (var modid : AnsiString);

implementation
	uses
		Sysutils;

	type
		Undef_pointer = ^Undef_pointer_desc;
		Undef_pointer_desc = Record
			types : Array of Base.Type_;
			base_type_name : AnsiString;
			next : Undef_pointer;
			end;

	var
		undef_ptr_list : Undef_pointer;

	procedure Check_int (var x : Base.Item);
		begin
		if (x.mode = Base.class_typ) or (x.mode = Base.class_proc)
		or (x.mode = Base.class_sproc) or (x.typ.form = Base.type_proc) then
			begin
			Scanner.Mark ('Error: Expecting an integer but found type or procedure');
			Generator.Make_clean_const (x, Base.int_type, 0)
			end
		else if x.typ.form <> Base.type_integer then
			begin
			Scanner.Mark ('Error: Not an integer');
			Generator.Make_clean_const (x, Base.int_type, 0)
			end
		end;

	procedure Check_set (var x : Base.Item);
		begin
		if (x.mode = Base.class_typ) or (x.mode = Base.class_proc)
		or (x.mode = Base.class_sproc) or (x.typ.form = Base.type_proc) then
			begin
			Scanner.Mark ('Error: Expecting a set but found type or procedure');
			Generator.Make_clean_const (x, Base.set_type, 0)
			end
		else if x.typ.form <> Base.type_set then
			begin
			Scanner.Mark ('Error: Not a set');
			Generator.Make_clean_const (x, Base.set_type, 0)
			end
		end;

	procedure Check_bool (var x : Base.Item);
		begin
		if (x.mode = Base.class_typ) or (x.mode = Base.class_proc)
		or (x.mode = Base.class_sproc) or (x.typ.form = Base.type_proc) then
			begin
			Scanner.Mark ('Error: Expecting a boolean value but found type or procedure');
			Generator.Make_clean_const (x, Base.bool_type, 0)
			end
		else if x.typ.form <> Base.type_boolean then
			begin
			Scanner.Mark ('Error: Not a boolean value');
			Generator.Make_clean_const (x, Base.bool_type, 0)
			end
		end;

	procedure Check_operator (var x : Base.Item; op : Integer);
		var
			form : Integer;
		begin
		if (x.mode = Base.class_typ) or (x.mode = Base.class_proc)
		or (x.mode = Base.class_sproc) or (x.typ^.form = Base.type_proc) then
			begin
			Scanner.Mark ('Error: There is no operator for type or procedure');
			Generator.Make_clean_const (x, Base.int_type, 0)
			end;
		form := x.typ.form;
		if (op = Scanner.sym_plus) or (op = Scanner.sym_minus) or (op = Scanner.sym_times) then
			begin
			if (form <> Base.type_integer) and (form <> Base.type_set) then
				begin
				Scanner.Mark ('Error: +, -, * only compatible with INTEGER or SET types');
				Generator.Make_clean_const (x, Base.int_type, 0)
				end
			end
		else if (op = Scanner.sym_div) or (op = Scanner.sym_mod) then
			begin
			if form <> Base.type_integer then
				begin
				Scanner.Mark ('Error: DIV, MOD only compatible with INTEGER types');
				Generator.Make_clean_const (x, Base.int_type, 0)
				end
			end
		else if op = Scanner.sym_slash then
			begin
			if form <> Base.type_set then
				begin
				Scanner.Mark ('Error: / only compatible with SET type');
				Generator.Make_clean_const (x, Base.set_type, 0)
				end
			end
		else if (op = Scanner.sym_and) or (op = Scanner.sym_or) or (op = Scanner.sym_not) then
			begin
			if form <> Base.type_boolean then
				begin
				Scanner.Mark ('Error: &, OR, ~ only compatible with BOOLEAN type');
				Generator.Make_clean_const (x, Base.bool_type, 0)
				end
			end
		else if (op = Scanner.sym_equal) or (op = Scanner.sym_not_equal) then
			begin
			if (form <> Base.type_integer) and (form <> Base.type_boolean)
			and (form <> Base.type_set) and (form <> Base.type_pointer) then
				begin
				Scanner.Mark ('Error: =, # only compatible with INTEGER, BOOLEAN, SET or POINTER types');
				Generator.Make_clean_const (x, Base.int_type, 0)
				end
			end
		else if (op = Scanner.sym_greater) or (op = Scanner.sym_less) then
			begin
			if form <> Base.type_integer then
				begin
				Scanner.Mark ('Error: >, < only compatible with INTEGER types');
				Generator.Make_clean_const (x, Base.int_type, 0)
				end
			end
		else if (op = Scanner.sym_greater_equal) or (op = Scanner.sym_less_equal) then
			begin
			if (form <> Base.type_integer) and (form <> Base.type_set) then
				begin
				Scanner.Mark ('Error: >=, <= only compatible with INTEGER or SET types');
				Generator.Make_clean_const (x, Base.int_type, 0)
				end
			end;
		end;

	function Is_structured_type (tp : Base.Type_) : Boolean;
		begin
		if (tp^.form = Base.type_record)
		or (tp^.form = Base.type_array)
		or (tp^.form = Base.type_dynArray) then
			Result := True
		else
			Result := False;
		end;

	procedure Fix_undef_ptr (obj : Base.Object_);
		var
			undef : Undef_pointer;
		begin
		undef := undef_ptr_list;
		while undef <> nil do
			begin
			if (undef.typ.base = Base.int_type) and (obj.name = undef.base_type_name) then
				begin
				if obj.typ.form = Base.type_record then
					undef.typ.base := obj.typ
				else
					begin
					Scanner.Mark ('Error: ' + obj.name + ' must be a RECORD type')
					end
				end;
			undef := undef.next
			end
		end;

	function Is_base_type (typ1, typ2 : Base.Type_) : Boolean;
		begin
		Result := False;
		while typ2 <> nil do
			begin
			if typ2 = typ1 then Result := True;
			typ2 := typ2.base
			end
		end;

	function Check_compatible_open_array (typ1, typ2 : Base.Type_) : Boolean;
		begin
		if typ1.base = typ2.base then
			Result := True
		else if (typ1.base.form = Base.type_array)
		and (typ2.base.form = Base.type_array)
		and (typ1.base.len = 0) and (typ2.base.len = 0) then
			Result := Check_compatible_open_array (typ1.base, typ2.base)
		else
			Result := False
		end;

	function Check_compatible_procedure1 (typ1, typ2 : Base.Type_) : Boolean;
		var
			obj1, obj2 : Base.Object_;
		begin
		if typ1 = typ2 then
			Result := True
		else if typ1.base <> typ2.base then
			Result := False
		else
			begin
			obj1 := typ1.fields; obj2 := typ2.fields;
			Result := True;
			while Result and (obj1 <> Base.guard) and (obj2 <> Base.guard) do
				begin
				if (obj1.class_ <> obj2.class_)
				or (Base.read_only in (obj1.flag >< obj2.flag)) then
					Result := False
				else if obj1.typ = obj2.typ then
					(* Do nothing *)
				else if (obj1.typ.form = Base.type_array)
				and (obj2.typ.form = Base.type_array)
				and (obj1.typ.len = 0) and (obj2.typ^.len = 0) then
					Result := Check_compatible_open_array (obj1.typ, obj2.typ)
				else if (obj1.typ.form = Base.type_proc)
				and (obj2.typ.form = Base.type_proc) then
					Result := Check_compatible_procedure1 (obj1.typ, obj2.typ)
				else Result := False;
				obj1 := obj1.next; obj2 := obj2.next;
				end;
			if obj1 <> obj2 then Result := False
			end
		end;

	function Check_compatible_procedure2 (typ : Base.Type_; proc : Base.Object_) : Boolean;
		var
			obj1, obj2 : Base.Object_;
		begin
		if typ.base <> proc.typ then
			Result := False
		else
			begin
			obj1 := typ.fields; obj2 := proc.dsc;
			Result := True;
			while Result and (obj1 <> Base.guard) and (obj2 <> Base.guard) do
				begin
				if (obj1.class_ <> obj2.class_)
				or (Base.read_only in (obj1.flag >< obj2.flag))
				or not (Base.is_param in obj2.flag) then
					Result := False
				else if obj1.typ = obj2.typ then
					(* Do nothing *)
				else if (obj1.typ.form = Base.type_array)
				and (obj2.typ.form = Base.type_array)
				and (obj1.typ.len = 0) and (obj2.typ.len = 0) then
					Result := Check_compatible_open_array (obj1.typ, obj2.typ)
				else if (obj1.typ.form = Base.type_proc)
				and (obj2.typ.form = Base.type_proc) then
					Result := Check_compatible_procedure1 (obj1.typ, obj2.typ)
				else Result := False;
				obj1 := obj1.next; obj2 := obj2.next;
				end;
			if (obj1 = Base.guard) and (obj2 <> Base.guard)
			and (Base.is_param in obj2.flag) then
				Result := False
			end
		end;

	procedure find_undef (var undef : Undef_pointer; name : AnsiString);
		var
			found : Boolean;
		begin
		undef := undef_ptr_list;
		found := False;
		while (undef <> nil) and not found do
			begin
			if undef.base_type_name = name then found := True
			else undef := undef.next
			end
		end;

(* ------------------------------------------------------------------------------------------ *)
// Begin parser procedures part

	procedure expression (var x : Base.Item); forward;

	procedure ImportList (modid : AnsiString);
		var
			modul_name : AnsiString;
		begin
		Scanner.Get (sym);
		while true do
			begin
			if sym < Scanner.sym_ident then
				begin
				Scanner.Mark ('Error: Identifier expected');
				repeat Scanner.Get (sym) until sym >= Scanner.sym_ident;
				end;
			if sym = Scanner.sym_ident then
				begin
				modul_name := Scanner.id;
				Scanner.Get (sym);
				if sym = Scanner.sym_becomes then
					begin
					Scanner.Get (sym);
					if Scanner.id <> modid then
						Base.Import_module (modul_name, Scanner.id)
					else Scanner.Mark ('Error: Self importing is forbidden');
					Scanner.Get (sym)
					end
				else if modul_name <> modid then
					Base.Import_module (modul_name, modul_name)
				else Scanner.Mark ('Error: Self importing is forbidden');
				
				if sym = Scanner.sym_comma then Scanner.Get (sym)
				else if sym = Scanner.sym_semicolon then break
				else begin Scanner.Mark ('Error: Semicolon or comma expected'); break end
				end
			else	
				begin
				Scanner.Mark ('Error: Identifier expected');
				break
				end
			end
		end;

	procedure IdentList (class_ : Integer; var first : Base.Object_);
		var
			obj : Base.Object_;
		begin
		if sym = Scanner.sym_ident then
			begin
			Base.New_obj (first, class_); Scanner.Get (sym);
			if sym = Scanner.sym_times then
				begin
				if Base.cur_lev = 0 then first.flag := first.flag + [Base.exported]
				else Scanner.Mark ('Error: Not able to export a non-global identifier');
				Scanner.Get (sym)
				end;
			while sym = Scanner.sym_comma do
				begin
				Scanner.Get (sym);
				if sym = Scanner.sym_ident then
					begin
					Base.New_obj (obj, class_); Scanner.Get (sym);
					if sym = Scanner.sym_times then
						begin
						if Base.cur_lev = 0 then obj.flag := obj.flag + [Base.exported]
						else Scanner.Mark ('Error: Not able to export a non-global identifier');
						Scanner.Get (sym)
						end;
					end
				else Scanner.Mark ('Error: No identifier after ,');
				end;
			if sym = Scanner.sym_colon then Scanner.Get (sym)
			else Scanner.Mark ('Error: No : after identifier list');
			end;
		end;

	procedure FormalParameters (proc : Base.Object_; var para_block_size : Base.MachineInteger);
		var
			obj : Base.Object_;

		procedure OpenArray (var tp : Base.Type_; exported : Boolean);
			var
				t : Base.Type_;
				obj : Base.Object_;
			begin
			Base.New_typ (tp);
			tp.form := Base.type_array;
			tp.len := 0; tp.size := Base.Word_size * 2; tp.obj := nil;

			Scanner.Get (sym);
			if sym = Scanner.sym_of then Scanner.Get (sym)
			else Scanner.Mark ('Missing OF or not an open array');

			t := tp;
			while sym = Scanner.sym_array do
				begin
				Base.New_typ (t.base);
				t := t.base;
				t.form := Base.type_array;
				t.len := 0; t.size := 0; t.obj := nil;
				tp.size := tp.size + Base.Word_size;
				Scanner.Get (sym);
				if sym = Scanner.sym_of then Scanner.Get (sym)
				else Scanner.Mark ('Missing OF or not an open array');
				end;

			if sym = Scanner.sym_ident then
				begin
				Base.find (obj); Scanner.Get (sym);
				if obj.class_ = Base.class_typ then
					t.base := obj.typ
				else
					begin
					Scanner.Mark ('Type not found');
					t.base := Base.int_type;
					end;
				end
			else
				begin
				Scanner.Mark ('No type identifier?');
				t.base := Base.int_type;
				end;
			end;

		procedure FPSection (var para_block_size : Base.MachineInteger; proc : Base.Object_);
			var
				obj, first : Base.Object_;
				tp : Base.Type_;
				para_size : Base.MachineInteger;
				cls : Integer;
				read_only : Boolean;
			begin
			if sym = Scanner.sym_var then
				begin
				Scanner.Get (sym);
				IdentList (Base.class_par, first)
				end
			else IdentList (Base.class_var, first);

			if sym = Scanner.sym_ident then
				begin
				Base.find (obj); Scanner.Get (sym);
				if obj.class_ = Base.class_typ then tp := obj.typ
				else begin Scanner.Mark ('Type not found'); tp := Base.int_type; end;
				end
			else if sym = Scanner.sym_array then
				OpenArray (tp, Base.exported in proc.flag)
			else
				begin
				Scanner.Mark ('Identifiers list without type');
				tp := Base.int_type;
				end;

			if (Base.exported in proc.flag)
			and (tp.flag * [Base.exported, Base.predefined, Base.imported] = []) then
				begin
				Scanner.Mark ('Error: Not able to export procedure with non-public parameter type');
				proc.flag := proc.flag - [Base.exported]
				end;

			cls := first.class_; read_only := False;
			if (tp.form = Base.type_array) and (tp.len = 0) then
				para_size := tp.size
			else if (tp.form = Base.type_record) and (cls = Base.class_par) then
				para_size := Base.Word_size * 2
			else
				para_size := Base.Word_size;

			if (cls = Base.class_var) and Is_structured_type (tp) then
				begin
				cls := Base.class_par;
				read_only := True;
				end;

			obj := first;
			while obj <> Base.guard do
				begin
				obj.typ := tp; obj.lev := Base.cur_lev;
				obj.class_ := cls;
				if read_only then obj.flag := obj.flag + [Base.read_only, Base.is_param]
				else obj.flag := obj.flag + [Base.is_param];
				para_block_size := para_block_size - para_size;
				obj.val := para_block_size;
				obj := obj.next;
				end;
			end;

		begin (* FormalParameters *)
		Scanner.Get (sym);
		if sym = Scanner.sym_rparen then
			Scanner.Get (sym)
		else
			begin
			FPSection (para_block_size, proc);
			while sym = Scanner.sym_semicolon do
				begin
				Scanner.Get (sym);
				FPSection (para_block_size, proc);
				end;
			if sym = Scanner.sym_rparen then Scanner.Get (sym)
			else Scanner.Mark ('Error: No closing )');
			end;
		if sym = Scanner.sym_colon then
			begin
			proc.typ := Base.int_type;
			Scanner.Get (sym);
			if sym = Scanner.sym_ident then
				begin
				Base.find (obj); Scanner.Get (sym);
				if obj.class_ = Base.class_typ then
					begin
					if (obj.typ.form = Base.type_array)	or (obj.typ.form = Base.type_record) then
						Scanner.Mark ('Error: Function result type must be scalar type')
					else proc.typ := obj.typ;
					
					if (Base.exported in proc.flag)
					and (obj.typ.flag * [Base.exported, Base.predefined, Base.imported] = []) then
						begin
						Scanner.Mark ('Error: Not able to export function with non-public result type');
						proc.flag := proc.flag - [Base.exported]
						end
					end
				else Scanner.Mark ('Error: Type not found');
				end
			else Scanner.Mark ('Error: Function result type missing?');
			end;
		end;

	procedure Type_ (var typ : Base.Type_; name : AnsiString; exported : Boolean); forward;
		// The parameter 'name' is for preventing recursive definition

	procedure StrucType (var typ : Base.Type_; name : AnsiString; exported : Boolean);
	
		(* Dynamic array type is just a big pointer to an array *)
		(* Syntax: ARRAY OF type *)
		(* Only support 1-dimension dynamic array *)
		{ Dynamic array temporary not working }
		
		{ procedure DynArrayType (var typ : Base.Type_; name : AnsiString);
			begin
			Base.New_typ (typ);
			typ.form := Base.type_dynArray;
			typ.len := 0;
			typ.size := 3 * Base.Word_size; (* places for address and array desc *)
			typ.num_of_pointers := 1;

			Scanner.Get (sym);
			if sym = Scanner.sym_array then
				begin
				Scanner.Mark ('Dynamic array of array type is not supported');
				typ.base := Base.int_type;
				end
			else
				begin
				Type_ (typ.base, name);
				if typ.base.form = Base.type_record then
					typ.size := typ.size + Base.Word_size (* place for type tag *)
				else if typ.base.form = Base.type_array then
					begin
					Scanner.Mark ('Dynamic array of array type is not supported');
					typ.base := Base.int_type;
					end
				end
			end; }

		procedure ArrayType (var typ : Base.Type_; name : AnsiString; exported : Boolean);
			var
				tp : Base.Type_;
				x : Base.Item;
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_of then
				Scanner.Mark ('Error: Dynamic array not supported')
				{ DynArrayType (typ, name) }
			else
				begin
				Base.New_typ (typ);
				typ.form := Base.type_array;
				
				expression (x);
				if (x.mode <> Base.class_const) or (x.a <= 0)
				or (x.typ.form <> Base.type_integer) then
					begin
					Scanner.Mark ('Error: Invalid array size');
					typ.len := 1
					end
				else typ.len := x.a;
				
				if sym = Scanner.sym_of then Scanner.Get (sym)
				else Scanner.Mark ('Error: No OF in array declaration');
				
				Type_ (tp, name, exported);
				
				typ.base := tp;
				typ.size := typ.len * tp.size;
				typ.num_of_pointers := tp.num_of_pointers * typ.len;
				if exported and (tp.flag * [Base.exported, Base.predefined, Base.imported] <> []) then
					typ.flag := typ.flag + [Base.exported]
				end
			end;

		procedure RecordType (var typ : Base.Type_; name : AnsiString; exported : Boolean);
			var
				tp : Base.Type_;
				obj, first : Base.Object_;
			begin
			Base.New_typ (typ);
			typ.form := Base.type_record;
			typ.size := 0; typ.base := nil;
			if exported then typ.flag := typ.flag + [Base.exported];

			Scanner.Get (sym);
			if sym = Scanner.sym_lparen then
				begin
				Scanner.Get (sym);
				if sym = Scanner.sym_ident then
					begin
					Base.find (obj);
					Scanner.Get (sym)
					end
				else Scanner.Mark ('Error: Record type identifier expected');
				
				if (obj.class_ <> Base.class_typ) or (obj.typ.form <> Base.type_record) then
					Scanner.Mark ('Error: Record type identifier expected')
				else if typ = obj.typ then
					Scanner.Mark ('Error: Recursive type definition')
				else
					begin
					typ.base := obj.typ;
					typ.size := obj.typ.size
					end;
					
				if sym = Scanner.sym_rparen then Scanner.Get (sym)
				else Scanner.Mark ('Error: No closing )')
				end;

			if typ.base <> nil then
				begin
				typ.len := typ.base.len + 1;
				typ.num_of_pointers := typ.base.num_of_pointers;
				end
			else typ.len := 1;
				
			Base.Open_scope;
			while true do
				begin
				if sym = Scanner.sym_ident then
					begin
					IdentList (Base.class_var, first);
					if sym = Scanner.sym_pointer then StrucType (tp, '', exported)
					else Type_ (tp, name, exported);
					
					typ.num_of_pointers := typ.num_of_pointers + tp.num_of_pointers;
						
					obj := first;
					while obj <> Base.guard do
						begin
						obj.typ := tp; obj.val := typ.size;
						typ.size := typ.size + obj.typ.size;
						obj := obj.next;
						if Base.exported in obj.flag then
							begin
							if not exported then
								begin
								obj.flag := obj.flag - [Base.exported];
								Scanner.Mark ('Error: Not able to export fields of a non-public record')
								end
							else if tp.flag * [Base.exported, Base.predefined, Base.imported] = [] then
								begin
								obj.flag := obj.flag - [Base.exported];
								Scanner.Mark ('Error: Not able to export fields with non-public type')
								end
							end
						end;
					end;
				if sym = Scanner.sym_semicolon then Scanner.Get (sym)
				else if sym = Scanner.sym_ident then Scanner.Mark ('Error: No ; after identifier list')
				else break;
				end;
			typ.fields := Base.top_scope.next;
			Base.Close_scope;
			Base.Add_type_tag (typ);
			if sym = Scanner.sym_end then Scanner.Get (sym)
			else Scanner.Mark ('Error: No END after record type declaration');
			end;

		procedure PointerType (var typ : Base.Type_; name : AnsiString; exported : Boolean);
			var
				obj : Base.Object_;
				undef : Undef_pointer;
			begin
			Base.New_typ (typ);
			typ.form := Base.type_pointer; typ.size := Base.Word_size;
			typ.num_of_pointers := 1;
			
			Scanner.Get (sym);
			if sym = Scanner.sym_to then Scanner.Get (sym)
			else Scanner.Mark ('Error: No TO in pointer type declaration');
			
			if sym = Scanner.sym_ident then
				begin
				Base.find2 (obj); Scanner.Get (sym);
				if obj.name = name then
					begin
					Scanner.Mark ('Error: Recursive type definition');
					Dispose (typ);
					typ := Base.int_type
					end
				else if obj = Base.guard then
					begin
					New (undef);
					undef.typ := typ;
					undef.base_type_name := obj.name;
					undef.next := undef_ptr_list;
					undef_ptr_list := undef;
					typ.base := Base.int_type;
					if exported then typ.flag := typ.flag + [Base.exported]
					end
				else if (obj.class_ = Base.class_typ)
				and (obj.typ.form = Base.type_record) then
					begin
					typ.base := obj.typ;
					if exported and (obj.flag * [Base.exported, Base.imported] <> []) then
						typ.flag := typ.flag + [Base.exported]
					end
				else
					begin
					Scanner.Mark ('Error: RECORD type expected');
					Dispose (typ);
					typ := Base.int_type
					end
				end
			else if sym = Scanner.sym_record then
				begin
				RecordType (typ.base, '', exported);
				if exported then typ.flag := typ.flag + [Base.exported]
				end
			else
				begin
				Scanner.Mark ('Error: RECORD type expected');
				Dispose (typ);
				typ := Base.int_type
				end
			end;

		procedure ProcedureType (var typ : Base.Type_; exported : Boolean);
			var
				obj : Base.Object_;
				x : Base.MachineInteger;
			begin
			Base.New_typ (typ);
			typ.form := Base.type_proc;
			typ.size := 8; 
			Base.Open_scope;
			Scanner.Get (sym);
			if sym = Scanner.sym_lparen then
				begin
				New (obj);
				if exported then obj.flag := [Base.exported];
				FormalParameters (obj, x);
				typ.base := obj.typ;
				if Base.exported in obj.flag then typ.flag := typ.flag + [Base.exported];
				Dispose (obj)
				end
			else
				begin
				typ.base := nil;
				if exported then typ.flag := typ.flag + [Base.exported]
				end;
			typ.fields := top_scope.next;
			Base.Close_scope;
			end;
		
		begin (* StrucType *)
		if sym = Scanner.sym_array then
			ArrayType (typ, name, exported)
		else if sym = Scanner.sym_record then
			RecordType (typ, name, exported)
		else if sym = Scanner.sym_pointer then
			PointerType (typ, name, exported)
		else if sym = Scanner.sym_procedure then
			ProcedureType (typ, exported)
		else
			begin
			Scanner.Mark ('Error: No type defining?');
			typ := Base.int_type
			end
		end;

	procedure Type_ (var typ : Base.Type_; name : AnsiString; exported : Boolean);
		var
			obj : Base.Object_;
		begin
		typ := Base.int_type;
		if (sym <> Scanner.sym_ident) and (sym < Scanner.sym_array) then
			begin
			Scanner.Mark ('Error: No type?');
			repeat Scanner.Get (sym) until (sym = Scanner.sym_ident) or (sym >= Scanner.sym_array);
			end;
		if sym = Scanner.sym_ident then
			begin
			Base.find (obj); Scanner.Get (sym);
			if obj.class_ = Base.class_typ then
				begin
				typ := obj.typ;
				if typ.flag * [Base.is_used, Base.imported] = [Base.imported] then
					Base.Add_imported_type (typ);
				typ.flag := typ.flag + [Base.is_used];
				end
			else Scanner.Mark ('Error: Type not found');
			end
		else
			begin
			StrucType (typ, name, exported)
			end
		end; (* Type_ *)

	procedure Declarations (var var_base : Base.MachineInteger);
		var
			obj, first : Base.Object_;
			tp : Base.Type_;
			x : Base.Item;
			undef : Undef_pointer;
		begin
		while true do
			begin
			(* Const declaration *)
			if sym = Scanner.sym_const then
				begin
				Scanner.Get (sym);
				while sym = Scanner.sym_ident do
					begin
					Base.New_obj (obj, Base.class_const);
					Scanner.Get (sym);
					if sym = Scanner.sym_times then
						begin
						if cur_lev = 0 then
							begin
							obj.flag := obj.flag + [Base.exported];
							Scanner.Get (sym)
							end
						else Scanner.Mark ('Error: Not able to export a non-global identifier')
						end;
					if sym = Scanner.sym_equal then Scanner.Get (sym)
					else Scanner.Mark ('Error: No = in const declaration');
					expression (x);
					if x.mode = Base.class_const then
						begin
						obj.val := x.a;
						obj.typ := x.typ;
						end
					else Scanner.Mark ('Error: Expression is not const');
					if sym = Scanner.sym_semicolon then Scanner.Get (sym)
					else Scanner.Mark ('Error: No ; after const declaration');
					end;
				end;
			(* Type declaration *)
			if sym = Scanner.sym_type then
				begin
				Scanner.Get (sym);
				while sym = Scanner.sym_ident do
					begin
					Base.New_obj (obj, Base.class_typ);
					Scanner.Get (sym);
					
					if sym = Scanner.sym_times then
						begin
						if cur_lev = 0 then obj.flag := obj.flag + [Base.exported]
						else Scanner.Mark ('Error: Not able to export a non-global identifier');
						Scanner.Get (sym)
						end;
						
					if sym = Scanner.sym_equal then Scanner.Get (sym)
					else Scanner.Mark ('Error: No = in type declaration');

					if sym = Scanner.sym_ident then
						begin
						Scanner.Mark ('Error: Alias type is not supported');
						Scanner.Get (sym);
						obj.typ := Base.int_type
						end
					else
						begin
						StrucType (obj.typ, obj.name, Base.exported in obj.flag);
						if obj.typ <> Base.int_type then
							begin
							obj.typ.obj := obj;
							if (Base.exported in obj.flag)
							and not (Base.exported in obj.typ.flag) then
								begin
								Scanner.Mark ('Error: Defined type was exported but defining type is not public');
								obj.flag := obj.flag - [Base.exported]
								end;
							end
						end;
					
					if sym = Scanner.sym_semicolon then Scanner.Get (sym)
					else Scanner.Mark ('Error: No ; after type declaration');
					if undef_ptr_list <> nil then Fix_undef_ptr (obj)
					end
				end;
			(* Variable declaration *)
			if sym = Scanner.sym_var then
				begin
				Scanner.Get (sym);
				while sym = Scanner.sym_ident do
					begin
					IdentList (Base.class_var, first);
					Type_ (tp, '', False);
					obj := first;
					while obj <> Base.guard do
						begin
						obj.typ := tp;
						if Base.cur_lev > 0 then
							begin
							var_base := var_base - obj.typ.size;
							obj.val := var_base;
							end
						else if Base.cur_lev = 0 then
							begin
							Base.Add_global_var (obj);
							var_base := var_base + obj.typ.size;
							end;
						if (Base.exported in obj.flag)
						and (tp.flag * [Base.exported, Base.predefined, Base.imported] = []) then
							begin
							Scanner.Mark ('Error: Not able to export variables with non-public type');
							obj.flag := obj.flag - [Base.exported]
							end;
						obj := obj.next;
						end;
					if sym = Scanner.sym_semicolon then Scanner.Get (sym)
					else Scanner.Mark ('Error: No ; after variable declaration');
					end;
				end;
			if (sym >= Scanner.sym_const) and (sym <= Scanner.sym_var) then
				Scanner.Mark ('Error: Bad declaration sequence')
			else break;
			end;
		(* Check for remaining undef pointer types *)
		while undef_ptr_list <> nil do
			begin
			if undef_ptr_list.typ.base = Base.int_type then
				Scanner.Mark ('Base type ' + undef_ptr_list.base_type_name + ' is not defined');
			undef := undef_ptr_list;
			undef_ptr_list := undef.next;
			Dispose (undef)
			end
		end;

	procedure ActualParameters (var x : Base.Item; obj : Base.Object_);
		var
			y : Base.Item;
			par, obj2 : Base.Object_;
		begin
		if obj = nil then par := x.typ.fields else par := obj.dsc;

		Scanner.Get (sym);
		if sym <> Scanner.sym_rparen then
			begin
			while true do
				begin
				expression (y);
				if Base.is_param in par.flag then
					begin
					if (y.mode = Base.class_typ) or (y.mode = Base.class_sproc) then
						Scanner.Mark ('Invalid actual parameter')
					(* PROCEDURE parameter *)
					else if y.mode = Base.class_proc then
						begin
						Base.find_proc (obj2, y);
						if obj2 = Base.guard then
							Scanner.Mark ('Procedure not found or not a global declared procedure')
						else if (par.typ.form = Base.type_proc)
						and Check_compatible_procedure2 (par.typ, obj2) then
							begin
							if par.class_ = Base.class_var then
								Generator.Procedure_parameter (y)
							else Scanner.Mark ('Not able to pass a procedure as a VAR param')
							end
						else Scanner.Mark ('Incompatible param types');
						end
					else if par.typ.form = Base.type_proc then
						begin
						if (y.typ.form = Base.type_proc)
						and Check_compatible_procedure1 (par.typ, y.typ) then
							Generator.Normal_parameter (y, par)
						else Scanner.Mark ('Incompatible param types')
						end
					(* Open ARRAY parameter *)
					else if (par.typ.form = Base.type_array) and (par.typ.len = 0) then
						begin
						if y.typ.form = Base.type_array then
							Generator.Open_array_Param1 (y, par, par.typ, 1)
						else if y.typ^.form = Base.type_dynArray then
							Generator.Open_array_Param2 (y, par)
						else Scanner.Mark ('Incompatible param types');
						end
					(* RECORD variable parameter *)
					else if (par.typ.form = Base.type_record)
					and not (Base.read_only in par.flag) then
						begin
						if y.typ.form = Base.type_record then
							begin
							if Is_base_type (par.typ, y.typ) then
								Generator.Record_variable_parameter (y)
							else Scanner.Mark ('Actual type must be an extension of formal type')
							end
						else Scanner.Mark ('Incompatible param types');
						end
					(* POINTER parameter *)
					else if par.typ.form = Base.type_pointer then
						begin
						if y.typ.form = Base.type_pointer then
							begin
							if Is_base_type (par.typ.base, y.typ.base) then
								Generator.Normal_parameter (y, par)
							else Scanner.Mark ('Actual type must be an extension of formal type')
							end
						else Scanner.Mark ('Incompatible param types');
						end
					(* the others *)
					else if y.typ = par.typ then
						Generator.Normal_parameter (y, par)
					else Scanner.Mark ('Incompatible param types');
					par := par.next;
					end
				else Scanner.Mark ('Too many parameters');

				if sym = Scanner.sym_comma then Scanner.Get (sym)
				else if sym = Scanner.sym_rparen then begin Scanner.Get (sym); break; end
				else if sym >= Scanner.sym_semicolon then begin Scanner.Mark ('No closing )'); break; end
				else Scanner.Mark ('No ) or ,');
				end;
			end
		else Scanner.Get (sym); (* Empty parameters list *)
		
		if Base.is_param in par.flag then Scanner.Mark ('Too few parameters');
		end;

   procedure selector (var x : Base.Item);
		var
			y : Base.Item;
			obj : Base.Object_;

		procedure TypeGuard (var x : Base.Item);
			var
				typ : Base.Type_;
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_ident then
				begin
				Type_ (typ, '', False);
				if x.typ.form = Base.type_pointer then
					begin
					if typ.form = Base.type_pointer then
						begin
						if Is_base_type (x.typ.base, typ.base) then
							begin
							Generator.Type_guard (x, typ);
							x.typ := typ
							end
						else Scanner.Mark ('Type guard only applicable with an extension type of the declared type')
						end
					else Scanner.Mark ('Expect a pointer type in type guard')
					end
				else if (x.typ.form = Base.type_record)
				and (x.mode = Base.class_par) and not (Base.read_only in x.flag) then
					begin
					if typ.form = Base.type_record then
						begin
						if Is_base_type (x.typ, typ) then
							begin
							Generator.Type_guard (x, typ);
							x.typ := typ
							end
						else Scanner.Mark ('Type guard only applicable with an extension type of the declared type')
						end
					else Scanner.Mark ('Expect a record type in type guard')
					end
				else Scanner.Mark ('Type guard only applicable with pointers or var parameters of record type');
				end
			else Scanner.Mark ('Expect an identifier in type guard');
			if sym = Scanner.sym_rparen then Scanner.Get (sym)
			else Scanner.Mark ('Missing )')
			end;

		begin (* selector *)
		while ((sym = Scanner.sym_lbrak) or (sym = Scanner.sym_period)
		or (sym = Scanner.sym_arrow) or (sym = Scanner.sym_lparen))
		and (x.mode <> Base.class_proc) and (x.mode <> Base.class_typ)
		and (x.mode <> Base.class_sproc) and (x.typ.form <> Base.type_proc) do
			begin
			if sym = Scanner.sym_lparen then
				begin
				TypeGuard (x)
				end
			else if sym = Scanner.sym_arrow then (* POINTER dereference *)
				begin
				Scanner.Get (sym);
				if x.typ.form = Base.type_pointer then
					begin
					Generator.Deref (x);
					x.typ := x.typ.base
					end
				else Scanner.Mark ('Not a pointer variable');
				end
			else if sym = Scanner.sym_lbrak then (* ARRAY index selector *)
				begin
				Scanner.Get (sym); expression (y); Check_int (y);
				if x.typ.form = Base.type_array then
					begin
					if x.typ.len = 0 then (* Open array case *)
						Generator.Open_array_Index (x, y)
					else Generator.Index (x, y);
					x.typ := x.typ.base;
					end
				else if x.typ.form = Base.type_dynArray then
					begin
					Generator.Dyn_array_Index (x, y);
					x.typ := x.typ.base
					end
				else Scanner.Mark ('Not an array type');
				if sym = Scanner.sym_rbrak then Scanner.Get (sym)
				else Scanner.Mark ('Missing ]')
				end
			else (* RECORD field selector *)
				begin
				Scanner.Get (sym);
				if sym = Scanner.sym_ident then
					begin
					if x.typ.form = Base.type_record then
						begin
						Base.find_field (obj, x.typ); Scanner.Get (sym);
						if obj <> Base.guard then
							begin
							Generator.Field (x, obj);
							x.typ := obj.typ;
							end
						else Scanner.Mark ('Undefined field')
						end
					else if x.typ.form = Base.type_pointer then
						begin
						Generator.Deref (x); x.typ := x.typ.base;
						Base.find_field (obj, x.typ); Scanner.Get (sym);
						if obj <> Base.guard then
							begin
							Generator.Field (x, obj);
							x.typ := obj^.typ;
							end
						else Scanner.Mark ('Undefined field')
						end
					else Scanner.Mark ('Not a record type');
					end
				else Scanner.Mark ('No identifier after record selector');
				end;
			end;
		end;

	procedure StatementSequence;
		var
			obj : Base.Object_;
			x : Base.Item;

		procedure AssignmentStatement (var x : Base.Item);
			var
				y : Base.Item;
			begin
			Scanner.Get (sym); expression (y);
			if (y.mode = Base.class_typ) or (y.mode = Base.class_sproc)
			or (y.mode = Base.class_proc) then
				begin
				Scanner.Mark ('Expect a variable or value but found type or procedure');
				Generator.Make_const (x, Base.int_type, 0)
				end
			(* INTEGER assignment *)
			else if (x.typ.form = Base.type_integer) and (y.typ.form = Base.type_integer) then
				begin
				if x.typ <> Generator.Get_result_int_type (x, y) then
					Scanner.Mark ('Can not store bigger int into smaller int type');
				Generator.Store (x, y)
				end
			(* BOOLEAN assignment *)
			else if (x.typ = Base.bool_type) and (y.typ = Base.bool_type) then
				begin
				Generator.Store (x, y)
				end
			(* SET assignment *)
			else if (x.typ = Base.set_type) and (y.typ = Base.set_type) then
				begin
				Generator.Store (x, y)
				end
			(* Dynamic array assignment *)
			else if (x.typ.form = Base.type_dynArray) and (x.typ = y.typ) then
				begin
				Generator.Copy (x, y, x.typ.size)
				end
			(* Static ARRAY assignment *)
			else if (x.typ.form = Base.type_array) and (y.typ.form = Base.type_array)
			and (x.typ.base = y.typ.base) and (y.typ.len > 0) and (x.typ.len > 0) then
				begin
				if x.typ.len < y.typ.len then Scanner.Mark ('Destination array is shorter than source array');
				Generator.Copy (x, y, y.typ.len * y.typ.base.size)
				end
			(* Open ARRAY assignment *)
			else if (x.typ.form = Base.type_array) and (y.typ.form = Base.type_array)
			and (x.typ.base = y.typ.base) and (x.typ.base.size > 0) then
				begin
				Generator.Copy2 (x, y)
				end
			(* POINTER assignment *)
			else if (x.typ.form = Base.type_pointer) and (y.typ.form = Base.type_pointer) then
				begin
				if (y.mode = Base.class_const) or Is_base_type (x.typ.base, y.typ.base) then
					Generator.Store (x, y)
				else Scanner.Mark ('Pointer base types incompatible')
				end
			(* RECORD assignment *)
			else if (x.typ.form = Base.type_record) and (y.typ.form = Base.type_record) then
				begin
				if Is_base_type (x.typ, y.typ) then
					Generator.Copy (x, y, x.typ.size)
				else Scanner.Mark ('Record base types incompatible')
				end
			else
				begin
				Scanner.Mark ('Incompatible types');
				Generator.Store (x, y)
				end
			end;

		procedure ProperStandFunc (var x : Base.Item);
			var
				y : Base.Item;
			begin
			if (x.a > 1) or (x.a < 0) then
				Scanner.Mark ('Not a proper standard procedure')
			else if sym = Scanner.sym_lparen then
				begin
				Scanner.Get (sym); expression (x);
				if (x.mode = Base.class_typ) or (x.mode = Base.class_sproc)
				or (x.mode = Base.class_proc) then
					begin
					Scanner.Mark ('Expect actual parameter but found type or procedure');
					Generator.Make_const (x, Base.int_type, 0)
					end;
				if sym = Scanner.sym_comma then
					begin
					Scanner.Get (sym); expression (y);
					if (y.mode = Base.class_typ) or (y.mode = Base.class_sproc)
					or (y.mode = Base.class_proc) then
						begin
						Scanner.Mark ('Expect actual parameter but found type or procedure');
						Generator.Make_const (y, Base.int_type, 0)
						end;
					if x.a = 0 then (* GET *)
						Generator.SFunc_GET (x, y)
					else if x.a = 1 then (* PUT *)
						Generator.SFunc_PUT (x, y);
					end
				else Scanner.Mark ('Comma expected');
				if sym = Scanner.sym_rparen then Scanner.Get (sym)
				else Scanner.Mark ('No closing )');
				end
			else Scanner.Mark ('No paramaters list?');
			end;

		procedure ProcVarAssignment (var x : Base.Item);
			var
				obj : Base.Object_;
				y : Base.Item;
			begin
			Scanner.Get (sym);
			if sym = sym_ident then
				begin
				Base.find (obj); Generator.Make_item (y, obj);
				Scanner.Get (sym); selector (y);
				if y.mode = Base.class_proc then
					begin
					if obj.lev > 0 then
						Scanner.Mark ('Not a global declared procedure')
					else if Check_compatible_procedure2 (x.typ, obj) then
						Generator.Store_proc_addr (x, y)
					else Scanner.Mark ('Assignment with incompatible procedure types')
					end
				else if (y.mode = Base.class_typ)
				or (y.mode = Base.class_sproc) then
					begin
					Scanner.Mark ('Expecting a procedure or a procedure variable')
					end
				else if y.typ^.form = Base.type_proc then
					begin
					if Check_compatible_procedure1 (x.typ, y.typ) then
						Generator.Store (x, y)
					else Scanner.Mark ('Assignment with incompatible procedure types')
					end
				else Scanner.Mark ('Expecting a procedure or a procedure variable')
				end
			else if sym = Scanner.sym_nil then
				begin
				Generator.Make_const (y, x.typ, 0);
				Generator.Store (x, y);
				Scanner.Get (sym)
				end
			else Scanner.Mark ('Expecting a procedure or a procedure variable')
			end;

		procedure IfStatement;
			var
				x : Base.Item;
				elb, lb : AnsiString;
				l : Integer;
			begin
			l := Base.code_num;
			elb := 'IF_' + IntToStr (l) + '_END';

			Scanner.Get (sym); expression (x);
			Check_bool (x); Generator.Cond_jump (x);

			if sym = Scanner.sym_then then Scanner.Get (sym) else Scanner.Mark ('THEN missing');
			StatementSequence;

			while sym = Scanner.sym_elsif do
				begin
				Generator.Jump (elb);
				lb := 'ELSIF_' + IntToStr (Base.code_num);
				Generator.Fix_link (x.a, lb); Generator.Emit_label (lb);

				Scanner.Get (sym); expression (x);
				Check_bool (x); Generator.Cond_jump (x);

				if sym = Scanner.sym_then then Scanner.Get (sym) else Scanner.Mark ('THEN missing');
				StatementSequence;
				end;

			if sym = Scanner.sym_else then
				begin
				Generator.Jump (elb);
				lb := 'ELSE_' + IntToStr (Base.code_num);
				Generator.Fix_link (x.a, lb); Generator.Emit_label (lb);

				Scanner.Get (sym);
				StatementSequence;
				end
			else
				begin Generator.Fix_link (x.a, elb); end;

			Generator.Emit_label (elb);
			if sym = Scanner.sym_end then Scanner.Get (sym) else Scanner.Mark ('No END for IF statement');
			end;

		procedure WhileStatement;
			var
				x : Base.Item;
				slb, elb, lb : AnsiString;
				l : Integer;
			begin
			l := Base.code_num;
			slb := 'WHILE_' + IntToStr (l);
			elb := slb + '_END';
			Generator.Emit_label (slb);

			Scanner.Get (sym); expression (x);
			Check_bool (x); Generator.Cond_jump (x);

			if sym = Scanner.sym_do then Scanner.Get (sym) else Scanner.Mark ('DO missing');
			StatementSequence;
			Generator.Jump (slb);

			while sym = Scanner.sym_elsif do
				begin
				lb := 'WHILE_ELSIF_' + IntToStr (Base.code_num);
				Generator.Fix_link (x.a, lb); Generator.Emit_label (lb);

				Scanner.Get (sym); expression (x);
				Check_bool (x); Generator.Cond_jump (x);

				if sym = Scanner.sym_do then Scanner.Get (sym) else Scanner.Mark ('DO missing');
				StatementSequence;
				Generator.Jump (slb);
				end;

			Generator.Fix_link (x.a, elb);
			Generator.Emit_label (elb);
			if sym = Scanner.sym_end then Scanner.Get (sym) else Scanner.Mark ('No END for WHILE statement');
			end;

		procedure RepeatStatement;
			var
				x : Base.Item;
				slb : AnsiString;
				l : Integer;
			begin
			l := Base.code_num;
			slb := 'REPEAT_' + IntToStr (l);
			Generator.Emit_label (slb);

			Scanner.Get (sym);
			StatementSequence;

			if sym = Scanner.sym_until then
				begin
				Scanner.Get (sym); expression (x);
				Check_bool (x); Generator.Cond_jump (x);
				Generator.Fix_link (x.a, slb);
				end
			else
				begin
				Scanner.Mark ('No UNTIL for REPEAT statement');
				Scanner.Get (sym);
				end;
			end;

		begin (* procedure StatementSequence *)
		while true do
			begin
			if sym < Scanner.sym_ident then
				begin
				Scanner.Mark ('Statement beginning?');
				repeat Scanner.Get (sym) until sym >= Scanner.sym_ident;
				end;
			if sym = Scanner.sym_ident then
				begin
				Base.find (obj); Generator.Make_item (x, obj);
				Scanner.Get (sym); selector (x);
				if x.mode = Base.class_proc then
					begin
					if obj.typ = nil then
						begin
						Generator.Prepare_to_call (x, obj);
						if sym = Scanner.sym_lparen then ActualParameters (x, obj)
						else if Base.is_param in obj.dsc.flag then Scanner.Mark ('Error: Missing paramaters list');
						Generator.Call (x);
						Generator.Cleanup_after_call (x)
						end
					else Scanner.Mark ('Error: Function procedure must be called in expression')
					end
				else if x.mode = Base.class_sproc then
					ProperStandFunc (x)
				else if x.mode = Base.class_typ then
					Scanner.Mark ('Error, type identifier appear at statement''s beginning')
				else if x.typ.form = Base.type_proc then
					begin
					if sym = Scanner.sym_becomes then
						ProcVarAssignment (x)
					else if sym = Scanner.sym_lparen then
						begin
						if x.typ.base = nil then
							begin
							Generator.Prepare_to_call (x, nil);
							ActualParameters (x, nil);
							Generator.Indirect_call (x);
							Generator.Cleanup_after_call (x)
							end
						else Scanner.Mark ('Error: Function procedure must be called in expression')
						end
					else Scanner.Mark ('Expecting := or a parameter list')
					end
				else
					AssignmentStatement (x)
				end
			else if sym = Scanner.sym_if then
				IfStatement
			else if sym = Scanner.sym_while then
				WhileStatement
			else if sym = Scanner.sym_repeat then
				RepeatStatement;

			Generator.Check_reg_stack;

			if sym = Scanner.sym_semicolon then
				Scanner.Get (sym)
			else if (sym > Scanner.sym_semicolon) and (sym < Scanner.sym_if)
			or (sym >= Scanner.sym_array) then
				break
			else Scanner.Mark ('No semicolon');
			end;
		end;

	procedure element (var x : Base.Item);
		var
			y, z : Base.Item;
		begin
		expression (y);
		if sym = Scanner.sym_upto then
			begin
			Scanner.Get (sym); expression (z);
			if (y.mode = Base.class_typ) or (y.mode = Base.class_proc)
			or (y.mode = Base.class_sproc) or (z.mode = Base.class_typ)
			or (z.mode = Base.class_proc) or (z.mode = Base.class_sproc) then
				begin
				Scanner.Mark ('Integer value expected');
				Generator.Make_const (y, Base.int_type, 0);
				Generator.Make_const (z, Base.int_type, 0);
				end
			else if (y.typ.form <> Base.type_integer) or (z.typ.form <> Base.type_integer) then
				begin
				Scanner.Mark ('Integer value expected');
				Generator.Make_clean_const (y, Base.int_type, 0);
				Generator.Make_clean_const (z, Base.int_type, 0);
				end;
			Generator.Set3 (x, y, z);
			end
		else
			begin
			if (y.mode = Base.class_typ) or (y.mode = Base.class_proc)
			or (y.mode = Base.class_sproc) then
				begin
				Scanner.Mark ('Integer value expected');
				Generator.Make_const (y, Base.int_type, 0);
				end
			else if y.typ^.form <> Base.type_integer then
				begin
				Scanner.Mark ('Integer value expected');
				Generator.Make_clean_const (y, Base.int_type, 0);
				end;
			Generator.Set2 (x, y);
			end;
		end;

	procedure set_ (var x : Base.Item);
		begin
		Scanner.Get (sym);
		Generator.Make_const (x, Base.set_type, 0);
		if sym = Scanner.sym_rbrace then
			begin
			Scanner.Get (sym)
			end
		else
			begin
			while true do
				begin
				element (x);
				if sym = Scanner.sym_comma then Scanner.Get (sym)
				else if sym = Scanner.sym_rbrace then break
				else begin Scanner.Mark ('Comma expected'); break; end;
				end;
			Generator.Set1 (x);
			if sym = Scanner.sym_rbrace then Scanner.Get (sym)
			else Scanner.Mark ('SET without closing }');
			end;
		end;

	procedure StandFunc (var x : Base.Item; fctno : Base.MachineInteger);
		begin
		if sym = Scanner.sym_lparen then
			begin
			Scanner.Get (sym); expression (x);
			if (x.mode = Base.class_typ) or (x.mode = Base.class_proc)
			or (x.mode = Base.class_sproc) then
				begin
				Generator.Make_const (x, Base.int_type, 0);
				Scanner.Mark ('Invalid parameter for standard procedure')
				end;
			if fctno = 2 then (* ORD *)
				Generator.SFunc_ORD (x)
			else if fctno = 3 then (* ODD *)
				Generator.SFunc_ODD (x)
			else if fctno = 4 then (* TOINT8 *)
				Generator.SFunc_TOINT8 (x)
			else if fctno = 5 then (* TOINT16 *)
				Generator.SFunc_TOINT16 (x)
			else if fctno = 6 then (* TOINT32 *)
				Generator.SFunc_TOINT32 (x)
			else if fctno = 7 then (* LEN *)
				Generator.SFunc_LEN (x);
			if sym = Scanner.sym_rparen then Scanner.Get (sym)
			else Scanner.Mark ('No closing )');
			end
		else
			begin
			Scanner.Mark ('Paramaters missing?');
			Generator.Make_clean_const (x, Base.int_type, 0);
			end;
		end;

	procedure factor (var x : Base.Item);
		var
			obj : Base.Object_;
		begin
		if sym < Scanner.sym_lparen then
			begin
			Scanner.Mark ('Factor beginning?');
			repeat Scanner.Get (sym) until sym >= Scanner.sym_lparen;
			end;
		if sym = Scanner.sym_ident then
			begin
			Base.find (obj); Generator.Make_item (x, obj);
			Scanner.Get (sym); selector (x);
			if x.mode = Base.class_sproc then
				begin
				if sym = Scanner.sym_lparen then
					begin
					StandFunc (x, obj.val);
					x.typ := obj.typ;
					end
				end
			else if x.mode = Base.class_proc then
				begin
				if (x.typ <> nil) or (sym = Scanner.sym_lparen) then
					begin
					Generator.Prepare_to_call (x, obj);
					Generator.Call (x);
					Generator.Cleanup_after_call (x);
					Generator.Make_function_result_item (x)
					end
				end
			else if x.mode = Base.class_typ then
				(* Do nothing *)
			else if (x.typ.form = Base.type_proc)
			and (sym = Scanner.sym_lparen) and (x.typ.base <> nil) then
				begin
				Generator.Prepare_to_call (x, nil);
				Generator.Indirect_call (x);
				Generator.Cleanup_after_call (x);
				x.typ := x.typ.base;
				Generator.Make_function_result_item (x)
				end
			end
		else if sym = Scanner.sym_nil then
			begin
			Generator.Make_const (x, Base.nil_type, 0);
			Scanner.Get (sym);
			end
		else if sym = Scanner.sym_number then
			begin
			Generator.Make_const (x, Base.int_type, Scanner.value);
			Scanner.Get (sym);
			end
		else if sym = Scanner.sym_lparen then
			begin
			Scanner.Get (sym); expression (x);
			if sym = Scanner.sym_rparen then Scanner.Get (sym)
			else Scanner.Mark (') is missing');
			end
		else if sym = Scanner.sym_not then
			begin
			Scanner.Get (sym); factor (x);
			Check_operator (x, Scanner.sym_not);
			Generator.Op1 (Scanner.sym_not, x);
			end
		else if sym = Scanner.sym_lbrace then
			begin
			set_ (x);
			end
		else
			begin
			Scanner.Mark ('Error: Invalid factor?');
			Generator.Make_const (x, Base.int_type, 0);
			end;
		end;

	procedure term (var x : Base.Item);
		var
			y : Base.Item;
			op : Integer;
		begin
		factor (x);
		while (sym >= Scanner.sym_times) and (sym <= Scanner.sym_and) do
			begin
			op := sym;
			Check_operator (x, op);
			Scanner.Get (sym);

			if op = Scanner.sym_and then Generator.Op1 (op, x);

			factor (y);
			Check_operator (y, op);

			if x.typ.form = y.typ.form then Generator.Op2 (op, x, y)
			else Scanner.Mark ('Incompatible types');
			end;
		end;

	procedure SimpleExpression (var x : Base.Item);
		var
			y : Base.Item;
			op : Integer;
		begin
		if sym = Scanner.sym_plus then
			begin
			Scanner.Get (sym); term (x);
			Check_operator (x, Scanner.sym_plus);
			end
		else if sym = Scanner.sym_minus then
			begin
			Scanner.Get (sym); term (x); Check_operator (x, Scanner.sym_minus);
			Generator.Op1 (Scanner.sym_minus, x);
			end
		else
			term (x);

		while (sym >= Scanner.sym_plus) and (sym <= Scanner.sym_or) do
			begin
			op := sym;
			Check_operator (x, op);
			Scanner.Get (sym);

			if op = Scanner.sym_or then Generator.Op1 (op, x);

			term (y);
			Check_operator (y, op);

			if x.typ.form = y.typ.form then Generator.Op2 (op, x, y)
			else Scanner.Mark ('Incompatible types');
			end;
		end;

	procedure expression (var x : Base.Item);
		var
			op : Integer;
			y : Base.Item;

		procedure TypeTest (var x : Base.Item);
			var
				typ : Base.Type_;
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_ident then
				begin
				Type_ (typ, '', False);
				if x.typ.form = Base.type_pointer then
					begin
					if typ.form = Base.type_pointer then
						begin
						if Is_base_type (x.typ.base, typ.base) then Generator.Type_test (x, typ)
						else Scanner.Mark ('Type test only applicable with an extension type of the declared type')
						end
					else Scanner.Mark ('Expecting a pointer type in type test')
					end
				else if (x.typ.form = Base.type_record) and (x.mode = Base.class_par)
				and not (Base.read_only in x.flag) then
					begin
					if typ.form = Base.type_record then
						begin
						if Is_base_type (x.typ, typ) then Generator.Type_test (x, typ)
						else Scanner.Mark ('Type test only applicable with an extension type of the declared type')
						end
					else Scanner.Mark ('Expecting a record type in type test')
					end
				else Scanner.Mark ('Type test only applicable with pointer variables or variable parameters of record type')
				end
			else Scanner.Mark ('Type identifier expected in type test');
			if x.mode <> Generator.mode_cond then Generator.Make_clean_const (x, Base.bool_type, 0)
			else x.typ := Base.bool_type;
			end;

		begin (* expression *)
		SimpleExpression (x);
		if (sym >= Scanner.sym_equal) and (sym <= Scanner.sym_greater) then
			begin
			op := sym;
			Check_operator (x, op);
			Scanner.Get (sym);

			if x.typ = Base.bool_type then Generator.Finish_cond (x);
			SimpleExpression (y);
			Check_operator (y, op);

			if (x.typ = Base.set_type) and (x.typ = y.typ)
			and ((op = Scanner.sym_greater_equal) or (op = Scanner.sym_less_equal)) then
				Generator.Inclusion_test (op, x, y)
			else if x.typ.form = y.typ.form then
				Generator.Relation (op, x, y)
			else
				begin
				Scanner.Mark ('Incompatible types');
				Generator.Make_clean_const (x, Base.bool_type, 0);
				Generator.Make_clean_const (y, Base.int_type, 0)
				end;
			x.typ := Base.bool_type;
			end
		else if sym = Scanner.sym_in then
			begin
			Check_int (x);

			Scanner.Get (sym);
			SimpleExpression (y);
			Check_set (y);

			Generator.Membership_test (x, y);
			x.typ := Base.bool_type;
			end
		else if sym = Scanner.sym_is then
			begin
			if (x.mode = Base.class_typ) or (x.mode = Base.class_proc)
			or (x.mode = Base.class_proc) then
				Scanner.Mark ('Not a pointer or record variable parameter')
			else if x.typ.form = Base.type_pointer then
				TypeTest (x)
			else if (x.typ.form = Base.type_record) and (x.mode = Base.class_par)
			and not (Base.read_only in x.flag) then
				TypeTest (x)
			else
				begin
				Scanner.Mark ('Not a pointer or record variable parameter');
				Generator.Make_clean_const (x, Base.bool_type, 0)
				end
			end;
		end;

	procedure ProcedureDecl;
		var
			proc : Base.Object_;
			proc_id : AnsiString;
			local_block_size, para_block_size : Base.MachineInteger;
			x : Base.Item;

		procedure Fix_param_adr (var para_block_size : Base.MachineInteger);
			var
				obj : Base.Object_;
			begin
			para_block_size := -para_block_size;
			obj := top_scope^.next;
			while obj <> Base.guard do
				begin
				obj.val := obj.val + para_block_size + 16;
				obj := obj.next;
				end
			end;

		begin (* begin ProcedureDecl *)
		Scanner.Get (sym);
		if sym = Scanner.sym_ident then
			begin
			proc_id := Scanner.id; para_block_size := 0;
			Base.New_obj (proc, Base.class_proc);
			proc.val := -1; proc.typ := nil;
			
			Scanner.Get (sym);
			if sym = Scanner.sym_times then
				begin
				if proc.lev = 0 then proc.flag := proc.flag + [Base.exported]
				else Scanner.Mark ('Error: Not able to export a non-global identifier');
				Scanner.Get (sym)
				end;
			
			Base.Inc_level (1);
			Base.Open_scope;

			if sym = Scanner.sym_lparen then FormalParameters (proc, para_block_size);
			if para_block_size < 0 then Fix_param_adr (para_block_size);

			local_block_size := 0; proc.dsc := top_scope.next;
			if sym = Scanner.sym_semicolon then Scanner.Get (sym)
			else Scanner.Mark ('Error: No semicolon');
			Declarations (local_block_size);
			local_block_size := -local_block_size;

			while sym = Scanner.sym_procedure do
				begin
				ProcedureDecl;
				if sym = Scanner.sym_semicolon then Scanner.Get (sym)
				else Scanner.Mark ('Error: No semicolon');
				end;

			proc.val := Base.code_num;
			Generator.Emit_label (proc.name + '_');
			Generator.Enter (para_block_size, local_block_size);
			if sym = Scanner.sym_begin then
				begin
				Scanner.Get (sym);
				StatementSequence;
				end;

			if proc.typ <> nil then
				begin
				if sym = Scanner.sym_return then
					begin
					Scanner.Get (sym); expression (x);
					if x.typ = proc.typ then Generator.Set_function_result (x)
					else Scanner.Mark ('Error: Incompatible function result type');
					end
				else Scanner.Mark ('Error: Function procedure without RETURN');
				end
			else if sym = Scanner.sym_return then
				begin
				Scanner.Mark ('Error: Normal procedures can not have return value!');
				expression (x);
				Make_clean_const (x, int_type, 0);
				end;

			Generator.Return (para_block_size, local_block_size);
			Base.Close_scope;
			Base.Inc_level (-1);
			Generator.Check_reg_stack;

			if sym = Scanner.sym_end then Scanner.Get (sym)
			else Scanner.Mark ('Error: No END for PROCEDURE');
			if sym = Scanner.sym_ident then
				begin
				if Scanner.id <> proc_id then Scanner.Mark ('Error: Wrong procedure identifier');
				Scanner.Get (sym);
				end
			else Scanner.Mark ('Error: No procedure identifier after END');
			end;
		end; (* ProcedureDecl *)

	procedure Module (var modid : AnsiString);
		var
			vars_size : Int64;
		begin
		if sym = Scanner.sym_module then Scanner.Get (sym)
		else Scanner.Mark ('No MODULE keyword');
		if sym = Scanner.sym_ident then
			begin
			modid := Scanner.id;
			Scanner.Get (sym);
			end
		else Scanner.Mark ('No module identifier');
		if sym = Scanner.sym_semicolon then Scanner.Get (sym)
		else Scanner.Mark ('Missing semicolon');

		vars_size := 0;
		Declarations (vars_size);

		while sym = Scanner.sym_procedure do
			begin
			ProcedureDecl;
			if sym = Scanner.sym_semicolon then Scanner.Get (sym)
			else Scanner.Mark ('No semicolon');
			end;

		Generator.Begin_Main;
		if sym = Scanner.sym_begin then
			begin
			Scanner.Get (sym);
			StatementSequence;
			end;	
		Generator.End_Main;

		if sym = Scanner.sym_end then Scanner.Get (sym)
		else Scanner.Mark ('No END keyword');
		if sym = Scanner.sym_ident then
			begin
			if Scanner.id <> modid then Scanner.Mark ('Wrong module identifier');
			Scanner.Get (sym);
			end
		else Scanner.Mark ('No module identifier after END');
		if sym = Scanner.sym_period then
			(* Do nothing *)
		else Scanner.Mark ('No . at module end');
		end;

initialization
	undef_ptr_list := nil;

end.
