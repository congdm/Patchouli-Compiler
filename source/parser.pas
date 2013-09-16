unit Parser;

interface
	uses
		Scanner, Generator;
		
	const
		Word_size = 8;
		
	var
		sym : Integer;

	procedure Module;

implementation
	uses
		Sysutils;
		
	type
		Undef_pointer = ^Undef_pointer_desc;
		Undef_pointer_desc = Record
			typ : Generator.Type_;
			base_type_name : AnsiString;
			next : Undef_pointer;
			end;
	
	var
		top_scope, guard, universe : Generator.Object_;
		undef_ptr_list : Undef_pointer;

	procedure New_obj (var obj : Generator.Object_; class_ : Integer);
		var
			new_, x : Generator.Object_;
		begin
		x := top_scope;
		guard^.name := Scanner.id;
		while x^.next^.name <> Scanner.id do x := x^.next;
		if x^.next = guard then
			begin
			New (new_);
			new_^.name := Scanner.id;
			new_^.class_ := class_;
			new_^.lev := Generator.cur_lev;
			new_^.read_only := False;
			new_^.next := guard;
			x^.next := new_; obj := new_;
			end
		else
			begin
			obj := x^.next;
			Scanner.Mark ('Duplicated identifier declaration');
			end;
		end;
		
	procedure Open_scope;
		var
			s : Generator.Object_;
		begin
		New (s);
		s^.class_ := Generator.class_head;
		s^.dsc := top_scope;
		s^.next := guard;
		top_scope := s;
		end;
		
	procedure Close_scope;
		var
			s : Generator.Object_;
		begin
		s := top_scope;
		top_scope := top_scope^.dsc;
		Dispose (s);
		end;
		
	procedure enter (cl : Integer; n : Int64; name : String; typ : Generator.Type_);
		var
			obj : Generator.Object_;
		begin
		New (obj);
		obj^.class_ := cl; obj^.val := n; obj^.name := name;
		obj^.typ := typ; obj^.dsc := nil;
		obj^.next := top_scope^.next;
		top_scope^.next := obj;
		end;
		
	procedure find (var obj : Generator.Object_);
		var
			s, x : Generator.Object_;
		begin
		s := top_scope; guard^.name := Scanner.id;
		while true do
			begin
			x := s^.next;
			while x^.name <> Scanner.id do
         	x := x^.next;
			if x <> guard then begin obj := x; break; end;
			if s = universe then
				begin
				obj := x;
				Scanner.Mark ('Undefined identifier');
				break;
				end;
			s := s^.dsc;
			end;
		end;
		
	procedure find_proc (var obj : Generator.Object_; var x : Generator.Item);
		var
			s : Generator.Object_;
		begin
		s := top_scope;
		while s <> universe do s := s^.dsc;
		while (s <> guard) do
			begin
			if (s^.class_ = Generator.class_proc) and (s^.val = x.a) then
				begin
				obj := s;
				s := guard
				end
			else s := s^.next
			end
		end;
		
	procedure find2 (var obj : Generator.Object_);
		var
			s, x : Generator.Object_;
		begin
		s := top_scope; guard^.name := Scanner.id;
		while true do
			begin
			x := s^.next;
			while x^.name <> Scanner.id do
         	x := x^.next;
			if x <> guard then begin obj := x; break end;
			if s = universe then begin obj := x; break end
			else s := s^.dsc;
			end;
		end;
		
	procedure Check_int (var x : Generator.Item);
		begin
		if (x.mode = Generator.class_typ) or (x.mode = Generator.class_proc)
		or (x.mode = Generator.class_sproc) or (x.typ^.form = Generator.type_proc) then
			begin
			Scanner.Mark ('Expecting an integer but found type or procedure');
			Generator.Make_clean_const (x, Generator.set_type, 0)
			end
		else if x.typ^.form <> Generator.type_integer then
			begin
			Scanner.Mark ('Not an integer');
			Generator.Make_clean_const (x, Generator.set_type, 0)
			end
		end;
		
	procedure Check_set (var x : Generator.Item);
		begin
		if (x.mode = Generator.class_typ) or (x.mode = Generator.class_proc)
		or (x.mode = Generator.class_sproc) or (x.typ^.form = Generator.type_proc) then
			begin
			Scanner.Mark ('Expecting a set but found type or procedure');
			Generator.Make_clean_const (x, Generator.set_type, 0)
			end
		else if x.typ^.form <> Generator.type_set then
			begin
			Scanner.Mark ('Not a set');
			Generator.Make_clean_const (x, Generator.set_type, 0)
			end
		end;
		
	procedure Check_bool (var x : Generator.Item);
		begin
		if (x.mode = Generator.class_typ) or (x.mode = Generator.class_proc)
		or (x.mode = Generator.class_sproc) or (x.typ^.form = Generator.type_proc) then
			begin
			Scanner.Mark ('Expecting a boolean value but found type or procedure');
			Generator.Make_clean_const (x, Generator.bool_type, 0)
			end
		else if x.typ^.form <> Generator.type_boolean then
			begin
			Scanner.Mark ('Not a boolean value');
			Generator.Make_clean_const (x, Generator.bool_type, 0)
			end
		end;
		
	procedure Check_operator (var x : Generator.Item; op : Integer);
		var
			form : Integer;
		begin
		if (x.mode = Generator.class_typ) or (x.mode = Generator.class_proc)
		or (x.mode = Generator.class_sproc) or (x.typ^.form = Generator.type_proc) then
			begin
			Scanner.Mark ('Error, there is no operator for type or procedure');
			Generator.Make_clean_const (x, Generator.int_type, 0)
			end;
		form := x.typ^.form;
		if (op = Scanner.sym_plus) or (op = Scanner.sym_minus) or (op = Scanner.sym_times) then
			begin
			if (form <> Generator.type_integer) and (form <> Generator.type_set) then
				begin
				Scanner.Mark ('+, -, * only compatible with INTEGER or SET types');
				Generator.Make_clean_const (x, Generator.int_type, 0)
				end
			end
		else if (op = Scanner.sym_div) or (op = Scanner.sym_mod) then
			begin
			if form <> Generator.type_integer then
				begin
				Scanner.Mark ('DIV, MOD only compatible with INTEGER types');
				Generator.Make_clean_const (x, Generator.int_type, 0)
				end
			end
		else if op = Scanner.sym_slash then
			begin
			if form <> Generator.type_set then
				begin
				Scanner.Mark ('/ only compatible with SET type');
				Generator.Make_clean_const (x, Generator.set_type, 0)
				end
			end
		else if (op = Scanner.sym_and) or (op = Scanner.sym_or) or (op = Scanner.sym_not) then
			begin
			if form <> Generator.type_boolean then
				begin
				Scanner.Mark ('&, OR, ~ only compatible with BOOLEAN type');
				Generator.Make_clean_const (x, Generator.bool_type, 0)
				end
			end
		else if (op = Scanner.sym_equal) or (op = Scanner.sym_not_equal) then
			begin
			if (form <> Generator.type_integer) and (form <> Generator.type_boolean)
			and (form <> Generator.type_set) and (form <> Generator.type_pointer) then
				begin
				Scanner.Mark ('=, # only compatible with INTEGER, BOOLEAN, SET or POINTER types');
				Generator.Make_clean_const (x, Generator.int_type, 0)
				end
			end
		else if (op = Scanner.sym_greater) or (op = Scanner.sym_less) then
			begin
			if form <> Generator.type_integer then
				begin
				Scanner.Mark ('>, < only compatible with INTEGER types');
				Generator.Make_clean_const (x, Generator.int_type, 0)
				end
			end
		else if (op = Scanner.sym_greater_equal) or (op = Scanner.sym_less_equal) then
			begin
			if (form <> Generator.type_integer) and (form <> Generator.type_set) then
				begin
				Scanner.Mark ('>=, <= only compatible with INTEGER or SET types');
				Generator.Make_clean_const (x, Generator.int_type, 0)
				end
			end;
		end;
		
	function Is_structured_type (tp : Generator.Type_) : Boolean;
		begin
		if (tp^.form = Generator.type_record)
		or (tp^.form = Generator.type_array)
		or (tp^.form = Generator.type_dynArray) then
			Result := True
		else
			Result := False;
		end;
		
	procedure Find_field (var obj : Generator.Object_; typ : Generator.Type_);
		var
			list : Generator.Object_;
		begin
		guard^.name := Scanner.id;
		while typ <> nil do
			begin
			list := typ^.fields;
			while list^.name <> Scanner.id do list := list^.next;
			obj := list;
			if obj <> guard then break;
			typ := typ^.base
			end
		end;
		
	procedure Fix_undef_ptr (obj : Generator.Object_);
		var
			undef : Undef_pointer;
		begin
		undef := undef_ptr_list;
		while undef <> nil do
			begin
			if (undef^.typ^.base = Generator.int_type) and (obj^.name = undef^.base_type_name) then
				begin
				if obj^.typ^.form = Generator.type_record then
					undef^.typ^.base := obj^.typ
				else
					begin
					undef^.typ := Generator.dummy_record_type;
					Scanner.Mark (obj^.name + ' must be a RECORD type')
					end
				end;
			undef := undef^.next
			end
		end;
		
	function Is_base_type (typ1, typ2 : Generator.Type_) : Boolean;
		begin
		Result := False;
		while typ2 <> nil do
			begin
			if typ2 = typ1 then Result := True;
			typ2 := typ2^.base
			end
		end;
		
	function Check_compatible_open_array (typ1, typ2 : Generator.Type_) : Boolean;
		begin
		if typ1^.base = typ2^.base then
			Result := True
		else if (typ1^.base^.form = Generator.type_array)
		and (typ2^.base^.form = Generator.type_array)
		and (typ1^.base^.len = 0) and (typ2^.base^.len = 0) then
			Result := Check_compatible_open_array (typ1^.base, typ2^.base)
		else
			Result := False
		end;
		
	function Check_compatible_procedure1 (typ1, typ2 : Generator.Type_) : Boolean;
		var
			obj1, obj2 : Generator.Object_;
		begin
		if typ1 = typ2 then
			Result := True
		else if typ1^.base <> typ2^.base then
			Result := False
		else
			begin
			obj1 := typ1^.fields; obj2 := typ2^.fields;
			Result := True;
			while Result and (obj1 <> guard) and (obj2 <> guard) do
				begin
				if (obj1^.class_ <> obj2^.class_)
				or (obj1^.read_only <> obj2^.read_only) then
					Result := False
				else if obj1^.typ = obj2^.typ then
					(* Do nothing *)
				else if (obj1^.typ^.form = Generator.type_array)
				and (obj2^.typ^.form = Generator.type_array)
				and (obj1^.typ^.len = 0) and (obj2^.typ^.len = 0) then
					Result := Check_compatible_open_array (obj1^.typ, obj2^.typ)
				else if (obj1^.typ^.form = Generator.type_proc)
				and (obj2^.typ^.form = Generator.type_proc) then
					Result := Check_compatible_procedure1 (obj1^.typ, obj2^.typ)
				else Result := False;
				obj1 := obj1^.next; obj2 := obj2^.next;
				end;
			if obj1 <> obj2 then Result := False
			end
		end;
		
	function Check_compatible_procedure2 (typ : Generator.Type_; proc : Generator.Object_) : Boolean;
		var
			obj1, obj2 : Generator.Object_;
		begin
		if typ^.base <> proc^.typ then
			Result := False
		else
			begin
			obj1 := typ^.fields; obj2 := proc^.dsc;
			Result := True;
			while Result and (obj1 <> guard) and (obj2 <> guard) do
				begin
				if (obj1^.class_ <> obj2^.class_)
				or (obj1^.read_only <> obj2^.read_only) 
				or not obj2^.is_param then
					Result := False
				else if obj1^.typ = obj2^.typ then
					(* Do nothing *)
				else if (obj1^.typ^.form = Generator.type_array)
				and (obj2^.typ^.form = Generator.type_array)
				and (obj1^.typ^.len = 0) and (obj2^.typ^.len = 0) then
					Result := Check_compatible_open_array (obj1^.typ, obj2^.typ)
				else if (obj1^.typ^.form = Generator.type_proc)
				and (obj2^.typ^.form = Generator.type_proc) then
					Result := Check_compatible_procedure1 (obj1^.typ, obj2^.typ)
				else Result := False;
				obj1 := obj1^.next; obj2 := obj2^.next;
				end;
			if (obj1 = guard) and (obj2 <> guard) and obj2^.is_param then
				Result := False
			end
		end;
		
(* ------------------------------------------------------------------------------------------ *)		
// Begin parser procedures part

	procedure expression (var x : Generator.Item); forward;
	
	procedure IdentList (class_ : Integer; var first : Generator.Object_);
		var
			obj : Generator.Object_;
		begin
		if sym = Scanner.sym_ident then
			begin
			New_obj (first, class_); Scanner.Get (sym);
			while sym = Scanner.sym_comma do
				begin
				Scanner.Get (sym);
				if sym = Scanner.sym_ident then
					begin New_obj (obj, class_); Scanner.Get (sym); end
				else Scanner.Mark ('No identifier after ,');
				end;
			if sym = Scanner.sym_colon then Scanner.Get (sym)
			else Scanner.Mark ('No : after identifier list');
			end;
		end;
		
	procedure FormalParameters (var proc : Generator.Object_; var para_block_size : Int64);
		var
			obj : Generator.Object_;
			
		procedure OpenArray (var tp : Generator.Type_);
			var
				t : Generator.Type_;
				obj : Generator.Object_;
			begin
			New (tp); tp^.form := Generator.type_array;
			tp^.len := 0; tp^.size := Word_size * 2;
			
			Scanner.Get (sym);
			if sym = Scanner.sym_of then Scanner.Get (sym)
			else Scanner.Mark ('Missing OF or not an open array');
				
			t := tp;
			while sym = Scanner.sym_array do
				begin
				New (t^.base); t := t^.base;
				t^.form := Generator.type_array;
				t^.len := 0; t^.size := 0;
				tp^.size := tp^.size + Word_size;
				Scanner.Get (sym);
				if sym = Scanner.sym_of then Scanner.Get (sym)
				else Scanner.Mark ('Missing OF or not an open array');
				end;
				
			if sym = Scanner.sym_ident then
				begin
				find (obj); Scanner.Get (sym);
				if obj^.class_ = Generator.class_typ then t^.base := obj^.typ
				else begin Scanner.Mark ('Type not found'); t^.base := Generator.int_type; end;
				end
			else
				begin
				Scanner.Mark ('No type identifier?');
				t^.base := Generator.int_type;
				end;
			end;
			
		procedure FPSection (var para_block_size : Int64);
			var
				obj, first : Generator.Object_;
				tp : Generator.Type_;
				para_size : Int64;
				cls : Integer;
				read_only : Boolean;
			begin
			if sym = Scanner.sym_var then
				begin
				Scanner.Get (sym);
				IdentList (Generator.class_par, first)
				end
			else IdentList (Generator.class_var, first);
				
			if sym = Scanner.sym_ident then
				begin
				find (obj); Scanner.Get (sym);
				if obj^.class_ = Generator.class_typ then tp := obj^.typ
				else begin Scanner.Mark ('Type not found'); tp := Generator.int_type; end;
				end
			else if sym = Scanner.sym_array then
				OpenArray (tp)
			else
				begin
				Scanner.Mark ('Identifiers list without type');
				tp := Generator.int_type;
				end;
			
			cls := first^.class_; read_only := first^.read_only;
			if (tp^.form = Generator.type_array) and (tp^.len = 0) then
				para_size := tp^.size
			else if (tp^.form = Generator.type_record) and (cls = Generator.class_par) then
				para_size := Word_size * 2
			else
				para_size := Word_size;

			if (cls = Generator.class_var) and Is_structured_type (tp) then
				begin	
				cls := Generator.class_par;
				read_only := True;
				end;
				
			obj := first;
			while obj <> guard do
				begin
				obj^.typ := tp; obj^.lev := Generator.cur_lev;
				obj^.class_ := cls; obj^.read_only := read_only;
				para_block_size := para_block_size - para_size;
				obj^.val := para_block_size;
				obj^.is_param := True;
				obj := obj^.next;
				end;
			end;
			
		begin (* FormalParameters *)
		Scanner.Get (sym);
		if sym = Scanner.sym_rparen then Scanner.Get (sym)
		else
			begin
			FPSection (para_block_size);
			while sym = Scanner.sym_semicolon do
				begin Scanner.Get (sym); FPSection (para_block_size); end;
			if sym = Scanner.sym_rparen then Scanner.Get (sym) else Scanner.Mark ('No closing )');
			end;
		if sym = Scanner.sym_colon then
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_ident then
				begin
				find (obj); Scanner.Get (sym);
				if obj^.class_ = Generator.class_typ then
					proc^.typ := obj^.typ
				else
					begin
					Scanner.Mark ('Type not found');
					proc^.typ := Generator.int_type;
					end;
				end
			else Scanner.Mark ('Function result type missing?');
			end;
		end;
		
	procedure Type_ (var typ : Generator.Type_; name, name2 : AnsiString);
		var
			obj : Object_;
			
		(* Dynamic array type is just a big pointer to an array *)
		(* Syntax: ARRAY OF type *)
		(* Only support 1-dimension dynamic array *)
		procedure DynArrayType (var typ : Generator.Type_; name, name2 : AnsiString);
			begin
			New (typ);
			typ^.form := Generator.type_dynArray;
			typ^.len := 0;
			typ^.size := 3 * Word_size; (* places for address and array desc *)
			
			Scanner.Get (sym);
			if sym = Scanner.sym_array then
				begin
				Scanner.Mark ('Dynamic array of array type is not supported');
				typ^.base := Generator.int_type;
				end
			else
				begin
				Type_ (typ^.base, name, '');
				if typ^.base^.form = Generator.type_record then
					typ^.size := typ^.size + Word_size (* place for type tag *)
				else if typ^.base^.form = Generator.type_array then
					begin
					Scanner.Mark ('Dynamic array of array type is not supported');
					typ^.base := Generator.int_type;
					end;
				end
			end;
			
		procedure ArrayType (var typ : Generator.Type_; name, name2 : AnsiString);
			var
				tp : Generator.Type_;
				x : Generator.Item;
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_of then
				DynArrayType (typ, name, '')
			else
				begin
				expression (x);
				if (x.mode <> Generator.class_const) or (x.a <= 0) then
					begin Scanner.Mark ('Invalid array size'); x.a := 1; end;
				if sym = Scanner.sym_of then Scanner.Get (sym) else Mark ('No OF in array declaration');
				Type_ (tp, name, ''); New (typ);
				typ^.form := Generator.type_array;
				typ^.base := tp;
				typ^.len := x.a;
				typ^.size := typ^.len * tp^.size
				end
			end;
			
		procedure RecordType (var typ : Generator.Type_; name, name2 : AnsiString);
			var
				tp : Generator.Type_;
				obj, first : Generator.Object_;
			begin
			Scanner.Get (sym); New (typ);
			typ^.form := Generator.type_record;
			typ^.size := 0; typ^.base := nil;
			if sym = Scanner.sym_lparen then
				begin
				Scanner.Get (sym);
				if sym = Scanner.sym_ident then begin find (obj); Scanner.Get (sym) end
				else Scanner.Mark ('No base type of RECORD or not an identifier');
				if (obj.class_ = Generator.class_typ)
				and (obj^.typ^.form = Generator.type_record) and (typ <> obj^.typ) then
					begin
					typ^.base := obj^.typ;
					typ^.size := obj^.typ^.size
					end;
				if sym = Scanner.sym_rparen then Scanner.Get (sym)
				else Scanner.Mark ('No closing )')
				end;
			Open_scope;
			while true do
				begin
				if sym = Scanner.sym_ident then
					begin
					IdentList (Generator.class_field, first);
					if sym = Scanner.sym_pointer then Type_ (tp, '', '')
					else Type_ (tp, name, '');
					obj := first;
					while obj <> guard do
						begin
						obj^.typ := tp; obj^.val := typ^.size;
						typ^.size := typ^.size + obj^.typ^.size;
						obj := obj^.next;
						end;
					end;
				if sym = Scanner.sym_semicolon then Scanner.Get (sym)
				else if sym = Scanner.sym_ident then Scanner.Mark ('No ; after identifier list')
				else break;
				end;
			typ^.fields := top_scope^.next; Close_scope;
			Generator.Emit_type_tag (name2, typ);
			if sym = Scanner.sym_end then Scanner.Get (sym) else Scanner.Mark ('No END after record type declaration');
			end;
			
		procedure PointerType (var typ : Generator.Type_; name, name2 : AnsiString);
			var
				obj : Generator.Object_;
				undef : Undef_pointer;
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_to then Scanner.Get (sym) else Scanner.Mark ('No TO in pointer type declaration');
			New (typ); typ^.form := Generator.type_pointer; typ^.size := Word_size;
			if sym = Scanner.sym_ident then
				begin
				find2 (obj); Scanner.Get (sym);
				if obj^.name = name then Scanner.Mark ('Recursive type definition')
				else if obj = guard then
					begin
					New (undef);
					undef^.typ := typ;
					undef^.base_type_name := obj^.name;
					undef^.next := undef_ptr_list;
					undef_ptr_list := undef;
					typ^.base := Generator.int_type
					end
				else if (obj^.class_ = Generator.class_typ)
				and (obj^.typ^.form = Generator.type_record) then
					typ^.base := obj^.typ
				else
					begin
					Scanner.Mark ('RECORD type expected');
					typ^.base := Generator.dummy_record_type
					end
				end
			else if sym = Scanner.sym_record then
				RecordType (typ^.base, '', name2)
			else
				begin
				Scanner.Mark ('RECORD type expected');
				typ^.base := Generator.dummy_record_type
				end
			end;
			
		procedure ProcedureType (var typ : Generator.Type_);
			var
				obj : Generator.Object_;
				x : Int64;
			begin
			New (typ);
			typ^.form := Generator.type_proc;
			typ^.size := 8; typ^.base := nil;
			Open_scope;
			Scanner.Get (sym);
			if sym = Scanner.sym_lparen then
				begin
				New (obj);
				FormalParameters (obj, x);
				typ^.base := obj^.typ;
				Dispose (obj)
				end;
			typ^.fields := top_scope^.next;
			Close_scope;
			end;
				
		begin (* Type_ *)
		typ := Generator.int_type;
		if (sym <> Scanner.sym_ident) and (sym < Scanner.sym_array) then
			begin
			Scanner.Mark ('No type identifier');
			repeat Scanner.Get (sym) until (sym = Scanner.sym_ident) or (sym >= Scanner.sym_array);
			end;
		if sym = Scanner.sym_ident then
			begin
			find (obj); Scanner.Get (sym);
			if obj^.name = name then Scanner.Mark ('Recursive type definition')
			else if obj^.class_ = Generator.class_typ then typ := obj^.typ else Scanner.Mark ('Type not found');
			end
		else if sym = Scanner.sym_array then
			ArrayType (typ, name, name2)
		else if sym = Scanner.sym_record then
			RecordType (typ, name, name2)
		else if sym = Scanner.sym_pointer then
			PointerType (typ, name, name2)
		else if sym = Scanner.sym_procedure then
			ProcedureType (typ)
		else Scanner.Mark ('No type identifier');
		end; (* Type_ *)
		
	procedure Declarations (var var_base : Int64; global : Boolean);
		var
			obj, first : Generator.Object_;
			tp : Generator.Type_;
			x : Generator.Item;
			undef : Undef_pointer;
		begin
		while true do
			begin
			if sym = Scanner.sym_const then
				begin
				Scanner.Get (sym);
				while sym = Scanner.sym_ident do
					begin
					New_obj (obj, Generator.class_const);
					Scanner.Get (sym);
					if sym = Scanner.sym_equal then Scanner.Get (sym)
					else Scanner.Mark ('No = in const declaration');
					expression (x);
					if x.mode = Generator.class_const then
						begin
						obj^.val := x.a;
						obj^.typ := x.typ;
						end
					else Scanner.Mark ('Expression is not const');
					if sym = Scanner.sym_semicolon then Scanner.Get (sym)
					else Scanner.Mark ('No ; after const declaration');
					end;
				end;
			if sym = Scanner.sym_type then
				begin
				Scanner.Get (sym);
				while sym = Scanner.sym_ident do
					begin
					New_obj (obj, Generator.class_typ);
					Scanner.Get (sym);
					if sym = Scanner.sym_equal then Scanner.Get (sym)
					else Scanner.Mark ('No = in type declaration');
					Type_ (obj^.typ, obj^.name, obj^.name);
					if sym = Scanner.sym_semicolon then Scanner.Get (sym)
					else Scanner.Mark ('No ; after type declaration');
					if undef_ptr_list <> nil then Fix_undef_ptr (obj)
					end
				end;		
			if sym = Scanner.sym_var then
				begin
				Scanner.Get (sym);
				while sym = Scanner.sym_ident do
					begin
					IdentList (Generator.class_var, first);
					Type_ (tp, '', '');
					obj := first;
					while obj <> guard do
						begin
						obj^.typ := tp;
						obj^.lev := Generator.cur_lev;
						obj^.is_param := False;
						if not global then
							begin
							var_base := var_base - obj^.typ^.size;
							obj^.val := var_base;
							end;
						if global then Generator.Global_var_decl (obj);
						obj := obj^.next;
						end;
					if sym = Scanner.sym_semicolon then Scanner.Get (sym)
					else Scanner.Mark ('No ; after variable declaration');
					end;
				end;
			if (sym >= Scanner.sym_const) and (sym <= Scanner.sym_var) then
				Scanner.Mark ('Bad declaration sequence')
			else break;
			end;
		(* Check for remaining undef pointer types *)
		while undef_ptr_list <> nil do
			begin
			if undef_ptr_list^.typ^.base = Generator.int_type then
				Scanner.Mark ('Base type ' + undef_ptr_list^.base_type_name + ' is not defined');
			undef := undef_ptr_list;
			undef_ptr_list := undef^.next;
			Dispose (undef)
			end
		end; (* Declarations *)
		
	procedure ProcedureCall (var x : Generator.Item; obj : Generator.Object_);
		var
			y : Generator.Item;
			par, obj2 : Generator.Object_;
		begin
		if x.mode = Generator.class_proc then par := obj^.dsc
		else par := x.typ^.fields;
		Generator.Save_registers;
		
		if sym = Scanner.sym_lparen then
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_rparen then
				Scanner.Get (sym)
			else
				begin
				while true do
					begin
					expression (y);
					if par^.is_param then
						begin
						if (y.mode = Generator.class_typ) or (y.mode = Generator.class_sproc) then
							Scanner.Mark ('Invalid actual parameter')
						(* PROCEDURE parameter *)
						else if y.mode = Generator.class_proc then
							begin
							find_proc (obj2, y);
							if obj2 = guard then
								Scanner.Mark ('Procedure not found or not a global declared procedure')
							else if (par^.typ^.form = Generator.type_proc)
							and Check_compatible_procedure2 (par^.typ, obj2) then
								begin
								if par^.class_ = Generator.class_var then
									Generator.Procedure_Param (y)
								else Scanner.Mark ('Not able to pass a procedure as a VAR param')
								end
							else Scanner.Mark ('Incompatible param types');
							end
						else if par^.typ^.form = Generator.type_proc then
							begin
							if (y.typ^.form = Generator.type_proc)
							and Check_compatible_procedure1 (par^.typ, y.typ) then
								Generator.Parameter (y, par)
							else Scanner.Mark ('Incompatible param types')
							end
						(* Open ARRAY parameter *)
						else if (par^.typ^.form = Generator.type_array) and (par^.typ^.len = 0) then
							begin
							if y.typ^.form = Generator.type_array then
								Generator.Open_array_Param1 (y, par, par^.typ, 1)
							else if y.typ^.form = Generator.type_dynArray then
								Generator.Open_array_Param2 (y, par)
							else Scanner.Mark ('Incompatible param types');
							end
						(* RECORD variable parameter *)
						else if (par^.typ^.form = Generator.type_record) and not par^.read_only then
							begin
							if y.typ^.form = Generator.type_record then
								begin
								if Is_base_type (par^.typ, y.typ) then
									Generator.Record_variable_parameter (y)
								else Scanner.Mark ('Actual type must be an extension of formal type')
								end
							else Scanner.Mark ('Incompatible param types');
							end
						(* POINTER parameter *)
						else if par^.typ^.form = Generator.type_pointer then
							begin
							if y.typ^.form = Generator.type_pointer then
								begin
								if Is_base_type (par^.typ^.base, y.typ^.base) then
									Generator.Parameter (y, par)
								else Scanner.Mark ('Actual type must be an extension of formal type')
								end
							else Scanner.Mark ('Incompatible param types');
							end
						(* the others *)
						else if y.typ = par^.typ then
							Generator.Parameter (y, par)
						else Scanner.Mark ('Incompatible param types');
						par := par^.next;
						end
					else Scanner.Mark ('Too many parameters');
					
					if sym = Scanner.sym_comma then Scanner.Get (sym)
					else if sym = Scanner.sym_rparen then begin Scanner.Get (sym); break; end
					else if sym >= Scanner.sym_semicolon then begin Scanner.Mark ('No closing )'); break; end
					else Scanner.Mark ('No ) or ,');
					end;
				end
			end;
			
		if x.mode = Generator.class_proc then Generator.Call (x)
		else Generator.Indirect_call (x);
		if par^.is_param then Scanner.Mark ('Too few parameters');
		end;

   procedure selector (var x : Generator.Item);
		var
			y : Generator.Item;
			obj : Generator.Object_;
			
		procedure TypeGuard (var x : Generator.Item);
			var
				obj : Generator.Object_;
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_ident then
				begin
				if x.typ^.form = Generator.type_pointer then
					begin
					find (obj);
					if (obj^.class_ = Generator.class_typ)
					and (obj^.typ^.form = Generator.type_pointer) then
						begin
						if Is_base_type (x.typ^.base, obj^.typ^.base) then
							begin
							Generator.Type_guard (x, obj^.typ);
							x.typ := obj^.typ
							end
						else Scanner.Mark ('Type guard only applicable with an extension type of the declared type')
						end
					else Scanner.Mark ('Expect a pointer type identifier in type guard')
					end
				else if (x.typ^.form = Generator.type_record)
				and (x.mode = Generator.class_par) and not x.read_only then
					begin
					find (obj);
					if (obj^.class_ = Generator.class_typ)
					and (obj^.typ^.form = Generator.type_record) then
						begin
						if Is_base_type (x.typ, obj^.typ) then
							begin
							Generator.Type_guard (x, obj^.typ);
							x.typ := obj^.typ
							end
						else Scanner.Mark ('Type guard only applicable with an extension type of the declared type')
						end
					else Scanner.Mark ('Expect a record type identifier in type guard')
					end
				else Scanner.Mark ('Type guard only applicable with pointers or var parameters of record type');
				Scanner.Get (sym)
				end
			else
				Scanner.Mark ('Expect an identifier in type guard');
			if sym = Scanner.sym_rparen then Scanner.Get (sym)
			else Scanner.Mark ('Missing )')
			end;	
					
		begin (* selector *)
		while ((sym = Scanner.sym_lbrak) or (sym = Scanner.sym_period)
		or (sym = Scanner.sym_arrow) or (sym = Scanner.sym_lparen))
		and (x.mode <> Generator.class_proc) and (x.mode <> Generator.class_typ)
		and (x.mode <> Generator.class_sproc) and (x.typ^.form <> Generator.type_proc) do
			begin
			if sym = Scanner.sym_lparen then
				begin
				TypeGuard (x)
				end
			else if sym = Scanner.sym_arrow then (* POINTER dereference *)
				begin
				Scanner.Get (sym);
				if x.typ^.form = Generator.type_pointer then
					begin
					Generator.Deref (x);
					x.typ := x.typ^.base
					end
				else Scanner.Mark ('Not a pointer variable');
				end
			else if sym = Scanner.sym_lbrak then (* ARRAY index selector *)
				begin
				Scanner.Get (sym); expression (y); Check_int (y);
				if x.typ^.form = Generator.type_array then
					begin
					if x.typ^.len = 0 then (* Open array case *)
						Generator.Open_array_Index (x, y)
					else Generator.Index (x, y);
					x.typ := x.typ^.base;
					end
				else if x.typ^.form = Generator.type_dynArray then
					begin
					Generator.Dyn_array_Index (x, y);
					x.typ := x.typ^.base
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
					if x.typ^.form = Generator.type_record then
						begin
						Find_field (obj, x.typ); Scanner.Get (sym);
						if obj <> guard then begin Field (x, obj); x.typ := obj^.typ; end
						else Scanner.Mark ('Undefined field')
						end
					else if x.typ^.form = Generator.type_pointer then
						begin
						Generator.Deref (x); x.typ := x.typ^.base;
						if x.typ <> Generator.dummy_record_type then
							begin
							Find_field (obj, x.typ); Scanner.Get (sym);
							if obj <> guard then begin Generator.Field (x, obj); x.typ := obj^.typ; end
							else Scanner.Mark ('Undefined field')
							end
						end
					else Scanner.Mark ('Not a record type');
					end
				else Scanner.Mark ('No identifier after record selector');
				end;
			end;
		end;
		
	procedure StatementSequence;
		var
			obj : Generator.Object_;
			x : Generator.Item;
			
		procedure AssignmentStatement (var x : Generator.Item);
			var
				y : Generator.Item;
			begin
			Scanner.Get (sym); expression (y);
			if (y.mode = Generator.class_typ) or (y.mode = Generator.class_sproc)
			or (y.mode = Generator.class_proc) then
				begin
				Scanner.Mark ('Expect a variable or value but found type or procedure');
				Generator.Make_const (x, Generator.int_type, 0)
				end
			(* INTEGER assignment *)
			else if (x.typ^.form = Generator.type_integer) and (y.typ^.form = Generator.type_integer) then
				begin
				if x.typ <> Generator.Get_result_int_type (x, y) then
					Scanner.Mark ('Can not store bigger int into smaller int type');
				Generator.Store (x, y)
				end
			(* BOOLEAN assignment *)
			else if (x.typ = Generator.bool_type) and (y.typ = Generator.bool_type) then
				begin
				Generator.Store (x, y)
				end
			(* SET assignment *)
			else if (x.typ = Generator.set_type) and (y.typ = Generator.set_type) then
				begin
				Generator.Store (x, y)
				end
			(* Dynamic array assignment *)
			else if (x.typ^.form = Generator.type_dynArray) and (x.typ = y.typ) then
				begin
				Generator.Copy (x, y, x.typ^.size)
				end
			(* Static ARRAY assignment *)
			else if (x.typ^.form = Generator.type_array) and (y.typ^.form = Generator.type_array)
			and (x.typ^.base = y.typ^.base) and (y.typ^.len > 0) and (x.typ^.len > 0) then
				begin
				if x.typ^.len < y.typ^.len then Scanner.Mark ('Destination array is shorter than source array');
				Generator.Copy (x, y, y.typ^.len * y.typ^.base^.size)
				end
			(* Open ARRAY assignment *)
			else if (x.typ^.form = Generator.type_array) and (y.typ^.form = Generator.type_array)
			and (x.typ^.base = y.typ^.base) and (x.typ^.base^.size > 0) then
				begin
				Generator.Copy2 (x, y)
				end
			(* POINTER assignment *)
			else if (x.typ^.form = Generator.type_pointer) and (y.typ^.form = Generator.type_pointer) then
				begin
				if (y.mode = Generator.class_const) or Is_base_type (x.typ^.base, y.typ^.base) then
					Generator.Store (x, y)
				else Scanner.Mark ('Pointer base types incompatible')			
				end
			(* RECORD assignment *)
			else if (x.typ^.form = Generator.type_record) and (y.typ^.form = Generator.type_record) then
				begin
				if Is_base_type (x.typ, y.typ) then
					Generator.Copy (x, y, x.typ^.size)
				else Scanner.Mark ('Record base types incompatible')
				end
			else
				begin
				Scanner.Mark ('Incompatible types');
				Generator.Store (x, y)
				end
			end;
			
		procedure ProperStandFunc (var x : Generator.Item);
			var
				y : Generator.Item;
			begin
			if (x.a > 1) or (x.a < 0) then
				Scanner.Mark ('Not a proper standard procedure')
			else if sym = Scanner.sym_lparen then
				begin
				Scanner.Get (sym); expression (x);
				if (x.mode = Generator.class_typ) or (x.mode = Generator.class_sproc)
				or (x.mode = Generator.class_proc) then
					begin
					Scanner.Mark ('Expect actual parameter but found type or procedure');
					Generator.Make_const (x, Generator.int_type, 0)
					end;
				if sym = Scanner.sym_comma then
					begin
					Scanner.Get (sym); expression (y);
					if (y.mode = Generator.class_typ) or (y.mode = Generator.class_sproc)
					or (y.mode = Generator.class_proc) then
						begin
						Scanner.Mark ('Expect actual parameter but found type or procedure');
						Generator.Make_const (y, Generator.int_type, 0)
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
			
		procedure ProcVarAssignment (var x : Generator.Item);
			var
				obj : Generator.Object_;
				y : Generator.Item;
			begin
			Scanner.Get (sym);
			if sym = sym_ident then
				begin
				find (obj); Generator.Make_item (y, obj);
				Scanner.Get (sym); selector (y);
				if y.mode = Generator.class_proc then
					begin
					if obj^.lev > 0 then
						Scanner.Mark ('Not a global declared procedure')
					else if Check_compatible_procedure2 (x.typ, obj) then
						Generator.Store_proc_addr (x, y)
					else Scanner.Mark ('Assignment with incompatible procedure types')
					end
				else if (y.mode = Generator.class_typ)
				or (y.mode = Generator.class_sproc) then
					begin
					Scanner.Mark ('Expecting a procedure or a procedure variable')
					end
				else if y.typ^.form = Generator.type_proc then
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
				x : Generator.Item;
				elb, lb : AnsiString;
				l : Integer;
			begin
			l := Generator.line_num;
			elb := 'IF_' + IntToStr (l) + '_END';
			
			Scanner.Get (sym); expression (x);
			Check_bool (x); Generator.Cond_jump (x);
			
			if sym = Scanner.sym_then then Scanner.Get (sym) else Scanner.Mark ('THEN missing');
			StatementSequence;

			while sym = Scanner.sym_elsif do
				begin
				Generator.Jump (elb);
				lb := 'ELSIF_' + IntToStr (Generator.line_num);
				Generator.Fix_link (x.a, lb); Generator.Emit_label (lb);
				
				Scanner.Get (sym); expression (x);
				Check_bool (x); Generator.Cond_jump (x);
				
				if sym = Scanner.sym_then then Scanner.Get (sym) else Scanner.Mark ('THEN missing');
				StatementSequence;
				end;
			
			if sym = Scanner.sym_else then
				begin
				Generator.Jump (elb);
				lb := 'ELSE_' + IntToStr (Generator.line_num);
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
				x : Generator.Item;
				slb, elb, lb : AnsiString;
				l : Integer;
			begin
			l := Generator.line_num;
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
				lb := 'WHILE_ELSIF_' + IntToStr (Generator.line_num);
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
				x : Generator.Item;
				slb : AnsiString;
				l : Integer;
			begin
			l := Generator.line_num;
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
				find (obj); Generator.Make_item (x, obj);
				Scanner.Get (sym); selector (x);
				if x.mode = Generator.class_proc then
					ProcedureCall (x, obj)
				else if x.mode = Generator.class_sproc then
					ProperStandFunc (x)
				else if x.mode = Generator.class_typ then
					Scanner.Mark ('Error, type identifier appear at statement''s beginning')
				else if x.typ^.form = Generator.type_proc then
					begin
					if sym = Scanner.sym_becomes then
						ProcVarAssignment (x)
					else if sym = Scanner.sym_lparen then
						ProcedureCall (x, obj)
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
			else if ((sym > Scanner.sym_semicolon) and (sym < Scanner.sym_if)) or (sym >= Scanner.sym_array) then
				break
			else Scanner.Mark ('No semicolon');
			end;
		end;
		
	procedure element (var x : Generator.Item);
		var
			y, z : Generator.Item;
		begin
		expression (y);
		if sym = Scanner.sym_upto then
			begin
			Scanner.Get (sym); expression (z);
			if (y.mode = Generator.class_typ) or (y.mode = Generator.class_proc)
			or (y.mode = Generator.class_sproc) or (z.mode = Generator.class_typ)
			or (z.mode = Generator.class_proc) or (z.mode = Generator.class_sproc) then
				begin
				Scanner.Mark ('Integer value expected');
				end
			else if (y.typ^.form <> Generator.type_integer) or (z.typ^.form <> Generator.type_integer) then
				begin
				Scanner.Mark ('Integer value expected');
				Generator.Make_clean_const (y, Generator.int_type, 0);
				Generator.Make_clean_const (z, Generator.int_type, 0);
				end
			else
				Generator.Set3 (x, y, z);
			end
		else
			begin
			if (y.mode = Generator.class_typ) or (y.mode = Generator.class_proc)
			or (y.mode = Generator.class_sproc) then
				begin
				Scanner.Mark ('Integer value expected');
				end
			else if y.typ^.form <> Generator.type_integer then
				begin
				Scanner.Mark ('Integer value expected');
				Generator.Make_clean_const (y, Generator.int_type, 0);
				end
			else
				Generator.Set2 (x, y);
			end;
		end;
		
	procedure set_ (var x : Generator.Item);
		begin
		Scanner.Get (sym);
		Generator.Make_const (x, Generator.set_type, 0);
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
			if sym = Scanner.sym_rbrace then Scanner.Get (sym) else Scanner.Mark ('SET without closing }');
			end;
		end;
		
	procedure StandFunc (var x : Generator.Item; fctno : Int64);
		begin
		if sym = Scanner.sym_lparen then
			begin
			Scanner.Get (sym); expression (x);
			if (x.mode = Generator.class_typ) or (x.mode = Generator.class_proc)
			or (x.mode = Generator.class_sproc) then
				begin
				Generator.Make_const (x, Generator.int_type, 0);
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
			Generator.Make_clean_const (x, Generator.int_type, 0);
			end;
		end;
		
	procedure factor (var x : Generator.Item);			
		var
			obj : Generator.Object_;
		begin
		if sym < Scanner.sym_lparen then
			begin
			Scanner.Mark ('Factor beginning?');
			repeat Scanner.Get (sym) until sym >= Scanner.sym_lparen;
			end;
		if sym = Scanner.sym_ident then
			begin
			find (obj); Generator.Make_item (x, obj);
			Scanner.Get (sym); selector (x);
			if x.mode = Generator.class_sproc then
				begin
				if sym = Scanner.sym_lparen then
					begin
					StandFunc (x, obj^.val);
					x.typ := obj^.typ;
					end
				end
			else if x.mode = Generator.class_proc then
				begin
				if (x.typ <> nil) and (sym = Scanner.sym_lparen) then
					begin
					ProcedureCall (x, obj);
					Generator.Make_function_result_item (x);
					end
				end
			else if x.mode = Generator.class_typ then
				(* Do nothing *)
			else if (x.typ^.form = Generator.type_proc)
			and (sym = Scanner.sym_lparen) and (x.typ^.base <> nil) then
				begin
				ProcedureCall (x, obj);
				x.typ := x.typ^.base;
				Generator.Make_function_result_item (x);
				end
			end
		else if sym = Scanner.sym_nil then
			begin
			Generator.Make_const (x, Generator.nil_type, 0);
			Scanner.Get (sym);
			end
		else if sym = Scanner.sym_number then
			begin
			Generator.Make_const (x, Generator.int_type, Scanner.value);
			Scanner.Get (sym);
			end
		else if sym = Scanner.sym_lparen then
			begin
			Scanner.Get (sym); expression (x);
			if sym = Scanner.sym_rparen then Scanner.Get (sym) else Scanner.Mark (') is missing');
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
			Scanner.Mark ('Invalid factor?');
			Generator.Make_item (x, guard);
			end;
		end;
		
	procedure term (var x : Generator.Item);
		var
			y : Generator.Item;
			op : Integer;
		begin
		factor (x);
		while (sym >= Scanner.sym_times) and (sym <= Scanner.sym_and) do
			begin
			op := sym;
			Check_operator (x, op);
			Scanner.Get (sym);
			
			if op = Scanner.sym_and then
				begin Generator.Op1 (op, x); end;
				
			factor (y);
			Check_operator (y, op);
			
			if x.typ^.form = y.typ^.form then Generator.Op2 (op, x, y)
			else Scanner.Mark ('Incompatible types');
			end;
		end;
		
	procedure SimpleExpression (var x : Generator.Item);
		var
			y : Generator.Item;
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
			
			if x.typ^.form = y.typ^.form then Generator.Op2 (op, x, y)
			else Scanner.Mark ('Incompatible types');
			end;
		end;
		
	procedure expression (var x : Generator.Item);
		var
			op : Integer;
			y : Generator.Item;
			
		procedure TypeTest (var x : Generator.Item);
			var
				obj : Generator.Object_;
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_ident then
				begin
				if x.typ^.form = Generator.type_pointer then
					begin
					find (obj); Scanner.Get (sym);
					if (obj^.class_ = Generator.class_typ)
					and (obj^.typ^.form = Generator.type_pointer) then
						begin
						if Is_base_type (x.typ^.base, obj^.typ^.base) then
							Generator.Type_test (x, obj^.typ)
						else Scanner.Mark ('Type test only applicable with an extension type of the declared type')
						end
					else Scanner.Mark ('Expecting a pointer type identifier in type test')
					end
				else if (x.typ^.form = Generator.type_record)
				and (x.mode = Generator.class_par) and not x.read_only then
					begin
					find (obj); Scanner.Get (sym);
					if (obj^.class_ = Generator.class_typ)
					and (obj^.typ^.form = Generator.type_record) then
						begin
						if Is_base_type (x.typ, obj^.typ) then
							Generator.Type_test (x, obj^.typ)
						else Scanner.Mark ('Type test only applicable with an extension type of the declared type')
						end
					else Scanner.Mark ('Expecting a record type identifier in type test')
					end
				else Scanner.Mark ('Type test only applicable with pointer variables or variable parameters of record type')
				end
			else Scanner.Mark ('Type identifier expected in type test');
			if x.mode <> Generator.mode_cond then
				Generator.Make_clean_const (x, Generator.bool_type, 0)
			else x.typ := Generator.bool_type;
			end;
			
		begin (* expression *)
		SimpleExpression (x);
		if (sym >= Scanner.sym_equal) and (sym <= Scanner.sym_greater) then
			begin
			op := sym;
			Check_operator (x, op);
			Scanner.Get (sym);
			
			if x.typ = Generator.bool_type then Generator.Finish_cond (x);			
			SimpleExpression (y);
			Check_operator (y, op);
			
			if (x.typ = Generator.set_type) and (x.typ = y.typ)
			and ((op = Scanner.sym_greater_equal) or (op = Scanner.sym_less_equal)) then
				Generator.Inclusion_test (op, x, y)
			else if x.typ^.form = y.typ^.form then
				Generator.Relation (op, x, y)
			else
				begin
				Scanner.Mark ('Incompatible types');
				Generator.Make_clean_const (x, Generator.bool_type, 0);
				Generator.Make_clean_const (y, Generator.int_type, 0)
				end;
			x.typ := Generator.bool_type;
			end
		else if sym = Scanner.sym_in then
			begin
			Check_int (x);
			
			Scanner.Get (sym);
			SimpleExpression (y);
			Check_set (y);
			
			Generator.Membership_test (x, y);
			x.typ := Generator.bool_type;
			end
		else if sym = Scanner.sym_is then
			begin
			if (x.mode = Generator.class_typ) or (x.mode = Generator.class_proc)
			or (x.mode = Generator.class_proc) then
				Scanner.Mark ('Not a pointer or record variable parameter')
			else if x.typ^.form = Generator.type_pointer then
				TypeTest (x)
			else if (x.typ^.form = Generator.type_record)
			and (x.mode = Generator.class_par) and not x.read_only then
				TypeTest (x)
			else
				begin
				Scanner.Mark ('Not a pointer or record variable parameter');
				Generator.Make_clean_const (x, Generator.bool_type, 0)
				end
			end;
		end;
		
	procedure ProcedureDecl;
		var
			proc : Generator.Object_;
			proc_id : AnsiString;
			local_block_size, para_block_size : Int64;
			x : Generator.Item;
			
		procedure Fix_param_adr (var para_block_size : Int64);
			var
				obj : Generator.Object_;
			begin
			para_block_size := -para_block_size;
			obj := top_scope^.next;
			while obj <> guard do
				begin
				obj^.val := obj^.val + para_block_size + 16;
				obj := obj^.next;
				end
			end;
			
		begin (* begin ProcedureDecl *)
		Scanner.Get (sym);
		if sym = Scanner.sym_ident then
			begin
			proc_id := Scanner.id;
			New_obj (proc, Generator.class_proc); Scanner.Get (sym); para_block_size := 0;
			Generator.Inc_level (1); Open_scope; proc.val := -1; proc^.typ := nil;
			
			if sym = Scanner.sym_lparen then FormalParameters (proc, para_block_size);
			if para_block_size < 0 then Fix_param_adr (para_block_size);
			
			local_block_size := 0; proc^.dsc := top_scope^.next;
			if sym = Scanner.sym_semicolon then Scanner.Get (sym) else Scanner.Mark ('No semicolon');
			Declarations (local_block_size, False);
			local_block_size := -local_block_size;
			
			while sym = Scanner.sym_procedure do
				begin
				ProcedureDecl;
				if sym = Scanner.sym_semicolon then Scanner.Get (sym)
				else Scanner.Mark ('No semicolon');
				end;
			
			proc^.val := Generator.line_num;
			Generator.Emit_label (proc^.name + '_');
			Generator.Enter (para_block_size, local_block_size);
			if sym = Scanner.sym_begin then
				begin
				Scanner.Get (sym);
				StatementSequence;
				end;
			
			if proc^.typ <> nil then
				begin
				if sym = Scanner.sym_return then
					begin
					Scanner.Get (sym); expression (x);
					if x.typ = proc^.typ then Generator.Set_Function_result (x)
					else Scanner.Mark ('Incompatible function result type');
					end
				else Scanner.Mark ('Function procedure without RETURN');
				end
			else if sym = Scanner.sym_return then
				begin
				Scanner.Mark ('Normal procedures can not have return value!');
				(* expression (x);
				Make_clean_const (x, int_type, 0); *)
				end;
				
			Generator.Return (para_block_size);
			Close_scope;
			Generator.Inc_level (-1);
			Generator.Check_reg_stack;
				
			if sym = Scanner.sym_end then Scanner.Get (sym) else Scanner.Mark ('No END for PROCEDURE');
			if sym = Scanner.sym_ident then
				begin
				if Scanner.id <> proc_id then Scanner.Mark ('Wrong procedure identifier');
				Scanner.Get (sym);
				end
			else
				Scanner.Mark ('No procedure identifier after END');
			end;
		end; (* ProcedureDecl *)
		
	procedure Module;
		var
			modid : String;
			vars_size : Int64;
			
		begin
		if sym = Scanner.sym_module then Scanner.Get (sym) else Scanner.Mark ('No MODULE keyword');
		if sym = Scanner.sym_ident then
			begin
			modid := Scanner.id;
			Scanner.Get (sym);
			end
		else Scanner.Mark ('No module identifier');
		if sym = Scanner.sym_semicolon then Scanner.Get (sym) else Scanner.Mark ('Missing semicolon');
		
		vars_size := 0;
		Declarations (vars_size, True);
		
		while sym = Scanner.sym_procedure do
			begin
			ProcedureDecl;
			if sym = Scanner.sym_semicolon then Scanner.Get (sym) else Scanner.Mark ('No semicolon');
			end;
		
		Generator.Begin_Main;
		if sym = Scanner.sym_begin then begin Scanner.Get (sym); StatementSequence; end;	
		Generator.End_Main;
		
		if sym = Scanner.sym_end then Scanner.Get (sym) else Scanner.Mark ('No END keyword');
		if sym = Scanner.sym_ident then
			begin
			if Scanner.id <> modid then Scanner.Mark ('Wrong module identifier');
			Scanner.Get (sym);
			end
		else Scanner.Mark ('No module identifier after END');
		if sym = Scanner.sym_period then Scanner.Get (sym) else Scanner.Mark ('No . at module end');
		end;
		
initialization
	New (guard); guard^.class_ := Generator.class_var;
	guard^.typ := Generator.int_type; guard^.val := 0;
	guard^.is_param := False; undef_ptr_list := nil;
	top_scope := nil; Open_scope;
	
	enter (Generator.class_typ, 1, 'BOOLEAN', Generator.bool_type);
	enter (Generator.class_typ, 2, 'INTEGER', Generator.int_type);
	enter (Generator.class_typ, 3, 'INT8', Generator.int8_type);
	enter (Generator.class_typ, 4, 'INT16', Generator.int16_type);
	enter (Generator.class_typ, 5, 'INT32', Generator.int32_type);
	enter (Generator.class_typ, 6, 'SET', Generator.set_type);
	
	enter (Generator.class_const, 1, 'TRUE', Generator.bool_type);
	enter (Generator.class_const, 0, 'FALSE', Generator.bool_type);
	
	enter (Generator.class_sproc, 0, 'GET', nil);
	enter (Generator.class_sproc, 1, 'PUT', nil);
	enter (Generator.class_sproc, 2, 'ORD', Generator.int_type);
	enter (Generator.class_sproc, 3, 'ODD', Generator.bool_type);
	enter (Generator.class_sproc, 4, 'TOINT8', Generator.int8_type);
	enter (Generator.class_sproc, 5, 'TOINT16', Generator.int16_type);
	enter (Generator.class_sproc, 6, 'TOINT32', Generator.int32_type);
	enter (Generator.class_sproc, 7, 'LEN', Generator.int_type);
	universe := top_scope;
end.
