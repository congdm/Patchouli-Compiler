unit Base;

interface
	uses
		Scanner;
		
	const
		Word_size = 8;

		class_head = 0; class_module = 1;
		class_var = 2; class_par = 3; class_const = 4; class_field = 5; class_typ = 6;
		class_proc = 7; class_sproc = 8;
		class_MAX = 19;

		type_boolean = 0; type_integer = 1; type_array = 2; type_record = 3; type_set = 4;
		type_dynArray = 9; type_pointer = 10; type_proc = 11;

		is_param = 0; exported = 1; imported = 2; is_used = 3; read_only = 4;
		in_stack = 5; predefined = 6;

	type
		MachineInteger = Int64;

		Type_ = ^Type_desc;
		Object_ = ^Obj_desc;

		Type_desc = record
			flag : Set of 0..31;
			form, tag, export_num, import_id, num_of_pointers : Integer;
			fields, obj, imported_module : Object_;
			base : Type_;
			size, len : MachineInteger;
			end;

		Obj_desc = record
			flag : Set of 0..31;
			class_, lev, import_id, export_num : Integer;
			name, actual_name : AnsiString;
			typ : Type_;
			next, dsc, imported_module : Object_;
			type_list : Array of Type_;
			re_export_module_list : Array of Object_;
			val, parblksize : MachineInteger;
			end;

		Item = record
			flag, old_reg_stack : Set of 0..31;
			mode, lev : Integer;
			typ : Type_;
			a, b, c, r : MachineInteger;
			end;

		Code_line = record
			lb, inst, o1, o2, o3, comment : AnsiString;
			end;

		Label_line_ptr = ^Label_line;
		Label_line = record
			typ : Type_; obj : Object_;
			lb : AnsiString; next : Integer;
			data_size : MachineInteger;
			end;

	var
		code_num, label_num : Integer;
		range_check_flag, overflow_check_flag : Boolean;
		codes : Array of Code_line;
		labels : Array of Label_line;
		gdata_list, imported_list, tag_list : Integer;
		gdata_list_tail, imported_list_tail, tag_list_tail : Integer;

		guard, universe, top_scope : Object_;
		cur_lev : Integer;
		int_type, bool_type, int8_type, int16_type, int32_type, set_type : Type_;
		nil_type : Type_;

		Import_module : Procedure (modul_name, actual_name : AnsiString);

	procedure Write_op (inst, o1, o2, o3 : AnsiString);
	procedure Write_line (lb, inst, o1, o2, o3, comment : AnsiString);
	procedure Emit_blank_line;

	(*procedure Global_var_decl (var obj : Object_);
	procedure Emit_type_tag (name : AnsiString; var typ : Type_);
	procedure Imported_var_decl (var obj : Object_; module_name : AnsiString);*)
	procedure Add_global_var (obj : Object_);
	procedure Add_type_tag (typ : Type_);
	procedure Add_imported_obj (obj : Object_);
	procedure Add_imported_type (typ : Type_);
	
	procedure Write_codes_to_file (var f : Text);
	procedure Write_bss_to_file (var f : Text);

	procedure New_obj (var obj : Object_; class_ : Integer);
	procedure New_typ (var typ : Type_);

	procedure New_predefined_type (var typ : Type_; form : Integer; size : MachineInteger);
	procedure enter (cl : Integer; n : MachineInteger; name : String; typ : Type_);
	
	procedure find (var obj : Object_);
	procedure find_proc (var obj : Object_; var x : Item);
	procedure find2 (var obj : Object_);
	procedure find_field (var obj : Object_; typ : Type_);
	procedure find_module (var obj : Object_; actual_name : AnsiString);
	
	procedure Open_scope;
	procedure Close_scope;
	procedure Inc_level (i : Integer);

implementation
	uses
		SysUtils;

	procedure Add_imported_obj (obj : Object_);
		var
			i : Integer;
		begin
		i := label_num; Inc (label_num);
		SetLength (labels, label_num);

		labels [i].lb := obj.imported_module.name + '.' + obj.name;
		labels [i].obj := obj;
		labels [i].next := 0;
		
		if imported_list = 0 then imported_list := i
		else labels [imported_list_tail].next := i;
		imported_list_tail := i;
		obj.val := i;
		end;

	procedure Add_imported_type (typ : Type_);
		var
			i : Integer;
		begin
		i := label_num; Inc (label_num);
		SetLength (labels, label_num);

		if typ.obj <> nil then
			labels [i].lb := typ.imported_module.name + '.' + typ.obj.name
		else labels [i].lb := typ.imported_module.name + '.TYPE' + IntToStr (i);
		labels [i].typ := typ;
		labels [i].next := 0;
		
		if imported_list = 0 then imported_list := i
		else labels [imported_list_tail].next := i;
		imported_list_tail := i;
		typ.tag := i;
		end;

	procedure Add_global_var (obj : Object_);
		var
			i : Integer;
		begin
		i := label_num; Inc (label_num);
		SetLength (labels, label_num);

		labels [i].lb := obj.name + '_';
		labels [i].data_size := obj.typ.size;
		labels [i].obj := obj;
		labels [i].next := 0;
		
		if gdata_list = 0 then gdata_list := i
		else labels [gdata_list_tail].next := i;
		gdata_list_tail := i;
		obj.val := i;
		end;

	procedure Add_type_tag (typ : Type_);
		var
			i : Integer;
		begin
		i := label_num; Inc (label_num);
		SetLength (labels, label_num);

		if typ.obj <> nil then labels [i].lb := typ.obj.name + '_'
		else labels [i].lb := 'TYPE' + IntToStr (i);
		labels [i].typ := typ;
		labels [i].next := 0;
		
		if tag_list = 0 then tag_list := i
		else labels [tag_list_tail].next := i;
		tag_list_tail := i;
		typ.tag := i;
		end;

(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure New_obj (var obj : Object_; class_ : Integer);
		var
			new_, x : Object_;
		begin
		x := top_scope;
		guard.name := Scanner.id;
		while x.next.name <> Scanner.id do x := x.next;
		if x.next = guard then
			begin
			New (new_);
			new_.name := Scanner.id;
			new_.class_ := class_;
			new_.lev := cur_lev;
			new_.next := guard;
			new_.flag := [];
			new_.export_num := -1;
			x.next := new_; obj := new_;
			end
		else
			begin
			obj := x.next;
			Scanner.Mark ('Duplicated identifier declaration');
			end;
		end;

	procedure New_typ (var typ : Base.Type_);
		begin
		New (typ);
		typ.export_num := -1;
		typ.obj := nil;
		typ.import_id := -1;
		typ.imported_module := nil;
		typ.flag := [];
		typ.num_of_pointers := 0
		end;

	procedure Open_scope;
		var
			s : Object_;
		begin
		New (s);
		s.class_ := class_head;
		s.dsc := top_scope;
		s.next := guard;
		top_scope := s;
		end;

	procedure Close_scope;
		var
			s : Object_;
		begin
		s := top_scope;
		top_scope := top_scope.dsc;
		Dispose (s);
		end;

	procedure enter (cl : Integer; n : MachineInteger; name : String; typ : Type_);
		var
			obj : Object_;
		begin
		New (obj);
		obj.class_ := cl; obj.val := n; obj.name := name;
		obj.typ := typ; obj.dsc := nil;
		obj.flag := [predefined];
		if cl = class_typ then typ.obj := obj;
		obj.next := top_scope.next;
		top_scope.next := obj;
		end;

	procedure New_predefined_type (var typ : Type_; form : Integer; size : MachineInteger);
		begin
		New_typ (typ);
		typ.form := form;
		typ.size := size;
		typ.flag := typ.flag + [predefined];
		end;

	procedure find (var obj : Object_);
		var
			s, x : Object_;
		begin
		s := top_scope; guard.name := Scanner.id;
		while true do
			begin
			x := s.next;
			while x.name <> Scanner.id do
			x := x.next;
			if x <> guard then
				begin
				if not (is_used in x.flag) and (imported in x.flag) then
					begin
					Add_imported_obj (x);
					end;
				x.flag := x.flag + [is_used];
				obj := x; break
				end;
			if s = universe then
				begin
				obj := x;
				Scanner.Mark ('Undefined identifier');
				break;
				end;
			s := s.dsc;
			end;
		end;

	procedure find_proc (var obj : Object_; var x : Item);
		var
			s : Object_;
		begin
		s := top_scope;
		while s <> universe do s := s.dsc;
		while (s <> guard) do
			begin
			if (s.class_ = Base.class_proc) and (s.val = x.a) then
				begin
				obj := s;
				s := guard
				end
			else s := s.next
			end
		end;

	procedure find2 (var obj : Object_);
		var
			s, x : Object_;
		begin
		s := top_scope; guard.name := Scanner.id;
		while true do
			begin
			x := s.next;
			while x.name <> Scanner.id do x := x.next;
			if x <> guard then begin obj := x; break end;
			if s = universe then begin obj := x; break end
			else s := s.dsc;
			end;
		end;

	procedure find_field (var obj : Object_; typ : Type_);
		var
			list : Base.Object_;
		begin
		guard.name := Scanner.id;
		while typ <> nil do
			begin
			list := typ.fields;
			while list.name <> Scanner.id do list := list.next;
			obj := list;
			if obj <> guard then break;
			typ := typ.base
			end
		end;

	procedure find_module (var obj : Object_; actual_name : AnsiString);
		var
			s, x : Object_;
		begin
		s := universe; guard.name := actual_name;
		x := s.next;
		while x.name <> actual_name do x := x.next;
		obj := x;
		end;

	procedure Inc_level (i : Integer);
		begin
		cur_lev := cur_lev + i
		end;

(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Write_line (lb, inst, o1, o2, o3, comment : AnsiString);
		begin
		SetLength (codes, code_num + 1);
		codes [code_num].lb := lb;
		codes [code_num].inst := inst;
		codes [code_num].o1 := o1;
		codes [code_num].o2 := o2;
		codes [code_num].o3 := o3;
		codes [code_num].comment := comment;
		code_num := code_num + 1;
		end;

	procedure Write_op (inst, o1, o2, o3 : AnsiString);
		begin
		Write_line ('', inst, o1, o2, o3, '');
		end;

	procedure Emit_blank_line;
		begin
		Write_line ('', '', '', '', '', '');
		end;

(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Write_codes_to_file (var f : Text);
		var
			i : Integer;
		begin
		for i := 1 to Length (codes) - 1 do
			begin
			if codes [i].lb <> '' then Write (f, codes [i].lb, ':');
			if codes [i].inst <> '' then Write (f, #9, codes [i].inst);
			if codes [i].o1 <> '' then Write (f, ' ', codes [i].o1);
			if codes [i].o2 <> '' then Write (f, ', ', codes [i].o2);
			if codes [i].o3 <> '' then Write (f, ', ', codes [i].o3);
			if codes [i].comment <> '' then Write (f, '; ', codes [i].comment);
			Writeln (f);
			end;
		end;

	procedure Write_bss_to_file (var f : Text);
		var
			p : Integer;
		begin
		Writeln (f, '; TYPE DESCRIPTORS SECTION');
		p := tag_list;
		while p <> 0 do
			begin
			Writeln (f, labels [p].lb, #9'rb ' , labels [p].data_size, ' dup ?');
			p := labels [p].next
			end;
			
		Writeln (f, '; GLOBAL VARIABLES SECTION');
		Writeln (f, 'Global:');
		p := gdata_list;
		while p <> 0 do
			begin
			Writeln (f, labels [p].lb, #9'rb ' , labels [p].data_size, ' dup ?');
			p := labels [p].next
			end
		end;

(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

initialization
	gdata_list := 0; imported_list := 0; tag_list := 0;
	code_num := 1; label_num := 1;
	range_check_flag := True;
	overflow_check_flag := True;
	cur_lev := 0;

	New_predefined_type (bool_type, type_boolean, 1);
	New_predefined_type (int_type, type_integer, 8);
	New_predefined_type (int8_type, type_integer, 1);
	New_predefined_type (int16_type, type_integer, 2);
	New_predefined_type (int32_type, type_integer, 4);
	New_predefined_type (set_type, type_set, 8);

	New (nil_type);
	nil_type^.form := Base.type_pointer;
	nil_type^.size := 8;
	nil_type^.base := nil;

	New (guard); guard.class_ := class_var;
	guard.typ := int_type; guard.val := 0;
	guard.flag := [];

	top_scope := nil;
	Open_scope;

	enter (class_typ, 1, 'BOOLEAN', bool_type);
	enter (class_typ, 2, 'INTEGER', int_type);
	enter (class_typ, 3, 'INT8', int8_type);
	enter (class_typ, 4, 'INT16', int16_type);
	enter (class_typ, 5, 'INT32', int32_type);
	enter (class_typ, 6, 'SET', set_type);

	enter (class_const, 1, 'TRUE', bool_type);
	enter (class_const, 0, 'FALSE', bool_type);

	enter (class_sproc, 0, 'GET', nil);
	enter (class_sproc, 1, 'PUT', nil);
	enter (class_sproc, 2, 'ORD', set_type);
	enter (class_sproc, 3, 'ODD', bool_type);
	enter (class_sproc, 4, 'TOINT8', int8_type);
	enter (class_sproc, 5, 'TOINT16', int16_type);
	enter (class_sproc, 6, 'TOINT32', int32_type);
	enter (class_sproc, 7, 'LEN', set_type);
	
	universe := top_scope;
end.
