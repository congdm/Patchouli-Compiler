unit Win64;

interface
	uses
		Base, Scanner, Parser, Generator;

	procedure Compile;

implementation
	uses
		SysUtils;

	const
		delim = ', '; prefix8 = #9'db'#9; prefix16 = #9'dw'#9; prefix32 = #9'dd'#9; prefix64 = #9'dq'#9;
		const_sym = 0; type_sym = 1; var_sym = 2; proc_sym = 3; module_sym = 7;
		field_sym = 4; param_sym = 5; varparam_sym = 6;
		end_sym = 9;
		tint = -2; tint8 = -3; tint16 = -4; tint32 = -5; tbool = -6; tset = -7; notype = -8; forwardtype = -9;
		array_form = 10; record_form = 11; pointer_form = 12; proc_form = 13; dynArray_form = 14;

		import_mask = Generator.MIN_INT;

		success = True; failed = False;
		found = True; not_found = False;

	type
		Export_entry = Record
			in_codes : Boolean;
			adr : Integer;
			end;

		Import_entry = Record
			adr : Integer;
			end;

	var
		export_num, re_export_num : Integer;
		export_table : Array of Export_entry;

(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Write_symbol_to_file (var f : Text);
		var
			obj : Base.Object_;

		procedure Export_type (typ : Base.Type_; var f : Text); forward;

		procedure Detect_type (typ : Base.Type_; var f : Text);
			begin
			Write (f, prefix32);
			if typ = nil then Writeln (f, notype)
			else if typ = Base.int_type then Writeln (f, tint)
			else if typ = Base.int8_type then Writeln (f, tint8)
			else if typ = Base.int16_type then Writeln (f, tint16)
			else if typ = Base.int32_type then Writeln (f, tint32)
			else if typ = Base.bool_type then Writeln (f, tbool)
			else if typ = Base.set_type then Writeln (f, tset)
			else
				begin
				if Base.imported in typ.flag then
					begin
					Writeln (f, forwardtype);
					Writeln (f, prefix32, typ.imported_module.export_num);
					Writeln (f, prefix32, typ.export_num)
					end
				else
					begin
					Writeln (f, typ.export_num);
					if typ.export_num = -1 then Export_type (typ, f)
					end
				end;
			end;

		procedure Re_export_module (obj : Base.Object_; var f : Text);
			begin
			// Format: <Module> <nameLen> <name>
			Writeln (f, prefix8, module_sym);
			Writeln (f, prefix32, Length (obj.actual_name));
			Writeln (f, prefix8, '''', obj.actual_name, '''');
			obj.export_num := re_export_num;
			Inc (re_export_num);
			end;

		procedure Export_const (obj : Base.Object_; var f : Text);
			begin
			// Format: <Const> <nameLen> <name> <val> <type>
			Writeln (f, prefix8, const_sym);
			Writeln (f, prefix32, Length (obj.name));
			Writeln (f, prefix8, '''', obj.name, '''');
			Writeln (f, prefix64, obj.val);
			Detect_type (obj.typ, f)
			end;

		procedure Create_export_entry (in_codes : Boolean; adr : Integer);
			begin
			SetLength (export_table, Length (export_table) + 1);
			export_table [Length (export_table) - 1].adr := adr;
			export_table [Length (export_table) - 1].in_codes := in_codes;
			end;

		procedure Export_type (typ : Base.Type_; var f : Text);
			var
				obj : Base.Object_;
			begin
			// Format: <Type> <nameLen> <name> <type description>
			typ.export_num := export_num;
			Inc (export_num);

			Writeln (f, prefix8, type_sym);
			if typ.obj = nil then
				Writeln (f, prefix32, 0)
			else
				begin
				Writeln (f, prefix32, Length (typ.obj.name));
				Writeln (f, prefix8, '''', typ.obj.name, '''');
				end;

			(* Emit type description *)
			if typ.form = Base.type_array then
				begin
				Writeln (f, prefix8, array_form);
				Writeln (f, prefix64, typ.len);
				Detect_type (typ.base, f)
				end
			else if typ.form = Base.type_dynArray then
				begin
				Writeln (f, prefix8, dynArray_form);
				Detect_type (typ.base, f)
				end
			else if typ.form = Base.type_pointer then
				begin
				Writeln (f, prefix8, pointer_form);
				Writeln (f, prefix32, typ.base.export_num);
				if typ.base.export_num = -1 then Export_type (typ.base, f);
				end
			else if typ.form = Base.type_record then
				begin
				Create_export_entry (False, typ.tag);
				
				// Format: <Record> <size> <baseType> <fieldsList> <end>
				Writeln (f, prefix8, record_form);
				Writeln (f, prefix64, typ.size);
				Detect_type (typ.base, f);

				(* Processing fieldsList *)
				obj := typ.fields;
				while obj <> guard do
					begin
					if Base.exported in obj.flag then
						begin
						// Format: <Field> <nameLen> <name> <offset> <type>
						Writeln (f, prefix8, field_sym);
						Writeln (f, prefix32, Length (obj.name));
						Writeln (f, prefix8, '''', obj.name, '''');
						Writeln (f, prefix64, obj.val);
						Detect_type (obj.typ, f)
						end;
					obj := obj.next;
					end;
				Writeln (f, prefix8, end_sym)
				end
			else if typ.form = Base.type_proc then
				begin
				// Format: <Proc> <resultType> <paramsList> <end>
				Writeln (f, prefix8, proc_form);
				Detect_type (typ.base, f);
				
				obj := typ.fields;
				while obj <> guard do
					begin
					// Format: <Param> <nameLen> <name> <type>
					if (obj.class_ = Base.class_var)
					or (Base.read_only in obj.flag) then
						Writeln (f, prefix8, param_sym)
					else Writeln (f, prefix8, varparam_sym);
					Writeln (f, prefix32, Length (obj.name));
					Writeln (f, prefix8, '''', obj.name, '''');
					Detect_type (obj.typ, f);
					obj := obj.next;
					end;
				Writeln (f, prefix8, end_sym);
				end
			end;

		procedure Export_var (obj : Base.Object_; var f : Text);
			begin
			// Format: <Var> <nameLen> <name> <type>
			Create_export_entry (False, obj.val);
			Write (f, prefix8, var_sym);
			Writeln (f, prefix32, Length (obj.name));
			Writeln (f, prefix8, '''', obj.name, '''');
			Detect_type (obj.typ, f)
			end;

		procedure Export_proc (obj : Base.Object_; var f : Text);
			begin
			// Format: <Proc> <nameLen> <name> <resultType> <paramsList> <end>
			Create_export_entry (True, obj.val);
			Write (f, prefix8, proc_sym);
			Writeln (f, prefix32, Length (obj.name));
			Writeln (f, prefix8, '''', obj.name, '''');
			Detect_type (obj.typ, f);
			
			obj := obj.dsc;
			while (obj <> guard) and (Base.is_param in obj.flag) do
				begin
				// Format: <Param> <nameLen> <name> <type>
				if (obj.class_ = Base.class_var)
				or (Base.read_only in obj.flag) then
					Writeln (f, prefix8, param_sym)
				else Writeln (f, prefix8, varparam_sym);
				Writeln (f, prefix32, Length (obj.name));
				Writeln (f, prefix8, '''', obj.name, '''');
				Detect_type (obj.typ, f);
				obj := obj.next;
				end;
			Writeln (f, prefix8, end_sym);
			end;
			
		begin (* Generate_export_symbol *)
		obj := universe.next;
		export_num := 0;
		re_export_num := 0;
		while obj <> guard do
			begin
			if Base.exported in obj.flag then
				begin
				if obj.class_ = Base.class_proc then
					Export_proc (obj, f)
				else if obj.class_ = Base.class_typ then
					Export_type (obj.typ, f)
				else if obj.class_ = Base.class_const then
					Export_const (obj, f)
				else if obj.class_ = Base.class_var then
					Export_var (obj, f)
				else if obj.class_ = Base.class_module then
					Re_export_module (obj, f)
				end;
			obj := obj.next;
			end;
		Writeln (f, prefix8, end_sym)
		end;

(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Import_module (modul_name, actual_name : AnsiString);
		type
			File_of_Byte = File of Byte;

			Import_state = Record
				f : File_of_Byte;
				modul, head : Base.Object_;
				import_id : Integer;
				modul_name, actual_name : AnsiString
				end;
				
		var
			state : Import_state;
			old_head, other_modul : Base.Object_;
			b : Byte; i : Integer; s : AnsiString;
			typ : Base.Type_;

		function New_module (var state : Import_state) : Boolean;
			var
				exit_loop : Boolean;
			begin 
			state.modul := universe;
			exit_loop := False;
			repeat
				if state.modul.class_ = Base.class_module then
					begin
					if state.modul.actual_name = state.actual_name then
						begin
						Scanner.Mark ('Error when import ' + state.actual_name + '.DLL: Duplicate or circular importing');
						exit_loop := True; state.modul := nil; Result := failed;
						end
					else if state.modul.name = state.modul_name then
						begin
						Scanner.Mark ('Error: Duplicate identifier ' + state.modul_name);
						exit_loop := True; state.modul := nil; Result := failed;
						end
					end
				else if state.modul.next = Base.guard then
					exit_loop := True
				else state.modul := state.modul.next;
				until exit_loop;
			if state.modul <> nil then
				begin
				New (state.modul.next);
				state.modul := state.modul.next;
				state.modul.next := Base.guard;
				state.modul.flag := [];
				state.modul.class_ := Base.class_module;
				state.modul.actual_name := state.actual_name;
				state.modul.name := state.modul_name;
				Result := success;
				end;
			end;

		procedure New_import_obj (var obj : Base.Object_; cls : Integer; var state : Import_state);
			begin
			obj := state.head; while obj.next <> Base.guard do obj := obj.next;
			New (obj.next); obj := obj.next;
			obj.next := Base.guard;
			obj.lev := 0; obj.class_ := cls; obj.flag := [];
			end;

		procedure Open_import_scope (var state : Import_state; var old_head : Base.Object_);
			begin
			old_head := state.head;
			New (state.head);
			state.head.next := Base.guard;
			end;

		procedure Close_import_scope (var state : Import_state; old_head : Base.Object_);
			begin
			Dispose (state.head);
			state.head := old_head;
			end;

		function Find_symbols_in_DLL (var state : Import_state) : Boolean;
			const
				signature = $00004E4F5245424F;	// 'OBERON'#0#0
			var
				i, num_of_sections, size : Integer;
				sect_name : Int64;
				found : Boolean;
			begin
			Seek (state.f, $3C);
			BlockRead (state.f, i, 4);
			Seek (state.f, i);
			BlockRead (state.f, i, 4); // PE Header signature
			BlockRead (state.f, num_of_sections, 4); // Machine type and NumOfSections
			num_of_sections := num_of_sections div $10000;
			BlockRead (state.f, i, 4); // Timestamp
			BlockRead (state.f, i, 4); // PointerToSymbolTable
			BlockRead (state.f, i, 4); // NumberOfSymbols
			BlockRead (state.f, i, 4); // SizeOfOptionHeader and Charateristics
			i := i and $FFFF;
			Seek (state.f, FilePos (state.f) + i);
			found := False;
			for i := 1 to num_of_sections do
				begin
				BlockRead (state.f, sect_name, 8);
				if sect_name = signature then begin found := True; break end;
				Seek (state.f, FilePos (state.f) + 32)
				end;
			if not found then
				begin
				Scanner.Mark ('Error: ' + state.actual_name + '.DLL' + 'is not a compatible module');
				Result := failed
				end
			else
				begin
				Seek (state.f, FilePos (state.f) + 8);
				BlockRead (state.f, size, 4);
				BlockRead (state.f, i, 4);
				Seek (state.f, i);
				Result := success
				end;
			end;

		procedure Import_type (var typ : Base.Type_; var state : Import_state); forward;

		procedure Detect_type (var typ : Base.Type_; var state : Import_state);
			var
				i, modno : Integer;
			begin
			BlockRead (state.f, i, 4);
			case i of
				tint: typ := Base.int_type;
				tint8: typ := Base.int8_type;
				tint16: typ := Base.int16_type;
				tint32: typ := Base.int32_type;
				tbool: typ := Base.bool_type;
				tset: typ := Base.bool_type;
				notype: typ := nil;
				forwardtype:
					begin
					BlockRead (state.f, modno, 4);
					BlockRead (state.f, i, 4);
					typ := state.modul.re_export_module_list [modno].type_list [i]
					end;
				-1: Import_type (typ, state);
				else if i >= 0 then typ := state.modul.type_list [i];
				end;
			end;

		procedure Import_const (var state : Import_state);
			var
				obj : Base.Object_;
				i : Integer; b : Byte;
			begin
			// Format: <Const> <nameLen> <name> <val> <type>
			New_import_obj (obj, Base.class_const, state);
			Read (state.f, b);
			BlockRead (state.f, i, 4);
			BlockRead (state.f, obj.name, i);
			BlockRead (state.f, obj.val, 8);
			Detect_type (obj.typ, state);
			end;

		procedure Import_var (var state : Import_state);
			var
				obj : Base.Object_;
				i : Integer; b : Byte;
			begin
			// Format: <Var> <nameLen> <name> <type>
			New_import_obj (obj, Base.class_par, state);
			Read (state.f, b);
			BlockRead (state.f, i, 4);
			BlockRead (state.f, obj.name, i);
			Detect_type (obj.typ, state);
			
			obj.flag := obj.flag + [Base.imported, Base.read_only];
			Inc (state.import_id); obj.import_id := state.import_id
			end;

		procedure Import_type (var typ : Base.Type_; var state : Import_state);
			var
				i : Integer; b : Byte;
				obj, old_head : Base.Object_;
			begin
			// Format: <Type> <nameLen> <name> <type description>
			Read (state.f, b);
			BlockRead (state.f, i, 4);
			if i > 0 then
				begin
				New_import_obj (obj, Base.class_typ, state);
				BlockRead (state.f, obj.name, i);
				Base.New_typ (obj.typ);
				typ := obj.typ;
				end
			else Base.New_typ (typ);
			
			typ.imported_module := state.modul;
			SetLength (state.modul.type_list, Length (state.modul.type_list) + 1);
			state.modul.type_list [Length (state.modul.type_list) - 1] := typ;
			
			Read (state.f, b);
			case b of
				array_form:
					begin
					typ.form := Base.type_array;
					BlockRead (state.f, typ.len, 8);
					Detect_type (typ.base, state);
					typ.size := typ.len * typ.base.size;
					end;
				dynArray_form:
					begin
					typ.form := Base.type_dynArray;
					Detect_type (typ.base, state);
					if typ.base.form = Base.type_record then typ.size := 32
					else typ.size := 24;
					end;
				pointer_form:
					begin
					typ.form := Base.type_pointer;
					typ.size := 8;
					Detect_type (typ.base, state)
					end;
				record_form:
					begin
					// Format: <Record> <size> <baseType> <fieldsList> <end>
					typ.form := Base.type_record;
					BlockRead (state.f, typ.size, 8);
					Detect_type (typ.base, state);
					Open_import_scope (state, old_head);
					repeat
						Read (state.f, b);
						if b <> end_sym then
							begin
							// Format: <Field> <nameLen> <name> <offset> <type>
							New_import_obj (obj, Base.class_field, state);
							BlockRead (state.f, i, 4);
							BlockRead (state.f, obj.name, i);
							BlockRead (state.f, obj.val, 8);
							Detect_type (obj.typ, state)
							end
					until b = end_sym;
					typ.fields := state.head.next;
					Close_import_scope (state, old_head);
					Inc (state.import_id); typ.import_id := state.import_id;
					typ.flag := [Base.imported];
					end;
				proc_form:
					begin
					// Format: <Proc> <resultType> <paramsList> <end>
					typ.form := Base.type_proc;
					Detect_type (typ.base, state);
					Open_import_scope (state, old_head);
					repeat
						Read (state.f, b);
						// Format: <Param> <nameLen> <name> <type>
						if b <> end_sym then
							begin
							if b = param_sym then New_import_obj (obj, Base.class_var, state)
							else New_import_obj (obj, Base.class_par, state);
							BlockRead (state.f, i, 4);
							BlockRead (state.f, obj.name, i);
							Detect_type (obj.typ, state)
							end
					until b = end_sym;
					typ.fields := state.head.next;
					Close_import_scope (state, old_head);
					end;
				end;
			end;

		procedure Import_proc (var state : Import_state);
			var
				obj, obj2, old_head : Base.Object_;
				b : Byte; i : Integer;
			begin
			// Format: <Proc> <nameLen> <name> <resultType> <paramsList> <end>
			New_import_obj (obj, Base.class_proc, state);
			Read (state.f, b);
			BlockRead (state.f, i, 4);
			BlockRead (state.f, obj.name, i);
			Detect_type (obj.typ, state);
			
			obj.flag := [Base.imported];
			Inc (state.import_id); obj.import_id := state.import_id;
			
			Open_import_scope (state, old_head);
			repeat
				Read (state.f, b);
				// Format: <Param> <nameLen> <name> <type>
				if b <> end_sym then
					begin
					if b = param_sym then Base.New_obj (obj2, Base.class_var)
					else Base.New_obj (obj2, Base.class_par);
					BlockRead (state.f, i, 4);
					BlockRead (state.f, obj2.name, i);
					Detect_type (obj2.typ, state)
					end
			until b = end_sym;
			obj.dsc := state.head.next;
			Close_import_scope (state, old_head);
			end;
			
		begin (* Import_module *)
		if FileExists (actual_name + '.DLL') then
			begin
			Assign (state.f, actual_name + '.DLL');
			Reset (state.f);

			state.modul_name := modul_name;
			state.actual_name := actual_name;
			
			if Find_symbols_in_DLL (state) = success then
				begin
				if New_module (state) = success then
					begin
					state.import_id := 0;
					Open_import_scope (state, old_head);
					Read (state.f, b);
					while b = module_sym do
						begin
						BlockRead (state.f, i, 4);
						BlockRead (state.f, s, i);
						Base.find_module (other_modul, s);
						if other_modul = Base.guard then
							begin
							Scanner.Mark ('Error: To import this module, you must import ' + s);
							Close (state.f);
							Exit
							end
						else
							begin
							SetLength (state.modul.re_export_module_list, Length (state.modul.re_export_module_list) + 1);
							state.modul.re_export_module_list [Length (state.modul.re_export_module_list) - 1] := other_modul;
							Read (state.f, b)
							end
						end;
					repeat
						case b of
							const_sym: begin Seek (state.f, FilePos(state.f) - 1); Import_const (state) end;
							var_sym:   begin Seek (state.f, FilePos(state.f) - 1); Import_var (state) end;
							proc_sym:  begin Seek (state.f, FilePos(state.f) - 1); Import_proc (state) end;
							type_sym:  begin Seek (state.f, FilePos(state.f) - 1); Import_type (typ, state) end;
							end;
						until b = end_sym;
					state.modul.dsc := state.head.next;
					Close_import_scope (state, old_head)
					end
				end
			else Scanner.Mark ('Error: Found ' + state.actual_name + '.DLL file but not a Aya Oberon module');
			Close (state.f);
			end
		else Scanner.Mark ('Error: Module DLL not found');
		end;

(* --------------------------------------------------------------------------------------- *)
(* --------------------------------------------------------------------------------------- *)

	procedure Write_export_table (var f : Text; output_name : AnsiString);
		var
			i, rva : Integer;
		begin
		if Length (export_table) > 0 then
			begin
			Writeln (f, 'section ''.edata'' export data readable writeable');
			Writeln (f, #9'dd'#9'0, 0, 0, RVA DLL_NAME, 1, ', Length (export_table), ', 0, RVA EXPORT_TABLE, 0, 0');
			Writeln (f, 'EXPORT_TABLE:');
			for i := 0 to Length (export_table) - 1 do
				begin
				rva := export_table [i].adr;
				if export_table [i].in_codes then
					Writeln (f, #9'dd'#9'RVA ', Base.codes [rva].lb)
				else
					Writeln (f, #9'dd'#9'RVA ', Base.labels [rva].lb)
				end;
			Writeln (f, 'DLL_NAME:');
			Writeln (f, #9'db'#9'''', Upcase (output_name), '.DLL'', 0');
			end;
		end;

	procedure Write_import_table (var f : Text);
		var
			first_modul, modul, obj : Base.Object_;
			i : Integer; typ : Base.Type_;
			have_import : Boolean;
		begin
		first_modul := Base.universe;
		repeat first_modul := first_modul.next
		until (first_modul.class_ = Base.class_module) or (first_modul = Base.guard);

		if first_modul <> Base.guard  then
			begin
			modul := first_modul;
			repeat
				have_import := False;
				if Base.is_used in modul.flag then
					begin
					if not have_import then
						begin
						Writeln (f, 'section ''.idata'' import data readable writeable');
						have_import := True;
						end;
					Writeln (f, #9'dd'#9'0, 0, 0, RVA ', modul.name, '_name, RVA ', modul.name, '_table');
					end;
				modul := modul.next;
				until modul = Base.guard;
			
			if have_import then
				begin
				Writeln (f, #9'dd'#9'0, 0, 0, 0, 0');
				modul := first_modul;
				repeat
					if Base.is_used in modul.flag then
						begin
						Writeln (f, modul.name, '_table:');
						obj := modul.dsc;
						while obj <> guard do
							begin
							if (obj.class_ <> Base.class_typ) and (obj.class_ <> Base.class_const)
							and (Base.is_used in obj.flag) then
								Writeln (f, Base.labels [obj.val].lb, #9'dq'#9, obj.import_id + import_mask);
							obj := obj.next;
							end;
						for i := 0 to Length (modul.type_list) - 1 do
							begin
							typ := modul.type_list [i];
							if Base.is_used in typ.flag then
								Writeln (f, Base.labels [typ.tag].lb, #9'dq'#9, typ.import_id + import_mask);
							end;
						Writeln (f, #9'dq'#9'0');
						end;
					modul := modul.next;
					until modul = Base.guard;
				modul := first_modul;
				repeat
					if Base.is_used in modul.flag then
						Writeln (f, modul.name, '_name', #9'db'#9'''', modul.name, '.DLL'', 0');
					modul := modul.next;
					until modul = Base.guard;
				end;
			end;
		end;

	procedure Write_entry_function (var f : Text);
		begin
		Writeln (f, 'start:');
		Writeln (f, '	PUSH  RBX');
		Writeln (f, '	PUSH  RSI');
		Writeln (f, '	PUSH  RDI');
		Writeln (f, '	PUSH  R12');
		Writeln (f, '	PUSH  R13');
		Writeln (f, '	PUSH  R14');
		Writeln (f, '	PUSH  R15');
		Writeln (f);
		
		Writeln (f, '	CALL INIT_MODULE');
		Writeln (f);
		
		Writeln (f, '	POP  R15');
		Writeln (f, '	POP  R14');
		Writeln (f, '	POP  R13');
		Writeln (f, '	POP  R12');
		Writeln (f, '	POP  RDI');
		Writeln (f, '	POP  RSI');
		Writeln (f, '	POP  RBX');
		Writeln (f, '	RET');
		Writeln (f);
		end;

	procedure Compile;
		var
			f : Text;
			modid : AnsiString;
		begin
		Scanner.Get (Parser.sym);
		Parser.Module (modid);
		
		if error > 0 then
			begin
			Writeln ('FAILED: There are errors in source code! Please check your source code again.');
			end
		else
			begin
			Assign (f, modid + '.asm');
			Rewrite (f);

			Writeln (f, 'format PE64 GUI');
			Writeln (f, 'entry start');
			Writeln (f);
			
			Writeln (f, 'section ''.text'' code readable executable');
			Writeln (f);		
			Base.Write_codes_to_file (f);
			Write_entry_function (f);

			Writeln (f, 'section ''.bss'' data readable writeable');
			Base.Write_bss_to_file (f);
			Writeln (f);

			Writeln (f, 'section ''.OBERON'' data readable writeable');
			Write_symbol_to_file (f);
			Writeln (f);

			//Write_import_table (f);
			//Writeln (f);
			//Write_export_table (f, modid);
			
			Close (f);
			Writeln (modid, ' module was compiled successfully')
			end
		end;

initialization

end.
