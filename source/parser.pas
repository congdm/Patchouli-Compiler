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
	
	var
		top_scope, guard, universe : Generator.Object_;

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
			begin	obj := x^.next; Scanner.Mark ('Duplicated identifier declaration'); end;
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
		
	procedure Check_int (var x : Generator.Item);
		begin
		if x.typ^.form <> Generator.type_integer then
			Scanner.Mark ('Not an integer');
		end;
		
	procedure Check_operator (var x : Generator.Item; op : Integer);
		var
			form : Integer;
		begin
		form := x.typ^.form;
		if (op = Scanner.sym_plus) or (op = Scanner.sym_minus) or (op = Scanner.sym_times) then
			begin
			if (form <> Generator.type_integer) and (form <> Generator.type_set) then
				Scanner.Mark ('+, -, * only compatible with INTEGER or SET types');
			end
		else if (op = Scanner.sym_div) or (op = Scanner.sym_mod) then
			begin
			if form <> Generator.type_integer then
				Scanner.Mark ('DIV, MOD only compatible with INTEGER types');
			end
		else if op = Scanner.sym_slash then
			begin
			if form <> Generator.type_set then
				Scanner.Mark ('/ only compatible with SET type');
			end
		else if (op = Scanner.sym_and) or (op = Scanner.sym_or) or (op = Scanner.sym_not) then
			begin
			if form <> Generator.type_set then
				Scanner.Mark ('&, OR, ~ only compatible with BOOLEAN type');
			end
		else if (op = Scanner.sym_equal) or (op = Scanner.sym_not_equal) then
			begin
			if (form <> Generator.type_integer) and (form <> Generator.type_boolean) and (form <> Generator.type_set) then
				Scanner.Mark ('=, # only compatible with INTEGER, BOOLEAN or SET types');
			end
		else if (op = Scanner.sym_greater) or (op = Scanner.sym_less) then
			begin
			if form <> Generator.type_integer then
				Scanner.Mark ('>, < only compatible with INTEGER types');
			end
		else if (op = Scanner.sym_greater_equal) or (op = Scanner.sym_less_equal) then
			begin
			if (form <> Generator.type_integer) and (form <> Generator.type_set) then
				Scanner.Mark ('>=, <= only compatible with INTEGER or SET types');
			end;
		end;
		
	procedure Check_prefix_operator (var x : Generator.Item; op : Integer);
		var
			form : Integer;
		begin
		form := x.typ^.form;
		if op = Scanner.sym_plus then
			begin
			if form <> Generator.type_integer then
				Scanner.Mark ('Prefix + only compatible with INTEGER types');
			end
		else if op = Scanner.sym_minus then
			begin
			if (form <> Generator.type_integer) and (form <> Generator.type_set) then
				Scanner.Mark ('Prefix - only compatible with INTEGER or SET types');
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
		
	procedure Find_field (var obj : Generator.Object_; list : Generator.Object_);
		begin
		guard^.name := Scanner.id;
		while list^.name <> Scanner.id do list := list^.next;
		obj := list;
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
				else
					Scanner.Mark ('No identifier after ,');
				end;
			if sym = Scanner.sym_colon then
				begin Scanner.Get (sym); end
			else
				Scanner.Mark ('No : after identifier list');
			end;
		end;
		
	procedure Type_ (var typ : Generator.Type_; name : AnsiString);
		var
			obj, first : Generator.Object_;
			tp : Generator.Type_;
			x : Generator.Item;
			
		(* Dynamic array type is just a big pointer to an array *)
		(* Syntax: ARRAY OF type *)
		(* Only support 1-dimension dynamic array *)
		procedure DynArrayType (var typ : Generator.Type_; name : AnsiString);
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
				Type_ (typ^.base, name);
				if typ^.base^.form = Generator.type_record then
					typ^.size := typ^.size + Word_size (* place for type tag *)
				else if typ^.base^.form = Generator.type_array then
					begin
					Scanner.Mark ('Dynamic array of array type is not supported');
					typ^.base := Generator.int_type;
					end;
				end
			end;
			
		begin
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
			else if obj^.class_ = class_typ then typ := obj^.typ else Scanner.Mark ('Type not found');
			end
			
		else if sym = Scanner.sym_array then
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_of then
				DynArrayType (typ, name)
			else
				begin
				expression (x);
				if (x.mode <> Generator.class_const) or (x.a <= 0) then
					begin Scanner.Mark ('Invalid array size'); x.a := 1; end;
				if sym = Scanner.sym_of then Scanner.Get (sym) else Mark ('No OF in array declaration');
				Type_ (tp, name);	New (typ);
				typ^.form := Generator.type_array;
				typ^.base := tp;
				typ^.len := x.a;
				typ^.size := typ^.len * tp^.size
				end
			end
			
		else if sym = Scanner.sym_record then
			begin
			Scanner.Get (sym); New (typ);
			typ^.form := Generator.type_record;
			typ^.size := 0;
			Open_scope;
			while true do
				begin
				if sym = Scanner.sym_ident then
					begin
					IdentList (Generator.class_field, first);
					Type_ (tp, name);
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
			if sym = Scanner.sym_end then Scanner.Get (sym) else Scanner.Mark ('No END after record type declaration');
			end
			
		else if sym = Scanner.sym_pointer then
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_to then Scanner.Get (sym) else Scanner.Mark ('No TO in pointer type declaration');
			(*if sym*)
			end
			
		else
			begin Scanner.Mark ('No type identifier'); end;
		end;
		
	procedure Declarations (var var_base : Int64; is_grow_up : Boolean);
		var
			obj, first : Generator.Object_;
			tp : Generator.Type_;
			x : Generator.Item;
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
					if sym = Scanner.sym_equal then Scanner.Get (sym) else Scanner.Mark ('No = in const declaration');
					expression (x);
					if x.mode = class_const then
						begin
						obj^.val := x.a;
						obj^.typ := x.typ;
						end
					else
						Scanner.Mark ('Expression is not const');
					if sym = Scanner.sym_semicolon then Scanner.Get (sym) else Scanner.Mark ('No ; after const declaration');
					end;
				end;
				
			if sym = Scanner.sym_type then
				begin
				Scanner.Get (sym);
				while sym = Scanner.sym_ident do
					begin
					New_obj (obj, Generator.class_typ);
					Scanner.Get (sym);
					if sym = Scanner.sym_equal then Scanner.Get (sym) else Scanner.Mark ('No = in type declaration');
					Type_ (obj^.typ, obj^.name);
					if sym = Scanner.sym_semicolon then Scanner.Get (sym) else Scanner.Mark ('No ; after type declaration');
					end;
				end;
				
			if sym = Scanner.sym_var then
				begin
				Scanner.Get (sym);
				while sym = Scanner.sym_ident do
					begin
					IdentList (Generator.class_var, first);
					Type_ (tp, '');
					obj := first;
					while obj <> guard do
						begin
						obj^.typ := tp;
						obj^.lev := Generator.cur_lev;
						obj^.is_param := False;
						if is_grow_up then
							begin obj^.val := var_base; var_base := var_base + obj^.typ^.size; end
						else
							begin var_base := var_base - obj^.typ^.size; obj^.val := var_base; end;
						if obj^.lev = 0 then Generator.Global_var_decl (obj);
						obj := obj^.next;
						end;
					if sym = Scanner.sym_semicolon then Scanner.Get (sym) else Scanner.Mark ('No ; after variable declaration');
					end;
				end;
				
			if (sym >= Scanner.sym_const) and (sym <= Scanner.sym_var) then
				Scanner.Mark ('Bad declaration sequence')
			else break;
			
			end;
		end;

   procedure selector (var x : Generator.Item);
		var
			y : Generator.Item;
			obj : Generator.Object_;
		begin
		while (sym = Scanner.sym_lbrak) or (sym = Scanner.sym_period) or (sym = Scanner.sym_arrow) do
			begin
			if sym = Scanner.sym_arrow then
				begin
				Scanner.Get (sym);
				if x.typ^.form = Generator.type_pointer then
					begin
					Generator.Deref (x);
					x.typ := x.typ^.base
					end
				else
					Scanner.Mark ('Not a pointer variable');
				end
			else if sym = Scanner.sym_lbrak then
				begin
				Scanner.Get (sym); expression (y); Check_int (y);
				if x.typ^.form = Generator.type_array then
					begin
					if x.typ^.len = 0 then (* Open array case *)
						Generator.Open_array_Index (x, y)
					else
						Generator.Index (x, y);
					x.typ := x.typ^.base;
					end
				else if x.typ^.form = Generator.type_dynArray then
					begin
					Generator.Dyn_array_Index (x, y);
					x.typ := x.typ^.base
					end
				else
					begin Scanner.Mark ('Not an array type') end;
					
				if sym = Scanner.sym_rbrak then Scanner.Get (sym)
				else Scanner.Mark ('Missing ]')
				end
			else (* RECORD case *)
				begin
				Scanner.Get (sym);
				if sym = Scanner.sym_ident then
					begin
					if x.typ^.form = Generator.type_record then
						begin
						Find_field (obj, x.typ^.fields); Scanner.Get (sym);
						if obj <> guard then
							begin Field (x, obj); x.typ := obj^.typ; end
						else
							begin Scanner.Mark ('Undefined field'); end
						end
					else
						begin Scanner.Mark ('Not a record type') end;
					end
				else
					begin Scanner.Mark ('No identifier after record selector') end;
				end;
			end;
		end;
		
	procedure ProcedureStatement (var x : Generator.Item; obj : Generator.Object_);
		var
			y : Generator.Item;
			par : Generator.Object_;
		begin
		par := obj^.dsc;
		Generator.Save_registers;
		if sym = Scanner.sym_lparen then
			begin
			Scanner.Get (sym);
			if sym = Scanner.sym_rparen then
				Scanner.Get (sym)
			else
				while true do
					begin
					expression (y);
					if par^.is_param then
						begin
						if (par^.typ^.form = Generator.type_array) and (par^.typ^.len = 0) then
							begin
							if y.typ^.form = Generator.type_array then
								Generator.Open_array_Param1 (y, par, par^.typ, 1)
							else if y.typ^.form = Generator.type_dynArray then
								Generator.Open_array_Param2 (y, par)
							else
								Scanner.Mark ('Incompatible param types');
							end
						else if y.typ = par^.typ then
							Generator.Parameter (y, par)
						else
							Scanner.Mark ('Incompatible param types');
						par := par^.next;
						end
					else
						Scanner.Mark ('Too many parameters');
					if sym = Scanner.sym_comma then Scanner.Get (sym)
					else if sym = Scanner.sym_rparen then begin Scanner.Get (sym); break; end
					else if sym >= Scanner.sym_semicolon then begin Scanner.Mark ('No closing )'); break; end
					else Scanner.Mark ('No ) or ,');
					end;
			end;
		if obj^.val <= 0 then
			Scanner.Mark ('Forward call unsupported')
		else
			begin
			Generator.Call (x);
			if par^.is_param then Scanner.Mark ('Too few parameters');
			end;
		Generator.Restore_registers;
		end;
		
	procedure AssignmentStatement (var x : Generator.Item);
		var
			y : Generator.Item;
		begin
		Scanner.Get (sym); expression (y);
		if (x.typ^.form = Generator.type_integer) and (x.typ^.form = y.typ^.form) then
			begin
			if x.typ <> Generator.Get_result_int_type (x, y) then
				Scanner.Mark ('Can not store bigger int into smaller int type');
			Generator.Store (x, y)
			end
		else if (x.typ^.form = Generator.type_boolean) and (x.typ^.form = y.typ^.form) then
			begin Generator.Store (x, y) end
		else if (x.typ^.form = Generator.type_set) and (x.typ^.form = y.typ^.form) then
			begin Generator.Store (x, y) end
		else
			begin
			Scanner.Mark ('Incompatible types');
			Generator.Store (x, y)
			end
		end;
		
	procedure StandFuncStatement (var x : Generator.Item);
		var
			y : Generator.Item;
		begin
		if sym = Scanner.sym_lparen then
			begin
			Scanner.Get (sym); expression (x);
			if sym = Scanner.sym_comma then
				begin
				Scanner.Get (sym); expression (y);
				if x.a = 0 then (* GET *)
					Generator.SFunc_GET (x, y)
				else if x.a = 1 then (* PUT *)
					Generator.SFunc_PUT (x, y);
				end
			else
				begin
				Scanner.Mark ('Comma expected');
				end;
			if sym = Scanner.sym_rparen then Scanner.Get (sym)
			else Scanner.Mark ('No closing )');
			end
		else
			begin
			Scanner.Mark ('Paramaters missing?');
			end;
		end;
		
	procedure StatementSequence;
		var
			obj : Generator.Object_;
			x : Generator.Item;
			
		procedure IfStatement;
			var
				x : Generator.Item;
				elb, lb : AnsiString;
				l : Integer;
			begin
			l := Generator.line_num;
			elb := 'IF_' + IntToStr (l) + '_END';
			
			Scanner.Get (sym); expression (x);
			Generator.Cond_jump (x);
			
			if sym = Scanner.sym_then then Scanner.Get (sym) else Scanner.Mark ('THEN missing');
			StatementSequence;

			while sym = Scanner.sym_elsif do
				begin
				Generator.Jump (elb);
				lb := 'ELSIF_' + IntToStr (Generator.line_num);
				Generator.Fix_link (x.a, lb); Generator.Emit_label (lb);
				
				Scanner.Get (sym); expression (x);
				Generator.Cond_jump (x);
				
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
			Generator.Cond_jump (x);
			
			if sym = Scanner.sym_do then Scanner.Get (sym) else Scanner.Mark ('DO missing');
			StatementSequence;
			Generator.Jump (slb);
			
			while sym = Scanner.sym_elsif do
				begin
				lb := 'WHILE_ELSIF_' + IntToStr (Generator.line_num);
				Generator.Fix_link (x.a, lb); Generator.Emit_label (lb);
				
				Scanner.Get (sym); expression (x);
				Generator.Cond_jump (x);
				
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
				Generator.Cond_jump (x);
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
				find (obj); Scanner.Get (sym);
				Generator.Make_item (x, obj);
				selector (x);
				if sym = Scanner.sym_becomes then
					begin
					AssignmentStatement (x);
					end
				else if (x.mode = Generator.class_proc) or (x.mode = Generator.class_func) then
					ProcedureStatement (x, obj)
				else if x.mode = Generator.class_sproc then
					StandFuncStatement (x);
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
			if (y.typ^.form <> Generator.type_integer) or (z.typ^.form <> Generator.type_integer) then
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
			if y.typ^.form <> Generator.type_integer then
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
			find (obj); Scanner.Get (sym);
			if obj^.class_ = Generator.class_sproc then
				begin
				StandFunc (x, obj^.val);
				x.typ := obj^.typ;
				end
			else if obj^.class_ = Generator.class_func then
				begin
				Generator.Make_item (x, obj);
				ProcedureStatement (x, obj);
				Generator.Make_function_result_item (x);
				end
			else
				begin Generator.Make_item (x, obj); end;
			selector (x);
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
			begin Scanner.Mark ('Invalid factor?'); Generator.Make_item (x, guard); end;
		end;
		
	procedure term (var x : Generator.Item);
		var
			y : Generator.Item;
			op : Integer;
		begin
		factor (x);
		while (sym >= Scanner.sym_times) and (sym <= Scanner.sym_and) do
			begin
			op := sym; Scanner.Get (sym);
			Check_operator (x, op);
			
			if op = Scanner.sym_and then
				begin Generator.Op1 (op, x); end;
				
			factor (y);
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
			begin Scanner.Get (sym); term (x); Check_operator (x, Scanner.sym_plus); end
		else if sym = Scanner.sym_minus then
			begin
			Scanner.Get (sym); term (x); Check_operator (x, Scanner.sym_minus);
			Generator.Op1 (Scanner.sym_minus, x);
			end
		else
			term (x);
			
		while (sym >= Scanner.sym_plus) and (sym <= Scanner.sym_or) do
			begin
			op := sym; Scanner.Get (sym);
			Check_operator (x, op);
			
			if op = Scanner.sym_or then
				begin Generator.Op1 (op, x); end;
			
			term (y);
			if x.typ^.form = y.typ^.form then Generator.Op2 (op, x, y)
			else Scanner.Mark ('Incompatible types');
			end;
		end;
		
	procedure expression (var x : Generator.Item);
		var
			op : Integer;
			y : Item;
		begin
		SimpleExpression (x);
		if (sym >= Scanner.sym_equal) and (sym <= Scanner.sym_greater) then
			begin
			op := sym; Scanner.Get (sym);
			Check_operator (x, op);
			
			if x.typ = Generator.bool_type then Generator.Finish_cond (x);			
			SimpleExpression (y);
			
			if (x.typ = Generator.set_type) and (x.typ = y.typ)
			and ((op = Scanner.sym_greater_equal) or (op = Scanner.sym_less_equal)) then
				begin
				Generator.Inclusion_test (op, x, y);
				end
			else if x.typ^.form = y.typ^.form then
				Generator.Relation (op, x, y)
			else
				Scanner.Mark ('Incompatible types');
				
			x.typ := Generator.bool_type;
			end
		else if sym = Scanner.sym_in then
			begin
			Scanner.Get (sym);
			SimpleExpression (y);
			if (x.typ^.form = Generator.type_integer) and (y.typ^.form = Generator.type_set) then
				begin Generator.Membership_test (x, y); end
			else
				Scanner.Mark ('Incompatible types');
			x.typ := Generator.bool_type;
			end;
		end;
		
	procedure ProcedureDecl;
		var
			proc, obj : Generator.Object_;
			proc_id : AnsiString;
			local_block_size, para_block_size : Int64;
			x : Generator.Item;
			
		procedure OpenArray (var tp : Generator.Type_);
			var
				t : Generator.Type_;
			begin
			New (tp);
			tp^.form := Generator.type_array;
			tp^.len := 0;
			tp^.size := Word_size * 2;
			
			Scanner.Get (sym);
			if sym = Scanner.sym_of then Scanner.Get (sym)
			else Scanner.Mark ('Missing OF or not an open array');
				
			t := tp;
			while sym = Scanner.sym_array do
				begin
				New (t^.base); t := t^.base;
				t^.form := Generator.type_array;
				t^.len := 0;
				t^.size := 0;
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
			
		procedure FPSection;
			var
				obj, first : Generator.Object_;
				tp : Generator.Type_;
				para_size : Int64;
				cls : Integer;
				read_only : Boolean;
			begin
			if sym = Scanner.sym_var then
				begin Scanner.Get (sym); IdentList (Generator.class_par, first) end
			else
				begin IdentList (Generator.class_var, first) end;
				
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
			para_size := Word_size;
			
			if (tp^.form = Generator.type_array) and (tp^.len = 0) then
				para_size := tp^.size;

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
			
		procedure Fix_param_adr;
			var obj : Generator.Object_;
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
			Generator.Inc_level (1); Open_scope; proc.val := -1;
			if sym = Scanner.sym_lparen then
				begin
				Scanner.Get (sym);
				if sym = Scanner.sym_rparen then
					Scanner.Get (sym)
				else
					begin
					FPSection; while sym = Scanner.sym_semicolon do begin Scanner.Get (sym); FPSection; end;
					if sym = Scanner.sym_rparen then Scanner.Get (sym) else Scanner.Mark ('No closing )');
					end
				end;
			if para_block_size < 0 then Fix_param_adr;
			
			if sym = Scanner.sym_colon then
				begin
				Scanner.Get (sym);
				if sym = Scanner.sym_ident then
					begin
					proc^.class_ := Generator.class_func;
					find (obj); Scanner.Get (sym);
					if obj^.class_ = Generator.class_typ then
						begin proc^.typ := obj^.typ; end
					else
						begin Scanner.Mark ('Type not found'); proc^.typ := Generator.int_type; end;
					end
				else
					begin Scanner.Mark ('Function result type missing?'); end;
				end
			else
				begin
				proc^.typ := nil;
				end;
			
			local_block_size := 0; proc^.dsc := top_scope^.next;
			if sym = Scanner.sym_semicolon then Scanner.Get (sym) else Scanner.Mark ('No semicolon');
			Declarations (local_block_size, False);
			local_block_size := -local_block_size;
			
			while sym = Scanner.sym_procedure do
				begin
				ProcedureDecl;
				if sym = Scanner.sym_semicolon then Scanner.Get (sym) else Scanner.Mark ('No semicolon');
				end;
			
			proc^.val := Generator.line_num;
			Generator.Emit_label (proc^.name + '_');
			Generator.Enter (para_block_size, local_block_size);
			if sym = Scanner.sym_begin then
				begin
            Scanner.Get (sym);
            StatementSequence;
         	end;
			
			if proc^.class_ = Generator.class_func then
				begin
				if sym = Scanner.sym_return then
					begin
					Scanner.Get (sym); expression (x);
					if x.typ = proc^.typ then Generator.Set_Function_result (x)
					else Scanner.Mark ('Incompatible function result type');
					end
				else
					Scanner.Mark ('Function procedure without RETURN');
				end
			else if sym = Scanner.sym_return then
				begin
				Scanner.Mark ('Normal procedures can not have return value!');
				expression (x);
				Make_clean_const (x, int_type, 0);
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
	New (guard); guard^.class_ := Generator.class_var; guard^.typ := Generator.int_type; guard^.val := 0;
   guard^.is_param := False;
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
