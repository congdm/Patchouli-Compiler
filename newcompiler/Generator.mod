MODULE Generator;

IMPORT
	SYSTEM, Sys, Base, Scanner;
	
TYPE
	ProcCall* = RECORD
		fpar*: Base.Object;
		nofact*, nofpar*: INTEGER
	END;

VAR
	static_buf: ARRAY 1048576 OF BYTE;
	static_size, static_base: INTEGER;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Align* (VAR offset: INTEGER; alignment: INTEGER);
END Align;

PROCEDURE FitInReg* (size: INTEGER) : BOOLEAN;
	RETURN (size = 1) & (size = 2) & (size = 3) & (size = 8)
END FitInReg;

PROCEDURE Fix_param_adr* (VAR adr: INTEGER);
BEGIN INC (adr, 16)
END Fix_param_adr;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Clean_item* (VAR x: Base.Item);
END Clean_item;

PROCEDURE Make_const* (VAR x: Base.Item; tp: Base.Type; val: INTEGER);
BEGIN
	x.mode := Base.cConst; x.type := tp; x.a := val
END Make_const;

PROCEDURE Make_item* (VAR x: Base.Item; obj: Base.Object);
BEGIN
	x.readonly := obj.readonly; x.param := obj.param;
	x.mode := obj.class; x.lev := obj.lev;
	x.type := obj.type; x.obj := obj;
	x.a := obj.val; x.b := obj.val2; x.c := 0
END Make_item;

PROCEDURE Make_string* (VAR x: Base.Item);
	VAR i, slen: INTEGER; str: Base.String;
BEGIN
	x.b := Scanner.slen; DEC (static_size, slen * Base.CharSize);
	x.a := static_size; i := 0; str := Scanner.str;
	WHILE i < x.b DO
		SYSTEM.PUT (SYSTEM.ADR(static_size) + i * Base.CharSize, str[i]);
		INC (i)
	END;
	x.mode := Base.cVar; x.type := Base.stringType; x.lev := -1
END Make_string;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE load* (VAR x: Base.Item);
END load;

PROCEDURE Str_to_char* (VAR x: Base.Item);
END Str_to_char;

PROCEDURE Store* (VAR x, y: Base.Item);
END Store;

PROCEDURE Store_struct* (VAR x, y: Base.Item);
END Store_struct;

PROCEDURE Store_string* (VAR x, y: Base.Item);
END Store_string;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Not* (VAR x: Base.Item);
END Not;

PROCEDURE Negate* (VAR x: Base.Item);
END Negate;

PROCEDURE Int_op1* (op: INTEGER; VAR x, y: Base.Item);
END Int_op1;

PROCEDURE Int_op2* (op: INTEGER; VAR x, y: Base.Item);
END Int_op2;

PROCEDURE Real_op1* (op: INTEGER; VAR x, y: Base.Item);
END Real_op1;

PROCEDURE Real_op2* (op: INTEGER; VAR x, y: Base.Item);
END Real_op2;

PROCEDURE Set_op1* (op: INTEGER; VAR x, y: Base.Item);
END Set_op1;

PROCEDURE Set_op2* (op: INTEGER; VAR x, y: Base.Item);
END Set_op2;

PROCEDURE Or1* (VAR x: Base.Item);
END Or1;

PROCEDURE Or2* (VAR x, y: Base.Item);
END Or2;

PROCEDURE And1* (VAR x: Base.Item);
END And1;

PROCEDURE And2* (VAR x, y: Base.Item);
END And2;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Int_relation* (rel: INTEGER; VAR x, y: Base.Item);
END Int_relation;

PROCEDURE Real_relation* (rel: INTEGER; VAR x, y: Base.Item);
END Real_relation;

PROCEDURE Set_relation* (rel: INTEGER; VAR x, y: Base.Item);
END Set_relation;

PROCEDURE String_relation* (rel: INTEGER; VAR x, y: Base.Item);
END String_relation;

PROCEDURE Member_test* (VAR x, y: Base.Item);
END Member_test;

PROCEDURE Type_test* (VAR x: Base.Item; tp: Base.Type; guard: BOOLEAN);
END Type_test;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Set1* (VAR x: Base.Item);
END Set1;

PROCEDURE Set2* (VAR x, y: Base.Item);
END Set2;

PROCEDURE Set3* (VAR x, y, z: Base.Item);
END Set3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* selector *)

PROCEDURE Deref* (VAR x: Base.Item);
END Deref;

PROCEDURE Field* (VAR x: Base.Item; field: Base.Object);
END Field;

PROCEDURE Index* (VAR x, y: Base.Item);
END Index;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Procedure call *)

PROCEDURE Prepare_to_call* (VAR x: Base.Item; VAR call: ProcCall);
END Prepare_to_call;

PROCEDURE Call* (VAR call: ProcCall);
END Call;

PROCEDURE Return_value* (VAR x: Base.Item);
BEGIN
END Return_value;

PROCEDURE Value_param* (VAR x: Base.Item; VAR call: ProcCall);
END Value_param;

PROCEDURE Ref_param* (VAR x: Base.Item; VAR call: ProcCall);
END Ref_param;

PROCEDURE Record_param* (VAR x: Base.Item; VAR call: ProcCall);
END Record_param;

PROCEDURE Array_param* (VAR x: Base.Item; VAR call: ProcCall);
END Array_param;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Enter* (proc: Base.Object; locblksize: INTEGER);
END Enter;

PROCEDURE Return*;
END Return;

PROCEDURE Module_init*;
END Module_init;

PROCEDURE Module_exit*;
END Module_exit;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init* (modid: Base.IdentStr);
BEGIN
	static_size := 0; static_base := -24
END Init;

PROCEDURE Finish*;
END Finish;

END Generator.