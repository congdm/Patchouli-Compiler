MODULE Generator;

IMPORT
	Base, Scanner;
	
PROCEDURE Clean_item* (VAR x: Base.Item);
END Clean_item;

PROCEDURE Make_const* (VAR x: Base.Item; tp: Base.Type; val: INTEGER);
END Make_const;

PROCEDURE Make_item* (VAR x: Base.Item; obj: Base.Object);
END Make_item;

PROCEDURE Make_string* (VAR x: Base.Item; slen: INTEGER);
END Make_string;

PROCEDURE Align* (VAR offset: INTEGER; alignment: INTEGER);
END Align;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE load* (VAR x: Base.Item);
END load;

PROCEDURE Str_to_char* (VAR x: Base.Item);
END Str_to_char;

PROCEDURE Store* (VAR x, y: Base.Item);
END Store;

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
END Init;

PROCEDURE Finish*;
END Finish;

END Generator.