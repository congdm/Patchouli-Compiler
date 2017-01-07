MODULE Dwarf2;

IMPORT
	Rtl, S := Scanner, B := Base;
	
CONST
	DW_CHILDREN_YES = 1; DW_CHILDREN_NO = 0;

	DW_TAG_compile_unit = 11H;
	DW_TAG_subroutine_type = 15H;
	
	DW_AT_name = 3H;
	DW_AT_low_pc = 11H;
	DW_AT_high_pc = 12H;
	
	DW_FORM_addr = 1H;
	DW_FORM_string = 8H;
	
VAR
	out: Rtl.File;

PROCEDURE Write(x: INTEGER);
	VAR b: BYTE;
BEGIN ASSERT(x >= 0);
	REPEAT
		b := x MOD 128; x := ASR(x, 7);
		IF x # 0 THEN INC(b, 128) END; Rtl.Write1(out, b)
	UNTIL x = 0
END Write;
	
PROCEDURE WriteAbbrev*(f: Rtl.File);
BEGIN out := f;
	Write(1); (* abbrev code 1 *)
	Write(DW_TAG_compile_unit); Write(DW_CHILDREN_YES);
	Write(DW_AT_name); Write(DW_FORM_string);
	Write(DW_AT_low_pc); Write(DW_FORM_addr);
	Write(DW_AT_high_pc); Write(DW_FORM_addr);
	Write(0);
	
	Write(2); (* abbrev code 2 *)
	Write(DW_TAG_subroutine_type); Write(DW_CHILDREN_NO);
	Write(DW_AT_name); Write(DW_FORM_string);
	Write(DW_AT_low_pc); Write(DW_FORM_addr);
	Write(DW_AT_high_pc); Write(DW_FORM_addr);
	Write(0);
	
	Write(0)
END WriteAbbrev;

PROCEDURE WriteHeader*(f: Rtl.File; len: INTEGER);
BEGIN
	Rtl.Write4(f, len-4); Rtl.Write2(f, 2);
	Rtl.Write4(f, 0); Rtl.Write1(f, 8)
END WriteHeader;

PROCEDURE WriteCompileUnit*(f: Rtl.File; modid: B.IdStr; lo, hi: INTEGER);
BEGIN out := f;
	Write(1);
	Rtl.WriteAnsiStr(out, modid);
	Rtl.Write8(out, lo);
	Rtl.Write8(out, hi)
END WriteCompileUnit;

PROCEDURE WriteSubroutine*(f: Rtl.File; name: B.IdStr; lo, hi: INTEGER);
BEGIN out := f;
	Write(2);
	Rtl.WriteAnsiStr(out, name);
	Rtl.Write8(out, lo);
	Rtl.Write8(out, hi)
END WriteSubroutine;

PROCEDURE WriteNull*(f: Rtl.File);
BEGIN out := f; Write(0)
END WriteNull;

END Dwarf2.