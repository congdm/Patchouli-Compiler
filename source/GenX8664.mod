MODULE GenX8664;

IMPORT
	Scn, Base;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Pass 1 *)

PROCEDURE CheckTypeSize(VAR sz: INTEGER);
BEGIN
	IF sz > MaxSize THEN S.Mark('type too big'); sz := 8 END
END CheckTypeSize;

PROCEDURE SetTypeSize*(tp: B.Type);
	VAR size, align, falg: INTEGER;
		ident: B.Ident; ftype, btype: B.Type;
BEGIN
	IF tp.size = 0 THEN btype := tp.base;
		IF tp.form = B.tPtr THEN tp.size := 8; tp.align := 8
		ELSIF tp.form = B.tProc THEN
			tp.size := 8; tp.align := 8; ident := tp.fields; size := 0;
			WHILE ident # NIL DO
				ftype := ident.obj.type; ident.obj(B.Var).adr := size + 16;
				IF B.IsOpenArray(ftype) & ~ftype.notag
				OR (ftype.form = B.tRec) & (ident.obj(B.Par).varpar)
				THEN INC(size, 16) ELSE INC(size, 8)
				END;
				ident := ident.next
			END;
			tp.parblksize := size
		ELSIF (tp.form = B.tArray) & (tp.len >= 0) THEN
			SetTypeSize(btype); tp.align := btype.align;
			tp.size := btype.size * tp.len; CheckTypeSize(tp.size)
		ELSIF tp.form = B.tRec THEN
			IF btype # NIL THEN SetTypeSize(btype);
				align := btype.align; size := btype.size0
			ELSE size := 0; align := 0
			END;
			ident := tp.fields;
			WHILE ident # NIL DO
				ftype := ident.obj.type; SetTypeSize(ftype);
				IF ftype.align > align THEN align := ftype.align END;
				IF ~tp.union THEN
					Align(size, ftype.align); ident.obj(B.Field).off := size;
					INC(size, ftype.size); CheckTypeSize(size);
				ELSE
					ident.obj(B.Field).off := 0;
					IF ftype.size > size THEN size := ftype.size END
				END;
				ident := ident.next
			END;
			tp.size0 := size; Align(size, align);
			CheckTypeSize(size); tp.size := size; tp.align := align
		ELSE ASSERT(FALSE)
		END
	END
END SetTypeSize;

PROCEDURE SetGlobalVarSize*(x: B.Object);
BEGIN
	Align(varSize, x.type.align); INC(varSize, x.type.size);
	x(B.Var).adr := -varSize;
	IF varSize > MaxSize THEN varSize := 8;
		S.Mark('global var size limit reached')
	END
END SetGlobalVarSize;

PROCEDURE SetProcVarSize*(proc: B.Proc; x: B.Object);
	VAR size: INTEGER;
BEGIN size := proc.locblksize;
	Align(size, x.type.align); INC(size, x.type.size);
	x(B.Var).adr := -size; proc.locblksize := size;
	IF size > MaxLocBlkSize THEN proc.locblksize := 8;
		S.Mark('local var size limit reached')
	END
END SetProcVarSize;

PROCEDURE AllocImport*(x: B.Object; module: B.Module);
	VAR p: B.Ident; adr: INTEGER;
BEGIN
	NEW(p); p.obj := x; p.next := module.impList;
	module.impList := p; adr := staticSize; INC(staticSize, 8);
	IF x IS B.Var THEN x(B.Var).adr := adr
	ELSIF x IS B.Proc THEN x(B.Proc).adr := adr
	ELSIF x.class = B.cType THEN
		ASSERT(x.type.form = B.tRec); x.type.adr := adr
	END
END AllocImport;

PROCEDURE AllocImportModules;
	VAR size: INTEGER; imod: B.Module;
		str: ARRAY 512 OF CHAR;
BEGIN
	Align(staticSize, 16); imod := B.modList;
	WHILE imod # NIL DO
		IF imod.import OR (imod.impList # NIL) THEN
			B.ModIdToStr(imod.id, str); size := (B.StrLen(str)+5)*2;
			imod.adr := staticSize; INC(staticSize, size)
		END;
		imod := imod.next
	END
END AllocImportModules;

PROCEDURE AllocStaticData;
	VAR p: B.StrList; q: B.TypeList;
		x: B.Object; y: B.Str; strSize, tdSize, align: INTEGER;
BEGIN
	Align(staticSize, 16); p := B.strList;
	WHILE p # NIL DO
		y := p.obj; strSize := 2*y.len;
		y.adr := staticSize; INC(staticSize, strSize); p := p.next
	END;
	Align(staticSize, 16); q := B.recList;
	WHILE q # NIL DO
		tdSize := (24 + 8*(B.MaxExt + q.type.nTraced)) DIV 16 * 16;
		q.type.adr := staticSize; INC(staticSize, tdSize); q := q.next
	END;
	AllocImportModules;
	IF staticSize + varSize > MaxSize THEN
		S.Mark('static variables size too big'); ASSERT(FALSE)
	END
END AllocStaticData;

PROCEDURE ScanNode(node: B.Node);
	VAR left, right: B.Object;
		fpar: B.Ident; e: INTEGER; 
	
	PROCEDURE ScanPar(node: B.Node; fpar: B.Ident; n: INTEGER);
		VAR vpar, open: BOOLEAN;
			i: INTEGER; ftype: B.Type;
	BEGIN (* ScanPar *)
		ftype := fpar.obj.type;
		IF node.left IS B.Node THEN
			ScanNode(node.left(B.Node));
			node.regUsed := node.left(Node).regUsed;
			node.xRegUsed := node.left(Node).xRegUsed
		END;
		open := B.IsOpenArray(ftype) & ~ftype.notag;
		vpar := fpar.obj(B.Par).varpar; 
		IF open OR vpar & (ftype.form = B.tRec) THEN i := 2 ELSE i := 1 END;
		WHILE i > 0 DO 
			IF (ftype.form # B.tReal) OR vpar THEN
				IF n = 0 THEN INCL(node.regUsed, reg_C)
				ELSIF n = 1 THEN INCL(node.regUsed, reg_D)
				ELSIF n = 2 THEN INCL(node.regUsed, reg_R8)
				ELSIF n = 3 THEN INCL(node.regUsed, reg_R9)
				END
			ELSE INCL(node.xRegUsed, n)
			END;
			DEC(i); INC(n)
		END;
		IF node.right # NIL THEN
			ScanPar(node.right(B.Node), fpar.next, n);
			node.regUsed := node.regUsed + node.right(Node).regUsed;
			node.xRegUsed := node.xRegUsed + node.right(Node).xRegUsed
		END
	END ScanPar;
	
BEGIN (* ScanNode *)
	left := node.left; right := node.right;
	IF node.op # S.call THEN
		IF (left # NIL) & (left IS B.Node) THEN
			ScanNode(left(B.Node));
			IF node.op # S.semicolon THEN
				node.regUsed := left(Node).regUsed;
				node.xRegUsed := left(Node).xRegUsed
			END
		END;
		IF (right # NIL) & (right IS B.Node) THEN
			ScanNode(right(B.Node));
			IF node.op # S.semicolon THEN
				node.regUsed := right(Node).regUsed;
				node.xRegUsed := right(Node).xRegUsed
			END
		END;
		IF node.op = S.times THEN
			IF (node.type = B.intType) & (right IS B.Const) THEN
				e := log2(right(B.Const).val);
				IF e >= 0 THEN
					right := B.NewConst(B.intType, e);
					node.op := S.sfLSL; node.right := right
				END
			END
		ELSIF (node.op = S.div) OR (node.op = S.mod) THEN e := -1;
			IF right IS B.Const THEN e := log2(right(B.Const).val) END;
			IF e = -1 THEN node.regUsed := node.regUsed + {reg_A, reg_D} END
		ELSIF (node.op >= S.sfLSL) & (node.op <= S.sfROR) THEN
			IF ~(right IS B.Const) THEN INCL(node.regUsed, reg_C) END
		ELSIF (node.op >= S.eql) & (node.op <= S.geq) THEN
			IF B.IsStr(left.type) THEN
				node.regUsed := node.regUsed + {reg_SI, reg_DI}
			END
		ELSIF node.op = S.upto THEN INCL(node.regUsed, reg_C)
		ELSIF node.op = S.becomes THEN
			IF left.type.form IN {B.tArray, B.tRec} THEN
				node.regUsed := node.regUsed + {reg_SI, reg_DI}
			END
		ELSIF node.op = S.sfCAS THEN INCL(node.regUsed, reg_A)
		END
	ELSIF node.op = S.call THEN
		IF left IS B.Node THEN
			ScanNode(left(B.Node));
			node.regUsed := left(Node).regUsed;
			node.xRegUsed := left(Node).xRegUsed
		END;
		node.regUsed := node.regUsed + {0 .. 2, 8 .. 11};
		node.xRegUsed := node.xRegUsed + {0 .. 5};
		IF right # NIL THEN
			fpar := left.type.fields; ScanPar(right(B.Node), fpar, 0);
			node.regUsed := node.regUsed + right(Node).regUsed;
			node.xRegUsed := node.xRegUsed + right(Node).xRegUsed
		END
	END
END ScanNode;

PROCEDURE ScanProc(proc: B.Proc);
BEGIN
	proc.adr := -1; proc.fix := -1;
	IF proc.nTraced > 0 THEN
		INC(proc.locblksize, 24); Align(staticSize, 16);
		proc.descAdr := staticSize; INC(staticSize, (proc.nTraced+1)*8)
	END;
	IF curProc # NIL THEN NEW(curProc.next); curProc := curProc.next
	ELSIF curProc = NIL THEN NEW(procList); curProc := procList
	END;
	curProc.obj := proc;
	IF proc.statseq # NIL THEN ScanNode(proc.statseq) END;
	IF proc.return # NIL THEN
		IF proc.return IS B.Node THEN ScanNode(proc.return(B.Node)) END
	END
END ScanProc;

PROCEDURE ScanDeclaration(decl: B.Ident; lev: INTEGER);
	VAR ident: B.Ident; obj: B.Object; ptrTableSize: INTEGER;
BEGIN ident := decl;
	IF lev = 0 THEN ptrTableSize := 8 END;
	WHILE ident # NIL DO obj := ident.obj;
		IF obj IS B.Proc THEN
			ScanDeclaration(obj(B.Proc).decl, lev+1); ScanProc(obj(B.Proc))
		ELSIF (lev = 0) & (obj IS B.Var) & ~(obj IS B.Str) THEN
			IF obj.type.nTraced > 0 THEN
				INC(ptrTableSize, obj.type.nTraced*8)
			END
		END;
		ident := ident.next
	END;
	IF lev = 0 THEN
		Align(staticSize, 16); modPtrTable := staticSize;
		INC(staticSize, ptrTableSize)
	END
END ScanDeclaration;

PROCEDURE NewProc(VAR proc: Proc; statseq: Node);
BEGIN
	proc := B.NewProc(); proc.statseq := statseq;
	proc.locblksize := 0; proc.adr := -1; proc.fix := -1;
	IF curProc # NIL THEN NEW(curProc.next); curProc := curProc.next
	ELSIF curProc = NIL THEN NEW(procList); curProc := procList
	END;
	curProc.obj := proc
END NewProc;

PROCEDURE Pass1(VAR modinit: B.Node);
	VAR fixAmount: INTEGER; obj: B.Proc;
		str, str2: ARRAY 512 OF CHAR;
BEGIN
	B.ModIdToStr(B.modid, str); modidStr := B.NewStr2(str);
	errFmtStr := B.NewStr2('Error code: %d; Source pos: %d');
	err2FmtStr := B.NewStr2('Module key of %s is mismatched');
	err3FmtStr := B.NewStr2('Unknown exception; Pc: %x');
	err4FmtStr := B.NewStr2('Cannot load module %s (not exist?)');
	rtlName := B.NewStr2(B.RtlName); user32name := B.NewStr2('USER32.DLL');
	
	str2 := 'Error in module '; B.Append(str, str2);
	err5FmtStr := B.NewStr2(str2);
	
	AllocStaticData; ScanDeclaration(B.universe.first, 0);
	baseOffset := (-staticSize) DIV 4096 * 4096;
	
	IF modinit # NIL THEN
		ScanNode(modinit(B.Node)); NewProc(modInitProc, modinit)
	END;
	NewProc(dllAttachProc, NIL); NewProc(dllDetachProc, NIL);
	NewProc(dllInitProc, NIL); NewProc(trapProc, NIL); NewProc(trapProc2, NIL)
END Pass1;

END GenX8664.