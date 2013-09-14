program Aya;
uses Scanner, Parser, Generator;
var
	t : Textfile_type;
	output : Text;
	srcfilename : AnsiString;
	
procedure Change_file_ext;
	var
		i : Integer;
	begin
	for i := Length (srcfilename) downto 1 do
		begin if srcfilename [i] = '.' then break; end;
	if srcfilename [i] <> '.' then i := Length (srcfilename);
	SetLength (srcfilename, i + 3);
	srcfilename [i + 1] := 'a';
	srcfilename [i + 2] := 's';
	srcfilename [i + 3] := 'm';
	end;
	
begin
if ParamCount = 0 then
	begin Writeln ('Usage: Aya <source>'); Halt; end
else if ParamCount = 1 then
	srcfilename := ParamStr (1)
else
	begin Writeln ('Too many parameters!'); Halt; end;

Assign (t, srcfilename);
Reset (t);
Change_file_ext;
Assign (output, srcfilename);
Rewrite (output);

Scanner.Init (t, 0);
Scanner.Get (Parser.sym);
Parser.Module;
Generator.Write_asm_output_to_file (output);

Close (t);
Close (output);
end.
