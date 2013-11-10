program Aya;
uses Scanner, Win64;
var
	t : Scanner.Textfile_type;
	srcfilename : AnsiString;
	
procedure Remove_file_ext;
	var
		i : Integer;
	begin
	for i := Length (srcfilename) downto 1 do
		begin if srcfilename [i] = '.' then break; end;
	if srcfilename [i] <> '.' then i := Length (srcfilename)
	else Dec (i);
	SetLength (srcfilename, i);
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

Scanner.Init (t, 0);
Win64.Compile;

Close (t);
end.
