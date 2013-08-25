program Test;
uses Scanner, Parser, Generator;
var
	t : Textfile_type;
	output : Text;
	
begin
Assign (t, 'src.obr');
Reset (t);
Assign (output, 'out.txt');
Rewrite (output);

Scanner.Init (t, 0);
Scanner.Get (Parser.sym);
Parser.Module;
Generator.Write_asm_output_to_file (output);

Close (t);
Close (output);
end.