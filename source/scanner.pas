unit Scanner;

interface

type
	Textfile_type = File of Char;

const
	sym_null = 0;
	sym_times = 1; sym_slash = 2; sym_div = 3; sym_mod = 4; sym_and = 5; sym_plus = 6; sym_minus = 7; sym_or = 8;
	sym_equal = 9; sym_not_equal = 10; sym_less = 11; sym_greater_equal = 12; sym_less_equal = 13; sym_greater = 14; 
	sym_period = 18; sym_comma = 19; sym_colon = 20; sym_upto = 21; sym_rparen = 22; sym_rbrak = 23; sym_rbrace = 24;
	sym_of = 25; sym_then = 26; sym_do = 27;
	sym_lparen = 29; sym_lbrak = 30; sym_lbrace = 31; sym_not = 32; sym_becomes = 33; sym_number = 34; sym_ident = 37;
	sym_semicolon = 38; sym_end = 40; sym_else = 41; sym_elsif = 42; sym_until = 43;
	sym_if = 44; sym_while = 46; sym_repeat = 47;
	sym_array = 54; sym_record = 55;
	sym_const = 57; sym_type = 58; sym_var = 59; sym_procedure = 60; sym_begin = 61; sym_return = 63; 
	sym_module = 64;
	sym_eof = 65;
	
var
	value : Int64;
	id : String;
	error : Boolean;
	
procedure Init (var t : Textfile_type; pos : Integer);
procedure Get (var sym : Integer);
procedure Mark (msg : String);

implementation

uses
	Sysutils;

var
	textfile : Textfile_type;
	ch : Char;
	eof_flag : Boolean;
	line_num : Integer;

procedure Init (var t : Textfile_type; pos : Integer);
	begin
	textfile := t;
	Seek (textfile, pos);
	ch := #0; line_num := 1;
	eof_flag := False;
	end;
	
procedure Read_char;
	begin
	try
		Read (textfile, ch);
		if ch = #13 then Inc (line_num);
	except
		ch := #0;
		eof_flag := True;
		end;
	end;
	
procedure Get_word (var sym : Integer);
	var
		s : String;
		
	begin
	s := '';
	while ((ch >= 'A') and (ch <= 'Z'))
			or ((ch >= 'a') and (ch <= 'z'))
			or ((ch >= '0') and (ch <= '9')) 
			or (ch = '_') do
		begin
		s := s + ch;
		Read_char;
		end;
	
	if s = 'DIV' then
		sym := sym_div
	else if s = 'MOD' then
		sym := sym_mod
	else if s = 'AND' then
		sym := sym_and
	else if s = 'OR' then
		sym := sym_or
	else if s = 'OF' then
		sym := sym_of
	else if s = 'THEN' then
		sym := sym_then
	else if s = 'DO' then
		sym := sym_do
	else if s = 'UNTIL' then
		sym := sym_until
	else if s = 'NOT' then
		sym := sym_not
	else if s = 'END' then
		sym := sym_end
	else if s = 'ELSE' then
		sym := sym_else
	else if s = 'ELSIF' then
		sym := sym_elsif
	else if s = 'IF' then
		sym := sym_if
	else if s = 'WHILE' then
		sym := sym_while
	else if s = 'REPEAT' then
		sym := sym_repeat
	else if s = 'ARRAY' then
		sym := sym_array
	else if s = 'RECORD' then
		sym := sym_record
	else if s = 'CONST' then
		sym := sym_const
	else if s = 'TYPE' then
		sym := sym_type
	else if s = 'VAR' then
		sym := sym_var
	else if s = 'PROCEDURE' then
		sym := sym_procedure
	else if s = 'BEGIN' then
		sym := sym_begin
	else if s = 'MODULE' then
		sym := sym_module
	else if s = 'RETURN' then
		sym := sym_return
	else
		begin
		sym := sym_ident;
		id := s;
		end;
		
	end;
	
procedure Get_number (var sym : Integer);
	var
		s : String;
		
	begin
	s := '';
	while (ch >= '0') and (ch <= '9') do
		begin
		s := s + ch;
		Read_char;
		end;
		
	value := StrToInt64 (s);
	sym := sym_number;
	end;
	
procedure Get (var sym : Integer);
	var
		old_ch : Char;		
	begin
	// Skip blank characters
	while (not EOF (textfile)) and (ch <= ' ') do Read_char;
	// Skip comment
	while ch = '(' do
		begin
		Read_char;
		if ch <> '*' then
			begin
			Seek (textfile, Filepos (textfile) - 1);
			ch := '(';
			break;
			end
		else
			begin
			Read_char;
			repeat
				old_ch := ch;
				Read_char;
			until (old_ch = '*') and (ch = ')');
			Read_char;
			end;
		end;
	// Skip blank characters again
	while (not EOF (textfile)) and (ch <= ' ') do Read_char;
	
	if eof_flag then
		begin sym := sym_eof; exit; end;
	
	// Detect symbol
	if ((ch >= 'A') and (ch <= 'Z')) or ((ch >= 'a') and (ch <= 'z')) or (ch = '_') then
		Get_word (sym)
	else if (ch >= '0') and (ch <= '9') then
		Get_number (sym)
	else
	case ch of
		'*' : begin sym := sym_times; Read_char; end;
		'/' : begin sym := sym_slash; Read_char; end;
		'+' : begin sym := sym_plus; Read_char; end;
		'-' : begin sym := sym_minus; Read_char; end;
		'=' : begin sym := sym_equal; Read_char; end;
		'#' : begin sym := sym_not_equal; Read_char; end;
		'&' : begin sym := sym_and; Read_char; end;
		'~' : begin sym := sym_not; Read_char; end;
		'<' : begin 
				Read_char; 
				if ch = '=' then begin sym := sym_less_equal; Read_char; end
				else sym := sym_less;
				end;
		'>' : begin 
				Read_char; 
				if ch = '=' then begin sym := sym_greater_equal; Read_char; end
				else sym := sym_greater;
				end;
		'.' : begin 
				Read_char; 
				if ch = '.' then begin sym := sym_upto; Read_char; end
				else sym := sym_period;
				end;
		',' : begin sym := sym_comma; Read_char; end;
		':' : begin 
				Read_char; 
				if ch = '=' then begin sym := sym_becomes; Read_char; end
				else sym := sym_colon;
				end;
		')' : begin sym := sym_rparen; Read_char; end;
		']' : begin sym := sym_rbrak; Read_char; end;
		'}' : begin sym := sym_rbrace; Read_char; end;
		'(' : begin sym := sym_lparen; Read_char; end;
		'[' : begin sym := sym_lbrak; Read_char; end;
		'{' : begin sym := sym_lbrace; Read_char; end;
		';' : begin sym := sym_semicolon; Read_char; end;
		else  begin sym := sym_null; Read_char; end;
		end;
	end;
	
	procedure Mark (msg : String);
		begin
		Writeln (IntToStr (line_num) + ': ' + msg);
		end;

end.