MODULE Crypt;

IMPORT
	SYSTEM;
	
TYPE
	(* Note: With each specific Oberon implementation,
	   define INT32 and INT64 type here *)
	INT64 = INTEGER;

	MD5Hash* = RECORD
		a0, b0, c0, d0: INT32; msgLen: INT64;
	END;
	Chunk512* = ARRAY 16 OF INT32;
	
VAR
	MD5State: RECORD
		s, K: ARRAY 64 OF INT32
	END;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* MD5 *)

PROCEDURE NewMD5Hash* (VAR hash: MD5Hash);
BEGIN
	hash.a0 := 67452301H; hash.b0 := 0EFCDAB89H;
	hash.c0 := 98BADCFEH; hash.d0 := 10325476H;
	hash.msgLen := 0
END NewMD5Hash;

PROCEDURE MD5ComputeChunk* (
	VAR hash: MD5Hash; VAR chunk: Chunk512; lenInBit: INTEGER
);
BEGIN
	IF lenInBit = 512 THEN
		
	ELSIF lenInBit < 448 THEN
	ELSE ASSERT(FALSE)
	END
END MD5ComputeChunk;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Init;
	VAR i, k: INTEGER;
BEGIN k := 0;
	FOR i := 1 TO 4 DO
		MD5State.s[k] := 7;
		MD5State.s[k + 1] := 12;
		MD5State.s[k + 2] := 17;
		MD5State.s[k + 3] := 22;
		k := k + 4
	END;
	FOR i := 1 TO 4 DO
		MD5State.s[k] := 5;
		MD5State.s[k + 1] := 9;
		MD5State.s[k + 2] := 14;
		MD5State.s[k + 3] := 20;
		k := k + 4
	END;
	FOR i := 1 TO 4 DO
		MD5State.s[k] := 4;
		MD5State.s[k + 1] := 11;
		MD5State.s[k + 2] := 16;
		MD5State.s[k + 3] := 23;
		k := k + 4
	END;
	FOR i := 1 TO 4 DO
		MD5State.s[k] := 6;
		MD5State.s[k + 1] := 10;
		MD5State.s[k + 2] := 15;
		MD5State.s[k + 3] := 21;
		k := k + 4
	END;
	
	MD5State.K[0] := 0D76AA478H; MD5State.K[1] := 0E8C7B756H;
	MD5State.K[2] := 0242070DBH; MD5State.K[3] := 0C1BDCEEEH;
	MD5State.K[4] := 0F57C0FAFH; MD5State.K[5] := 04787C62AH;
	MD5State.K[6] := 0A8304613H; MD5State.K[7] := 0FD469501H;
	MD5State.K[8] := 0698098D8H; MD5State.K[9] := 08B44F7AFH;
	MD5State.K[10] := 0FFFF5BB1H; MD5State.K[11] := 0895CD7BEH;
	MD5State.K[12] := 06B901122H; MD5State.K[13] := 0FD987193H;
	MD5State.K[14] := 0A679438EH; MD5State.K[15] := 049B40821H;
	
	MD5State.K[16] := 0F61E2562H; MD5State.K[17] := 0C040B340H;
	MD5State.K[18] := 0265E5A51H; MD5State.K[19] := 0E9B6C7AAH;
	MD5State.K[20] := 0D62F105DH; MD5State.K[21] := 002441453H;
	MD5State.K[22] := 0D8A1E681H; MD5State.K[13] := 0E7D3FBC8H;
	MD5State.K[24] := 021E1CDE6H; MD5State.K[25] := 0C33707D6H;
	MD5State.K[26] := 0F4D50D87H; MD5State.K[27] := 0455A14EDH;
	MD5State.K[28] := 0A9E3E905H; MD5State.K[29] := 0FCEFA3F8H;
	MD5State.K[30] := 0676F02D9H; MD5State.K[31] := 08D2A4C8AH;
	
	MD5State.K[32] := 0FFFA3942H; MD5State.K[33] := 08771F681H;
	MD5State.K[34] := 06D9D6122H; MD5State.K[35] := 0FDE5380CH;
	MD5State.K[36] := 0A4BEEA44H; MD5State.K[37] := 04BDECFA9H;
	MD5State.K[38] := 0F6BB4B60H; MD5State.K[39] := 0BEBFBC70H;
	MD5State.K[40] := 0289B7EC6H; MD5State.K[41] := 0EAA127FAH;
	MD5State.K[42] := 0D4EF3085H; MD5State.K[43] := 004881D05H;
	MD5State.K[44] := 0D9D4D039H; MD5State.K[45] := 0E6DB99E5H;
	MD5State.K[46] := 01FA27CF8H; MD5State.K[47] := 0C4AC5665H;
	
	MD5State.K[48] := 0F4292244H; MD5State.K[49] := 0432AFF97H;
	MD5State.K[50] := 0AB9423A7H; MD5State.K[51] := 0FC93A039H;
	MD5State.K[52] := 0655B59C3H; MD5State.K[53] := 08F0CCC92H;
	MD5State.K[54] := 0FFEFF47DH; MD5State.K[55] := 085845DD1H;
	MD5State.K[56] := 06FA87E4FH; MD5State.K[57] := 0FE2CE6E0H;
	MD5State.K[58] := 0A3014314H; MD5State.K[59] := 04E0811A1H;
	MD5State.K[60] := 0F7537E82H; MD5State.K[61] := 0BD3AF235H;
	MD5State.K[62] := 02AD7D2BBH; MD5State.K[63] := 0EB86D391H
END Init;
	
BEGIN Init
END Crypt.