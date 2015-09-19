DEFINITION Msftedit;

IMPORT
	User32;

TYPE
	CHARFORMATW* = RECORD
		cbSize*: CARD32;
		dwMask*, dwEffects*: CARD32;
		yHeight*, yOffset*: INT32;
		crTextColor*: CARD32;
		bCharSet*, bPitchAndFamily*: BYTE;
		szFaceName*: ARRAY User32.LF_FACESIZE OF CHAR
	END;

END Msftedit.