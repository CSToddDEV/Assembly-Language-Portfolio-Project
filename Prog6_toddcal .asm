TITLE Designing Low-level I/O Procedures! (Prog6_toddcal.asm)

; Author: Calvin Todd
; Last Modified: 11.24.20
; OSU email address: toddcal@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 11.24.20
; Description: Where we use Macros put 10 numbers in to an array and calculate their sum and average without some basic I/O functionality!

INCLUDE Irvine32.inc

ARRAYSIZE			=			10
DIVISIONCONSTANT	=			10
BUFFERSIZE			=			24
BUFFERSTRINGSZ		=			20

.data
programTitle		BYTE		"Programming Assignment 6: Desgining Low Level I/O Procedures!",13,10,0
authorName			BYTE		"By: Calvin Todd",13,10,0
extraCredit			BYTE		"**EC: Each input line is numbered!",13,10,0
instructions		BYTE		"Please provide 10 signed decimal integers!",13,10,13,10,"Each number needs to be small enough to fit inside a 32 bit register.  After all the numbers are inputted, the program will display the list of integers, their sum, and their average!",13,10,13,10,0
strInstructions		BYTE		"Please enter a signed number: ",0
errorMessage		BYTE		"ERROR: Your integer was either too large, or invalid.  Please try again!",13,10,0
inputtedNumMessage	BYTE		"The numbers you inputted are: ",13,10,0
totalMessage		BYTE		"The total sum of all your inputted numbers is: ",13,10,0
avgMessage			BYTE		"The rounded average of your inputted numbers is: ",13,10,0
farewellMessage		BYTE		"Thank you for everything!  Cheers!",0
bufferString		BYTE		BUFFERSIZE DUP(?)
decArray			SDWORD		ARRAYSIZE  DUP(?)
conversionArray		BYTE		BUFFERSIZE DUP(?)
revConversionArray	BYTE		BUFFERSIZE DUP(?)
usrString			BYTE		BUFFERSIZE DUP(?)
total				SDWORD		?
counter				DWORD		?
negNumber			SDWORD		?
average				SDWORD		?
negConverter		DWORD		?
numHolder			DWORD		?
nullTerminator		BYTE		0
periodSpace			BYTE		". ",0
commaSpace			BYTE		", ",0
lowerLimitHex		BYTE		"2147483648",0

.code
main PROC

	PUSH		OFFSET	extraCredit
	PUSH		OFFSET	programTitle
	PUSH		OFFSET	authorName
	PUSH		OFFSET	instructions
	CALL		introduction

	; Setup Loop Counter and Line counter
	MOV			ECX, ARRAYSIZE
	MOV			counter, 1
	MOV			EDI, OFFSET decArray
	MOV			EAX, OFFSET counter

_inputLoop:
	; Input numbers in to decArray
	PUSH		OFFSET numHolder
	PUSH		OFFSET negConverter
	PUSH		OFFSET lowerLimitHex
	PUSH		OFFSET usrString
	PUSH		BUFFERSIZE
    PUSH		OFFSET revConversionArray
	PUSH		OFFSET conversionArray
	PUSH		DIVISIONCONSTANT
	PUSH		EAX
	CALL		writeVal
	
	MOV			EDX, OFFSET periodSpace
	CALL		WriteString

	PUSH		OFFSET negConverter
	PUSH		OFFSET negNumber
	PUSH		OFFSET errorMessage
	PUSH		EDI
	PUSH		BUFFERSIZE
	PUSH		OFFSET bufferString
	PUSH		OFFSET strInstructions
	CALL		readVal
	ADD			EDI, 4
	ADD			counter, 1
	LOOP		_inputLoop

	; Display inputted numbers
	MOV			EDX, OFFSET inputtedNumMessage
	CALL		WriteString
	MOV			EDX, OFFSET commaSpace

	MOV			EAX, OFFSET decArray
	MOV			ECX,  ARRAYSIZE
	JMP			_skipFirstComma
_displayNumLoop:
	CALL		WriteString
_skipFirstComma:
	PUSH		OFFSET numHolder
	PUSH		OFFSET negConverter
	PUSH		OFFSET lowerLimitHex
	PUSH		OFFSET usrString
	PUSH		BUFFERSIZE
    PUSH		OFFSET revConversionArray
	PUSH		OFFSET conversionArray
	PUSH		DIVISIONCONSTANT
	PUSH		EAX
	CALL		writeVal
	ADD			EAX, 4
	LOOP		_displayNumLoop

	CALL		Crlf
	CALL		Crlf

	; Calculate and display total
	MOV			ECX, ARRAYSIZE
	MOV			EBX, OFFSET decArray
	MOV			EAX, 0

_addUpArray:
	ADD			EAX, [EBX]
	ADD			EBX, 4
	LOOP		_addUpArray

	MOV			total, EAX
	MOV			EAX, OFFSET total

	MOV			EDX, OFFSET totalMessage
	CALL		WriteString

	PUSH		OFFSET numHolder
	PUSH		OFFSET negConverter
	PUSH		OFFSET lowerLimitHex
	PUSH		OFFSET usrString
	PUSH		BUFFERSIZE
    PUSH		OFFSET revConversionArray
	PUSH		OFFSET conversionArray
	PUSH		DIVISIONCONSTANT
	PUSH		EAX
	CALL		writeVal

	CALL		Crlf
	CALL		Crlf

	; Calculate and display Average
	MOV			EAX, total
	MOV			EBX, ARRAYSIZE
	CDQ
	IDIV		EBX
	MOV			average, EAX
	MOV			EAX, OFFSET average

	MOV			EDX, OFFSET avgMessage
	CALL		WriteString

	PUSH		OFFSET numHolder
	PUSH		OFFSET negConverter
	PUSH		OFFSET lowerLimitHex
	PUSH		OFFSET usrString
	PUSH		BUFFERSIZE
    PUSH		OFFSET revConversionArray
	PUSH		OFFSET conversionArray
	PUSH		DIVISIONCONSTANT
	PUSH		EAX
	CALL		writeVal

	; Farewell and thank you!
	CALL		Crlf
	CALL		Crlf
	MOV			EDX, OFFSET farewellMessage
	CALL		WriteString
	CALL		Crlf
	CALL		Crlf

	Invoke ExitProcess,0	; exit to operating system
main ENDP

; ---------------------------------------------------------------------------------
; Name: mGetString
;
; prompts and recieves a string form the user
;
; Preconditions: 
;		None
;
; Receives:
;		promptMessage		=		OFFSET strInstructions
;		strLocation			=		OFFSET bufferString
;		strBuffer			=		BUFFERSIZE
;
; returns: 
;		EDX		=		Location of string
;		ECX		=		Number of characters entered
;	
; ---------------------------------------------------------------------------------
mGetString MACRO promptMessage, strLocation, strBuffer 
	MOV			EDX, promptMessage
	CALL		WriteString
	MOV			EDX, strLocation
	MOV			ECX, strBuffer
	CALL		ReadString
	CALL		Crlf

ENDM

; ---------------------------------------------------------------------------------
; Name: mTwosCompliment
;
; Produces the Twos Compliment of a negative hex SDWORD
;
; Preconditions: 
;		None
;
; Receives:
;		negNumber		=		Number to convert
;
; returns: 
;		EBX				=		Twos OCmpliment
;	
; ---------------------------------------------------------------------------------
mTwosCompliment MACRO negNumber
	LOCAL		convertedNumber
	LOCAL		SUBSTRACTIONCONSTANT
	LOCAL		twosCompliment
	LOCAL		placeHolder
	SUBTRACTIONCONSTANT =	255
	.data
	convertedNumber		SDWORD		?
	twosCompliment		SDWORD		?
	placeHolder			BYTE		?
	.code
	PUSHAD
	MOV			EBX, SDWORD PTR negNumber
	MOV			convertedNumber, EBX
	MOV			ESI,	OFFSET convertedNumber
	MOV			EDI,	OFFSET twosCompliment
	MOV			ECX, 4

_convertLoop:
	; Perform Twos compliment
	CLD
	LODSB
	MOV			placeHolder, AL
	MOV			AL, SUBTRACTIONCONSTANT
	SUB			AL, placeHolder
	STOSB
	LOOP		_convertLoop
	ADD			twosCompliment, 1

	POPAD
	MOV			EBX, twosCompliment

ENDM


; ---------------------------------------------------------------------------------
; Name: mDisplayString
;
; takes an array of ASCII characters, ignores the leading 0's and writes the rest
; of the screen to the console
;
; Preconditions: 
;		Must have an array of ASCII characters in EAX		
;
; Receives:
;		stringDisplay		=		ESI, the string we want to display
;		counter				=		ECX, the number of non NULL terminator ASCII numbers to write
;		printString			=		OFFSET usrString, this is the string we are going to fill to use for writeString
;
; returns: 
;		None
; ---------------------------------------------------------------------------------
mDisplayString MACRO stringDisplay,counter,printString
	LOCAL		_skipZero

	PUSHAD
	MOV			ESI, stringDisplay
	MOV			EDI, printString
	MOV			EDX, printString
	MOV			ECX, counter
_skipZero:
	; Setup display string, skipping Null terminators
	CLD
	LODSB		
	CMP			AL, 0
	JE			_skipZero
	CLD
	STOSB
	LOOP		_skipZero
	CALL		WriteString
	POPAD

ENDM

; ---------------------------------------------------------------------------------
; Name: introduction
;
; Introduces the program, author and displays instructions
;
; Preconditions: 
;		NONE
;
; Postconditions: 
;		NONE
; Receives: 
;		[EBP + 8]	=	Offset for instructions
;		[EBP + 12]	=	Offset for authorName
;		[EBP + 16]	=	Offset for programTitle
;		[EBP + 20]	=	OFFSET for extraCredit
;
; Returns: 
;		None
; ---------------------------------------------------------------------------------
introduction PROC
	PUSH		EBP
	MOV			EBP, ESP
	PUSHAD

	; Introduce the program, author and display instructions
	MOV			EDX, [EBP + 16]
	CALL		WriteString
	MOV			EDX, [EBP + 12]
	CALL		WriteString
	MOV			EDX, [EBP + 20]
	CALL		WriteString
	MOV			EDX, [EBP + 8]
	CALL		WriteString

	POPAD
	POP			EBP
	RET			16
introduction ENDP

; ---------------------------------------------------------------------------------
; Name: readVal
;
; Reads a value from a user input, validates the data, converts it and stores it in 
; an array
;
; Preconditions: 
;		NONE
; Postconditions: 
;		NONE
; Receives: 
;		[EBP + 8]		=		OFFSET strInstructions
;		[EBP + 12]		=		OFFSET bufferString
;		[EBP + 16]		=		BUFFERSIZE
;		[EBP + 20]		=		Location in positiveDecArray
;		[EBP + 24]		=		OFFSET errorMessage
;		[EBP + 28]		=		negNumber
;		[EBP + 32]		=		negConverter
;
; Returns: 
;		Updates decArray
; ---------------------------------------------------------------------------------
readVal PROC 
	PUSH		EBP
	MOV			EBP, ESP
	PUSHAD

	; Run the mGetString Macro
_getString:
	; Reset the conversion arrays to Null terminated 0s
	MOV			ESI, [EBP + 12]
	MOV			EAX, [EBP + 16]
	CDQ
	MOV			EBX, 4
	DIV			EBX
	MOV			ECX, EAX
	MOV			EAX, 0
_resetLoop:
	MOV			[ESI], EAX
	ADD			EDI, 4
	ADD			ESI, 4
	LOOP		_resetLoop

	mGetString	[EBP + 8], [EBP + 12], [EBP + 16]

	; Do some data limit validation

	MOV			ESI, EDX
	PUSH		EAX
	LODSB
	CMP			AL, 45
	JE			_negNumValidation

	POP			EAX
	CMP			EAX, 10
	JA			_invalidNumber
	JMP			_valid

_negNumValidation:
	POP			EAX
	CMP			EAX, 11
	JA			_invalidNumber

_valid:
	MOV			ESI, EDX
	MOV			EDI, [EBP + 20]

	; Convert to decimal
	MOV			EBX, 0
	CLD		
	MOV			EAX, 0
	LODSB
	CMP			AL, 45
	JE			_negativeSign
	JMP			_enterDecLoop
_decLoop:
	CLD		
	MOV			EAX, 0
	LODSB
_enterDecLoop:
	CMP			AL, 0
	JE			_endDecLoop
	CMP			AL, 47
	JLE			_invalidNumber
	CMP			AL, 58
	JGE			_invalidNumber

	SUB			AL, 48
	PUSH		EAX
	MOV			EAX, 0
	MOV			EAX, EBX
	MOV			EBX, 10
	MUL			EBX
	MOV			EBX, EAX
	MOV			EAX, 0
	POP			EAX
	ADD			EBX, EAX
	LOOP		_decLoop

_endDecLoop:
	; More data validation
	CMP			EDX, 0
	JA			_invalidNumber
	CMP			EBX, 2147483647
	JA			_invalidNumber

	MOV			EAX, [EBP + 20]
	MOV			[EAX], EBX
	JMP			_endOfReadVal

	; Calculate if negative sign is first item read
_negativeSign:
	CLD		
	MOV			EAX, 0
	LODSB
	CMP			AL, 0
	JE			_endNegDecLoop
	CMP			AL, 47
	JLE			_invalidNumber
	CMP			AL, 58
	JGE			_invalidNumber

	SUB			AL, 48
	PUSH		EAX
	MOV			EAX, 0
	MOV			EAX, EBX
	MOV			EBX, 10
	MUL			EBX
	MOV			EBX, EAX
	MOV			EAX, 0
	POP			EAX
	ADD			EBX, EAX
	ADD			ECX, 1
	JMP		_negativeSign

	; Get twos compliment to store negative
_endNegDecLoop:
	; Do some negative number data validation
	CMP			EDX, 0
	JA			_invalidNumber
	CMP			EBX, 2147483648
	JA			_invalidNumber
	MOV			[EBP + 32], EBX

	mTwosCompliment [EBP + 32]
	MOV			EAX, [EBP + 20]
	MOV			[EAX], EBX
	JMP			_endOfReadVal

	; Invalid number prompt
_invalidNumber:
	MOV			EDX, [EBP + 24]
	CALL		WriteString
	JMP			_getString

	MOV			[EDI], EBX

_endOfReadVal:
	POPAD
	POP			EBP
	RET			28
readVal ENDP

; ---------------------------------------------------------------------------------
; Name: writeVal
;
; Takes a decimal and converts it to ASCII to display
;
; Preconditions: 
;		None
;
; Postconditions: 
;		NONE
;
; Receives: 
;		[EBP + 40]	=	OFFSET numHolder
;		[EBP + 36]  =   OFFSET negConverter
;		[EBP + 32]	=	OFFSET lowerLimitHex
;		[EBP + 28]	=	OFFSET usrString
;		[EBP + 24]	=	BUFFERSIZE
;		[EBP + 20]	=	OFFSET for revConversionArray
;		[EBP + 16]	=	OFFSET for conversionArray
;		[EBP + 12]	=	DIVISIONCONSTANT
;		[EBP + 8]	=	decimal value to write
;
; Returns: 
;		None
; ---------------------------------------------------------------------------------
writeVal PROC 
	PUSH		EBP
	MOV			EBP, ESP
	PUSHAD

	; Check if the decimal is negative
	MOV			ECX, 4
	MOV			ESI, [EBP + 8]
	CLD
_getLastByte:
	LODSB
	LOOP		_getLastByte
	CMP			AL, 128
	JB			_notNegative
	MOV			ESI, [EBP + 8]
	MOV			EBX, [ESI]

	; If negative get two's compliment
	MOV			[EBP + 36], EBX
	mTwosCompliment [EBP + 36]

	MOV			EAX, EBX
	PUSH		EAX
	MOV			EBX, [EBP + 12]
	MOV			EDI, [EBP + 16]
	MOV			ECX, 0

	; Print negative sign
	MOV			AL, 45
	CALL		WriteChar
	POP			EAX

	; Compare to limit
	CMP			EAX, -2147483648
	JE			_printLimit
	JMP			_arrayLoop

_notNegative:
	; Put the decimal number parts in an array
	MOV			EBX, [EBP + 12]
	MOV			EDI, [EBP + 16]
	MOV			ECX, [EBP + 8]
	MOV			EAX, [ECX]
	MOV			ECX, 0
_arrayLoop:
	CDQ
	DIV			EBX
	MOV			[EDI], EDX
	ADD			EDI, 1
	ADD			ECX, 1
	CMP			EAX, 0
	JNE			_arrayLoop

	; Reverse the string
	PUSH		ECX
	MOV			[EBP + 40], ECX
	MOV			ECX, [EBP + 24]
	MOV			ESI, [EBP + 16]
	ADD			ESI, ECX
	SUB			ESI, 1
	MOV			EDI, [EBP + 20]
_reverseLoop:
	STD
	LODSB
	CLD
	STOSB
	LOOP	_reverseLoop

	; Turn array numbers in to ASCII numbers
	MOV			ECX, [EBP + 24]
	MOV			ESI, [EBP + 20]
	ADD			ESI, ECX
	SUB			ESI, 1
	MOV			EDI, [EBP + 20]
	ADD			EDI, ECX
	SUB			EDI, 1
	POP			ECX
	MOV			EAX, 48
_asciiLoop:	
	STD
	LODSB
	ADD			AL, 48
	STOSB
	LOOP		_asciiLoop

	mDisplayString [EBP + 20], [EBP + 40], [EBP + 28]
	JMP			_reset
	
_printLimit:
	MOV			ECX, 10
	MOV			[EBP + 40], ECX
	mDisplayString [EBP + 32], [EBP + 40], [EBP + 28]


	; Reset the conversion arrays to Null terminated 0s
_reset:
	MOV			EDI, [EBP + 16]
	MOV			ESI, [EBP + 20]
	MOV			EAX, [EBP + 24]
	CDQ
	MOV			EBX, 4
	DIV			EBX
	MOV			ECX, EAX
	MOV			EBX, [EBP + 28]
	MOV			EAX, 0
_resetLoop:
	MOV			[EDI], EAX
	MOV			[ESI], EAX
	MOV			[EBX], EAX
	ADD			EDI, 4
	ADD			ESI, 4
	ADD			EBX, 4
	LOOP		_resetLoop

	POPAD
	POP			EBP
	RET			36

writeVal ENDP

END main