InitRndm PROC

	PUSH	AX	; Se preservan los regs utilizados.
	PUSH	CX
	PUSH	DX

	MOV	Ah, 00h			; Pedir numero de Ticks desde la media noche
	INT	1AH			; se devuelve el clock en CX:DX

	MOV	Semilla, DX		; mueve el word menos significativo como semilla
	
	POP	DX	; Recuperar registros alterados.
	POP	CX
	POP	AX

	RET

InitRndm EndP


Random Proc ; CX como parámetro -> numero_aleatorio (byte)
; Genera un número aleatorio entre 0 y CX-1. CX<256
; La primera vez debe llamarse a InitRndm para inicializar la Semilla
; El resultado se guarda en numero_aleatorio
;
PRIMO_1		EQU	13	;Primos para generar la secuencia, pueden ser otros valores
PRIMO_2		EQU	95

	push AX	 ; Preservar registros utilizados.
	push CX	
	push BX
	Push DX

; Se calcula la nueva semilla: Semilla*Primo_1 + Primo_2
	Mov BX, Semilla		; guarda en BL el AuxGenRandom
	Mov AX, Primo_1			; y lo multiplica por PRIMO_A	
	Mul BX			; el resultado en DX:AX
	Add AX, Primo_2		; Se preservan solo los últimos 16 bits
	Mov Semilla, AX		; se almacena el resultado en bruto para la próxima vez que se llame

; Calcular el módulo para asegurar que el numero quedará entre 0 y Max Solicitado.

	Xor	DX,DX		; Colocar Semilla en DX:AX
	DIV	CX;			; DX=(Semilla Modulo CX) 
	MOV	numero_aleatorio, Dl	; se guarda como byte en "numeroAleatorio" 

	Pop DX
	POP BX	; Se recuperan registros
	POP CX	
	POP AX	

	RET
Random endp