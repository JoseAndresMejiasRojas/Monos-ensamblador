Delay PROC
	;Hace una pausa en el programa de Di/18 segundos
	;Falla si se usa para una pausa que empieza desde antes de media noche y hasta despues de media noche pues ahi el contador 
	;se reinicia a 0
	;Di = x/18 segundos
	Push AX
	Push BX
	Push CX
	Push DX
	
	Mov	AH, 0		;leer el contador de reloj que se incrementa aprox 18.2 veces por segundo
	Int	1Ah		
	Mov	BX, DX		;guarda el contador actual en BX
	
gl_Delay:
	Mov	AH, 0		;leer el contador de reloj
	Int	1Ah
	Sub	DX, BX		;saca la diferencia entre el contador original y el actual
	Cmp	DI, DX		;se alcanzo el tiempo a esperar en Di?
	JGE	gl_Delay	;si no vuelva a leer
	
	Pop	DX
	Pop	CX
	Pop	BX
	Pop	AX

	Ret
Delay ENDP