;ToDo: 	determinar un ganador (cuando el banano alcance la posición de un gorila).
;		hacer un switch para ver o no el rastro del banano (se maneja con el buffer).
;		pedir los datos al usuario sobre el ángulo y la velocidad.
;		verificar que suma_t no se salga de los límites adecuados

; Definición de constantes

; DOS:
DOS			Equ 21H
Bios		Equ 10H
SegVideoG 	Equ 0A000H ; Segmento de video en modo real

; Colores:
Color_gris		Equ 07h
Color_negro		Equ 00h
Color_amarillo	Equ 0eh
Color_cafe		Equ 06h

; Memoria de video:
NColumnas		Equ 320					; x
NFilas			Equ 200					; y
tamaño_pantalla	Equ NColumnas*Nfilas	; Numero de bytes en cada pantalla
Largo_linea		Equ NColumnas

; Otros:
Altura_maxima_edificio 	Equ 60
Ancho_edificios			Equ 16
Pagina					Equ 2*2*1024 ; Multiplico n por Pagina y escribe en el inicio de la página n.

LBlanca		Equ 0Fh
FAzul		Equ 10h

;Gorilas:
Ubicacion_gorila_A		Equ Largo_linea*(NFilas-1) + 10
Ubicacion_gorila_B		Equ Ubicacion_gorila_A+290
Altura_gorila			Equ 14
Ancho_gorila			Equ 10

Largo_banano	Equ 4
Ancho_banano	Equ 7

;Segmento de datos
Datos Segment
	
	x_banano 			DD ?
	y_banano 			DD ?
	
	tiempo_t 			DD 0.0
	suma_t				DD 0.1
	suma_t_rastro		DD 0.05	; Para dejar el rastro continuo en el buffer.
	contenedor_t		DD 0.0
	
	velocidad			DD 50.0
	angulo				DD 90.0
	gravedad			DT -9.8
	
	semilla				DW ?		; guarda la semilla para la generacion de "random"
	numero_aleatorio	DB ?		; guarda el numero Aleatorio
	
	ancho_pantalla_flotante	DD 320.0
	largo_pantalla_flotante	DD 200.0
									   ;Esp  Esc
	vector_teclas_normales 			DB 020h, 01Bh, 0
	vector_procedimientos_normales	DW tirar_banano, salir_programa	; Espacio:tirar banano, Escape: salir del programa.

									    ;F10   				F7   			F5   					F4
	vector_teclas_especiales		 DB 044h, 				041h, 			03Fh, 					03Eh, 0
	vector_procedimientos_especiales DW nueva_partida, trayectoria_banano, aumentar_suma_t, decrementar_suma_t
	
	; Para poder sumarle a la posición actual estos valores, y así tener un margen de toda el área del banano.
	vector_extremos_banano DD 0, Ancho_banano, -Largo_banano, -Largo_banano+Ancho_banano
	
	posicion_actual_gorila	DD Ubicacion_gorila_A ; Siempre empieza en en el A.
	
	turno_jugador	DB 0	; 0 turno del gorila izquierdo, 1 derecho.
	
	rastro_banano	DB 0	; 0 sin rastro, 1 con rastro.
	
	string_filas 	DB "Prueba", "$"
	
Datos EndS


Buffer Segment
	DB tamaño_pantalla Dup(?)
	colchon_buffer	DB 10 Dup("X")		; Para evitar un exception.
Buffer Ends

; Incluye los de los 2 jugadores.  Lleva el estado de los edificios.
Edificios Segment
  DB tamaño_pantalla Dup(0)
  colchon_edificios DB 10 Dup("X")		; Para evitar un exception.
Edificios Ends


Pila Segment Stack "Stack"
	DW 100 Dup (0)
Pila EndS

Codigo Segment
Assume DS:Datos, SS:Pila, CS:Codigo
.386

include Rand.inc
include Coor.inc
include Delay.inc

mover_registro Macro Reg,Dato
    If Dato eq 0
		Xor Reg, Reg
    Else
		Mov Reg, Dato
    EndIf
EndM

mover_memoria Macro Seg1,Of1, Seg2,Of2, NumDWords
    IfNB <Seg1> ;; Solo si hay cambio de segmento
		Mov AX, Seg1
		Mov DS, AX
    EndIf
	
    IfNB <Seg2> ;; Solo si hay cambio de segmento
		Mov AX, Seg2
		Mov ES, AX
    EndIf
	mover_registro SI, Of1
	mover_registro DI, Of2
	Mov CX, NumDWords
	Rep MovSD
EndM

; El procedimieto mueve de DS a la pantalla ( DS->ES )
; Se aplicará a moverlo del buffer a la pantalla.
mover_a_pantalla Proc Near
; Recibe en DS el segmento a mover a la pantalla
; Mueve Toda la pantalla iniciando en 0
;	Mov AX, 0A000H
;	Mov ES, AX
;	Xor SI,SI
;	Xor DI,DI
;	Mov CX, tamaño_pantalla/4
;	Rep MovSD
	Push AX
	Push SI
	Push DI
	Push CX
	Cld
	mover_memoria , 0, SegVideoG, 0, tamaño_pantalla/4
	Pop CX
	Pop DI
	Pop SI
	Pop AX

	Ret
mover_a_pantalla	EndP

; Incluye a los gorilas.
asignar_area_edificios Proc Near
	Push EDI
	Push EAX

	Call crear_gorilas
									; Determinar la posición de los edificios del jugador izquierdo.
	Mov EDI, Largo_linea*(NFilas-1) ; 320 * (200-1) = Primera posición de última línea de abajo.
	
	Add EDI, 30						; Posición desde donde se empieza a pintar los 3 edificios:
	
	Mov AX, Edificios
	Mov ES, AX						; Para copiar a Edificios.
	
	Cld
	Call InitRndm					; Inicializo la semilla para los llamados a Random.
	Mov CX, 3 						; 3 edificios.
	Call construir_Nedificios		; Parámetros: EDI=la posición inicial del primer edificio, CX=cantidad de ; 
									;edificios.	
	Add EDI, 212
	Call construir_Nedificios
									; Determinar la posición de los edificios del jugador derecho.
	Pop EAX
	Pop EDI
	
	Ret
asignar_area_edificios EndP

; Se recibe por parámetro la posición inicial (EDI) y la cantidad de edificios (CX)
construir_Nedificios Proc
	Push AX
	Push EBX
	Push ECX
	Push EDX
	Push EDI
	
	
	Mov EBX, EDI	; EBX = la posición del suelo requerida.
	Mov EDX, EBX    ; GUARDO LA POSICIÓN INICIAL EN EL SUELO.

					; Ancho de edificios: 11 pixeles.
	Mov AL, Color_gris	; El color de los edificios (todos del mismo color), este será el distintivo para reconocerlos.
	
ciclo_edificios:
	Push ECX						; ciclo cantidad de edificios.
	
	Mov ECX,  40					; Aleatorio [0, 39]
	Call Random						; Modifica numero_aleatorio
	Movzx ECX, numero_aleatorio		; ECX = altura del edificio.
	Add ECX, 20						; Altura mínima: 20, Altura máxima: 60
un_edificio: 						; Pinto un edificio
		
	Push CX							; ciclo de construir un edificio.
	
	Mov CX, Ancho_edificios
	Rep StosB						; Copio en el buffer.
	
	Pop CX							; ALTURA.
	Sub EBX, Largo_linea			; Subo un piso.
	Mov EDI, EBX
				
	Loop un_edificio
	
	
	Add EDX, Ancho_edificios
	Mov EBX, EDX 					; Me muevo 15 espacios a la derecha (los edificios van a estar pegados).
	Mov EDI, EBX
	Pop ECX 						; Ciclo cantidad de edificios.
	
	Loop ciclo_edificios
	
	Pop EDI
	Pop EDX
	Pop ECX
	Pop EBX
	Pop AX
	
	Ret
construir_Nedificios EndP

esperar_tecla Proc Near
	Push ESI
	
	Xor ESI, ESI

	Call leer_tecla						; Se guarda en AL.
	
	Cmp AL, 0							; ¿Tecla especial?
	JE teclas_especiales
	
teclas_normales:
	Cmp vector_teclas_normales[ESI], 0	; ¿Terminó de leer todas las teclas normales requeridas?
	JE salir_esperar_tecla
	Cmp vector_teclas_normales[ESI], AL
	JE procesar_tecla_normal
	
	Inc ESI
	Jmp teclas_normales
	
teclas_especiales:
	Call leer_tecla
	
ciclo_teclas_especiales:
	Cmp vector_teclas_especiales[ESI], 0
	JE salir_esperar_tecla
	Cmp vector_teclas_especiales[ESI], AL
	JE procesar_tecla_especial
	
	Inc ESI
	Jmp ciclo_teclas_especiales

procesar_tecla_especial:
	Call vector_procedimientos_especiales[ESI*2]
	Jmp salir_esperar_tecla
	
procesar_tecla_normal:
	Call vector_procedimientos_normales[ESI*2]
	
salir_esperar_tecla:
	Pop ESI
	Ret
esperar_tecla EndP

; Empieza el gorila perdedor.
nueva_partida Proc Near
	Call cambiar_jugador
	Call asignar_area_edificios
	
	Push DS
	Mov AX, Edificios
	Mov DS, AX	
	Call mover_a_pantalla
	Pop DS
	Ret
nueva_partida EndP

salir_programa Proc Near
	Mov EAX, -1
	Ret
salir_programa EndP

; Cambiar de estado rastro_banano.
trayectoria_banano Proc Near

	Cmp rastro_banano, 1
	JE cambiar_0
	
	Mov rastro_banano, 1
	Jmp salir_trayectoria_banano
		
cambiar_0:
	Mov rastro_banano, 0
	
salir_trayectoria_banano:
	Ret
trayectoria_banano EndP

aumentar_suma_t Proc Near

	Fld suma_t
	Fld CS:cero_1_flotante
	Faddp ST(1), ST
	Fstp suma_t
	Ret
aumentar_suma_t EndP

decrementar_suma_t Proc Near

	Fld suma_t
	Fld CS:cero_1_flotante
	Fsubp ST(1), ST
	Fstp suma_t
	Ret
	
	cero_1_flotante	DD 0.1
decrementar_suma_t EndP

; Leo tecla, se guarda en AL.
leer_tecla Proc Near
	Mov AH, 07h 
	Int DOS
	Ret
leer_tecla EndP

crear_gorilas Proc Near
	
	Push AX
	Push EDI
	
	Mov AX, Edificios				; Lo dibujo en Edificios.
	Mov ES, AX
	
	Mov EDI, Ubicacion_gorila_A 	; Posición del primer gorila.
	Call dibujar_un_gorila
	
	Mov EDI, Ubicacion_gorila_B		; Posición del segundo gorila.
	Call dibujar_un_gorila
	
	Pop EDI
	Pop AX
	Ret
crear_gorilas EndP

; Recibe la posición donde dibujar al gorila.  Dibuja hacia la derecha.
dibujar_un_gorila Proc Near
	Push AX
	Push EBX
	Push ECX
	Push EDI
	
	Mov EBX, EDI; Guardo la posición inicial.
	Mov AL, 06h	; Color del gorila.
	Mov ECX, Altura_gorila	; El gorila tiene de altura 14 pixeles.
	
ciclo_gorila:
	Push ECX
	Mov ECX, Ancho_gorila
	Rep StosB
	Pop ECX
	
	Sub EBX, Largo_linea	; Hacia arriba.
	Mov EDI, EBX
	Loop ciclo_gorila
	
	Pop EDI
	Pop ECX
	Pop EBX
	Pop AX
	Ret
dibujar_un_gorila EndP

; Recibe la posición en EDI y el segmento donde se va a dibujar en AX.
dibujar_banano Proc Near
	Push AX
	Push EBX
	Push ECX
	Push EDI
	
	Mov ES, AX ; Parámetro!
	
	Mov EBX, EDI
	
	Mov AL, Color_amarillo
	
	; Dibujo un rectángulo 4x7
	Mov ECX, Largo_banano
	Cld
ciclo_banano:
	Push ECX
	Mov ECX, Ancho_banano
	Rep StosB
	
	Sub EBX, Largo_linea
	Mov EDI, EBX
	
	Pop ECX
	Loop ciclo_banano
	
	Pop EDI
	Pop ECX
	Pop EBX
	Pop AX
	Ret
dibujar_banano EndP

; Pinta el banano en la memoria de video y pone también la coordenada en el buffer (junto con el área del banano).
; Cada suma_t se va a pintar el banano en la memoria de video.  Pero en cada iteración se va a poner en el buffer.
tirar_banano Proc Near
	Push EAX
	Push EBX
	Push ECX
	Push EDI

	; Hay que escoger al gorila indicado.
	Cmp turno_jugador, 0
	JE turno_gorila_izquierdo
	
	Mov EDI, Ubicacion_gorila_B-(Largo_linea*(Altura_gorila+1))	; Gorila derecho.
	Fld cientochenta
	Fld angulo											
	Fsubp ST(1), ST
	Fstp angulo													; Necesario para invertir la dirección del banano.
	
	Jmp copio_ubicacion_gorila

turno_gorila_izquierdo:											; Gorila izquierdo.
	Mov EDI, Ubicacion_gorila_A-(Largo_linea*(Altura_gorila+1))
	
copio_ubicacion_gorila:
	Mov ECX, EDI
	
	Mov posicion_actual_gorila, EDI									; La posición del gorila escogido.

ciclo_t:
	Mov EDI, ECX					; Recuperar el valor original de EDI.

	Call calcular_x
	Call calcular_y	

	Mov EBX, y_banano
	Mov EAX, Largo_linea
	Mul EBX							; y*Largo_linea
	Sub EDI, EAX					; y campos para arriba/abajo.
	
	Add EDI, x_banano				; x campos para derecha/izquierda:	

	Call verificar_limite_pantalla	; Verificar que no se haya salido de la pantalla.
									; Devuelve en AX: 0, si se puede, 1 si no hay que pintar, 2 si hay que terminar.
	Cmp AX, 1
	JE no_dibujar_banano
	Cmp AX, 2
	JE salir_tirar_banano
	

	Call verificar_choque			; Verificar si chocó en un edificio o gorila.
									; Devuelve en AX: 0 si no hay choque, 1 si hay un choque a un edificio, 2 a un gorila.
	Cmp AX, 1
	JE dibujar_explosion			; Si hubo choque, hacer explosión y salir de tirar_banano.
	Cmp AX, 2	
	JE salir_tirar_banano			; Ganador.
	
	Call escribir_rastro			; En buffer, EDI como parámetro.

	Call comparar_contenedor_t		; Para dibujar cada t tiempo.

	Cmp AX, 1						; Si 1, no dibuje.
	JE no_dibujar_banano
	
	Mov EAX, 0
	Mov contenedor_t, EAX			; Al pintar, reinicio el contenedor_t.
	
	; Dibujar banano
	Mov AX, SegVideoG
	Call dibujar_banano				; El banano se dibuja cada tiempo_t en pantalla.
	
	Push DI
	Mov DI, 0
	Call Delay
	Pop DI
	Call limpiar_o_no_pantalla		; Si rastro_banano es 0, limpio.
	
	
no_dibujar_banano:
	Call suma_tiempo_t				; Se le suma 0.05
	
	Jmp ciclo_t

dibujar_explosion:
	Call explosion
	Push DS
	Mov AX, Edificios
	Mov DS, AX
	Call mover_a_pantalla
	Pop DS
	
salir_tirar_banano:
	Call cambiar_jugador
	Mov EAX, CS:prueba
	Mov angulo, EAX
	
	Pop EAX
	Pop EBX
	Pop ECX
	Pop EDI
	Ret
	
	prueba			DD ?
tirar_banano EndP

cambiar_jugador Proc Near
	Cmp turno_jugador, 1
	JE turno_0
	
	Mov turno_jugador, 1
	Jmp salir_cambiar_jugador
	
turno_0:
	Mov turno_jugador, 0
	
salir_cambiar_jugador:
	; Restauro valores iniciales.
	Mov contenedor_t, 0
	Mov tiempo_t, 0

	Ret
cambiar_jugador EndP

limpiar_o_no_pantalla Proc Near
	Cmp rastro_banano, 0
	JE limpiar_pantalla
	
	Jmp salir_limpiar_o_no_pantalla
	
limpiar_pantalla:
	Push AX
	Push DS
	
	Mov AX, Edificios
	Mov DS, AX

	Call mover_a_pantalla
	Pop DS
	Pop AX
	
salir_limpiar_o_no_pantalla:
	Ret
limpiar_o_no_pantalla EndP


comparar_contenedor_t Proc Near
	Push contenedor_t
	
	Fld contenedor_t
	Fld suma_t
	Fsubp ST(1), ST			; contenedor_t-suma_t
	Fstp contenedor_t
	
	Mov EAX, contenedor_t	; Cuando el contenedor_t sea positivo, se pinta.
	And EAX, 80000000h		; Si es 1, es negativo.
	Rol EAX, 1				; Muevo el primer bit hacia la última posición (para ver si es 1 o 0).
	
	Pop contenedor_t
	Ret
comparar_contenedor_t EndP

; Se supone que si se llega acá, la explosión no alcanza a los gorilas.
; Recibe por EDI el lugar de la explosión.
explosion Proc Near
	; La explosión es de 5x5
	Rango_explosion Equ 12	; Hay desventaja para el gorila derecho, pero no importa tanto!
	
	Push AX
	Push EDI
	
	Mov AX, Edificios
	Mov ES, AX				; La modificación se hace a Edificios.
	
	Mov AX, Datos
	Mov DS, AX
	
	Mov ECX, Rango_explosion
	Mov AL, Color_negro

ciclo_explosion:
	Push ECX
	Push EDI
	
	Mov ECX, Rango_explosion
	Cmp turno_jugador, 0
	JE explosion_derecha
	
	Std						; Resta EDI, explosión hacia la izquierda.
	Jmp hacer_explosion
explosion_derecha:
	Cld
hacer_explosion:
	Rep StosB				; Hago una línea de 4, a partir de EDI.
	
	Pop EDI
	Pop ECX
	
	Sub EDI, Largo_linea	; Hacia arriba.
	Loop ciclo_explosion
	
	Pop EDI
	Pop AX
	Ret
explosion EndP

; Recibe por EDI la posición actual del banano.
verificar_choque Proc Near
	Push BX
	Push CX
	Push ESI
	
	Mov ESI, EDI				; Guardo el valor de EDI.
	
	Xor ECX, ECX

ciclo_verificar_choque:
	Add ESI, vector_extremos_banano[ECX*4]
	
	; Probando.  Si hay algo distinto de 0, hay un choque.
	Push DS
	Mov AX, Edificios
	Mov DS, AX
	Lodsb
	Mov BL, AL					; Guardo en BL lo que hay en edificios, apuntado por ESI.
	Pop DS
	
	Xor AX, AX
	
	Cmp BL, 0					; ¿Hay alguna cosa en esa posición?
	JNE choque_confirmado
	
	Cmp ECX, 3					; ¿Recorrí todo el vector?
	JE salir_verificar_choque	
	
	Inc ECX
	Mov ESI, EDI				; Recupero el valor original.
	Jmp ciclo_verificar_choque

choque_confirmado:
	Cmp BL, Color_cafe
	JE ganador
	
	Mov AX, 1					; Aún no hay ganador.
	Jmp salir_verificar_choque

ganador:
	Mov AX, 2
salir_verificar_choque:
	Pop ESI
	Pop CX
	Pop BX
	Ret
verificar_choque EndP

; Se verifica mediante EDI.
; Devuelve en AX: 0, si se puede, 1 si no hay que pintar, 2 si hay que terminar.
; Verificando con EDI, EDI+Ancho_banano, EDI-(Largo_banano), EDI-(Largo_banano)+Ancho_banano.
verificar_limite_pantalla Proc Near

	Push EBX
	Push ECX
	Push EDX
	Push ESI
	Push EDI
	
	Xor EAX, EAX
	Xor ECX, ECX						; El vector para moverse a partir de EDI.
	
	Mov EBX, EDI						; Guardo el valor original.
ciclo_limite_pantalla:
	Add EDI, vector_extremos_banano[ECX*4]
	
	Cmp EDI, 0							; Si se salió por arriba de la pantalla.
	JL no_pintar
	
	; Se calcula los límites de primero, si están mal, se termina de pintar.

	Call limites_laterales				; EDI como parámetro: posicion actual.
	Cmp EAX, 2							; ¿Posición incorrecta?
	JE salir_verificar_limite_pantalla
	
	Cmp EDI, tamaño_pantalla			; Si se salió por abajo de la pantalla.
	JGE terminar_pintar 
	
	Cmp ECX, 3
	JE salir_verificar_limite_pantalla	; Si está entre los límites permitidos.
	
	Mov EDI, EBX						; Recupero el valor original.
	Inc ECX
	Jmp ciclo_limite_pantalla
	
terminar_pintar:
	Mov EAX, 2
	Jmp salir_verificar_limite_pantalla
	
no_pintar:
	Mov AX, 1
	
salir_verificar_limite_pantalla:
	Pop EDI
	Pop ESI
	Pop EDX
	Pop ECX
	Pop EBX
	Ret
verificar_limite_pantalla EndP

; Depende de la posicion_actual_gorila, x, y, ya asignados anteriormente.  Usa EDI como parámetro.
; Devuelve un 2 en EAX si no está en la posición correcta.
limites_laterales Proc Near
	Push EBX
	Push ECX
	

	Mov EAX, y_banano
	Mov EBX, Largo_linea
	Mul EBX						; EAX=y*Largo_linea
	
	Mov EBX, posicion_actual_gorila
	Sub EBX, EAX				; EBX=desplazamiento en y (de la iteración actual).
	
	; Cálculo los límites derechos e izquierdos.
	Mov EAX, EBX
	Mov ECX, NColumnas
	Div ECX						; EAX=posicion/NColumnas=#Fila
	
	Mul ECX						; EAX=#Fila/NColumnas = Límite izquierdo
	
	Cmp EDI, EAX
	JL posicion_incorrecta		; Si la posicion actual es menor a Límite izquierdo, posicion incorrecta.
	
	Add EAX, ECX				; Límite izquierdo+NColumnas
	Dec EAX						; EAX=Límite derecho
	
	Cmp EDI, EAX
	JA posicion_incorrecta		; Si la posicion actual es mayor al límite derecho, posicion incorrecta.
	
	Xor EAX, EAX
	Jmp posicion_correcta
	
	
posicion_incorrecta:
	Mov EAX, 2
	
posicion_correcta:
	Pop ECX
	Pop EBX

	Ret
limites_laterales EndP


; Recibe EDI como la posición donde poner el banano.
escribir_rastro Proc Near
	Mov AX, Buffer		; Parámetro
	Call dibujar_banano
	Ret
escribir_rastro EndP

; Toma en cuenta un t ya definido.
suma_tiempo_t Proc Near
	Fld contenedor_t
	Fld suma_t_rastro
	
	Faddp ST(1), ST
	Fstp contenedor_t		; Sumar al contenedor_t.
	
	Fld suma_t_rastro
	Fld tiempo_t
	Faddp ST(1), ST
	Fstp tiempo_t			; tiempo_t += suma_t_rastro
	
	Ret
suma_tiempo_t EndP

captar_datos Proc Near

ciclo_angulo:
	Call leer_tecla
	
	Cmp AL, 0Dh ; Si presiona Enter, sale del ciclo.
	JE ciclo_velocidad
	
	Cmp AL, 0
	JE leer_flecha_angulo
	
	Jmp ciclo_angulo
	
leer_flecha_angulo:
	Call leer_tecla
	Cmp AL, 48h	; Tecla arriba
	JE sumar_angulo
	
	; Restar	; Tecla abajo.
	Fld angulo
	Fld CS:un_medio
	Fsubp ST(1), ST
	Fstp angulo
	Jmp ciclo_angulo
	
sumar_angulo:
	Fld angulo
	Fld CS:un_medio
	Faddp ST(1), ST
	Fstp angulo

	Jmp ciclo_angulo
	
ciclo_velocidad:

	Call leer_tecla
	
	Cmp AL, 0Dh ; Si presiona Enter, sale.
	JE salir_captar_datos
	
	Cmp AL, 0
	JE leer_flecha_velocidad
	
	Jmp ciclo_angulo
	
leer_flecha_velocidad:
	Call leer_tecla
	Cmp AL, 48h	; Tecla arriba
	JE sumar_velocidad
	
	; Restar	; Tecla abajo.
	Fld velocidad
	Fld CS:un_medio
	Fsubp ST(1), ST
	Fstp velocidad
	Jmp ciclo_velocidad
	
sumar_velocidad:
	Fld velocidad
	Fld CS:un_medio
	Faddp ST(1), ST
	Fstp velocidad

	Jmp ciclo_velocidad


salir_captar_datos:
	Ret
captar_datos EndP


escribir Proc Near
; Recibe como parámetros:
; AH el color de la hilera.
; BX el offset de la hilera. NOTA: tiene que tener fin ($).
; DI el offset de la memoria de video donde quiero que inicie la hilera.
	Push DI
escribir_caracter:

	;Note que se recorre caracter por caracter 
	Mov AL, [BX] ; se guarda en AL un caracter de la hilera a pintar.
	Cmp AL, "$"  ; ¿Es el final de la hilera?
	JE terminar_escribir ; Si es el final, termine.
	Mov Word ptr ES:[DI], AX ; Pinte.
	Inc DI
	Inc DI ; Siguiente palabra en la memoria de video.
	Inc BX ; Siguiente caracter de la hilera a pintar.
	Jmp escribir_caracter
	
terminar_escribir:
	Pop DI
	Ret ; Note que en pintar siempre se llama con un Call, por lo que el Ret lo que hace es devolverlo en la línea siguiente
		; a donde se llamó pintar.
escribir EndP

main:
	
	Mov AX, Datos
	Mov DS, AX

	Mov AX, SegVideoG
	Mov ES, AX
	
	Mov EAX, angulo
	Mov CS:prueba, EAX
	
	Mov AH, 0
	Mov AL, 13h
	Int 10h				;320x200 256 colores.  ¡Para usar la memoria de video!
	
	Mov AX, Buffer 		; Apunto al Buffer, que es lo que voy a modificar.
	Mov ES, AX
	
	Call asignar_area_edificios
	mover_memoria Edificios,0,Buffer,0, (tamaño_pantalla/4) ; Copio de edificio a Buffer.
	
	Mov AX, Buffer
	Mov DS, AX
	
	Mov EDI, Largo_linea*(NFilas-1)
	Call mover_a_pantalla	
	
	Mov AX, Datos
	Mov DS, AX
	
	Xor EAX, EAX
ciclo_espera:
	Call esperar_tecla
	Cmp EAX, -1		; Si se presiona Escape, se sale.
	JE salir
	
	Jmp ciclo_espera

salir:
	Mov AH, 4Ch
	Int DOS
	
EndS Codigo
End main

El main funciona como un controlador que se hace los llamados respectivos, para modularizar.  Se pinta los edificios
y luego se espera una tecla para verificar qué hacer.

Edificios:
Se necesita ubicar el "suelo" de la pantalla.  Luego hay que dibujo 3 edificios sin distancia entre sí.  Esto para ambos bandos.
Para copiar el segmento de Edificios al buffer, lo que se hará es copiar de abajo hacia arriba (hasta un límite superior 
definido). El límite superior para dejar de copiar será la altura máxima posible de un edificio.  Así pues, se trabajará por
regiones.
La altura de los 6 edificios es aleatorio, en un rango de [0, Altura_maxima_edificio - 1].

Las coordenadas del banano:
Se calculan con las fórmulas, luego, con esas coordenadas se dibuja (o no) el banano.
Se tiene pensado que -por comodidad- es mejor dibujar el banano desde la coordenada (x, Y) hacia la derecha, que la coordenada
sea como el lado izquierdo del banano.  Además, se tiene que verificar ese espacio dibujado en caso de que choque con el
edificio.
Lo que se va a hacer es, dada una ubicación z, cada interación t se va a sumar a esa ubicación fija.
La ubicación del banano en cada iteración se calcula de la siguiente manera: A partir de la ubicación del gorila:

ubicación_gorila-(ubicacion_gorila-coordenada_banano).  

Note que toma en cuenta cuando es para arriba y para abajo.
Si es para abajo, la resta se convierte en suma (--), por lo tanto, baja.

Ángulo:
El coprocesador matemático trabaja los ángulos en radianes, por lo que hay que pasar los grados a radianes: grados*(PI/180)

Dibujar el rastro del banano:
Cada t tiempo que se calcule la coodenada, se hacen dos cosas: pintar directamente en la memoria de video y pasarlo al buffer.
¿Qué se logra con esto?  Poder hacer una visualización o no del rastro, ya que, si se quiere ver el rastro, basta con copiar
lo que hay en el buffer a la memoria de video.  Y si se quiere pasar nuevamente a NO ver el rastro,  se copia solo lo que hay
en la región de edificios (para limpiar la pantalla completa).  Todo esto, mientras el banano está en su trayectoria, note
que al pasar de buffer a la memoria de video, la posición actual no modifica, simplemente "le cae encima".
Para tirar el banano desde la derecha de la pantalla, 180-angulo.

Por cuestiones de diseño, se va a guardar el rastro completo (sin saltos), esto para detectar un choque de un edificio.

Verificar choques:
El banano tiene 4 esquinas, por lo que se tiene que verificar en cada esquina que no esté saliendo de los límites de la
pantalla.  Para esto está vector_extremos_banano.  Luego se hace lo de siempre a la hora de verificar los límites de la matriz
"imaginaria".

Límites laterales:
Con la posición anterior ya establecida, lo que se hace es sumar el y_banano, y de ese resultado calcular los límites laterales.
Luego de determinar los límites, se verifica si con la suma a x_banano se encuentra entre los límites, para que esté entre 
los límites, tiene que ser: 
LIz < Pos <LDe.
