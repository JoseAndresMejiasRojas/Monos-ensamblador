cien_flotante	DD 100.0 ; Usada en calcular_x y calcular_y.

; Se calcula (x, y) en un tiempo t.

; Velocidad*cos(ángulo)*t
; Se necesita el tiempo t ya dado.
calcular_x Proc Near

	Fld angulo					; Parámetro
	Call convertir_a_radianes
	
	Fcos
	
	Fld velocidad
	Fmulp ST(1), ST				; cos(radianes)*velocidad
	
	Fld tiempo_t
	Fmulp ST(1), ST				; angulo*(PI/180)*velocidad*t
	
	Fld ancho_pantalla_flotante	; Escala
	Fmulp ST(1), ST
	Fld CS:cien_flotante
	Fdivp ST(1), ST	

	Fistp x_banano				; Guardo la coordenada escalada.	
	Ret
	
calcular_x EndP

;V*sen(angulo)*t+0.5*g*t*t
calcular_y Proc Near

	Fld angulo
	Call convertir_a_radianes
	Fsin
	
	Fld velocidad
	Fmulp ST(1), ST				; V*sen(angulo)
	
	Fld tiempo_t
	Fmulp ST(1), ST				; V*sen(angulo)*t
	
	Fld CS:un_medio
	Fld gravedad
	Fmulp ST(1), ST				; 0.5*g
	
	Fld tiempo_t
	Fmul ST(0), ST				; t*t
	
	Fmulp ST(1), ST				; 0.5*g*t*t
	
	Faddp ST(1), ST				; V*sen(angulo)*t+0.5*g*t*t
	
	Fld largo_pantalla_flotante	; Escala
	Fmulp ST(1), ST
	Fld CS:cien_flotante
	Fdivp ST(1), ST

	Fistp y_banano
	Ret
	
	un_medio	DD 0.5
	
calcular_y EndP


; Recibe por parámetro en el tope de la pila el ángulo en grados. Lo devuelve en el tope de la pila.
; Recibe el ángulo por parámetros (en grados).  Lo deja en pila.
convertir_a_radianes Proc Near

	FldPI
	Fld CS:cientochenta
	Fdivp ST(1), ST
	
	Fmulp ST(1), ST		; angulo*(PI/180)
						; Ángulo en radianes
	Ret
	
	cientochenta	DD 180.0
convertir_a_radianes EndP