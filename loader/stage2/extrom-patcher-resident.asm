rPORTA	EQU	0FB08H		; Порт Канала A интерфейса расширения
rPORTC	EQU	0FB0AH		; Порт Канала С интерфейса расширения
rPCWR	EQU	0FB0BH		; Порт управляющих команд

_DSK 	EQU 	0xFFFF
_OPER 	EQU 	0xFFFF
_TRK 	EQU 	0xFFFF
_SEC 	EQU 	0xFFFF
_DMA 	EQU 	0xFFFF

RRBUFF	EQU 	0xEE00

	.phase 0xea06
resident_addr 	EQU 	$

	db 	">resident<",0
; kbd_hook_2101:
; 	ld 	a,c
; 	cp 	0x01
; 	jp 	kbcmp




kbd_hook_noCtrl_03;
	ld 	(save_a),a
	cp 	0x03
	jp 	kbd_hook_noCtrl_chk


kbd_hook_noCtrl_33:
	ld 	(save_a),a
	cp 	0x33

kbd_hook_noCtrl_chk:
	jp 	nz,bios_hook

	ld 	a,(0xf880)
	and 	00100001b
	cp 	00100001b

	jp 	nz,bios_hook

	jp 	do_key_action

kbd_hook_2133:
	ld 	(save_a),a
	ld 	a,c
	cp 	0x33 	;stop
	jp 	nz,bios_hook

	ld 	a,b
	cp 	0x21 	;ctrl+stop
	jp 	nz,bios_hook

do_key_action:
	push 	hl
	push 	de
	push 	bc
	ld 	hl,msg_hook
	ld 	de,0xfc00
.kmlp:	
	ld 	a,(hl)
	or 	a
	jp 	z,.kmlp_ext
	ld 	(de),a
	inc 	hl
	inc 	de
	jp 	.kmlp

.kmlp_ext:
	pop 	bc
	pop 	de
	pop 	hl

	ld 	bc,0 	;simpulate no key
	ld 	a,0
	jp 	old_hook
; 	halt


bios_hook:
	ld 	a,(save_a)

old_hook:
	jp 	$
save_a:
	db 	0	
msg_hook:
	db 	'CTRL+SHIFT+STOP pressed',0

;RAM:DA27 C3 EB DC                 jp      j_READ

;RAM:DCEB          j_READ:                                 ; CODE XREF: RAM:_READj
;RAM:DCEB                                                  ; RAM:DB84p
;RAM:DCEB
;RAM:DCEB          ; FUNCTION CHUNK AT RAM:DDC4 SIZE 0000007B BYTES
;RAM:DCEB
;RAM:DCEB 3E 04                    ld      a, 4
;RAM:DCED 32 66 E0                 ld      (_OPERATION), a
;RAM:DCF0 3A 64 E0                 ld      a, (_DSK)
;RAM:DCF3 FE 04                    cp      4
;RAM:DCF5 CA C4 DD                 jp      z, loc_DDC4

;сюда попадает из биоса JP READ
;а если не наше то прыгаем на старый read
res_READ:
; 	ld	 A,4	;Режим чтения
; 	ld  	(_OPER),a
r_p_DSK1:
	ld 	a,(_DSK) 	;_DSK
	cp 	2
	jp 	nc,_old_read

	LD	(EXR_DRV),A	; # устройства

r_p_TRK1:
	LD	A,(_TRK)	
	LD	(EXR_TRK),A	; дорожка
r_p_SEC1:
	LD	A,(_SEC)	; сектор
	DEC	A		; у нас номер сектора начинается с 0
	LD	(EXR_SEC),A

	LD	A,1
	LD	(EXR_CMD),A	; 1 - команда чтения

	CALL	EXR_SENDCMD	; отсылаем команду
	DEC	A		; ответ, 0 - ошибка, 1 - ОК
	RET	NZ		; 0 - ошибка
r_p_DMA1:
	LD	HL,(_DMA)	; адрес буфера для приема данных
	CALL	EXR_GETSEC	; принимаем данные
	XOR	A		; завершение всегда успешно
	RET
;

_old_read:
	jp 	$
; 	jp 	0xDCEB
; 	jp 	0xDD15

res_WRITE:
; 	ld	 A,6	;Режим чтения
; 	ld  	(_OPER),a

r_p_DSK2:
	ld 	a,(_DSK) 	;_DSK
	cp 	2
	jp 	nc,_old_write

	LD	(EXR_DRV),A
r_p_TRK2:
	LD	A,(_TRK)
	LD	(EXR_TRK),A
r_p_SEC2:
	LD	A,(_SEC)
	DEC	A
	LD	(EXR_SEC),A

	LD	A,2		; 2 - команда записи
	LD	(EXR_CMD),A

	CALL	EXR_SENDCMD
	DEC	A		; ответ, 0 - ошибка, 1 - ОК
	RET	NZ		; 0 - ошибка
r_p_DMA2:
	LD	HL,(_DMA)
	CALL	EXR_PUTSEC
	XOR	A		; запись успешна
	RET

_old_write:
	jp 	$
; 	jp 	0xDEAF
; 	jp 	0xDE4B

res_GETINFO:

	LD 	A,(HL)	; A= маска выбора привода
	or 	a
	jp 	p,_old_getinfo 	;0x80+drvnum
;
;  Чтение с эмулируемых дисков A или B
;
	push 	hl
	CALL	EXR_READINFO
	DEC	A		; ответ, 0 - ошибка, 1 - ОК
; 	JP	NZ,0xE6ff	;IERR		; 0 - ошибка
_old_getinfo_chkdo:
	JP	$ 		;IERR jnz xxx, CHKDO
; ;
_old_getinfo:
	jp 	$ 		;GETINFO


; 	JP	0xE67f 		;IERR jnz xxx, CHKDO
; ; ;
; _old_getinfo:
; 	jp 	0xE648 		;GETINFO
	
; 	JP	NZ,0xE6ff	;IERR		; 0 - ошибка
; 	JP	0xE705 		;CHKDO
; ; ;
; _old_getinfo:
; 	jp 	0xE6BF 		;GETINFO


;==================  Драйвер ExtROM-API ====================================
	
	
;****************************************
;*  Прием байта из порта А по стробу    *
;****************************************
EXR_GETBYTE:
	PUSH	HL
	LD	HL,rPORTC
pWG:
	LD	A,(HL)		; слово состояния ВВ55 - берется из порта С
	AND	20h		; выделяем сигнал IBF
	JP	Z,pWG		; IBF=0 - данных еще нет
	LD	A,(rPORTA)		; данные поступили - выбираем их из порта А
	POP	HL
	RET

;****************************************
;*  Отправка байта в порт A             *
;****************************************
EXR_PUTBYTE:
	PUSH	HL
	PUSH	AF
	LD	HL,rPORTC
pWP:
	LD	A,(HL)		; слово состояния ВВ55 - берется из порта С
	AND	80h		; выделяем сигнал -OBF
	JP	Z,pWP		; -OBF=0 - в передатчике сидит незабранный байт
	POP	AF
	LD	(rPORTA),A		; данные поступили - выбираем их из порта А
	POP	HL
	RET

;*******************************************
;*      Прием сектора                      *
;* HL - адрес размещения сектора в памяти  *
;*  Размер сектора - 128 байт              *
;*******************************************
EXR_GETSEC:
	di
	PUSH	BC
	PUSH	DE
	LD	C,128		; счетчик байтов сектора, всего 128 байт
	EX	DE,HL		; адрес буфера -> DE
	LD	HL,rPORTC
pGSL:
	LD	A,(HL)		; слово состояния ВВ55 - берется из порта С
	AND	20h		; выделяем сигнал IBF
	JP	Z,pGSL		; IBF=0 - данных еще нет
	LD	A,(rPORTA)		; данные поступили - выбираем их из порта А
	LD	(DE),A		; принимаем и размещаем очередной байт
	INC	DE		; указатель буфера ++
	DEC	C		; принимаем остальные байты
	JP	NZ,pGSL
	POP	DE
	POP	BC
; 	ei - int should be still disabled 
	RET

;*******************************************
;*      Передача сектора                   *
;* HL - адрес размещения сектора в памяти  *
;*  Размер сектора - 128 байт              *
;*******************************************
EXR_PUTSEC:
	di
	PUSH	BC
	PUSH	DE
	LD	C,128		; счетчик байтов сектора, всего 128 байт
	EX	DE,HL		; адрес буфера -> DE
	LD	HL,rPORTC
pPSL:
	LD	A,(HL)		; слово состояния ВВ55 - берется из порта С
	AND	80h		; выделяем сигнал -OBF
	JP	Z,pPSL		; -OBF=0 - в передатчике сидит незабранный байт
	LD	A,(DE)		; принимаем и размещаем очередной байт
	LD	(rPORTA),A		; данные поступили - выбираем их из порта А
	INC	DE
	DEC	C		; принимаем остальные байты
	JP	NZ,pPSL
	POP	DE
	POP	BC
; 	ei - int should be still disabled 
	RET

;****************************************
;*     Отправка командного пакета       *
;****************************************
EXR_SENDCMD:

	di
	PUSH	HL
	PUSH	BC
	LD	A,0Ch
	LD	(rPCWR),A	; переключаем порт в режим 2
	LD	HL,EXR_CMD
	LD	C,4		; пакет - 4 байта
	LD	B,0		; заготовка контрольной суммы
pSCL:
	LD	A,(HL)		; очередной байт пакета 
	ADD 	B		; добавляем к КС
	LD	B,A
	LD	A,(HL)		; очередной байт пакета 
	CALL	EXR_PUTBYTE		; - в порт
	INC	HL
	DEC	C
	JP	NZ,pSCL
	LD	A,B
	DEC 	A		; КС-1
	CALL	EXR_PUTBYTE     ; контрольная сумма
	CALL	EXR_GETBYTE	; ответ контроллера
	POP	BC
	POP	HL
; 	ei - int should be still disabled 
	RET

;*******************************************
;*  Чтение информационного сектора
;*******************************************
EXR_READINFO:
	LD	HL,EXR_CMD	; командный пакет
	LD	(HL),1		; команда чтения
	INC	HL
r_p_DSK3:
	LD	A,(_DSK)	; вписываем # устройства
	LD	(HL),A
	INC	HL
	LD	(HL),0		; обнуляем, поскольку читаем сектор 0 дорожки 0 
	INC	HL
	LD	(HL),0		
	CALL	EXR_SENDCMD	; отправляем команду
	OR	A
	RET 	Z		; ошибка
	LD	HL,RRBUFF	; принимаем данные
	CALL	EXR_GETSEC
	LD	A,1
	RET


; Командный пакет интерфейса Extrom-API
;===============================================
EXR_CMD:	DB	0	; Команда чтения(0)-записи(1)
EXR_DRV:	DB	0	; Устройство - A(0), B(1)	
EXR_TRK:	DB	0	; логическая дорожка
EXR_SEC:	DB	0	; логический сектор (128b)


	db 	">resident<",0

	.dephase


