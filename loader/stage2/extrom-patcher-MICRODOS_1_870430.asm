_MICRODOS_1_870430:

;ужас и кошмар
;параметры диска забиты гвоздями
;непонятно какого выводит бред в начале директории
;комманда O только выдает кол-во дорожек, не устанавливает

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xBE80,0xBE80,0xBE80 	
	db 	pW_CHK_PATCH
	dw 	0xBE82,0xBF00,0xBF00
	db 	pW_CHK_PATCH
	dw 	0xc000+1,0xC49A,0xC49A 	
	db 	pW_CHK_PATCH
	dw 	0xc003+1,0xDBFD,0xDBFD


	db 	pW_CHK_PATCH
	dw 	0xE5A2,0x451B,0x0a0d ;logo CLS to crlf

 	db 	pW_CHK_PATCH
	dw 	0xE497+1,0xE627,MD_READ
	db 	pW_CHK_PATCH
	dw 	0xE4A0+1,0xE62A,MD_WRITE

	db 	pB_CHK_PATCH
	dw 	0xEAAE
	db 	0xcd,0x00
	db 	pW_CHK_PATCH
	dw 	0xEAAE+1,0xE9E9,0x0000

	db 	pW_CHK_PATCH
	dw 	0xEAB4+1,0xEC5D,MD_READ_INFOSECTOR

; 	db 	pB_CHK_PATCH
; 	dw 	0xEACE
; 	DB 	0x21,0xC9

	;used in MD_READ_INFOSECTOR
	db 	pW_STORE
	dw 	0xDF5A,MD_DRV2+1

	db 	pW_STORE
	dw 	0xE09A,MD_RDBUF2+1

	db 	pW_STORE
	;RAM:DDC0 21 5A DF                 ld      hl, byte_DF5A
	dw 	0xDDC0+1,MD_PARAM2+1

	db 	pSETFLAG_MICRODOS
; 	db 	pNotSupported

	db 	pREQUIRED_OPTS1	

	db 	p_STOP
	db 	'MICRODOS_1_870430',0

