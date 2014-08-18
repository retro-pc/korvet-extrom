_MICRODOS_1_861115:

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xBC80,0xBC80,0xBC80 	
	db 	pW_CHK_PATCH
	dw 	0xBC82,0xBD00,0xBD00
	db 	pW_CHK_PATCH
	dw 	0xc000+1,0xC4BF,0xC4BF 	
	db 	pW_CHK_PATCH
	dw 	0xc003+1,0xDBFB,0xDBFB

; 	db 	pW_CHK_PATCH
; 	dw 	0xE609+1,0xE72D,0xE72D

	db 	pB_CHK_PATCH
	dw 	0xDF95
	db	0x40,0x40

	db 	pB_CHK_PATCH
	dw 	0xDF97
	db	0x10,0x10

;patch
	db 	pW_CHK_PATCH
	dw 	0xE693,0x451B,0x0a0d ;logo CLS to crlf

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
	dw 	0xDFC8,MD_DRV2+1

	db 	pW_STORE
	dw 	0xEEB0,MD_RDBUF2+1

	db 	pW_STORE
	;RAM:DDBB 21 C8 DF                 ld      hl, byte_DFC8
	dw 	0xDDBB+1,MD_PARAM2+1

	db 	pSETFLAG_MICRODOS

	db 	pREQUIRED_OPTS1	

	db 	p_STOP
	db 	'MICRODOS_1_861115',0

