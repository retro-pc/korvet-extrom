_MICRODOS_1_871220:

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xBD80,0xBD80,0xBD80 	
	db 	pW_CHK_PATCH
	dw 	0xBD82,0xBE00,0xBE00
	db 	pW_CHK_PATCH
	dw 	0xc000+1,0xC495,0xC495 	
	db 	pW_CHK_PATCH
	dw 	0xc003+1,0xDC25,0xDC25

	db 	pW_CHK_PATCH
	dw 	0xBE4C,0x451B,0x0a0d ;logo CLS to crlf

; ;not required
; ; 	db 	pB_CHK_PATCH
; ; 	dw 	0xbe07+1
; ; 	db 	0x02,0x00 	;remove FDD check
	
 	db 	pW_CHK_PATCH
	dw 	0xE97F+1,0xE9B6,MD_READ
	db 	pW_CHK_PATCH
	dw 	0xE984+1,0xEA20,MD_WRITE

	db 	pB_CHK_PATCH
	dw 	0xEB13
	db 	0xcd,0x00
	db 	pW_CHK_PATCH
	dw 	0xEB13+1,0xECA9,0x0000
	db 	pW_CHK_PATCH
	dw 	0xEB13+3+1,0xECAE,MD_READ_INFOSECTOR

	db 	pB_CHK_PATCH
	dw 	0xEACE
	DB 	0x21,0xC9

	;used in MD_READ_INFOSECTOR
	db 	pW_STORE
	dw 	0xEDA4,MD_DRV2+1

	db 	pW_STORE
	dw 	0xEDAC,MD_RDBUF2+1

	db 	pW_STORE
	dw 	0xED97,MD_PARAM2+1

	db 	pSETFLAG_MICRODOS

; 	db 	pNotSupported
	db 	pREQUIRED_OPTS1	

	db 	p_STOP
	db 	'MICRODOS_1_871220',0

