_MICRODOS_2_880630:

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xBD80,0xBD80,0xBD80 	
	db 	pW_CHK_PATCH
	dw 	0xBD82,0xBE00,0xBE00
	db 	pW_CHK_PATCH
	dw 	0xc000+1,0xC488,0xC488 	
	db 	pW_CHK_PATCH
	dw 	0xc003+1,0xDBFC,0xDBFC

	db 	pB_CHK_PATCH
	dw 	0xBEA3
	db 	0x1B,0x0d 	;logo CLS to crlf

	db 	pB_CHK_PATCH
	dw 	0xBEA3+1
	db 	0x45,0x0a


;not required
; 	db 	pB_CHK_PATCH
; 	dw 	0xbe07+1
; 	db 	0x02,0x00 	;remove FDD check
	
 	db 	pW_CHK_PATCH
	dw 	0xEAFD+1,0xEB34,MD_READ
	db 	pW_CHK_PATCH
	dw 	0xEB02+1,0xEB9E,MD_WRITE

	db 	pB_CHK_PATCH
	dw 	0xEC91
	DB 	0xcd,0x00
	db 	pW_CHK_PATCH
	dw 	0xEC91+1,0xee27,0x0000

	db 	pW_CHK_PATCH
	dw 	0xEC91+3+1,0xEE2C,MD_READ_INFOSECTOR

	db 	pB_CHK_PATCH
	dw 	0xEC4C
	DB 	0x21,0xC9

	;used in MD_READ_INFOSECTOR
	db 	pW_STORE
	dw 	0xEF23,MD_DRV2+1

	db 	pW_STORE
	dw 	0xEF2B,MD_RDBUF2+1

	db 	pW_STORE
	dw 	0xEF16,MD_PARAM2+1

	db 	pREQUIRED_OPTS2

	db 	pSETFLAG_MICRODOS

	db 	p_STOP
	db 	'MICRODOS_2_880630',0

