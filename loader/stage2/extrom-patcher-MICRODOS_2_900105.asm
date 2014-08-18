_MICRODOS_2_900105:

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xBD80,0xBD80,0xBD80 	
	db 	pW_CHK_PATCH
	dw 	0xBD82,0xBE00,0xBE00
	db 	pW_CHK_PATCH
	dw 	0xc000+1,0xC487,0xC487
	db 	pW_CHK_PATCH
	dw 	0xc003+1,0xDBC0,0xDBC0 	



	db 	pB_CHK_PATCH
	dw 	0xF04E
	db 	0x1B,0x0d 	;logo CLS to crlf

	db 	pB_CHK_PATCH
	dw 	0xF04E+1
	db 	0x45,0x0a


	db 	pB_CHK_PATCH
	dw 	0xbe07+1
	db 	0x02,0x00 	;remove FDD check
	
	db 	pW_CHK_PATCH
	dw 	0xEAFF+1,0xEB36,MD_READ
	db 	pW_CHK_PATCH
	dw 	0xEB04+1,0xEBA0,MD_WRITE

	db 	pB_CHK_PATCH
	dw 	0xEC87
	DB 	0xcd,0x00
	db 	pW_CHK_PATCH
	dw 	0xEC87+1,0xEE14,0x0000
	db 	pW_CHK_PATCH
	dw 	0xEC87+3+1,0xEE1E,MD_READ_INFOSECTOR

	db 	pB_CHK_PATCH
	dw 	0xEC42
	DB 	0x21,0xC9

	;used in MD_READ_INFOSECTOR
	db 	pW_STORE
	dw 	0xEF06,MD_DRV2+1

	db 	pW_STORE
	dw 	0xF04E,MD_RDBUF2+1

	db 	pW_STORE
	dw 	0xEEF8,MD_PARAM2+1


	db 	pSETFLAG_MICRODOS

	db 	p_STOP
	db 	'MICRODOS_2_900105',0
