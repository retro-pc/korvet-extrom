_BIOS_21_91___LAP:

	;db part
	db 	pB_CHK_PATCH
	dw 	0xDAEE
	db	0x1F,0x20 	;turn cls off in bios logo

	db 	pB_CHK_PATCH
	dw 	0xE0D2+1
	db	0x49,0xc9

	db 	pB_CHK_PATCH
	dw 	0xDA88
	db	0x01,0x81
; 	db	0x00,0x81

	db 	pB_CHK_PATCH
	dw 	0xDA9F
	db	0x02,0x82

	db 	pB_CHK_PATCH
	dw 	0xDAB6
	db	0x04,0x01

	db 	pB_CHK_PATCH
	dw 	0xDACD 
	db	0x08,0x02

	db 	pB_CHK_PATCH
	dw 	0xE69F
	db	0x77,0x00


	;DW part

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xDA1B+1,0xDCAE,0xDCAE 	;SELDSK

	;chk & update
	db 	pW_CHK_PATCH
	dw 	0xDA27+1,0xDD11,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDB5F+1,0xDD11,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDA2A+1,0xDE47,res_WRITE 	;WRITE
	db 	pW_CHK_PATCH
	dw 	0xdce0+1,0xE5D5,res_GETINFO 	;GET_INFO_CALL а это call внутри SELDSK

	db 	pW_STORE
	dw 	0xDD11,_old_read+1
	db 	pW_STORE
	dw 	0xDE47,_old_write+1
	db 	pW_STORE
	dw 	0xE5D5,_old_getinfo+1

	;getdskinfo_inside

	;check that addr where we jump contains correct data
	db 	pB_CHK_PATCH
	dw 	0xE615
	db 	0x21,0x21
	db 	pW_CHK_PATCH
	dw	0xE615+1,0xE645,0xE645 	

	db 	pW_STORE
	dw 	0xE615,_old_getinfo_chkdo+1

; 	db 	pW_STORE
; 	dw 	0xE637,_old_getinfo_chkdo+1 	

	;store  vars
;переменные которые BIOS использует в работе
	db 	pW_STORE
	dw 	0xDFE3,r_p_DSK1+1	;_DSK
	db 	pW_STORE
	dw 	0xDFE3,r_p_DSK2+1	;_DSK
	db 	pW_STORE
	dw 	0xDFE3,r_p_DSK3+1	;_DSK

	db 	pW_STORE
	dw 	0xDFE7,r_p_TRK1+1	;_TRK 
	db 	pW_STORE
	dw 	0xDFE7,r_p_TRK2+1	;_TRK 

	db 	pW_STORE
	dw 	0xDFE8,r_p_SEC1+1	;_SEC 
	db 	pW_STORE
	dw 	0xDFE8,r_p_SEC2+1	;_SEC 

	db 	pW_STORE
	dw 	0xDFE9,r_p_DMA1+1	;_DMA 
	db 	pW_STORE
	dw 	0xDFE9,r_p_DMA2+1	;_DMA 

	;kbd hook
	db 	pW_CHK_PATCH
	dw 	0xe011,0xdc21,kbd_hook_2133
	db 	pW_STORE
	dw 	0xdc21,old_hook+1


	db 	p_STOP

	db 	'21_91___LAP',0
