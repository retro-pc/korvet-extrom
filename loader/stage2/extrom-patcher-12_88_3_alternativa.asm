_BIOS_12_88_3_alternativa:

	;check difference with 12_88_3_niijaf
	db 	pB_CHK_PATCH
	dw 	0xE01D
	db	0x21,0x21

	db 	pW_CHK_PATCH
	dw 	0xE01D+1,0xFB21,0xFB21 	;SELDSK

	;db part
	db 	pB_CHK_PATCH
	dw 	0xDAEE
	db	0x1F,0x20 	;turn cls off in bios logo
	
	db 	pB_CHK_PATCH
	dw 	0xE077+1
	db	0x49,0xc9 	;hwinit write to PPI1.C 0x49 -> 0xC9

	db 	pB_CHK_PATCH
	dw 	0xDA88
	db	0x01,0x81 	;drv_A drvreg value, now EMUDISK 1

	db 	pB_CHK_PATCH
	dw 	0xDA9F
	db	0x02,0x82 	;drv_B drvreg value, now EMUDISK 2

	db 	pB_CHK_PATCH
	dw 	0xDAB6
	db	0x04,0x01 	;drv_C drvreg value, now real A

	db 	pB_CHK_PATCH
	dw 	0xDACD
	db	0x08,0x02 	;drv_D drvreg value, now real B

	db 	pB_CHK_PATCH
	dw 	0xE63E
	db	0x77,0x00 	;77  ld      (hl), a  -> nop, getinfo more not overwrite disk type


	;DW part

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xDA1B+1,0xDC7B,0xDC7B 	;SELDSK

	;chk & update
	db 	pW_CHK_PATCH
	dw 	0xDA27+1,0xDCDE,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDB74+1,0xDCDE,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDA2A+1,0xDE14,res_WRITE 	;WRITE
	db 	pW_CHK_PATCH
	dw 	0xDCAD+1,0xE574,res_GETINFO 	;GET_INFO_CALL а это call внутри SELDSK

	db 	pW_STORE
	dw 	0xDCDE,_old_read+1
	db 	pW_STORE
	dw 	0xDE14,_old_write+1
	db 	pW_STORE
	dw 	0xE574,_old_getinfo+1

	;getdskinfo_inside

	;check that addr where we jump contains correct data
	db 	pB_CHK_PATCH
	dw 	0xE5B4
	db 	0x21,0x21
	db 	pW_CHK_PATCH
	dw	0xE5B4+1,0xE5E4,0xE5E4 	

	db 	pW_STORE
	dw 	0xE5B4,_old_getinfo_chkdo+1 	

	;store  vars
;переменные которые BIOS использует в работе
	db 	pW_STORE
	dw 	0xDF88,r_p_DSK1+1	;_DSK
	db 	pW_STORE
	dw 	0xDF88,r_p_DSK2+1	;_DSK
	db 	pW_STORE
	dw 	0xDF88,r_p_DSK3+1	;_DSK

	db 	pW_STORE
	dw 	0xDF8C,r_p_TRK1+1	;_TRK 
	db 	pW_STORE
	dw 	0xDF8C,r_p_TRK2+1	;_TRK 

	db 	pW_STORE
	dw 	0xDF8D,r_p_SEC1+1	;_SEC 
	db 	pW_STORE
	dw 	0xDF8D,r_p_SEC2+1	;_SEC 

	db 	pW_STORE
	dw 	0xDF8E,r_p_DMA1+1	;_DMA 
	db 	pW_STORE
	dw 	0xDF8E,r_p_DMA2+1	;_DMA 

	;kbd hook
	
	db 	pW_CHK_PATCH
	dw 	0xDFB6,0xDC36,kbd_hook_2133
	db 	pW_STORE
	dw 	0xDC36,old_hook+1

	db 	p_STOP

	db 	'12_88_3_alternativa',0
