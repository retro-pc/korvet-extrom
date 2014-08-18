_BIOS_12_90_5_kontur:

 	;db part
	db 	pB_CHK_PATCH
	dw 	0xDAEF
	db	0x1F,0x20 	;turn cls off in bios logo
	
	db 	pB_CHK_PATCH
	dw 	0xE09B+1
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
	dw 	0xE6C0
	db	0x77,0x00 	;77  ld      (hl), a  -> nop, getinfo more not overwrite disk type


 	;DW part

 	;chk only
	db 	pW_CHK_PATCH
	dw 	0xDA1B+1,0xDC72,0xDC72 	;SELDSK

	;chk & update
	db 	pW_CHK_PATCH
	dw 	0xDA27+1,0xDCD5,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDB6B+1,0xDCD5,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDA2A+1,0xDE22,res_WRITE 	;WRITE
	db 	pW_CHK_PATCH
	dw 	0xdca4+1,0xE5F6,res_GETINFO 	;GET_INFO_CALL а это call внутри SELDSK

	db 	pW_STORE
	dw 	0xDCD5,_old_read+1
	db 	pW_STORE
	dw 	0xDE22,_old_write+1
	db 	pW_STORE
	dw 	0xE5F6,_old_getinfo+1

	;getdskinfo_inside

	;check that addr where we jump contains correct data
	db 	pB_CHK_PATCH
	dw 	0xE636
	db 	0x21,0x21
	db 	pW_CHK_PATCH
	dw	0xE636+1,0xE666,0xE666 	

	db 	pW_STORE
	dw 	0xE636,_old_getinfo_chkdo+1 	

; 	db 	pW_STORE
; 	dw 	0xE658,_old_getinfo_chkdo+1 	

	;store  vars
;переменные которые BIOS использует в работе
	db 	pW_STORE
	dw 	0xDFAC,r_p_DSK1+1	;_DSK
	db 	pW_STORE
	dw 	0xDFAC,r_p_DSK2+1	;_DSK
	db 	pW_STORE
	dw 	0xDFAC,r_p_DSK3+1	;_DSK

	db 	pW_STORE
	dw 	0xDFB0,r_p_TRK1+1	;_TRK 
	db 	pW_STORE
	dw 	0xDFB0,r_p_TRK2+1	;_TRK 

	db 	pW_STORE
	dw 	0xDFB1,r_p_SEC1+1	;_SEC 
	db 	pW_STORE
	dw 	0xDFB1,r_p_SEC2+1	;_SEC 

	db 	pW_STORE
	dw 	0xDFB2,r_p_DMA1+1	;_DMA 
	db 	pW_STORE
	dw 	0xDFB2,r_p_DMA2+1	;_DMA 

	;kbd hook
	
	db 	pW_CHK_PATCH
	dw 	0xDFDA,0xDC2D,kbd_hook_2133
	db 	pW_STORE
	dw 	0xDC2D,old_hook+1

	db 	p_STOP

	db 	'12_90_5_kontur',0
