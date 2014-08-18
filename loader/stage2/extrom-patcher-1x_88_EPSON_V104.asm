_BIOS_1x_88_EPSON_V104:

	;db part
	db 	pB_CHK_PATCH
	dw 	0xDAEE
	db	0x1F,0x20 	;turn cls off in bios logo
	
	db 	pB_CHK_PATCH
	dw 	0xE8B7 +1
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
	dw 	0xE6DE
	db	0x77,0x00 	;77  ld      (hl), a  -> nop, getinfo more not overwrite disk type


 	;DW part

 	;chk only
	db 	pW_CHK_PATCH
	dw 	0xDA1B+1,0xDE82,0xDE82 	;SELDSK

	;chk & update
	db 	pW_CHK_PATCH
	dw 	0xDA27+1,0xDEE5,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDBC2+1,0xDEE5,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDA2A+1,0xE01B,res_WRITE 	;WRITE
	db 	pW_CHK_PATCH
	dw 	0xdeb4+1,0xE614,res_GETINFO 	;GET_INFO_CALL а это call внутри SELDSK

	db 	pW_STORE
	dw 	0xDEE5,_old_read+1
	db 	pW_STORE
	dw 	0xE01B,_old_write+1
	db 	pW_STORE
	dw 	0xE614,_old_getinfo+1

	;getdskinfo_inside

	;check that addr where we jump contains correct data
	db 	pB_CHK_PATCH
	dw 	0xE654
	db 	0x21,0x21
	db 	pW_CHK_PATCH
	dw	0xE654+1,0xE684,0xE684 	

 	db 	pW_STORE
	dw 	0xE654,_old_getinfo_chkdo+1 	

	;store  vars
;переменные которые BIOS использует в работе
	db 	pW_STORE
	dw 	0xE18F,r_p_DSK1+1	;_DSK
	db 	pW_STORE
	dw 	0xE18F,r_p_DSK2+1	;_DSK
	db 	pW_STORE
	dw 	0xE18F,r_p_DSK3+1	;_DSK

	db 	pW_STORE
	dw 	0xE193,r_p_TRK1+1	;_TRK 
	db 	pW_STORE
	dw 	0xE193,r_p_TRK2+1	;_TRK 

	db 	pW_STORE
	dw 	0xE194,r_p_SEC1+1	;_SEC 
	db 	pW_STORE
	dw 	0xE194,r_p_SEC2+1	;_SEC 

	db 	pW_STORE
	dw 	0xE195,r_p_DMA1+1	;_DMA 
	db 	pW_STORE
	dw 	0xE195,r_p_DMA2+1	;_DMA 

; 	;kbd hook
	
	;03
	
	db 	pW_CHK_PATCH
	dw 	0xE842,0xDCAD,kbd_hook_noCtrl_03
	db 	pW_STORE
	dw 	0xDCAD,old_hook+1

	db 	pREQUIRED_OPTS1	

	db 	p_STOP

	db 	'1x_88_EPSON_V104',0
