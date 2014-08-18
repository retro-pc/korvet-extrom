_BIOS_12_87_11_niijaf:

	;01 	-dw check

	;db part
	db 	pB_CHK_PATCH
	dw 	0xDAEE
	db	0x1F,0x20 	;turn cls off in bios logo

	
	db 	pB_CHK_PATCH
	dw 	0xE30E+1
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
	dw 	0xE789
	db	0x77,0x00 	;77  ld      (hl), a  -> nop, getinfo more not overwrite disk type


	;DW part

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xDA1B+1,0xDCB2,0xDCB2 	;SELDSK

	;chk & update
	db 	pW_CHK_PATCH
	dw 	0xDA27+1,0xDD15,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDB70+1,0xDD15,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDA2A+1,0xDE4B,res_WRITE 	;WRITE
	db 	pW_CHK_PATCH
	dw 	0xDCE4+1,0xE6BF,res_GETINFO 	;GET_INFO_CALL а это call внутри SELDSK

	;getdskinfo_inside
	db 	pW_STORE
; 	dw 	0xe6ff+1,_old_getinfo_chkdo 	
	dw 	0xe6ff,_old_getinfo_chkdo+1 	

	;store  vars
	db 	pW_STORE
;переменные которые BIOS использует в работе
	dw 	0xDFBF,r_p_DSK1+1	;_DSK
	db 	pW_STORE
	dw 	0xDFBF,r_p_DSK2+1	;_DSK
	db 	pW_STORE
	dw 	0xDFBF,r_p_DSK3+1	;_DSK

	db 	pW_STORE
	dw 	0xDFC3,r_p_TRK1+1	;_TRK 
	db 	pW_STORE
	dw 	0xDFC3,r_p_TRK2+1	;_TRK 

	db 	pW_STORE
	dw 	0xDFC4,r_p_SEC1+1	;_SEC 
	db 	pW_STORE
	dw 	0xDFC4,r_p_SEC2+1	;_SEC 

	db 	pW_STORE
	dw 	0xDFC5,r_p_DMA1+1	;_DMA 

	db 	pW_STORE
	dw 	0xDFC5,r_p_DMA2+1	;_DMA 

	db 	p_STOP

	db 	'12_87_11_niijaf',0


