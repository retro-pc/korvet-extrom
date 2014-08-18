_BIOS_21_89_2_niijaf2:

; 	;db part
	db 	pB_CHK_PATCH
	dw 	0xDAEE
	db	0x1F,0x20 	;turn cls off in bios logo

	db 	pB_CHK_PATCH
	dw 	0xE487+1
	db	0x49,0xc9

	db 	pB_CHK_PATCH
	dw 	0xDA89
	db	0x01,0x81

	db 	pB_CHK_PATCH
	dw 	0xDAA0
	db	0x02,0x82

	db 	pB_CHK_PATCH
	dw 	0xDAB7
	db	0x04,0x01

	db 	pB_CHK_PATCH
	dw 	0xDACE
	db	0x08,0x02

	db 	pB_CHK_PATCH
	dw 	0xE6F2
	db	0x77,0x00


; 	;DW part

; 	;chk only
	db 	pW_CHK_PATCH
	dw 	0xDA1B+1,0xDC7C,0xDC7C 	;SELDSK

	;chk & update
	db 	pW_CHK_PATCH
	dw 	0xDA27+1,0xDCDA,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDB73+1,0xDCDA,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDA2A+1,0xDE97,res_WRITE 	;WRITE
	db 	pW_CHK_PATCH
	dw 	0xdcae+1,0xE630,res_GETINFO 	;GET_INFO_CALL а это call внутри SELDSK

	db 	pW_STORE
	dw 	0xDCDA,_old_read+1
	db 	pW_STORE
	dw 	0xDE97,_old_write+1
	db 	pW_STORE
	dw 	0xE630,_old_getinfo+1

	;getdskinfo_inside

	;check that addr where we jump contains correct data
	db 	pB_CHK_PATCH
	dw 	0xE667
	db 	0x21,0x21
	db 	pW_CHK_PATCH
	dw	0xE667+1,0xE697,0xE697 	

	db 	pW_STORE
	dw 	0xE667,_old_getinfo_chkdo+1
; 	db 	pW_STORE
; 	dw 	0xE689,_old_getinfo_chkdo+1 	

	;store  vars
;переменные которые BIOS использует в работе
	db 	pW_STORE
	dw 	0xE04C,r_p_DSK1+1	;_DSK
	db 	pW_STORE
	dw 	0xE04C,r_p_DSK2+1	;_DSK
	db 	pW_STORE
	dw 	0xE04C,r_p_DSK3+1	;_DSK

	db 	pW_STORE
	dw 	0xE050,r_p_TRK1+1	;_TRK 
	db 	pW_STORE
	dw 	0xE050,r_p_TRK2+1	;_TRK 

	db 	pW_STORE
	dw 	0xE051,r_p_SEC1+1	;_SEC 
	db 	pW_STORE
	dw 	0xE051,r_p_SEC2+1	;_SEC 

	db 	pW_STORE
	dw 	0xE052,r_p_DMA1+1	;_DMA 
	db 	pW_STORE
	dw 	0xE052,r_p_DMA2+1	;_DMA 

	;kbd hook
	db 	pW_CHK_PATCH
	dw 	0xe3cb,0xdc35,kbd_hook_2133
	db 	pW_STORE
	dw 	0xdc35,old_hook+1


	db 	p_STOP

	db 	'21_89_2_niijaf2',0
