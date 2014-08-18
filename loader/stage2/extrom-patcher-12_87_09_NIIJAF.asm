	_BIOS_12_87_09_NIIJAF:

	;01 	-dw check

	;db part
	db 	pB_CHK_PATCH
	dw 	0xDAEE
	db	0x1F,0x20 	;turn cls off in bios logo
	
	db 	pB_CHK_PATCH
	dw 	0xE738+1
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
	dw 	0xE531
	db	0x77,0x00 	;77  ld      (hl), a  -> nop, getinfo more not overwrite disk type


; 	;DW part

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xDA1B+1,0xDCB6,0xDCB6 	;SELDSK

	;chk & update
	db 	pW_CHK_PATCH
	dw 	0xDA27+1,0xDD19,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDB6D+1,0xDD19,res_READ	;READ
	db 	pW_CHK_PATCH
	dw 	0xDA2A+1,0xDE4F,res_WRITE 	;WRITE
	db 	pW_CHK_PATCH
	dw 	0xDCE8+1,0xE467,res_GETINFO 	;GET_INFO_CALL � ��� call ������ SELDSK

	db 	pW_STORE
	dw 	0xDD19,_old_read+1
	db 	pW_STORE
	dw 	0xDE4F,_old_write+1
	db 	pW_STORE
	dw 	0xE467,_old_getinfo+1

	;getdskinfo_inside

	;check that addr where we jump contains correct data
	db 	pB_CHK_PATCH
	dw 	0xE4A7
	db 	0x21,0x21
	db 	pW_CHK_PATCH
	dw	0xE4A7+1,0xE4D7,0xE4D7 	

	db 	pW_STORE
	dw 	0xE4A7,_old_getinfo_chkdo+1 	



	;store  vars
	db 	pW_STORE
;���������� ������� BIOS ���������� � ������
	dw 	0xDFC3,r_p_DSK1+1	;_DSK
	db 	pW_STORE
	dw 	0xDFC3,r_p_DSK2+1	;_DSK
	db 	pW_STORE
	dw 	0xDFC3,r_p_DSK3+1	;_DSK

	db 	pW_STORE
	dw 	0xDFC7,r_p_TRK1+1	;_TRK 
	db 	pW_STORE
	dw 	0xDFC7,r_p_TRK2+1	;_TRK 

	db 	pW_STORE
	dw 	0xDFC8,r_p_SEC1+1	;_SEC 
	db 	pW_STORE
	dw 	0xDFC8,r_p_SEC2+1	;_SEC 

	db 	pW_STORE
	dw 	0xDFC9,r_p_DMA1+1	;_DMA 
	db 	pW_STORE
	dw 	0xDFC9,r_p_DMA2+1	;_DMA 

	;kbd hook
	
	;03 - stop
	
	db 	pW_CHK_PATCH
	dw 	0xE695,0xDC58,kbd_hook_noCtrl_03
	db 	pW_STORE
	dw 	0xDC58,old_hook+1

	db 	p_STOP

	db 	'12_87_09_NIIJAF',0
