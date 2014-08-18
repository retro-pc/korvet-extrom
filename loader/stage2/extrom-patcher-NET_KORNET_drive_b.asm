_CPM_NET_KORNET_drive_b:


; kornet_b_base 	equ 	0x00ee ; for checker
kornet_b_base 	equ 	0x8000 	; for real check

	;for checker

	;chk only
	db 	pW_CHK_PATCH
	dw 	kornet_b_base,0x00EE,0x00EE 	
	db 	pW_CHK_PATCH
	dw 	kornet_b_base+2,0x0000,0x0000
	db 	pW_CHK_PATCH
	dw 	kornet_b_base+2,0x000E,0x000E

	db 	pNotSupported

	db 	p_STOP
	db 	'CPM_NET_KORNET_drive_b',0

