_CPM_NET_KORNET_drive_a:

	;chk only
	db 	pW_CHK_PATCH
	dw 	0xD980,0xD980,0xD980 	
	db 	pW_CHK_PATCH
	dw 	0xD982,0xDa00,0xDa00
	db 	pW_CHK_PATCH
	dw 	0xD984,0x0006,0x0006

	db 	pW_CHK_PATCH
	dw 	0xDB59+1,0xDAAA,0xDAAA ; HELLO MSG 	

	db 	pW_CHK_PATCH
	dw 	0xDA03+1,0xDBA1,0xDBA1
	db 	pW_CHK_PATCH
	dw 	0xDA27+1,0xDEB0,0xDEB0
	db 	pW_CHK_PATCH
	dw 	0xDA2A+1,0xE19A,0xE19A
	db 	pW_CHK_PATCH
	dw 	0xDA33+1,0xDFC8,0xDFC8

	db 	pNotSupported

	db 	p_STOP
	db 	'CPM_NET_KORNET_drive_a',0

