p_STOP 			EQU 	0x50
pW_CHK_PATCH 		EQU	0x51
pB_CHK_PATCH 		EQU 	0x52	
pW_STORE		EQU 	0x54
pNotSupported 		EQU 	0x55	
pSETFLAG_MICRODOS 	EQU 	0x56
pREQUIRED_OPTS1 	EQU 	0x57	
pREQUIRED_OPTS2 	EQU 	0x58	

cpm_bios_patcher:

	;dirty hasck
	;set jp 0 as first jp
	;strange dos behaviour,
	;sometime hangon when dir
	ld 	hl,0
	ld 	(BASE+1),hl 	;

	ld 	hl,msgPATCHER
	call 	PSTR

	ld 	de,bios_variants
chk_bios_lp:
	ld 	a,(de)
	ld 	l,a
	inc 	de
	ld 	a,(de)
	ld 	h,a
	inc 	de
	or 	l
	jp 	z,bios_not_found

	push 	de
	call 	try_bios
	pop 	de

	jp 	z,biod_found

	jp 	chk_bios_lp


bios_not_found:
	ld 	hl,msgNOTFOUND
	call 	PSTR

	ld 	a,1
	jp 	restart_disable	

biod_found:
	push 	hl 		;bios name
	ld 	hl,msgFOUND
	call 	PSTR
	pop 	hl
	call 	PSTR
	ld 	hl,msgCRLF
	call 	PSTR

	ld 	hl,(msg_warning)
	ld 	a,l
	or 	h
	call 	nz,PSTR

	ld 	a,(flag_unsupported)
	or 	a
	jp 	nz,bios_not_found

; 	halt
	ret

flag_unsupported:
	db 	0
flag_microdos:
	db 	0	

msg_warning:
	dw 	0


msgPATCHER:
	db 	0dh,0ah,'BIOS PATCHER by ESL :: ',0
msgFOUND:
	db 	'Detected: ',0
msgNOTFOUND:
	db 	'Unsupported BIOS, fallback to DEFAULT bios.'
msgCRLF:
	db 	0dh,0ah,0

msgREQ_OPTS:
	db 	' !! BIOS require OPTS '
msgREQ_OPTS_digit:
	db 	'1.x ROM and not compatible with current.',0dh,0ah,0

work_ptr:
	dw 	0;
patch_flag:
	db 	0
;проверит совпадает ли биос с табличкой
;на входе в HL табличка для биоса
;если совпало то HL укзывает на имя биоса

try_bios:
	ld 	(work_ptr),hl

	;check
	xor 	a
	ld 	hl,(work_ptr)
	call 	do_patch
	ret nz


	;скопировали резидент в дырку

	;preprare to PATCH

	;copy resident

	ld 	a,(flag_microdos)
	or 	a
	jp 	nz,move_microdos_resident

	ld 	hl,resident
	ld 	de,resident_addr
	ld 	bc,resident_len
	call	_p_ldir

	ld 	a,1
	ld 	hl,(work_ptr)
	call 	do_patch

	jp 	patch_done

move_microdos_resident:

	;microdos
	ld 	a,0x5c
	ld 	(SYSREG14),a

	ld 	hl,resident_md
	ld 	de,resident_md_addr
	ld 	bc,resident_md_len
	call	_p_ldir

	ld 	a,1
	ld 	hl,(work_ptr)
	call 	do_patch

	ld 	a,0x14
	ld 	(SYSREG5C),a


; 	HALT
patch_done:

	xor 	a
	ret

do_patch:
	ld 	(patch_flag),a

patch_loop:

	ld 	a,(hl)
	inc 	hl

	cp 	p_STOP
	ret 	z 	;FOUND

	cp 	pB_CHK_PATCH
	jp 	nz,l_pW_CHK

	call 	fetch_dw_to_bc 	;BC addr
	push 	bc
	call 	fetch_db_to_b 	;b

	ex 	de,hl
	pop 	hl
	ld 	a,(hl)
	ex 	de,hl 		;de - addr

	ld 	c,b

	call 	fetch_db_to_b 	;b store

	cp 	c
	ret 	nz


	ld 	a,(patch_flag)
	or 	a
	jp 	z,patch_loop

	ex 	de,hl
	ld 	(hl),b
	ex 	de,hl
	jp 	patch_loop

l_pW_CHK:
	cp 	pW_CHK_PATCH
	jp 	nz,l_pW_STORE

	call 	fetch_dw_to_bc 	
	call 	store_bc_to_tmp

	call 	fetch_dw_to_bc 	;BC OLD value

	call 	read_de_at_tmp 	;de - value
	call 	cp_de_bc

	call 	fetch_dw_to_bc 	;bc NEW value

	ret 	nz

	ld 	a,(patch_flag)
	or 	a
	jp 	z,patch_loop

	call 	store_bc_at_tmp

	jp 	patch_loop

l_pW_STORE:
	cp 	pW_STORE
	jp 	nz,.pNotSupported


	call 	fetch_dw_to_bc 	;BC value
	push 	bc

	call 	fetch_dw_to_bc 	
	call 	store_bc_to_tmp

	pop 	bc

	ld 	a,(patch_flag)
	or 	a
	jp 	z,patch_loop

	call 	store_bc_at_tmp

	jp 	patch_loop

.pNotSupported:
	cp 	pNotSupported
	jp 	nz,.pSETFLAG_MICRODOS

	jp 	Unsupported	

.pSETFLAG_MICRODOS:
	cp 	pSETFLAG_MICRODOS
	jp 	nz,.pREQUIRED_OPTS1

	ld 	a,1
	ld 	(flag_microdos),a
	jp 	patch_loop


.pREQUIRED_OPTS1:
	cp 	pREQUIRED_OPTS1
	jp 	nz,.pREQUIRED_OPTS2

	ld 	a,(patch_flag)
	or 	a
	jp 	nz,patch_loop

;opts1.1
; RAM:003B FF                       db 0FFh
;opts2.0
; RAM:003B 02                       db    2
; kontur
; RAM:003B 05                       dec     b
;kvant8 terminal 
; RAM:003B 00                       db    0
;kvant8 - tigris
; RAM:003B FF                       db 0FFh

; RAM:050F EF F0 F4+msgOPTS:        text "KOI8-R", 'OPTS  1.1',0; DATA XREF: RAM:03DDo
	ld 	a,(0x003b)
	cp 	0xff
	jp 	z,patch_loop

	ld 	a,'1'
	jp 	set_opts_warning

; 	jp 	patch_loop

.pREQUIRED_OPTS2:
	cp 	pREQUIRED_OPTS2
	jp 	nz,.pHALT

	ld 	a,(patch_flag)
	or 	a
	jp 	nz,patch_loop

;051B EF F0 F4+Logo1:          text "KOI8-R", 'OPTS  2.0',0; DATA XREF: ROM0:0397o
	ld 	a,(0x003b)
	cp 	2
	jp 	z,patch_loop

	ld 	a,'2'

set_opts_warning:
	ld 	(msgREQ_OPTS_digit),a

	push 	hl
	ld 	hl,msgREQ_OPTS
	ld 	(msg_warning),hl
	pop 	hl

Unsupported:
	ld 	a,1
	ld 	(flag_unsupported),a
	jp 	patch_loop

.pHALT:
	ld 	hl,msg_InvalidPatchCode
	call 	PSTR

	halt

	ret
msg_InvalidPatchCode:
	db 	'Invalid patchcode, check registers',0dh,0ah,0

cp_de_bc:
	ld 	a,e
	cp 	c
	ret 	nz
	ld 	a,d
	cp 	b
	ret

tmp: 	dw 	0

;stor bc to tmp
store_bc_to_tmp:
	ld 	a,c
	ld 	(tmp),a

	ld 	a,b
	ld 	(tmp+1),a
	ret

store_bc_at_tmp:
	push 	hl
	ld 	hl,(tmp)
	ld 	(hl),c
	inc 	hl
	ld 	(hl),b
	pop 	hl
	ret

;de - value at (tmp)
read_de_at_tmp:
	push 	hl
	ld 	hl,(tmp)

	ld 	e,(hl)
	inc 	hl
	ld 	d,(hl)

	pop 	hl
	ret


fetch_dw_to_bc:
	ld 	c,(hl)
	inc 	hl
fetch_db_to_b:
	ld 	b,(hl)
	inc 	hl
	ret

_p_ldir:
	ld 	a,(hl)
	ld 	(de),a
	inc 	hl
	inc 	de
	dec 	bc
	ld 	a,c
	or 	b
	jp 	nz,_p_ldir
	ret


bios_variants:

	dw _BIOS_21_EXTROM

	dw _BIOS_12_88_3_alternativa ;should be before _BIOS_12_88_3_niijaf because this is patched version
	dw _BIOS_12_88_3_niijaf      
	dw _BIOS_21_89___wiza	 
	dw _BIOS_21_89_2_niijaf2 	 
	dw _BIOS_21_89_2_niijaf   	 
	dw _BIOS_1x_89_03_30_RAVI    

	dw _BIOS_12_87_11_niijaf 	 
	dw _BIOS_12_90_5_kontur 	 
	dw _BIOS_20_88___miks 	 
	dw _BIOS_21_91___LAP 	 
	dw _BIOS_1x_88_EPSON_V104 	 
	dw _BIOS_12_87_09_NIIJAF     
	dw _BIOS_21m_____Shkanov 	 

	dw _CPM_NET_KORNET_drive_a  
	dw _CPM_NET_KORNET_drive_b  
	dw _CPM_NET_SFERA1 	 
	dw _CPM_NET_SFERA2 	 

	dw _MICRODOS_1_861011 	 
	dw _MICRODOS_1_861115	 
	dw _MICRODOS_1_870430 	 
	dw _MICRODOS_1_871220 	 
	dw _MICRODOS_2_880630   
	dw _MICRODOS_2_900105 	 

	dw 	0


	db 	"PATCHER DATA>>"
	
	include "extrom-patcher-21_EXTROM.asm"

	include	"extrom-patcher-12_88_3_alternativa.asm" ;  25
	include	"extrom-patcher-12_88_3_niijaf.asm"      ;  78
	include "extrom-patcher-21_89___wiza.asm"	 ;  42
	include	"extrom-patcher-21_89_2_niijaf2.asm" 	 ;  36
	include	"extrom-patcher-21_89_2_niijaf.asm"   	 ;  28
	include	"extrom-patcher-1x_89_03_30_RAVI.asm"    ;  17

	include	"extrom-patcher-12_87_11_niijaf.asm" 	 ;   3
	include	"extrom-patcher-12_90_5_kontur.asm" 	 ;   4
	include	"extrom-patcher-20_88___miks.asm" 	 ;   4
	include	"extrom-patcher-21_91___LAP.asm" 	 ;   4
	include	"extrom-patcher-1x_88_EPSON_V104.asm" 	 ;   1
	include	"extrom-patcher-12_87_09_NIIJAF.asm"     ;   2
	include	"extrom-patcher-21m_____Shkanov.asm" 	 ;   2

	include	"extrom-patcher-NET_KORNET_drive_a.asm"  ;   2
	include	"extrom-patcher-NET_KORNET_drive_b.asm"  ;   2
	include	"extrom-patcher-NET_SFERA1.asm" 	 ;   2
	include	"extrom-patcher-NET_SFERA2.asm" 	 ;   1

	include	"extrom-patcher-MICRODOS_1_861011.asm" 	 ;   7
	include	"extrom-patcher-MICRODOS_1_861115.asm"	 ;   2
	include	"extrom-patcher-MICRODOS_1_870430.asm" 	 ;  28
	include	"extrom-patcher-MICRODOS_1_871220.asm" 	 ;   2
	include	"extrom-patcher-MICRODOS_2_880630.asm"   ;  42
	include	"extrom-patcher-MICRODOS_2_900105.asm" 	 ;   4

	db 0xff
resident:
	include "extrom-patcher-resident.asm"
resident_len 	equ $-resident
	db "RESIDENT LEN>"
	dw resident_len
	db "<"

resident_md:
	include "extrom-patcher-resident-microdos.asm"
resident_md_len 	equ $-resident_md

	db "MDRESIDENT LEN>"
	dw resident_md_len
	db "<"
