MD_rPORTA	EQU	0FE08H		; ���� ������ A ���������� ����������
MD_rPORTC	EQU	0FE0AH		; ���� ������ � ���������� ����������
MD_rPCWR	EQU	0FE0BH		; ���� ����������� ������

MD_RRBUFF 	EQU 	0

; 	.phase 	0xFD80
	.phase 	0xFA00
resident_md_addr 	EQU 	$


;�� ��� � ����������� 0x5C

;��� Cfg    ���     ���        ���   ���2       ����  ���2  �/�   ���1
;ID  Name   ROM     RAM        GZU   Ram2       KBD   REGS  ACZU  DEV
;______________________________________________________________________
;1C  ODOSA          0000-F7FF                   F800  FA00  FC00  FB00
;5C  DOSA           0000-FDFF                         FF00        FE00



;PATCH --------------------------------------------------------------------------

;do_dskIO

; RAM:EAFD FE 04                    cp      4 		
; RAM:EAFF CA 36 EB                 jp      z, jREAD 		<--- MY write
; RAM:EB02 FE 06                    cp      6
; RAM:EB04 CA A0 EB                 jp      z, jWRITE 		<--- MY write


; SELDSK:
; RAM:EC87 CD 14 EE                 call    SEL_DSK_SEEK0       <--- 00 00 00
; RAM:EC8A CD 1E EE                 call    ReadToRDBUF 	<--- MY read info into F04E. nz if err 

; RAM:EC3A          Flush?:                                 ; CODE XREF: do_DSKIO+6Ap
; RAM:EC3A                                                  ; do_DSKIO+9Ap ...
; RAM:EC3A 21 FD EE                 ld      hl, flagWriteReuired?
; RAM:EC3D 7E                       ld      a, (hl)
; RAM:EC3E B7                       or      a
; RAM:EC3F C8                       ret     z  			<--- C9, newer flush
; RAM:EC42 21 0A EF                 ld      hl, WrBufDiskInfo   <--- or C9 HERE


;DISKINFO -----------------------------------------------------------------------

; RAM:EB36          jREAD:                                  ; CODE XREF: do_DSKIO+40j
; RAM:EB36 2A F8 EE                 ld      hl, (DSC_IO_HL) ; DSCDESCR
; RAM:EB39 46                       ld      b, (hl)         ; Drive
; RAM:EB3A 23                       inc     hl

; RAM:EAF1 7E                       ld      a, (hl) 	    ;operation 3-reset?, 4-read,6-write
; RAM:EAF2 23                       inc     hl

; RAM:EB3C 23                       inc     hl 		    ; ?chword?
; RAM:EB3D 23                       inc     hl              ; ?NumB?

; RAM:EB3E 4E                       ld      c, (hl)         ; track
; RAM:EB3F 23                       inc     hl

; RAM:EB40 7E                       ld      a, (hl)         ; sector
; RAM:EB3F 23                       inc     hl

; RAM:EAF6 5E                       ld      e, (hl)
; RAM:EAF7 23                       inc     hl
; RAM:EAF8 56                       ld      d, (hl)
; RAM:EAF9 EB                       ex      de, hl
; RAM:EAFA 22 FA EE                 ld      (_DMA??), hl
;DISKINFO -----------------------------------------------------------------------
	db 	"MDresident<",0

md_fetch_params:

	push 	af
	push 	bc
	push 	de
	push 	hl

MD_PARAM2:
	ld      hl, (0xEEF8) ; DSCDESCR
	ld      a, (hl)         ; Drive
	inc     hl
	ld 	(MD_EXR_DRV),a

; 	ld      a, (hl)		;operation 3-reset?, 4-read,6-write
	inc     hl

	inc     hl		; ?chword?
	inc     hl              ; ?NumB?

	ld      a, (hl)         ; track
	inc     hl
	ld 	(MD_EXR_TRK),a

	ld      a, (hl)         ; sector
	DEC	A
	inc     hl
	ld 	(MD_EXR_SEC),a

	ld      e, (hl)
	inc     hl
	ld      d, (hl)
	ex      de, hl
	ld      (MD_DMA), hl

	pop 	hl
	pop 	de
	pop 	bc
	pop 	af

	ret

MD_READ:
	call 	md_fetch_params
	
	LD	A,1
	LD	(MD_EXR_CMD),A	; 1 - ������� ������

	CALL	MD_EXR_SENDCMD	; �������� �������
	DEC	A		; �����, 0 - ������, 1 - ��
	RET	NZ		; 0 - ������
	LD	HL,(MD_DMA)	; ����� ������ ��� ������ ������
	CALL	MD_EXR_GETSEC	; ��������� ������
	XOR	A		; ���������� ������ �������
	RET

MD_WRITE:
	call	md_fetch_params

	LD	A,2		; 2 - ������� ������
	LD	(MD_EXR_CMD),A

	CALL	MD_EXR_SENDCMD
	DEC	A		; �����, 0 - ������, 1 - ��
	RET	NZ		; 0 - ������
	LD	HL,(MD_DMA)
	CALL	MD_EXR_PUTSEC
	XOR	A		; ������ �������
	RET

MD_READ_INFOSECTOR:
	CALL	MD_EXR_READINFO
	DEC	A		; �����, 0 - ������, 1 - ��
	ret


;==================  ������� ExtROM-API ====================================
	
	
;****************************************
;*  ����� ����� �� ����� � �� ������    *
;****************************************
MD_EXR_GETBYTE:
	PUSH	HL
	LD	HL,MD_rPORTC
MD_pWG:
	LD	A,(HL)		; ����� ��������� ��55 - ������� �� ����� �
	AND	20h		; �������� ������ IBF
	JP	Z,MD_pWG		; IBF=0 - ������ ��� ���
	LD	A,(MD_rPORTA)		; ������ ��������� - �������� �� �� ����� �
	POP	HL
	RET

;****************************************
;*  �������� ����� � ���� A             *
;****************************************
MD_EXR_PUTBYTE:
	PUSH	HL
	PUSH	AF
	LD	HL,MD_rPORTC
MD_pWP:
	LD	A,(HL)		; ����� ��������� ��55 - ������� �� ����� �
	AND	80h		; �������� ������ -OBF
	JP	Z,MD_pWP		; -OBF=0 - � ����������� ����� ����������� ����
	POP	AF
	LD	(MD_rPORTA),A		; ������ ��������� - �������� �� �� ����� �
	POP	HL
	RET

;*******************************************
;*      ����� �������                      *
;* HL - ����� ���������� ������� � ������  *
;*  ������ ������� - 128 ����              *
;*******************************************
MD_EXR_GETSEC:
	di
	PUSH	BC
	PUSH	DE
	LD	C,128		; ������� ������ �������, ����� 128 ����
	EX	DE,HL		; ����� ������ -> DE
	LD	HL,MD_rPORTC
MD_pGSL:
	LD	A,(HL)		; ����� ��������� ��55 - ������� �� ����� �
	AND	20h		; �������� ������ IBF
	JP	Z,MD_pGSL		; IBF=0 - ������ ��� ���
	LD	A,(MD_rPORTA)		; ������ ��������� - �������� �� �� ����� �
	LD	(DE),A		; ��������� � ��������� ��������� ����
	INC	DE		; ��������� ������ ++
	DEC	C		; ��������� ��������� �����
	JP	NZ,MD_pGSL
	POP	DE
	POP	BC
	ei 		
	RET

;*******************************************
;*      �������� �������                   *
;* HL - ����� ���������� ������� � ������  *
;*  ������ ������� - 128 ����              *
;*******************************************
MD_EXR_PUTSEC:
	di
	PUSH	BC
	PUSH	DE
	LD	C,128		; ������� ������ �������, ����� 128 ����
	EX	DE,HL		; ����� ������ -> DE
	LD	HL,MD_rPORTC
MD_pPSL:
	LD	A,(HL)		; ����� ��������� ��55 - ������� �� ����� �
	AND	80h		; �������� ������ -OBF
	JP	Z,MD_pPSL		; -OBF=0 - � ����������� ����� ����������� ����
	LD	A,(DE)		; ��������� � ��������� ��������� ����
	LD	(MD_rPORTA),A		; ������ ��������� - �������� �� �� ����� �
	INC	DE
	DEC	C		; ��������� ��������� �����
	JP	NZ,MD_pPSL
	POP	DE
	POP	BC
	ei 		
	RET

;****************************************
;*     �������� ���������� ������       *
;****************************************
MD_EXR_SENDCMD:

	di
	PUSH	HL
	PUSH	BC
	LD	A,0Ch
	LD	(MD_rPCWR),A	; ����������� ���� � ����� 2
	LD	HL,MD_EXR_CMD
	LD	C,4		; ����� - 4 �����
	LD	B,0		; ��������� ����������� �����
MD_pSCL:
	LD	A,(HL)		; ��������� ���� ������ 
	ADD 	B		; ��������� � ��
	LD	B,A
	LD	A,(HL)		; ��������� ���� ������ 
	CALL	MD_EXR_PUTBYTE		; - � ����
	INC	HL
	DEC	C
	JP	NZ,MD_pSCL
	LD	A,B
	DEC 	A		; ��-1
	CALL	MD_EXR_PUTBYTE     ; ����������� �����
	CALL	MD_EXR_GETBYTE	; ����� �����������
	POP	BC
	POP	HL
	ei 		
	RET

;*******************************************
;*  ������ ��������������� �������
;*******************************************
MD_EXR_READINFO:
	LD	HL,MD_EXR_CMD	; ��������� �����
	LD	(HL),1		; ������� ������
	INC	HL
MD_DRV2:
	LD	A,(0xEF06)	; ��������� # ����������
	LD	(HL),A
	INC	HL
	LD	(HL),0		; ��������, ��������� ������ ������ 0 ������� 0 
	INC	HL
	LD	(HL),0		
	CALL	MD_EXR_SENDCMD	; ���������� �������
	OR	A
	RET 	Z		; ������
MD_RDBUF2:
	LD	HL,MD_RRBUFF	; ��������� ������
	CALL	MD_EXR_GETSEC
	LD	A,1
	RET


; ��������� ����� ���������� Extrom-API
;===============================================
MD_EXR_CMD:	DB	0	; ������� ������(0)-������(1)
MD_EXR_DRV:	DB	0	; ���������� - A(0), B(1)	
MD_EXR_TRK:	DB	0	; ���������� �������
MD_EXR_SEC:	DB	0	; ���������� ������ (128b)
MD_DMA: 	DW 	0 	; ������ ����/������

	db 	">MDresident<",0

	.dephase


