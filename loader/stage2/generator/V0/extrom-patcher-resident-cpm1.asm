	;�������� ��� ����� ����� 12_87_11_niijaf
	;����� ��� ���� ��������� ���� E8AC-EBA6  = 762 �����
	;��� ����������� ����� �������� ������� �����

resident_cpm1	equ $

	.phase 0xE8AC

resident_cpm1_addr 	EQU 	$

	cpm_resident_body _cpm1_

	.dephase

resident_cpm1_len 		equ	$-resident_cpm1

; minimum hole_start .. min hole_end
max_resident_cpm1_len 	equ 	0xEBA6-0xE8AC ;=762

available_resident_cpm1_len	equ 	max_resident_cpm1_len-resident_cpm1_len

	.assert max_resident_cpm1_len>=resident_cpm1_len
