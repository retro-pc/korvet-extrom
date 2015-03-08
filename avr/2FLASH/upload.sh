#!/bin/sh
port="/dev/ttyACM0"

#fuses
avrdude -p m32 -c stk500 -P $port -V -U lfuse:w:0xe4:m -U hfuse:w:0xd1:m

#upload firmware
avrdude -p m32 -c stk500 -P $port -V -U flash:w:extrom.hex:i

#upload stage1 loader into eeprom
avrdude -p m32 -c stk500 -P $port -U eeprom:w:stage1.rom:r  -v
