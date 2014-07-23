#!/bin/sh
#avrdude -p m32 -c stk500 -P /dev/ttyACM0 -U flash:w:exr.hex:i -U eeprom:w:extrom.bin:r  -v
avrdude -p m32 -c stk500 -P /dev/ttyACM0 -V  -U flash:w:extrom.hex:i   
