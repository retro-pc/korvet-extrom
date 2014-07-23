#!/bin/sh
avrdude -p m32 -c stk500 -P /dev/ttyACM0 -U eeprom:w:xboot.bin:r  -v
