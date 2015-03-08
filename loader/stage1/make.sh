#!/bin/sh
#compile rom crc fixer
gcc ercs.c -o ercs

#compile stage1
z80asm stage1.asm -o stage1.rom

#fix satge1.rom crc
./ercs stage1.rom
rm ./ercs

