PROJECT = extrom
MCU = atmega32
TARGET = extrom.elf
CC = avr-gcc

CPP = avr-g++

## Options common to compile, link and assembly rules
COMMON = -mmcu=$(MCU)

## Compile options common for all C compilation units.
CFLAGS = $(COMMON) -Wunused
#CFLAGS += -gdwarf-2 -Os -std=gnu99 -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums -g
CFLAGS += -gdwarf-2 -O2 -std=gnu99 -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums -g
CFLAGS += -MD -MP -MT $(*F).o -MF dep/$(@F).d 

## Assembly specific flags
ASMFLAGS = $(COMMON)
ASMFLAGS += $(CFLAGS)
ASMFLAGS += -x assembler-with-cpp -Wa,-gdwarf2

## Linker flags
LDFLAGS = $(COMMON)
LDFLAGS +=  -Wl,-Map=extrom.map,-u,vfprintf -lprintf_min -L/usr/avr/lib/avr4


## Intel Hex file production flags
HEX_FLASH_FLAGS = -R .eeprom -R .fuse -R .lock -R .signature

HEX_EEPROM_FLAGS = -j .eeprom
HEX_EEPROM_FLAGS += --set-section-flags=.eeprom="alloc,load"
HEX_EEPROM_FLAGS += --change-section-lma .eeprom=0 --no-change-warnings


## Objects that must be built in order to link
OBJECTS = extrom.o fs.o

## Objects explicitly added by the user
LINKONLYOBJECTS = 

## Build
all: $(TARGET) extrom.hex extrom.eep extrom.lss size

## Compile
extrom.o: extrom.c
	$(CC) $(INCLUDES) $(CFLAGS) -c  $<

##Link
$(TARGET): $(OBJECTS)
	 $(CC) $(LDFLAGS) $(OBJECTS) $(LINKONLYOBJECTS) $(LIBDIRS) $(LIBS) -o $(TARGET)

%.hex: $(TARGET)
	avr-objcopy -O ihex $(HEX_FLASH_FLAGS)  $< $@

%.eep: $(TARGET)
	-avr-objcopy $(HEX_EEPROM_FLAGS) -O ihex $< $@ || exit 0

%.lss: $(TARGET)
	avr-objdump -h -S $< > $@

size: ${TARGET}
	@echo
#	@avr-size  --mcu=${MCU} ${TARGET}
	@avr-size  ${TARGET}

## Clean target
.PHONY: clean
clean:
	-rm -rf $(OBJECTS) extrom.elf extrom.hex extrom.eep extrom.lss extrom.map


## Other dependencies
-include $(shell mkdir dep 2>/dev/null) $(wildcard dep/*)

