
TEST_CUSTOM_DRIVE_CODE  = 0
TEST_INVALID_PARAMETERS = 0
NO_SPRITES_OR_IRQ       = 0
VERIFY                  = 0; verify correct loading by loading twice and comparing
PAUSE_BETWEEN_LOADS     = 0; delay value
INSTALL_ONLY            = 0
KEY_FOR_WATCHDOG        = 0

COUPLED_TIMERS          = NO_SPRITES_OR_IRQ; Plus/4: As TED timer emulation is horribly broken on VICE, only use coupled
                                           ;         timers if there is no other way to measure more than 64K cycles


.include "standard.inc"

.include "../include/loader.inc"

.include "../version.inc"

.include "cpu.inc"
.if PLATFORM = diskio::platform::COMMODORE_16
    .include "ted.inc"
.else
    .include "cia.inc"
    .include "vic.inc"
.endif

.include "float.inc"
.include "basic.inc"
.include "kernal.inc"

.macpack cbm
.macpack longbranch

.segment "ZEROPAGE" : zeropage

DRIVETYPE              = $02; unused
.if PLATFORM = diskio::platform::COMMODORE_16
PALNTSC                = $03; 0: NTSC
.else
PALNTSC                = $a8; RS232 Input Bit count/Tape temporary
.endif

.define X_PIC1_UNCOMPRESSED #.lobyte(pic1unc)
.define Y_PIC1_UNCOMPRESSED #.hibyte(pic1unc)
.define X_PIC2_UNCOMPRESSED #.lobyte(pic2unc)
.define Y_PIC2_UNCOMPRESSED #.hibyte(pic2unc)
.define X_PIC1_COMPRESSED #.lobyte(pic1compd)
.define Y_PIC1_COMPRESSED #.hibyte(pic1compd)
.define X_PIC2_COMPRESSED #.lobyte(pic2compd)
.define Y_PIC2_COMPRESSED #.hibyte(pic2compd)

POINTERS:              .res 6

.if PLATFORM = diskio::platform::COMMODORE_16
IRQ_SLACK = $00
.endif

LOAD_TO_UPPER_MEM = LOAD_UNDER_D000_DFFF; on C-16/+4, himem is $8000..$fcff, but it can be loaded to regardless of this setting

.if LOAD_TO_UPPER_MEM

.segment "BITMAPHI"
.import __BITMAPHI_LOAD__
BITMAP = __BITMAPHI_LOAD__

    .if PLATFORM = diskio::platform::COMMODORE_16
.segment "COLRAM"
.import __COLRAM_LOAD__
COLRAM = __COLRAM_LOAD__
    .else
.segment "COLRAMHI"
.import __COLRAMHI_LOAD__
COLRAM = __COLRAMHI_LOAD__
    .endif

.else; !LOAD_TO_UPPER_MEM

.segment "BITMAP"
.import __BITMAP_LOAD__
BITMAP = __BITMAP_LOAD__

.segment "COLRAM"
.import __COLRAM_LOAD__
COLRAM = __COLRAM_LOAD__

.endif

.if PLATFORM <> diskio::platform::COMMODORE_16
.segment "SPRITES"
.import __SPRITES_LOAD__
SPRITES = __SPRITES_LOAD__

    .if LOAD_TO_UPPER_MEM
SPRITESCR = SPRITES + $0400
    .else
SPRITESCR = COLRAM
    .endif
.endif

TESTNAMEPOS = $0b; x coord

; cannot define string symbols from the command line with ca65
.if DECOMPRESSOR = DECOMPRESSORS::BITNAX
    .define COMPEXT "BNX"
.elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER2
    .define COMPEXT "BB2"
.elseif DECOMPRESSOR = DECOMPRESSORS::DOYNAX_LZ
    .define COMPEXT "DNX"
.elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
    .define COMPEXT "EXO"
.elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
    .define COMPEXT "LC"
.elseif DECOMPRESSOR = DECOMPRESSORS::NUCRUNCH
    .define COMPEXT "NC"
.elseif DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
    .define COMPEXT "PU"
.elseif DECOMPRESSOR = DECOMPRESSORS::SUBSIZER
    .define COMPEXT "SSZ"
.elseif DECOMPRESSOR = DECOMPRESSORS::TINYCRUNCH
    .define COMPEXT "TC"
.endif


; testing macros

.if VERIFY
    .macro VERIFY_PROLOGUE
            ; copy away
            jsr piccopy
    .endmacro

    .macro VERIFY_EPILOGUE
            .local verifyok

            jsr piccomp
            beq verifyok
            jsr verifyfail
verifyok:
    .endmacro
.endif


.macro MEMCONFIG_BUFFER
    .if PLATFORM = diskio::platform::COMMODORE_16
            lda TED_CHARGEN_ADDR
            pha
    .else
            lda IO_PORT_DIRECTION
            pha
            lda IO_PORT
            pha
    .endif
.endmacro

.macro MEMCONFIG_CHECK
    .if PLATFORM = diskio::platform::COMMODORE_16
            pla
            eor TED_CHARGEN_ADDR
            and #ROM_IS_ENABLED | FORCE_SINGLE_CLOCK
            beq :+
            lda #MEMCONFIGCH
            jsr error
:
    .else
            pla
            tax
            ldy IO_PORT
            pla
            cpx IO_PORT
            bne :+
            tax
            ldy IO_PORT_DIRECTION
            cpx IO_PORT_DIRECTION
            beq :++
:           lda #MEMCONFIGCH
            jsr error
:
    .endif
.endmacro

.macro ENABLE_MEMCONFIG_CHECK
            lda #$ff
            sta memcfgchks
.endmacro

.macro DISABLE_MEMCONFIG_CHECK
            lda #$00
            sta memcfgchks
.endmacro

.macro ENABLE_ALL_ROM
    .if PLATFORM = diskio::platform::COMMODORE_16
            sta TED_ROM_ENABLE
    .else
            lda #MEMCONFIG_IO_KERNAL_BASIC
            sta IO_PORT
    .endif
.endmacro

.macro ENABLE_ALL_RAM
    .if PLATFORM = diskio::platform::COMMODORE_16
            sta TED_RAM_ENABLE
    .else
            lda #MEMCONFIG_ALL_RAM
            sta IO_PORT
    .endif
.endmacro

.macro PUSH_MEMCONFIG
    .if PLATFORM = diskio::platform::COMMODORE_16
            lda TED_CHARGEN_ADDR
            lsr
            php
    .else
            lda IO_PORT
            pha
    .endif
.endmacro

.macro PUSH_MEMCONFIG_AND_ENABLE_ALL_ROM
            PUSH_MEMCONFIG
            ENABLE_ALL_ROM
.endmacro

.macro PUSH_MEMCONFIG_AND_ENABLE_ALL_RAM
            PUSH_MEMCONFIG
            ENABLE_ALL_RAM
.endmacro

.macro POP_MEMCONFIG
    .if PLATFORM = diskio::platform::COMMODORE_16
            .local enable_ram

            plp
            ENABLE_ALL_RAM
            bcc enable_ram
            ENABLE_ALL_ROM
enable_ram:
    .else
            pla
            sta IO_PORT
    .endif
.endmacro


.macro TEST testmacro, param0, param1
            MEMCONFIG_BUFFER
            testmacro param0, param1
            MEMCONFIG_CHECK
            jsr uninstcheck
    .if VERIFY
            VERIFY_PROLOGUE
            testmacro param0, param1
            jsr uninstcheck
            VERIFY_EPILOGUE
    .endif
.endmacro

.macro CHECKADDRESSES
    .if END_ADDRESS_API
            .local endaddfail
            .local endaddrok

            clc
            lda #.lobyte(BITMAP_SIZE)
            adc loadaddrlo
            eor endaddrlo
            bne endaddfail
            lda #.hibyte(BITMAP_SIZE)
            adc loadaddrhi
            cmp endaddrhi
            beq endaddrok
endaddfail: lda #ERRENDADDR
            ldx endaddrlo
            ldy endaddrhi
            jmp error
endaddrok:
    .endif; END_ADDRESS_API
.endmacro

.macro PRINTTESTNAME testname
            .local filllength
            .local name
            .local nameend

            DISABLE_MEMCONFIG_CHECK
            ldx #TESTNAMEPOS
            ldy #$01
            jsr setplotxy
            ldx #.lobyte(name)
            ldy #.hibyte(name)
            jsr plottext
    .if PLATFORM = diskio::platform::COMMODORE_16
            sec
            lda #.lobyte(COLRAM + $0c38)
            sbc POINTERS + $02
    .else
            lda #$10
    .endif
            sta filllength
            sec
            lda #.lobyte(emptytexte)
filllength = * + $01
            sbc #$00
            tax
            lda #.hibyte(emptytexte)
            sbc #$00
            tay
            jsr plottext
            ENABLE_MEMCONFIG_CHECK
            jmp nameend

name:       scrcode testname
            .byte $00
nameend:
.endmacro

.macro TESTUNCOMPRESSED testmacro, testname
            PRINTTESTNAME testname
            TEST testmacro, X_PIC1_UNCOMPRESSED, Y_PIC1_UNCOMPRESSED
            CHECKADDRESSES
            TEST testmacro, X_PIC2_UNCOMPRESSED, Y_PIC2_UNCOMPRESSED
            CHECKADDRESSES
.endmacro

.macro TESTCOMPRESSED testmacro, testname
            PRINTTESTNAME .concat(testname, "/", COMPEXT)
            TEST testmacro, X_PIC1_COMPRESSED, Y_PIC1_COMPRESSED
            TEST testmacro, X_PIC2_COMPRESSED, Y_PIC2_COMPRESSED
.endmacro

.macro CONSOLE text
            lda #.lobyte(text)
            ldy #.hibyte(text)
            jsr consoleout
.endmacro

.macro INITSTAT
            jsr initstat
.endmacro

.macro PRINTSTAT numbytes
            DISABLE_MEMCONFIG_CHECK
            ldx #.hibyte(numbytes)
            ldy #.lobyte(numbytes)
            jsr printstat
            ENABLE_MEMCONFIG_CHECK
.endmacro


.macro MEMCLEAR address, size
            lda #.hibyte(size)
            ldx #.hibyte(address)
            jsr memclear
.endmacro

.macro MEMCOPY source, dest, size
            lda #.hibyte(size)
            ldx #.hibyte(source)
            ldy #.hibyte(dest)
            jsr memcopy
.endmacro

.macro MEMCOMP buffer1, buffer2, size
            lda #.lobyte(size)
            sta POINTERS + $00
            lda #.hibyte(size)
            ldx #.hibyte(buffer1)
            ldy #.hibyte(buffer2)
            jsr memcomp
.endmacro


.segment "CODE"

            sei
            ldx #$ff
            txs

.if PLATFORM = diskio::platform::COMMODORE_16
            ; pucrunch turns off the screen during decompression
            lda #DISPLAY_DISABLE
            sta TED_CTRL1
            ; YAPE doesn't set FA to the current
            ; drive when using autostart
            lda FA
            and #.lobyte(~$01)
            cmp #$08
            beq :+
            lda #$08
            sta FA
:
.endif
            ldx #$04
:           lda $00,x
            sta zpbuffer,x
            inx
            bne :-

            lda #.lobyte(nmihandler)
.if PLATFORM <> diskio::platform::COMMODORE_16
            sta NMINV + $00
.endif
            sta NMI_VECTORLO
            lda #.hibyte(nmihandler)
.if PLATFORM <> diskio::platform::COMMODORE_16
            sta NMINV + $01
.endif
            sta NMI_VECTORHI

.if PLATFORM = diskio::platform::COMMODORE_16

            ldx #$00
            stx tod_hrs
            stx tod_mins
            stx tod_secs
            stx tod_frames

            lda #PAL_NTSC_MASK
            and TED_CTRL2
            bne :+
            inx; PAL
:           stx PALNTSC

            jsr waitvbl
            lda TED_CHARGEN_ADDR
            and #.lobyte(~CHARGEN_ADDR_MASK)
            ora #MAKE_CHARGEN_ADDR(CHARSET_ADDR_UPPERLOWER)
            sta TED_CHARGEN_ADDR
            lda #COLOUR_BLACK
            sta TED_BGCOLOUR
            sta TED_BORDERCOLOUR

.if COUPLED_TIMERS = 0
calibrate:  jsr readctr
            stx prevctr3lo
            sty prevctr3hi
            lda #$00
            sta adjustdiff
            jsr docalibrte
            sta adjustdiff
            jsr docalibrte
            cmp #$00
            bne calibrate
            cpx #$00
            bne calibrate
            cpy #$00
            bne calibrate
.endif
            jsr CLRSCR

            jsr waitvbl
            lda #TEXT_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3
            sta TED_CTRL1

            lda #PALETTE_DEFAULT
            sta PALETTE

            CONSOLE startupmsg

            jsr waitvbl
            lda TED_RASTERLINE
:           cmp TED_RASTERLINE
            beq :-
:           ldx TED_RASTERLINE
            lda TED_RASTERLINE_MSB
            cpx TED_RASTERLINE
            bne :-
            stx TED_IRQ_RASTERLINE
            lsr
            lda TED_IMR
            and #.lobyte(~IRQ_RASTERLINE_MSB)
            bcc :+
            ora #IRQ_RASTERLINE_MSB
:           sta TED_IMR

            lda #.lobyte(clockirq)
            sta CINV + $00
            lda #.hibyte(clockirq)
            sta CINV + $01
            lda #.lobyte(ramirq)
            sta IRQ_VECTOR + $00
            lda #.hibyte(ramirq)
            sta IRQ_VECTOR + $01
            lda #RASTER_IRQ
            sta TED_IMR
            lda TED_IRR
            sta TED_IRR
            cli

.else; PLATFORM <> diskio::platform::COMMODORE_16

            lda #MEMCONFIG_IO_KERNAL_BASIC
            sta IO_PORT

            lda #$00
            sta CIA1_TOD_HRS
            sta CIA1_TOD_MIN
            sta CIA1_TOD_SEC
            sta CIA1_TOD_10S

:           bit VIC2_CTRL1
            bmi :-
:           bit VIC2_CTRL1
            bpl :-
            lda #$37
:           cmp VIC2_RASTERLINE
            bne :-
            lda VIC2_CTRL1
            asl
            lda #$00
            rol
            sta PALNTSC; 0: NTSC, 1: PAL
            sta zpbuffer + PALNTSC
            lsr
            lda #TOD_FREQ_60HZ
            bcc :+
            lda #TOD_FREQ_50HZ
:           sta CIA1_CRA

            jsr waitvbl
            lda #DISPLAY_DISABLE
            sta VIC2_CTRL1
            lda #VIC2_MAKE_ADDR($0400, CHARSET_ADDR_UPPERLOWER)
            sta VIC2_ADDR
            lda #COLOUR_BLACK
            sta VIC2_BGCOLOUR
            sta VIC2_BORDERCOLOUR
            jsr CLRSCR
            jsr waitvbl
            lda #TEXT_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3
            sta VIC2_CTRL1

            CONSOLE startupmsg

.endif; PLATFORM <> diskio::platform::COMMODORE_16

            lda #.lobyte(brkhandler)
            sta CBINV + $00
            lda #.hibyte(brkhandler)
            sta CBINV + $01

            CONSOLE installmsg

            ENABLE_MEMCONFIG_CHECK

            LOADER_INSTALL

            stx DRIVETYPE
            bcc :+
.if LOAD_VIA_KERNAL_FALLBACK
            cmp #diskio::status::DEVICE_INCOMPATIBLE
            beq :+
            cmp #diskio::status::TOO_MANY_DEVICES
            beq :+
.endif
            jmp error

:           tya
            pha

.if PLATFORM = diskio::platform::COMMODORE_64
            lda CIA1_CRA
            and #.lobyte(~TOD_FREQ_MASK)
            ldx PAL_NTSC; $02a6, as set by KERNAL
            beq ntsc
            ora #TOD_FREQ_50HZ
ntsc:       sta CIA1_CRA

            ; error on PAL/NTSC detection mismatch
            lda #TOD_FREQ_60HZ
            ldx PALNTSC; as detected by own routine
            beq :+
            lda #TOD_FREQ_50HZ
:           eor CIA1_CRA
            bpl :+
            lda #ERRPALNTSC
            jmp error
:
.endif; PLATFORM = diskio::platform::COMMODORE_64

            DISABLE_MEMCONFIG_CHECK

            inc memchktype

            CONSOLE donemsg
            CONSOLE quotemsg
            pla
            tax
            lda $00,x
            ldy $01,x
            ; print version string
            jsr consoleout
            lda #$00
            sta QTSW; leave quote mode
            sta consoletime
            CONSOLE quotemsg + 1
            lda #$00
            sta QTSW; leave quote mode
            CONSOLE onmsg

            lda DRIVETYPE
            bpl :+
            lda #diskio::drivetype::DRIVE_1581 + 3; generic drive
:           and #%00001111
            tax
            lda drivemsgsl - diskio::drivetype::DRIVE_1541,x
            ldy drivemsgsh - diskio::drivetype::DRIVE_1541,x
            jsr consoleout

.if PLATFORM = diskio::platform::COMMODORE_16

            ; don't continue as long as any key is pressed
            ldx #ALL_COLUMNS
:           sei
            stx TED_KEYBOARD
            ldy TED_KEYBOARD
            stx TED_KEYBOARD
            cpy TED_KEYBOARD
            cli
            bne :-
            cpy #NO_ROWS
            bne :-

            jsr waitvbl

            lda #DISPLAY_DISABLE
            sta TED_CTRL1

            ldx #$00
:           lda #INTENSITY_4 | (INTENSITY_6 >> 4)
            sta COLRAM + $00,x
            sta COLRAM + $0100,x
            sta COLRAM + $0200,x
            sta COLRAM + $0300,x
            lda #(COLOUR_WHITE << 4) | COLOUR_WHITE
            sta COLRAM + $0400,x
            sta COLRAM + $0500,x
            sta COLRAM + $0600,x
            sta COLRAM + $0700,x
            lda #INTENSITY_6 | COLOUR_YELLOW
            sta COLRAM + $0800,x
            sta COLRAM + $0900,x
            sta COLRAM + $0a00,x
            sta COLRAM + $0b00,x
            lda #' '
            sta COLRAM + $0c00,x
            sta COLRAM + $0d00,x
            sta COLRAM + $0e00,x
            sta COLRAM + $0f00,x
            inx
            bne :-
            txa
            ldx #$06 * $08 - 1
:           sta COLRAM + $0c60,x
            sta COLRAM + $0c88,x
            sta COLRAM + $0d00,x; space character
            sta COLRAM + $0860,x
            dex
            bpl :-
            ldx #$11
            stx COLRAM + $0c38
            inx
            stx COLRAM + $0c39
            inx
            stx COLRAM + $0c3a
            inx
            stx COLRAM + $0c3b
            inx
            stx COLRAM + $0c3c
            inx
            stx COLRAM + $0c3d

            lda #$b0
            sta TED_CURSOR_LO
            lda #$00
            sta TED_CURSOR_HI
            lda #INTENSITY_0 | COLOUR_BLACK
            sta TED_BGCOLOUR0

            MEMCLEAR BITMAP, ALIGN BITMAP_SIZE, $0100

            jsr waitvbl

            lda #BITMAP_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3
            sta TED_CTRL1
            lda TED_BITMAP_ADDR
            and #.lobyte(~(BITMAP_ADDR_MASK | CHARSET_BITMAP_IN_ROM))
            ora #MAKE_BITMAP_ADDR(BITMAP)
            sta TED_BITMAP_ADDR
            lda TED_SCREEN_ADDR
            and #.lobyte(~SCREEN_ADDR_MASK)
            ora #MAKE_SCREEN_ADDR(COLRAM)
            sta TED_SCREEN_ADDR

            sei
            lda #$51 - IRQ_SLACK
            sta TED_IRQ_RASTERLINE
            lda #.lobyte(~IRQ_RASTERLINE_MSB)
            and TED_IMR
            sta TED_IMR
            lda #.lobyte(irq1)
            sta CINV + $00
            lda #.hibyte(irq1)
            sta CINV + $01
            cli

.else; PLATFORM <> diskio::platform::COMMODORE_16

            ; don't continue as long as any key is pressed
            lda #$00
:           sta CIA1_PRA
            ldx CIA1_PRB
            cpx CIA1_PRB
            bne :-
            inx
            bne :-

            jsr waitvbl

            lda #DISPLAY_DISABLE
            sta VIC2_CTRL1

            ldx #$00
:
    .if LOAD_TO_UPPER_MEM
            ENABLE_ALL_RAM
    .endif
            lda #$00
            sta SPRITES + $00,x
            sta SPRITES + $0100,x
            lda #(COLOUR_LIGHTGREY << 4) | COLOUR_DARKGREY
            sta COLRAM + $00,x
            sta COLRAM + $0100,x
            sta COLRAM + $0200,x
            sta COLRAM + $0300,x
    .if LOAD_TO_UPPER_MEM
            lda #MEMCONFIG_IO_KERNAL_BASIC
            sta IO_PORT
    .endif
            inx
            bne :-

            MEMCLEAR BITMAP, ALIGN BITMAP_SIZE, $0100

SPRITESXPOS = $18

            lda #SPRITESXPOS + SPRITE_WIDTH * 0
            sta SPRITE0_X
            lda #SPRITESXPOS + SPRITE_WIDTH * 1
            sta SPRITE1_X
            lda #SPRITESXPOS + SPRITE_WIDTH * 2
            sta SPRITE2_X
            lda #SPRITESXPOS + SPRITE_WIDTH * 3
            sta SPRITE3_X
            lda #SPRITESXPOS + SPRITE_WIDTH * 4
            sta SPRITE4_X
            lda #SPRITESXPOS + SPRITE_WIDTH * 5
            sta SPRITE5_X
            lda #SPRITESXPOS + SPRITE_WIDTH * 6
            sta SPRITE6_X
            lda #SPRITESXPOS + SPRITE_WIDTH * 7
            sta SPRITE7_X

            lda #$fb
            sta SPRITE0_Y
            sta SPRITE1_Y
            sta SPRITE2_Y
            sta SPRITE3_Y
            sta SPRITE4_Y
            sta SPRITE5_Y
            sta SPRITE6_Y
            sta SPRITE7_Y

            lda #%00000000
            sta SPRITES_X_MSB
            lda #%11111111
            sta VIC2_SPR_ENABLE
            lda #COLOUR_YELLOW
            sta VIC2_SPR0_COLOUR
            sta VIC2_SPR1_COLOUR
            sta VIC2_SPR2_COLOUR
            sta VIC2_SPR3_COLOUR
            sta VIC2_SPR4_COLOUR
            sta VIC2_SPR5_COLOUR
            sta VIC2_SPR6_COLOUR
            sta VIC2_SPR7_COLOUR

            ldx #MAKE_SPRITE_POINTER(SPRITES)
            stx SPRITESCR + SPRITE_POINTERS + 0
            inx
            stx SPRITESCR + SPRITE_POINTERS + 1
            inx
            stx SPRITESCR + SPRITE_POINTERS + 2
            inx
            stx SPRITESCR + SPRITE_POINTERS + 3
            inx
            stx SPRITESCR + SPRITE_POINTERS + 4
            inx
            stx SPRITESCR + SPRITE_POINTERS + 5
            inx
            stx SPRITESCR + SPRITE_POINTERS + 6
            inx
            stx SPRITESCR + SPRITE_POINTERS + 7

            lda #BITMAP_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3
            sta VIC2_CTRL1
            lda #VIC2_MAKE_ADDR COLRAM, BITMAP
            sta VIC2_ADDR
            SET_VIC_BANK VIC2_MAKE_BANK BITMAP

    .if LOAD_TO_UPPER_MEM
            lda #$10
            sta VIC2_RASTERLINE
            lda #.lobyte(irq0)
            sta CINV + $00
            lda #.hibyte(irq0)
    .else
            lda #$f9
            sta VIC2_RASTERLINE
            lda #.lobyte(irq1)
            sta CINV + $00
            lda #.hibyte(irq1)
    .endif
            sta CINV + $01
            lda #.lobyte(ramirq)
            sta IRQ_VECTORLO
            lda #.hibyte(ramirq)
            sta IRQ_VECTORHI
            lda #ALL_COLUMNS
            sta CIA1_PRA
            lda #RASTER_IRQ
            sta VIC2_IMR
            lda #CIA_CLR_INTF | EVERY_IRQ
            sta CIA1_ICR
            bit CIA1_ICR
            dec VIC2_IRR

.endif; PLATFORM <> diskio::platform::COMMODORE_16

            ; print the static settings
            lda #.lobyte(printstatc)
            sta IBSOUT + 0
            lda #.hibyte(printstatc)
            sta IBSOUT + 1

            cli

            PUSH_MEMCONFIG_AND_ENABLE_ALL_ROM

            ldx #$00
            ldy #$00
            jsr setplotxy

            ldx #.lobyte(paltext)
            ldy #.hibyte(paltext)
            lda PALNTSC
            bne :+
            ldx #.lobyte(ntsctext)
            ldy #.hibyte(ntsctext)
:           jsr plottext

            lda #','
            jsr CHROUT

            lda #' '
            jsr CHROUT
            lda #'#'
            jsr CHROUT
            lda FA
            cmp #10
            bcc :+
            lda #'1'
            jsr CHROUT
            sec
            lda FA
            sbc #10
:           ora #'0'
            jsr CHROUT
            lda #'/'
            jsr CHROUT

            lda DRIVETYPE
            bpl :+
            lda #diskio::drivetype::DRIVE_1581 + 3; generic drive
:           and #%00001111
            tax
            lda drivtypesl - diskio::drivetype::DRIVE_1541,x
            ldy drivtypesh - diskio::drivetype::DRIVE_1541,x
            tax
            jsr plottext

            lda #','
            jsr CHROUT
            lda #' '
            jsr CHROUT
            lda #'$'
            jsr CHROUT

            lda #.hibyte(__DISKIO_SIZE__)
            jsr plothex
            lda #.lobyte(__DISKIO_SIZE__)
            jsr plothex

            POP_MEMCONFIG

.if !INSTALL_ONLY

    .if PLATFORM = diskio::platform::COMMODORE_16
            sta TED_RAM_ENABLE
            lda #.lobyte(~FORCE_SINGLE_CLOCK)
            sei
            and TED_CHARGEN_ADDR
            sta TED_CHARGEN_ADDR
            cli
    .else
            lda #MEMCONFIG_IO
            sta IO_PORT
    .endif
            ENABLE_MEMCONFIG_CHECK

.if NO_SPRITES_OR_IRQ
    .if PLATFORM = diskio::platform::COMMODORE_64
            sei
            lda #$00
            sta VIC2_SPR_ENABLE
    .else
            lda #$00
            sta TED_IMR
            lda TED_IRR
            sta TED_IRR
            sei
    .endif
.endif

testloop:

.if LOAD_RAW_API
            ; --- load raw ---

    .macro LOAD_RAW xsource, ysource
            .local done

            INITSTAT

        .if LOAD_TO_API
            LOADRAW_LOADTO xsource, ysource, #.lobyte(BITMAP + $20), #.hibyte(BITMAP + $20)
        .else
            LOADRAW xsource, ysource
        .endif
            jcs error

            PRINTSTAT BITMAP_SIZE
done:
    .endmacro

            TESTUNCOMPRESSED LOAD_RAW, "LOADRAW"

    .if TEST_INVALID_PARAMETERS
            MEMCONFIG_BUFFER
            LOADRAW #.lobyte(bogusname), #.hibyte(bogusname)
            jcc error
            cmp #diskio::status::FILE_NOT_FOUND
            jne error
loadrwdone: MEMCONFIG_CHECK
    .endif
.endif

.if TEST_CUSTOM_DRIVE_CODE

            PRINTTESTNAME "CUSTOMDRVCODE"

            ; --- upload and run drivecode ---

            ; here should be a check for the loader still being installed,
            ; but sudden uninstallation only happens if turning off true
            ; drive emulation in VICE while running

            lda DRIVETYPE
            bpl :+
            jmp skipcustom; generic drive
:

    .if ONLY_1541_AND_COMPATIBLE = 0

            cmp #diskio::drivetype::DRIVE_1570
            beq upload1571
            cmp #diskio::drivetype::DRIVE_1571
            beq upload1571
            cmp #diskio::drivetype::DRIVE_1571CR
            beq upload1571
            jmp :+

            .import dcodinit71
            .import drivecode71
            .import drvcodebeg71
            .import drvcodeend71

upload1571: UPLOAD_CUSTOM_DRIVE_CODE drivecode71, drvcodebeg71, drvcodeend71 - drvcodebeg71, dcodinit71
            jmp skipcustom

:           cmp #diskio::drivetype::DRIVE_1581
            beq upload1581
            cmp #diskio::drivetype::DRIVE_CMD_FD_2000
            beq upload1581
            cmp #diskio::drivetype::DRIVE_CMD_FD_4000
            beq upload1581
            jmp upload1541

            .import dcodinit81
            .import drivecode81
            .import drvcodebeg81
            .import drvcodeend81

upload1581: UPLOAD_CUSTOM_DRIVE_CODE drivecode81, drvcodebeg81, drvcodeend81 - drvcodebeg81, dcodinit81
            jmp skipcustom

    .endif ; ONLY_1541_AND_COMPATIBLE = 0

            .import dcodinit41
            .import drivecode41
            .import drvcodebeg41
            .import drvcodeend41

upload1541: UPLOAD_CUSTOM_DRIVE_CODE drivecode41, drvcodebeg41, drvcodeend41 - drvcodebeg41, dcodinit41

skipcustom:

.endif; TEST_CUSTOM_DRIVE_CODE

.if MEM_DECOMP_API

            ; --- load a compressed file, then decompress after loading

    .macro LOADANDMEMDECOMP_COMMON xsource, ysource
            .local done

            INITSTAT

        .if LOAD_RAW_API
            LOADRAW xsource, ysource
            jcs error
        .else
            .error "To test memory decompression, please set LOAD_RAW_API to non-0"
        .endif

            clc
done:
    .endmacro

    .macro LOADANDMEMDECOMP xsource, ysource
            .local done

        .if VERIFY
            .local verifyok

            MEMCLEAR BITMAP, ALIGN BITMAP_SIZE, $0100

            LOADANDMEMDECOMP_COMMON xsource, ysource

            PUSH_MEMCONFIG_AND_ENABLE_ALL_RAM
            MEMCOPY BITMAP + ALIGN (BITMAP_SIZE >> 1), $0100, COMPDATA + ALIGN BITMAP_SIZE, $0100, ALIGN BITMAP_SIZE, $0100
            POP_MEMCONFIG
            MEMCLEAR BITMAP, ALIGN BITMAP_SIZE, $0100

            LOADANDMEMDECOMP_COMMON xsource, ysource
            PUSH_MEMCONFIG_AND_ENABLE_ALL_RAM

            MEMCOMP BITMAP + ALIGN (BITMAP_SIZE >> 1), $0100, COMPDATA + ALIGN BITMAP_SIZE, $0100, ALIGN BITMAP_SIZE, $0100

            php
            pla
            tax
            POP_MEMCONFIG
            txa
            pha
            plp
            beq verifyok
            jsr verifyfail
verifyok:
        .else
            LOADANDMEMDECOMP_COMMON xsource, ysource
        .endif

        .if LOAD_TO_UPPER_MEM
            PUSH_MEMCONFIG_AND_ENABLE_ALL_RAM
        .endif

        .if MEM_DECOMP_TO_API
            MEMDECOMP_TO loadaddrlo, loadaddrhi, #.lobyte(BITMAP - $10), #.hibyte(BITMAP - $10)
        .else
            MEMDECOMP loadaddrlo, loadaddrhi
        .endif

        .if LOAD_TO_UPPER_MEM
            POP_MEMCONFIG
        .endif

            PRINTSTAT BITMAP_SIZE
done:
    .endmacro

            TESTCOMPRESSED LOADANDMEMDECOMP, "MEMDECOMP"
.endif

.if LOAD_COMPD_API

            ; --- load with decompression ---

    .macro LOADCOMPRESSED xsource, ysource
            .local done

            INITSTAT

        .if LOAD_TO_API
            LOADCOMPD_RELTO xsource, ysource, #.lobyte(-$10), #.hibyte(-$10)
        .else
            LOADCOMPD xsource, ysource
        .endif
            jcs error

            PRINTSTAT BITMAP_SIZE
done:
    .endmacro

            TESTCOMPRESSED LOADCOMPRESSED, "LOADCOMPD"

    .if TEST_INVALID_PARAMETERS
            MEMCONFIG_BUFFER
            LOADCOMPD #.lobyte(bogusname), #.hibyte(bogusname)
            jcc error
            cmp #diskio::status::FILE_NOT_FOUND
            jne error
loadcmpdun: MEMCONFIG_CHECK
    .endif
.endif; LOAD_COMPD_API

            jsr waitvbl
            inc BORDERCOLOUR
            jmp testloop

uninstcheck:
.if PAUSE_BETWEEN_LOADS
            lda #PAUSE_BETWEEN_LOADS
            ldx #$00
            ldy #$00
:           dex
            bne :-
            dey
            bne :-
            sec
            sbc #$01
            bne :-
.endif

.if !UNINSTALL_API
            rts
.else

    .if PLATFORM = diskio::platform::COMMODORE_16
            lda #ALL_COLUMNS
            sta TED_KEYBOARD
            lda TED_KEYBOARD
            cmp #NO_ROWS
    .else
            inc CIA1_PRB; uninstall on keypress
    .endif
            bne off
            rts

off:
            LOADER_UNINSTALL
.endif; !UNINSTALL_API

.else ; INSTALL_ONLY
    .if UNINSTALL_API
:
        .if PLATFORM = diskio::platform::COMMODORE_16
            lda #ALL_COLUMNS
            sta TED_KEYBOARD
            lda TED_KEYBOARD
            cmp #NO_ROWS
        .else
            inc CIA1_PRB; uninstall on keypress
        .endif
            beq :-
            LOADER_UNINSTALL
    .endif; UNINSTALL_API
.endif; INSTALL_ONLY

            DISABLE_MEMCONFIG_CHECK

.if UNINSTALL_API
            lda #MSGUNINST
            jsr printmsg
.endif
            jsr waitvbl
            lda #COLOUR_GREEN
            sta BORDERCOLOUR

halt:       jmp halt

waitvbl:
.if PLATFORM <> diskio::platform::COMMODORE_16
            lda PALNTSC
            cmp #$01
.endif
            WAIT_VBL
            rts

initstat:
.if PLATFORM = diskio::platform::COMMODORE_16
    .if COUPLED_TIMERS
            ldx #$ff

            lda TED_CHARGEN_ADDR
            and #FORCE_SINGLE_CLOCK
            pha
            bne :+
            lda #FORCE_SINGLE_CLOCK
            php
            sei
            ora TED_CHARGEN_ADDR
            sta TED_CHARGEN_ADDR
            plp

:           stx TED_COUNTER1_LO
            stx TED_COUNTER1_HI
            stx TED_COUNTER2_LO
            stx TED_COUNTER2_HI

            pla
            bne :+
            lda #.lobyte(~FORCE_SINGLE_CLOCK)
            php
            sei
            and TED_CHARGEN_ADDR
            sta TED_CHARGEN_ADDR
            plp
:
    .else
            jsr readcycles
            sta prvcycslo
            stx prvcycsmid
            sty prvcycshi
    .endif
.else
            lda #$ff
            sta CIA1_TA_LO
            sta CIA1_TB_LO
            sta CIA1_TA_HI
            sta CIA1_TB_HI
            lda CIA1_CRA
            and #.lobyte(~(COUNT_CNT | ONE_SHOT))
            ora #FORCE_LOAD | CONTINUOUS | TIMER_START
            sta CIA1_CRA
            lda #FORCE_LOAD | CONTINUOUS | COUNT_TA_UNDF | TIMER_START
            sta CIA1_CRB
.endif
            rts

            ; in: x/y - number of bytes transferred
printstat:
.if PLATFORM = diskio::platform::COMMODORE_16

            stx statxbuf
            sty statybuf

    .if COUPLED_TIMERS
            lda TED_CHARGEN_ADDR
            and #FORCE_SINGLE_CLOCK
            pha
            bne :+
            lda #FORCE_SINGLE_CLOCK
            php
            sei
            ora TED_CHARGEN_ADDR
            sta TED_CHARGEN_ADDR
            plp

:           ldy TED_COUNTER1_HI
            ldx TED_COUNTER2_HI
            lda TED_COUNTER1_LO
            sta :+ + $01
            lda TED_COUNTER2_LO
            cpy TED_COUNTER1_HI
            bne :-
            cpx TED_COUNTER2_HI
            bne :-
            pha; TED_COUNTER2_LO
            eor #$ff
            sbc #70; empirically found offset
            sta numcycles + $00
            txa; TED_COUNTER2_HI
            eor #$ff
            sbc #$00
            sta numcycles + $01
            pla; TED_COUNTER2_LO
:           sbc #$00; TED_COUNTER1_LO
            sta numcycles + $02
            lda #$00
            sta numcycles + $03

            pla
            bne :+
            lda #.lobyte(~FORCE_SINGLE_CLOCK)
            php
            sei
            and TED_CHARGEN_ADDR
            sta TED_CHARGEN_ADDR
            plp
:
    .else
        .if NO_SPRITES_OR_IRQ
            ; no speed measurement without IRQs nor coupled timers
            rts
        .endif

            jsr diffcycles
    .endif
            PUSH_MEMCONFIG_AND_ENABLE_ALL_ROM
.else
            lda #.lobyte(~TIMER_STOP)
            and CIA1_CRA
            sta CIA1_CRA
            lda #TIMER_STOP
            sta CIA1_CRB

            PUSH_MEMCONFIG_AND_ENABLE_ALL_ROM

            sec
            lda #$ff - 31; subtract overhead
            sbc CIA1_TA_LO
            sta numcycles + $00
            lda #$ff
            sbc CIA1_TA_HI
            sta numcycles + $01
            lda #$ff
            sbc CIA1_TB_LO
            sta numcycles + $02
            lda #$ff
            sbc CIA1_TB_HI
            sta numcycles + $03

            stx statxbuf
            sty statybuf
.endif
            ; print kB/s figure

.if NO_SPRITES_OR_IRQ
            lda #.lobyte(printstatb)
            sta IBSOUT + 0
            lda #.hibyte(printstatb)
            sta IBSOUT + 1

            lda #.lobyte(BITMAP + (8 * (SCREEN_ROWS - 1) * SCREEN_COLUMNS))
            pha
            lda #.hibyte(BITMAP + (8 * (SCREEN_ROWS - 1) * SCREEN_COLUMNS))
            pha
.else
            ldx #$00
            ldy #$01
            jsr setplotxy
            ; buffer pointer to stats
            lda POINTERS + $02
            pha
            lda POINTERS + $03
            pha
.endif
            jsr swapzp

statxbuf = * + $01
            lda #$00; number of bytes transferred
statybuf = * + $01
            ldy #$00
            jsr GIVAYF; fac#1 = numbytes
            ldx #.lobyte(floatbuff)
            ldy #.hibyte(floatbuff)
            jsr MOV2M; floatbuff = fac#1 = numbytes

            lda numcycles + $00
            ora numcycles + $01
            ora numcycles + $02
            ora numcycles + $03
            bne :+
            inc numcycles + $00
:           INT32TOFAC numcycles; fac#1 = numcycles
            ldx #.lobyte(floatbuff2)
            ldy #.hibyte(floatbuff2)
            jsr MOV2M; floatbuff2 = fac#1 = numcycles

            lda PALNTSC
            jeq ntsckbs
            INT32TOFAC numccpal; fac#1 = cyclespersecond
            jmp :+
ntsckbs:    INT32TOFAC numccntsc; fac#1 = cyclespersecond
:           lda #.lobyte(floatbuff)
            ldy #.hibyte(floatbuff)
            jsr FMULT; fac#1 *= numbytes
            jsr MOVAF; fac#2 = fac#1
            ldx #$00
            lda #.lobyte(floatbuff2)
            ldy #.hibyte(floatbuff2)
            jsr FDIVM; fac#1 = fac#2 / numcycles
            jsr MOVAF; fac#2 = fac#1

            lda #.hibyte(1000)
            ldy #.lobyte(1000)
            jsr GIVAYF; fac#1 = 1000
            lda FACEXP
            jsr FDIVT; fac#1 = fac#2 / fac#1

            ; fac#1 = cyclespersecond * numbytes / numcycles / 1000
            jsr FOUT

            ldx INDEX1 + $00
            stx FRFTOP + $00
            ldx INDEX1 + $01
            stx FRFTOP + $01

            jsr STRLIT

            ; remove leading space from number string
            ldy #STRDSC_LEN
            sec
            lda (STRDSC),y; string length -= 1
            sbc #$01
            sta (STRDSC),y
            sta statlen
            iny
           ;ldy #STRDSC_PTR
            clc
            lda (STRDSC),y; string pos += 1
            adc #$01
            sta (STRDSC),y

            ; truncate to 2 decimals (floored rounding)
            sta POINTERS + $02
            iny
            lda (STRDSC),y
            sta POINTERS + $03
.if PLATFORM = diskio::platform::COMMODORE_16
            PUSH_MEMCONFIG_AND_ENABLE_ALL_RAM
.endif
            ldy #.lobyte(-$01)
:           iny
statlen = * + 1
            cpy #$00
            bcs truncdone
            lda (POINTERS + $02),y
            cmp #'.'
            bne :-
            iny
            iny
            iny
            cpy #$07
            bcs truncdone
            sty statlen
            tya
            ldy #STRDSC_LEN
            sta (STRDSC),y
truncdone:
.if PLATFORM = diskio::platform::COMMODORE_16
            POP_MEMCONFIG
.endif

notrunc:    ; restore pointer to sprite bitmap
            pla
            sta POINTERS + $03
            pla
            sta POINTERS + $02

            jsr STROUT + $03

            lda POINTERS + $02
            pha
            lda POINTERS + $03
            pha
            jsr swapzp
            pla
            sta POINTERS + $03
            pla
            sta POINTERS + $02

            ldx #.lobyte(kbstext)
            ldy #.hibyte(kbstext)
            jsr plottext

.if NO_SPRITES_OR_IRQ
            lda #.lobyte(printstatc)
            sta IBSOUT + 0
            lda #.hibyte(printstatc)
            sta IBSOUT + 1
.else
            sec
            lda #TESTNAMEPOS; num chars
            sbc statlen
            sbc #$05; length of " kB/s"
            tax
            beq :++
            bmi :++
:           lda #' '
            jsr CHROUT
            dex
            bne :-
:
.endif
            POP_MEMCONFIG
            rts

            ; debug printout, called from resident loader code
            ; in: a - value to print
            ;     x - x-coordinate
            .export PRINTHEX

PRINTHEX:   sta hexvalue

            php
            pha
            txa
            pha
            tya
            pha
            PUSH_MEMCONFIG_AND_ENABLE_ALL_ROM

           ;ldx #xcoord
            ldy #$01
            jsr setplotxy

hexvalue = * + $01
            lda #$00
            jsr gethex
            pha
            txa
            jsr CHROUT
            pla
            jsr CHROUT
            
            POP_MEMCONFIG
            pla
            tay
            pla
            tax
            pla
            plp
            rts

error:      sei
            sta errora + $01
            stx errorx + $01
            sty errory + $01
            DISABLE_MEMCONFIG_CHECK
.if PLATFORM = diskio::platform::COMMODORE_16
            sta TED_ROM_ENABLE
.else
            lda #MEMCONFIG_IO_KERNAL
            sta IO_PORT
.endif
            tsx
            dex
            dex
            txs
            pla
            sec
            sbc #$02
            sta erroraddrl + 1
            pla
            cli
            sbc #$00
            sta erroraddrh + 1

errora:     lda #$00
errorx:     ldx #$00
errory:     ldy #$00
            ; print error message
            jsr printmsgex

errorhalt:  jsr waitvbl
            lda #COLOUR_RED
            sta BORDERCOLOUR
            lda #.lobyte(~MULTICOLOUR_MODE)
.if PLATFORM = diskio::platform::COMMODORE_16
            and TED_CTRL2
            sta TED_CTRL2
.else
            and VIC2_CTRL2
            sta VIC2_CTRL2
.endif
errorhaltl: ldy #$08
:           jsr waitvbl
            dey
            bne :-
            lda #COLOUR_RED ^ COLOUR_BLACK
            eor BORDERCOLOUR
            sta BORDERCOLOUR
            jmp errorhaltl

swapzp:     ldx #$04
:           lda zpbuffer,x
            ldy $00,x
            sta $00,x
            tya
            sta zpbuffer,x
            inx
            bne :-
            rts

consoleout: sta POINTERS + $00
            sty POINTERS + $01
            PUSH_MEMCONFIG_AND_ENABLE_ALL_ROM
            lda #DEVICE_SCREEN
            sta DFLTO
.if PLATFORM <> diskio::platform::COMMODORE_16
            cli
.endif
consoletime = * + $01
            lda #$00
            beq notime; don't print time if 0
            lda #$00
            sta consoletime
            lda #PETSCII_LIGHTBLUE
            jsr CHROUT
            lda #'['
            jsr CHROUT
.if PLATFORM = diskio::platform::COMMODORE_16
            lda tod_hrs
.else
            lda CIA1_TOD_HRS
.endif
            jsr putconbyte
            lda #':'
            jsr CHROUT
.if PLATFORM = diskio::platform::COMMODORE_16
            lda tod_mins
.else
            lda CIA1_TOD_MIN
.endif
            jsr putconbyte
            lda #':'
            jsr CHROUT
.if PLATFORM = diskio::platform::COMMODORE_16
            lda tod_secs
.else
            lda CIA1_TOD_SEC
.endif
            jsr putconbyte
.if PLATFORM <> diskio::platform::COMMODORE_16
            bit CIA1_TOD_10S; to free the latch
.endif
            lda #']'
            jsr CHROUT
            lda #' '
            jsr CHROUT
notime:

.if PLATFORM <> diskio::platform::COMMODORE_16
            lda #MEMCONFIG_IO_KERNAL
            sta IO_PORT
.endif
            ldy #$00
:
            lda (POINTERS + $00),y
            beq :++
            pha
            jsr CHROUT
            pla
            cmp #PETSCII_RETURN
            bne :+
            inc consoletime
:           iny
            bne :--
:

.if PLATFORM <> diskio::platform::COMMODORE_16
            sei
.endif
            POP_MEMCONFIG
            rts

putconword: txa
            pha
            tya
            jsr putconbyte
            pla

putconbyte: tax
            lsr
            lsr
            lsr
            lsr
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1
:           adc #'0'
            jsr CHROUT
            txa
            and #%00001111
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1
:           adc #'0'
            jmp CHROUT
            
printmsgex: sec
            SKIPBYTE

printmsg:   clc
            stx printx
            sty printy

            tax
            lda #$00
            tay
clearline:  sta BITMAP + $00,y
            sta BITMAP + $40,y
            iny
            bne clearline

            lda errormsgsl,x
            sta POINTERS + $00
            lda errormsgsh,x
            sta POINTERS + $01
            lda #.lobyte(BITMAP)
            sta POINTERS + $02
            lda #.hibyte(BITMAP)
            sta POINTERS + $03

            lda #BITMAP_MODE
.if PLATFORM = diskio::platform::COMMODORE_16
            and TED_CTRL1
.else
            and VIC2_CTRL1
.endif
            sta putchar + $01
            bne :+
.if PLATFORM = diskio::platform::COMMODORE_16
            lda TED_SCREEN_ADDR
            and #.lobyte(~SCREEN_ADDR_MASK)
            ora #MAKE_SCREEN_ADDR($0800)
            sta TED_SCREEN_ADDR
            lda #.hibyte($0c00)
.else
            lda #VIC2_MAKE_ADDR($0400, CHARSET_ADDR_UPPERLOWER)
            sta VIC2_ADDR
            lda #.hibyte($0400)
.endif
            sta POINTERS + $03

:
.if PLATFORM = diskio::platform::COMMODORE_16
            sta TED_ROM_ENABLE
.else
            lda #IO_PORT_DIRECTION_DEFAULT
            sta IO_PORT_DIRECTION
            lda #MEMCONFIG_CHARGEN
            sta IO_PORT
.endif
            txa
            pha
            eor #MSGUNINST
            beq doprintmsg
            bcc :+
            lda #$00; @
            jsr putchar
erroraddrh: lda #$00
            jsr puthex
erroraddrl: lda #$00
            jsr puthexnum
            lda #'/'
            jsr putchar

:           pla
            pha
            jsr puthex

            lda #'('
            jsr putchar
printx = * + 1
            lda #$00
            jsr puthex
            lda #','
            jsr putchar
printy = * + 1
            lda #$00
            jsr puthex
            lda #')'
            jsr putchar
            lda #':'
            jsr putchar
            lda #' '
            jsr putchar

            lda #$00
            sta doprintmsg + 1
doprintmsg: ldy #$00
            lda (POINTERS + $00),y
            beq :+
            jsr putchar
            inc doprintmsg + 1
            bne doprintmsg
:           pla
            cmp #ERRBRKOPC
            bne :+

brkaddrhi = * + $01
            lda #$00
            jsr puthexnum
brkaddrlo = * + $01
            lda #$00
            jsr puthexnum
            lda #'.'
            jsr putchar

:
.if PLATFORM = diskio::platform::COMMODORE_16
            sta TED_RAM_ENABLE
.else
            lda #MEMCONFIG_IO
            sta IO_PORT
.endif
            rts

gethex:     tay
            lsr
            lsr
            lsr
            lsr
            cmp #10
            bcc :+
            adc #.lobyte('a' - '0' - 10 - 1 - 64)
:           adc #'0'
            tax
            tya
            and #%00001111
            cmp #10
            bcc :+
            adc #.lobyte('a' - '0' - 10 - 1 - 64)
:           adc #'0'
            rts


puthex:     tax
            lda #'$'
            jsr putchar
            txa
puthexnum:  jsr gethex
            pha
            txa
            jsr putchar
            pla

putchar:    ldy #$00
            bne plotbitmap

            ; print char to screen
            sta (POINTERS + $02),y
            lda POINTERS + $03
            pha
            and #%00000011
.if PLATFORM = diskio::platform::COMMODORE_16
            ora #.hibyte($0800)
            sta POINTERS + $03
            lda #COLOUR_WHITE | INTENSITY_7
            sta (POINTERS + $02),y
.else
            ora #.hibyte(VIC2_COLOURRAM)
            sta POINTERS + $03
            lda IO_PORT
            pha
            lda #MEMCONFIG_IO
            sta IO_PORT
            lda #COLOUR_WHITE
            sta (POINTERS + $02),y
            pla
            sta IO_PORT
.endif
            pla
            sta POINTERS + $03
            inc POINTERS + $02
            rts

            ; plot char to bitmap
plotbitmap: ldy #$00
            sty POINTERS + $05
            asl
            rol POINTERS + $05
            asl
            rol POINTERS + $05
            asl
            rol POINTERS + $05
            sta POINTERS + $04
            lda #.hibyte(ROM_CHARSET_UPLOW)
            adc POINTERS + $05
            sta POINTERS + $05
            ldy #$07
:           lda (POINTERS + $04),y
            sta (POINTERS + $02),y
            dey
            bpl :-
            clc
            lda #$08
            adc POINTERS + $02
            sta POINTERS + $02
            bcc :+
            inc POINTERS + $03
:           rts


            ; in: a - length in pages
            ;     x - destination hibyte
memclear:   stx POINTERS + $01
            ldy #$00
            sty POINTERS + $00
            tax
            PUSH_MEMCONFIG_AND_ENABLE_ALL_RAM
            tya
memclrlp:   sta (POINTERS + $00),y
            iny
            bne memclrlp
            inc POINTERS + $01
            dex
            bne memclrlp
            POP_MEMCONFIG
            rts

            ; in: a - length in pages
            ;     x - source hibyte
            ;     y - destination hibyte
memcopy:    stx POINTERS + $01
            sty POINTERS + $03
            ldy #$00
            sty POINTERS + $00
            sty POINTERS + $02
            tax
memcpylp:   lda (POINTERS + $00),y
            sta (POINTERS + $02),y
            iny
            bne memcpylp
            inc POINTERS + $01
            inc POINTERS + $03
            dex
            bne memcpylp
            rts

.if VERIFY

verifyfail: tya
            jsr gethex
            sta verifysrc + $03
            stx verifysrc + $02
            sta verifydest + $03
            stx verifydest + $02
            lda POINTERS + $02
            jsr gethex
            sta verifysrc + $01
            stx verifysrc + $00
            lda POINTERS + $04
            jsr gethex
            sta verifydest + $01
            stx verifydest + $00
            pla
            pla
            lda #MSGVERIFAIL
            jmp error

            .import __VERIFYBUFFER_LOAD__
COMPDATA = __VERIFYBUFFER_LOAD__
COMPBMP  = COMPDATA

            .pushseg
            .segment "VERIFYBUFFER"
            .popseg

piccopy:
.if LOAD_TO_UPPER_MEM
            PUSH_MEMCONFIG_AND_ENABLE_ALL_RAM
.endif
            MEMCOPY BITMAP, COMPBMP, ALIGN BITMAP_SIZE, $0100
.if LOAD_TO_UPPER_MEM
            POP_MEMCONFIG
.endif
            rts

piccomp:    
.if LOAD_TO_UPPER_MEM
            PUSH_MEMCONFIG_AND_ENABLE_ALL_RAM
.endif
            MEMCOMP BITMAP, COMPBMP, ALIGN BITMAP_SIZE, $0100
.if LOAD_TO_UPPER_MEM
            php
            pla
            tax
            POP_MEMCONFIG
            txa
            pha
            plp
.endif
            rts

            ; in: POINTERS + $00 - lobyte of length
            ;     a - hibyte of length
            ;     x - memory area 1 hibyte
            ;     y - memory area 2 hibyte
memcomp:    stx POINTERS + $02
            sty POINTERS + $04
            ldy #$00
            sty POINTERS + $01
            sty POINTERS + $03
            tax
memcmplp:   lda (POINTERS + $01),y
            cmp (POINTERS + $03),y
            bne memcmpne
            iny
            bne memcmplp
            inc POINTERS + $02
            inc POINTERS + $04
            dex
            bne memcmplp
            lda POINTERS + $00
            beq memcmpne
memcmpl1:   lda (POINTERS + $01),y
            cmp (POINTERS + $03),y
            bne memcmpne
            iny
            cpy POINTERS + $00
            bne memcmpl1
memcmpne:   rts

.endif; VERIFY

plothex:    pha
            lsr
            lsr
            lsr
            lsr
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1
:           adc #'0'
            tax
            PUSH_MEMCONFIG_AND_ENABLE_ALL_ROM
            txa
            jsr CHROUT
            POP_MEMCONFIG
            pla
            and #%00001111
            cmp #10
            bcc :+
            adc #'a' - '0' - 10 - 1
:           adc #'0'
            jmp CHROUT

plottext:   PUSH_MEMCONFIG_AND_ENABLE_ALL_ROM
            stx POINTERS + $00
            sty POINTERS + $01

            ldy #$00
plotloop:   lda (POINTERS + $00),y
            beq plotend
            cmp #$1f; scrcode "_" fix
            bne :+
            lda #$64
:           jsr CHROUT
            iny
            bne plotloop

plotend:    POP_MEMCONFIG
            rts

.if PLATFORM = diskio::platform::COMMODORE_16

setplotxy:  txa
            pha
            lda #.lobyte(COLRAM + $0fe8)
            ldx #.hibyte(COLRAM + $0fe8)
            cpy #$00
            beq :+
            ; second stats char line
            lda #.lobyte(COLRAM + $0c10)
            ldx #.hibyte(COLRAM + $0c10)
:           sta POINTERS + $02
            stx POINTERS + $03
            pla
            clc
            adc POINTERS + $02
            sta POINTERS + $02
            bcc :+
            inc POINTERS + $03
:           rts

printstatb: stx printxbuf
            sty printybuf
            jsr plotbitmap
printxbuf = * + $01
            ldx #$00
            jmp printstatr

printstatc: sty printybuf
            ldy #$00
            sta (POINTERS + $02),y
            inc POINTERS + $02
            bne :+
            inc POINTERS + $03
:
printstatr: 
printybuf = * + $01
            ldy #$00
            clc
            rts

.else

setplotxy:  clc
            lda plotxposlo,x
            adc plotypos,y
            sta POINTERS + $02
            lda plotxposhi,x
            sta POINTERS + $03
            rts


plotxposlo: .byte .lobyte(SPRITES + $00), .lobyte(SPRITES + $01), .lobyte(SPRITES + $02)
            .byte .lobyte(SPRITES + $40), .lobyte(SPRITES + $41), .lobyte(SPRITES + $42)
            .byte .lobyte(SPRITES + $80), .lobyte(SPRITES + $81), .lobyte(SPRITES + $82)
            .byte .lobyte(SPRITES + $c0), .lobyte(SPRITES + $c1), .lobyte(SPRITES + $c2)
            .byte .lobyte(SPRITES + $0100), .lobyte(SPRITES + $0101), .lobyte(SPRITES + $0102)
            .byte .lobyte(SPRITES + $0140), .lobyte(SPRITES + $0141), .lobyte(SPRITES + $0142)
            .byte .lobyte(SPRITES + $0180), .lobyte(SPRITES + $0181), .lobyte(SPRITES + $0182)
            .byte .lobyte(SPRITES + $01c0), .lobyte(SPRITES + $01c1), .lobyte(SPRITES + $01c2)
plotxposhi: .byte .hibyte(SPRITES + $00), .hibyte(SPRITES + $01), .hibyte(SPRITES + $02)
            .byte .hibyte(SPRITES + $40), .hibyte(SPRITES + $41), .hibyte(SPRITES + $42)
            .byte .hibyte(SPRITES + $80), .hibyte(SPRITES + $81), .hibyte(SPRITES + $82)
            .byte .hibyte(SPRITES + $c0), .hibyte(SPRITES + $c1), .hibyte(SPRITES + $c2)
            .byte .hibyte(SPRITES + $0100), .hibyte(SPRITES + $0101), .hibyte(SPRITES + $0102)
            .byte .hibyte(SPRITES + $0140), .hibyte(SPRITES + $0141), .hibyte(SPRITES + $0142)
            .byte .hibyte(SPRITES + $0180), .hibyte(SPRITES + $0181), .hibyte(SPRITES + $0182)
            .byte .hibyte(SPRITES + $01c0), .hibyte(SPRITES + $01c1), .hibyte(SPRITES + $01c2)
plotypos:   .byte $00 * $03, $08 * $03, $10 * $03

printstatb: sta POINTERS + $05
            txa
            pha
            tya
            pha
            lda IO_PORT
            pha
            lda #MEMCONFIG_CHARGEN
            sta IO_PORT

            lda POINTERS + $05
            jsr plotbitmap
            jmp printstatr

printstatc: sta POINTERS + $05
            txa
            pha
            tya
            pha
            lda IO_PORT
            pha
            lda #MEMCONFIG_CHARGEN
            sta IO_PORT

            lda POINTERS + $05
            ldy #$00
            sty POINTERS + $05
            asl
            rol POINTERS + $05
            asl
            rol POINTERS + $05
            asl
            sta POINTERS + $04
            lda POINTERS + $05
            rol
            adc #.hibyte(CHARSET_ADDR_UPPERLOWER)
            sta POINTERS + $05

            ldx #$08
:           lda (POINTERS + $04),y
            sta (POINTERS + $02),y
            iny
            inc POINTERS + $02
            inc POINTERS + $02
            dex
            bne :-

            lda POINTERS + $02
            sbc #$0e
            sta POINTERS + $02

            and #%00111111
            cmp #$03
            beq :+
            cmp #$03 + $08 * 3
            bne :++
:           lda #$40 - 3 - 1
            adc POINTERS + $02
            sta POINTERS + $02
            bcc :+
            inc POINTERS + $03
:
printstatr: pla
            sta IO_PORT
            pla
            tay
            pla
            tax
            rts

.endif

.if PLATFORM = diskio::platform::COMMODORE_16

ramirq:     pha
            txa
            pha
            tya
            pha
            sta TED_ROM_BANK_0
            tsx
            inx
            inx
            inx
            inx
            lda STACK,x
            and #BREAK
            bne :+
            jmp (CINV)
:           jmp (CBINV)

clockirq:   LOADER_IRQ_HANDLER_PROLOGUE irqendy

            jsr update_clk
            jmp irqendx

            ; IRQ handler to switch on bitmap mode after the upper border area

irq0:       LOADER_IRQ_HANDLER_PROLOGUE irqendy

            lda #INVALID_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3
            sta TED_CTRL1; set scrolly

    .if IRQ_SLACK > 0
:           lda TED_RASTERLINE_MSB
            lsr
            bcs :-
    .endif
            lda #$01
:           cmp TED_RASTERLINE
            bcs :-
            nop
            bit $24
            STABILIZE_RASTER

            ldx #$08
:           dex
            bne :-

            lda #BITMAP_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3
            sta TED_CTRL1; disable invalid mode

            lda #.lobyte(~IRQ_RASTERLINE_MSB)
            and TED_IMR
            sta TED_IMR

            ; the last piture display line is 200 + 3 - 1 = 202 ($ca),
            ; at the end of it, set the raster count back
            ; to 203 - 17 = 186 = $ba (NTSC) or
            ; to 203 - 33 = 170 = $aa (PAL)
            lda #$c7 - IRQ_SLACK
            ldx #.lobyte(irq1)
            ldy #.hibyte(irq1)
            jmp irqend

            ; IRQ handler to extend the screen

irq1:       LOADER_IRQ_HANDLER_PROLOGUE irqendy

            lda TED_CHARGEN_ADDR
            pha
            and #.lobyte(~FORCE_SINGLE_CLOCK)
            sta TED_CHARGEN_ADDR

            lda #$10
            ldx PALNTSC
            beq :+
            lda #$18
:           sta lineoffset

            lda #$c9
:           cmp TED_RASTERLINE
            bcs :-
            ; $ca
            nop
            nop
            STABILIZE_RASTER

            lda #BITMAP_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_4
            sta TED_CTRL1; switch to text mode
            lda TED_SCREEN_ADDR
            and #.lobyte(~SCREEN_ADDR_MASK)
            ora #MAKE_SCREEN_ADDR(COLRAM + $0800)
            sta TED_SCREEN_ADDR
            sec
            lda TED_RASTERLINE; $ca
lineoffset = * + 1
            sbc #$00; $10 (NTSC) or $18 (PAL)
            sta TED_RASTERLINE
            ldx #$08
:           dex
            bne :-
            lda #CHARSET_BITMAP_IN_ROM
            ora TED_BITMAP_ADDR
            sta TED_BITMAP_ADDR
            lda #INVALID_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_4
            sta TED_CTRL1; hide garbage
            lda TED_CHARGEN_ADDR
            and #.lobyte(~CHARGEN_ADDR_MASK)
            ora #MAKE_CHARGEN_ADDR(CHARSET_ADDR_UPPERLOWER)
            sta TED_CHARGEN_ADDR

            lda #TEXT_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_4
            sta TED_CTRL1; switch to text mode
            ldx #$07
            stx TED_VERTSUBCOUNT
            lda #.lobyte(~MULTICOLOUR_MODE)
            and TED_CTRL2
            sta TED_CTRL2

            sec
            lda #$da - IRQ_SLACK
            sbc lineoffset
            sta TED_IRQ_RASTERLINE
            lda #.lobyte(irq3)
            sta CINV + $00
            lda #.hibyte(irq3)
            sta CINV + $01
            lda #RASTER_IRQ
            sta TED_IRR
            pla
            and #.lobyte(~CHARGEN_ADDR_MASK)
            ora #MAKE_CHARGEN_ADDR(CHARSET_ADDR_UPPERLOWER)
            sta TED_CHARGEN_ADDR
            jmp irqendy

            ; this handler is called just before the 2nd extra char row has finished displaying

irq3:       LOADER_IRQ_HANDLER_PROLOGUE irqendy

            lda TED_CHARGEN_ADDR
            pha
            and #.lobyte(~FORCE_SINGLE_CLOCK)
            sta TED_CHARGEN_ADDR
            and #.lobyte(~CHARGEN_ADDR_MASK)
            ora #MAKE_CHARGEN_ADDR(COLRAM + $0c00)
            tax
            sec
            lda #$db
            sbc lineoffset
            tay

            lda PALNTSC
            bne :+++; branch if PAL

            ; NTSC
:           cpy TED_RASTERLINE
            bcs :-
            nop
            nop
            STABILIZE_RASTER

            lda #INVALID_MODE | DISPLAY_ENABLE | LINES_24 | SCROLLY_4
            sta TED_CTRL1; open border
            ldy #$06
:           dey
            bne :-
            nop
            lda #.lobyte(~CHARSET_BITMAP_IN_ROM)
            and TED_BITMAP_ADDR
            jmp :+++

:           lda #.lobyte(~CHARSET_BITMAP_IN_ROM)
            and TED_BITMAP_ADDR
:           cpy TED_RASTERLINE
            bcs :-

:           stx TED_CHARGEN_ADDR
            sta TED_BITMAP_ADDR

            lda PALNTSC
            beq ntscborder

            pla
            and #.lobyte(~CHARGEN_ADDR_MASK)
            ora #MAKE_CHARGEN_ADDR(COLRAM + $0c00)
            sta TED_CHARGEN_ADDR

            lda #$ca - IRQ_SLACK
            ldx #.lobyte(irq4)
            ldy #.hibyte(irq4)
            jmp irqend

            ; this handler is called just before the 3rd extra char row has finished displaying

irq4:       LOADER_IRQ_HANDLER_PROLOGUE irqendy

            lda TED_CHARGEN_ADDR
            pha
            ora #FORCE_SINGLE_CLOCK
            sta TED_CHARGEN_ADDR

            lda #$ca
:           cmp TED_RASTERLINE
            bcs :-

            lda #INVALID_MODE | DISPLAY_ENABLE | LINES_24 | SCROLLY_4
            sta TED_CTRL1; open border

            lda TED_RASTERLINE
:           cmp TED_RASTERLINE
            beq :-
            nop
            STABILIZE_RASTER

ntscborder: lda TED_SCREEN_ADDR
            and #.lobyte(~SCREEN_ADDR_MASK)
            ora #MAKE_SCREEN_ADDR(COLRAM)
            sta TED_SCREEN_ADDR
            lda #.lobyte(~CHARSET_BITMAP_IN_ROM)
            and TED_BITMAP_ADDR
            sta TED_BITMAP_ADDR
            lda PALNTSC
            bne :++
            ldx #$08
:           dex
            bne :-
:           clc
            lda lineoffset
            adc TED_RASTERLINE
            sta TED_RASTERLINE

            jsr update_clk

            pla
            and #.lobyte(~CHARGEN_ADDR_MASK)
            ora #MAKE_CHARGEN_ADDR(COLRAM + $0c00)
            sta TED_CHARGEN_ADDR

    .if IRQ_SLACK > 0
            lda #IRQ_RASTERLINE_MSB
            ora TED_IMR
            sta TED_IMR
            lda PALNTSC
            bne :+
            lda #.lobyte(DISPLAY_LINES_NTSC - IRQ_SLACK)
            SKIPWORD
:           lda #.lobyte(DISPLAY_LINES_PAL - IRQ_SLACK)
    .else
            lda #$00
    .endif
            ldx #.lobyte(irq0)
            ldy #.hibyte(irq0)

irqend:     sta TED_IRQ_RASTERLINE
            stx CINV + $00
            sty CINV + $01
irqendx:    lda #RASTER_IRQ
            sta TED_IRR
irqendy:

memcfgchks = * + $01
            ldx #$00
            beq memchkok
memchktype = * + $01
            ldx #$00
            bne resicheck

            ; install memory check
            beq memchkok

            ; resident memory check
resicheck:
    .if !LOAD_VIA_KERNAL_FALLBACK
            lda TED_CHARGEN_ADDR
            lsr
            bcc memchkok
memchkfail: tax
            lda #MSGINVMCONF
            jmp error
    .endif
memchkok:   LOADER_IRQ_HANDLER_EPILOGUE
            pla
            tay
            pla
            tax
            pla
            rti

update_clk: 
    .if COUPLED_TIMERS = 0
            jsr readctrfix
            tya
            pha
            txa
            pha
            sec
            lda prevctr3lo
            stx :+ + 1
:           sbc #$00
            tax
            lda prevctr3hi
            sty :+ + 1
:           sbc #$00
            tay
            txa
            clc
            adc cycles_lo
            sta cycles_lo
            tya
            adc cycles_mid
            sta cycles_mid
            bcc :+
            inc cycles_hi

:           pla
            sta prevctr3lo
            pla
            sta prevctr3hi
    .endif; COUPLED_TIMERS = 0

            sed
            ldx #$00
            clc
            lda #$01
            adc tod_frames
            sta tod_frames
            ldy PALNTSC
            beq :+
            cmp #$50
            jmp :++
:           cmp #$60
:           bne tod_done
            stx tod_frames
            clc
            lda #$01
            adc tod_secs
            sta tod_secs
            cmp #$60
            bne tod_done
            stx tod_secs
            clc
            lda #$01
            adc tod_mins
            sta tod_mins
            cmp #$60
            bne tod_done
            stx tod_mins
            clc
            lda #$01
            adc tod_hrs
            sta tod_hrs
tod_done:   cld
            rts

    .if COUPLED_TIMERS = 0
readctr:    ldy TED_COUNTER3_HI
            ldx TED_COUNTER3_LO
            cpy TED_COUNTER3_HI
            bne readctr
            rts

readctrfix: lda TED_CHARGEN_ADDR
            and #FORCE_SINGLE_CLOCK
            pha
            bne :+
            lda #FORCE_SINGLE_CLOCK
            php
            sei
            ora TED_CHARGEN_ADDR
            sta TED_CHARGEN_ADDR
            plp
:
            ldx TED_COUNTER3_LO
            ldy TED_COUNTER3_HI
            cpx TED_COUNTER3_LO
            bcs :+
            ldx TED_COUNTER3_LO
            ldy TED_COUNTER3_HI
            nop
            jmp :++
:           txa
            sbc #$0e
            tax
            tya
            sbc #$00
            tay
:
            pla
            bne :+
            lda #.lobyte(~FORCE_SINGLE_CLOCK)
            php
            sei
            and TED_CHARGEN_ADDR
            sta TED_CHARGEN_ADDR
            plp
:           rts

readcycles: lda cycles_hi
            sta cycshibuf
            lda cycles_mid
            sta cycsmidbuf

            lda prevctr3lo
            sta prevctr3lb
            lda prevctr3hi
            sta prevctr3hb

            jsr readctr; counter runs backwards
            stx ctr3lobuf
            sty ctr3hibuf

            ldx cycles_lo
cycsmidbuf = * + $01
            lda #$00
            cmp cycles_mid
            bne readcycles
cycshibuf = * + $01
            lda #$00
            cmp cycles_hi
            bne readcycles

            stx cycslobuf

            sec
prevctr3lb = * + $01
            lda #$00
ctr3lobuf = * + $01
            sbc #$00
            tax
prevctr3hb = * + $01
            lda #$00
ctr3hibuf = * + $01
            sbc #$00
            tay

            clc
            txa
cycslobuf = * + $01
            adc #$00
            pha
            tya
            adc cycsmidbuf
            tax
            lda #$00
            adc cycshibuf
            tay
            pla
            rts

docalibrte: jsr initstat
            nop; ldx #byteslo
            nop; ldy #byteshi
            nop; jsr printstat
            nop
            nop
            nop; stx statxbuf
            nop
            nop; sty statybuf
            nop
            nop; jsr diffcycles
            nop
            nop

            ; fall through

diffcycles: jsr readcycles

            sec
            sbc prvcycslo
            sta numcycles + $00
            txa
            sbc prvcycsmid
            sta numcycles + $01
            tya
            sbc prvcycshi
            sta numcycles + $02
            lda #$00
            sta numcycles + $03

            sec
            lda numcycles + $00
adjustdiff = * + $01
            sbc #$00
            sta numcycles + $00
            bcs :+
            lda numcycles + $01
            dec numcycles + $01
            tax
            bne :+
            lda numcycles + $02
            dec numcycles + $02
            tax
            bne :+
            dec numcycles + $03
:           rts
    .endif; COUPLED_TIMERS = 0

.else; PLATFORM <> diskio::platform::COMMODORE_16

ramirq:     pha
            txa
            tsx
            pha
            tya
            pha
            inx
            inx
            lda STACK,x
            and #FLAG_B
            bne :+
            jmp (CINV)
:           jmp (CBINV)

    .if LOAD_TO_UPPER_MEM

            ; IRQ handler to switch back the bank setting,
            ; needed because the sprites are in another
            ; VIC bank than the bitmap

irq0:       lda IO_PORT
            pha
            lda #MEMCONFIG_IO
            sta IO_PORT

            lda #VIC2_MAKE_ADDR COLRAM, BITMAP
            sta VIC2_ADDR
            bit DRIVETYPE
            bpl :+; branch if not generic drive
            ; KERNAL fallback active
            ; note: this may glitch due to a race condition, the KERNAL ROM's serial bus code does
            ;       things like lda $dd00 : and/ora #value : sta $dd00, which may be interrupted
            ;       between lda and sta by this IRQ, thus setting the previous VIC bank again
            lda CIA2_PRA
            and #.lobyte(~VIC2_BANK)
            ora #VIC2_MAKE_BANK BITMAP
            sta CIA2_PRA
            jmp :++
:           SET_VIC_BANK VIC2_MAKE_BANK BITMAP
:
            lda #$f9
            ldx #.lobyte(irq1)
            ldy #.hibyte(irq1)
            jmp irqend

    .endif; LOAD_TO_UPPER_MEM

            ; IRQ handler to open the y-border

irq1:       lda IO_PORT
            pha
            lda #MEMCONFIG_IO
            sta IO_PORT

            lda #BITMAP_MODE | DISPLAY_ENABLE | LINES_24 | SCROLLY_3
            sta VIC2_CTRL1

    .if LOAD_TO_UPPER_MEM
            ldx #.lobyte(irq0)
            ldy #.hibyte(irq0)
    .else
            ldx #.lobyte(irq1)
            ldy #.hibyte(irq1)
    .endif

    .if LOAD_TO_UPPER_MEM
            lda #$fa
:           bit VIC2_CTRL1
            bmi :+
            cmp VIC2_RASTERLINE
            bcs :-
:
            lda #VIC2_MAKE_ADDR SPRITESCR, BITMAP
            sta VIC2_ADDR
            bit DRIVETYPE
            bpl :+; branch if not generic drive
            ; KERNAL fallback active
            ; note: this may glitch due to a race condition, the KERNAL ROM's serial bus code does
            ;       things like lda $dd00 : and/ora #value : sta $dd00, which may be interrupted
            ;       between lda and sta by this IRQ, thus setting the previous VIC bank again
            lda CIA2_PRA
            and #.lobyte(~VIC2_BANK)
            ora #VIC2_MAKE_BANK SPRITES
            sta CIA2_PRA
            jmp :++
:           SET_VIC_BANK VIC2_MAKE_BANK SPRITES
:
    .endif

            lda #$fb
:           bit VIC2_CTRL1
            bmi :+
            cmp VIC2_RASTERLINE
            bcs :-
:
            lda #BITMAP_MODE | DISPLAY_ENABLE | LINES_25 | SCROLLY_3; $3b
            sta VIC2_CTRL1; set 25 rows bit
            lda #SINGLECOLOUR_MODE | COLUMNS_40 | SCROLLX_0; $08
            sta VIC2_CTRL2

    .if LOAD_TO_UPPER_MEM
            lda #$10
    .else
            lda #$f9
    .endif

irqend:     sta VIC2_RASTERLINE
            stx CINV + $00
            sty CINV + $01

.if KEY_FOR_WATCHDOG
            lda #$00
            sta $dc00
            ldx $dc01
            inx
            beq :+
            lda #$3f
            sta $dd02
            lda $dd00
            and #$03
            ora #$c0
            sta $dd00
            inc $d020
            jmp *-3
:
.endif
            dec VIC2_IRR
            pla
            sta IO_PORT

            and #%00111111; mask out the upper 2 bits (C-128 ASCII/DIN and Flash8 clock control bits)
            ora #CASSETTE_SENSE; this is 0 on SX-64

memcfgchks = * + $01
            ldx #$00
            beq memchkok
memchktype = * + $01
            ldx #$00
            bne resicheck

            ; install memconfig check

            cmp #MEMCONFIG_IO_KERNAL
            beq memchkok
            bne memchkfail

            ; resident memconfig check
resicheck:
    .if LOAD_TO_UPPER_MEM
        .if LOAD_VIA_KERNAL_FALLBACK
            cmp #MEMCONFIG_ALL_RAM
            beq memchkok
            cmp #MEMCONFIG_IO
            beq memchkok
            cmp #MEMCONFIG_IO_KERNAL
            beq memchkok
        .else
            cmp #MEMCONFIG_ALL_RAM
            beq memchkok
            cmp #MEMCONFIG_IO
            beq memchkok
        .endif
    .else
        .if LOAD_VIA_KERNAL_FALLBACK
            cmp #MEMCONFIG_IO
            beq memchkok
            cmp #MEMCONFIG_IO_KERNAL
            beq memchkok
        .else
            cmp #MEMCONFIG_IO
            beq memchkok
        .endif
    .endif
memchkfail: tax; faulty config value displayed in x and y
            tay
            lda #MSGINVMCONF
            jmp error
memchkok:   pla
            tay
            pla
            tax
            pla
            rti

.endif; PLATFORM <> diskio::platform::COMMODORE_16

nmihandler: lda #SPURIOUSNMI
            jmp error

brkhandler: pla
            pla
            pla
            pla
            pla
            sec
            sbc #$02
            sta brkaddrlo
            pla
            sbc #$00
            sta brkaddrhi

            lda #ERRBRKOPC
            jmp error

.rodata

startupmsg: .byte PETSCII_WHITE
            .byte "Loader test application", PETSCII_RETURN
            .byte "by Krill/Plush", PETSCII_RETURN
            .byte "Repository version ", REPOSITORY_VERSION, PETSCII_RETURN, PETSCII_RETURN
            .byte 0

installmsg: .byte PETSCII_YELLOW, "Installing loader... ", PETSCII_RETURN
            .byte 0

donemsg:    .byte PETSCII_WHITE, "done", PETSCII_RETURN
            .byte 0

quotemsg:   .byte PETSCII_LIGHTGREEN, '"', 0
onmsg:      .byte PETSCII_YELLOW, " on ", 0
returnmsg:  .byte PETSCII_RETURN, 0

paltext:    scrcode "PAL"
            .byte $00
ntsctext:   scrcode "NTSC"
            .byte $00

drivemsgsl: .byte .lobyte(msg1541), .lobyte(msg1541c), .lobyte(msg1541ii), .lobyte(msg1541u)
            .byte .lobyte(msg1570), .lobyte(msg1571), .lobyte(msg1571cr), .lobyte(msg1581), .lobyte(msgfd2000), .lobyte(msgfd4000)
            .byte .lobyte(msggeneric)

drivemsgsh: .byte .hibyte(msg1541), .hibyte(msg1541c), .hibyte(msg1541ii), .hibyte(msg1541u)
            .byte .hibyte(msg1570), .hibyte(msg1571), .hibyte(msg1571cr), .hibyte(msg1581), .hibyte(msgfd2000), .hibyte(msgfd4000)
            .byte .hibyte(msggeneric)

msg1541:    .byte PETSCII_YELLOW, "CBM 1541", 0
msg1541c:   .byte PETSCII_YELLOW, "CBM 1541-C", 0
msg1541ii:  .byte PETSCII_YELLOW, "CBM 1541-II", 0
msg1541u:   .byte PETSCII_YELLOW, "1541U", 0
msg1570:    .byte PETSCII_YELLOW, "CBM 1570", 0
msg1571:    .byte PETSCII_YELLOW, "CBM 1571", 0
msg1571cr:  .byte PETSCII_YELLOW, "CBM 1571CR", 0
msg1581:    .byte PETSCII_YELLOW, "CBM 1581", 0
msgfd2000:  .byte PETSCII_YELLOW, "CBM FD 2000", 0
msgfd4000:  .byte PETSCII_YELLOW, "CBM FD 4000", 0
msggeneric: .byte PETSCII_YELLOW, "generic drive", 0

drivtypesl: .byte .lobyte(str1541), .lobyte(str1541c), .lobyte(str1541ii), .lobyte(str1541u)
            .byte .lobyte(str1570), .lobyte(str1571), .lobyte(str1571cr), .lobyte(str1581), .lobyte(strfd2000), .lobyte(strfd4000)
            .byte .lobyte(strgeneric)

drivtypesh: .byte .hibyte(str1541), .hibyte(str1541c), .hibyte(str1541ii), .hibyte(str1541u)
            .byte .hibyte(str1570), .hibyte(str1571), .hibyte(str1571), .hibyte(str1581), .hibyte(strfd2000), .hibyte(strfd4000)
            .byte .hibyte(strgeneric)

str1541:    scrcode "CBM1541"
            .byte $00
str1541c:   scrcode "CBM1541C"
            .byte $00
str1541ii:  scrcode "CBM1541II"
            .byte $00
str1541u:   scrcode "1541U"
            .byte $00
str1570:    scrcode "CBM1570"
            .byte $00
str1571:    scrcode "CBM1571"
            .byte $00
str1571cr:  scrcode "CBM1571CR"
            .byte $00
str1581:    scrcode "CBM1581"
            .byte $00
strfd2000:  scrcode "CMDFD2000"
            .byte $00
strfd4000:  scrcode "CMDFD4000"
            .byte $00
strgeneric: scrcode "generic"
            .byte $00

kbstext:    scrcode " kB/s"
            .byte $00

            scrcode "                                        "
emptytexte:
            .byte $00

.if LOAD_TO_UPPER_MEM

pic1unc:    .asciiz "ab-pic1hi.bin"
    .if LOAD_VIA_KERNAL_FALLBACK | VERIFY
pic2unc:    .asciiz "bb-pic2hi.bin"
    .else
pic2unc:    .asciiz ""; use loadnext functionality
    .endif

    .if DECOMPRESSOR <> DECOMPRESSORS::NONE
pic1compd:  .asciiz "ap-pic1hi.pak"
        .if LOAD_VIA_KERNAL_FALLBACK | VERIFY
pic2compd:  .asciiz "bp-pic2hi.pak"
        .else
pic2compd:  .asciiz ""; use loadnext functionality
        .endif
    .endif

.else; !LOAD_TO_UPPER_MEM

pic1unc:    .asciiz "1b-pic1.bin"
    .if LOAD_VIA_KERNAL_FALLBACK | VERIFY
pic2unc:    .asciiz "2b-pic2.bin"
    .else
pic2unc:    .asciiz ""; use loadnext functionality
    .endif

    .if DECOMPRESSOR <> DECOMPRESSORS::NONE
pic1compd:  .asciiz "1p-pic1.pak"
        .if LOAD_VIA_KERNAL_FALLBACK | VERIFY
pic2compd:  .asciiz "2p-pic2.pak"
        .else
pic2compd:  .asciiz ""; use loadnext functionality
        .endif
    .endif

.endif

bogusname:  .byte "Bli", 'p' | $80, 0

errormsgsl: .byte .lobyte(invaliderr); $00

            .repeat $75, I
                .byte .lobyte(invaliderr)
            .endrep

ERRPALNTSC = * - errormsgsl
            .byte .lobyte(emgpalntsc)
ERRENDADDR = * - errormsgsl
            .byte .lobyte(emsgendadd)
ERRBYTESLOADED = * - errormsgsl
            .byte .lobyte(emsgbytlod)
MSGUNINST = * - errormsgsl
            .byte .lobyte(emsguninst)
ERRBRKOPC = * - errormsgsl
            .byte .lobyte(emsgbrkopc)
SPURIOUSNMI = * - errormsgsl
            .byte .lobyte(emsgspunmi)
MEMCONFIGCH = * - errormsgsl
            .byte .lobyte(emsgmemcfg)
MSGINVMCONF = * - errormsgsl
            .byte .lobyte(emsginvmcf)
MSGVERIFAIL = * - errormsgsl
            .byte .lobyte(emsgverifl)

            .repeat $75, I
                .byte .lobyte(invaliderr)
            .endrep

            .byte .lobyte(invaliderr)
            .byte .lobyte(invaliderr)
            .byte .lobyte(invaliderr)
            .byte .lobyte(invaliderr)
            .byte .lobyte(invaliderr)

            .byte .lobyte(emsgillts)
            .byte .lobyte(emsgdevinc)
            .byte .lobyte(emsgtoomd)
            .byte .lobyte(emsggenker)
            .byte .lobyte(emsgparerr)
            .byte .lobyte(emsgfnotf)
            .byte .lobyte(emsgdevnp); $ff

errormsgsh: .byte .hibyte(invaliderr); $00

            .repeat $75, I
                .byte .hibyte(invaliderr)
            .endrep

            .byte .hibyte(emgpalntsc)
            .byte .hibyte(emsgendadd)
            .byte .hibyte(emsgbytlod)
            .byte .hibyte(emsguninst)
            .byte .hibyte(emsgbrkopc)
            .byte .hibyte(emsgspunmi)
            .byte .hibyte(emsgmemcfg)
            .byte .hibyte(emsginvmcf)
            .byte .hibyte(emsgverifl)

            .repeat $75, I
                .byte .hibyte(invaliderr)
            .endrep

            .byte .hibyte(invaliderr)
            .byte .hibyte(invaliderr)
            .byte .hibyte(invaliderr)
            .byte .hibyte(invaliderr)
            .byte .hibyte(invaliderr)

            .byte .hibyte(emsgillts)
            .byte .hibyte(emsgdevinc)
            .byte .hibyte(emsgtoomd)
            .byte .hibyte(emsggenker)
            .byte .hibyte(emsgparerr)
            .byte .hibyte(emsgfnotf)
            .byte .hibyte(emsgdevnp); $ff

.assert errormsgsh - errormsgsl = $0100, error, "errormsgsl has wrong length"
.assert * - errormsgsh = $0100, error, "errormsgsh has wrong length"

emgpalntsc: scrcode "PAL/NTSC detection mismatch"
            .byte $00
emsgendadd: scrcode "Wrong end address"
            .byte $00
emsgbytlod: scrcode "Wrong amount of bytes loaded"
            .byte $00
emsginvmcf: scrcode "Invalid memory configuration"
            .byte $00
emsguninst: scrcode "The loader is now uninstalled"
            .byte $00
emsgbrkopc: scrcode "Executed BRK instruction at $"
            .byte $00
emsgspunmi: scrcode "Spurious NMI"
            .byte $00
emsgmemcfg: scrcode "Memory config changed"
            .byte $00
emsgverifl: scrcode "Verify failed at $"
verifysrc:  scrcode "0000/$"
verifydest: scrcode "0000"
            .byte $00

emsgillts:  scrcode "Illegal track or sector"
            .byte $00
emsgdevinc: scrcode "Device incompatible"
            .byte $00
emsgtoomd:  scrcode "Too many devices"
            .byte $00
emsggenker: scrcode "Generic KERNAL error"
            .byte $00
emsgparerr: scrcode "Invalid parameters"
            .byte $00
emsgfnotf:  scrcode "File not found"
            .byte $00
emsgdevnp:  scrcode "Device not present"
            .byte $00

invaliderr: scrcode "Invalid error code"
            .byte $00

numccpal:   CYCLES_PER_SECOND_PAL
numccntsc:  CYCLES_PER_SECOND_NTSC

.bss

zpbuffer = * - 4
            .res $fc

.if PLATFORM = diskio::platform::COMMODORE_16
tod_hrs:    .res 1
tod_mins:   .res 1
tod_secs:   .res 1
tod_frames: .res 1

    .if COUPLED_TIMERS = 0
prevctr3lo: .res 1
prevctr3hi: .res 1

cycles_hi:  .res 1
cycles_mid: .res 1
cycles_lo:  .res 1

prvcycslo:  .res 1
prvcycsmid: .res 1
prvcycshi:  .res 1
    .endif; COUPLED_TIMERS
.endif

numcycles:  .res 4
floatbuff:  .res 5
floatbuff2: .res 5

.segment "END"
