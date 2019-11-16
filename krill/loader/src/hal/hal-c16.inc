
.ifndef _HAL_C16_INC_
_HAL_C16_INC_ = 1

.include "cpu.inc"
.include "ted.inc"


.define CHECK_INSTALL_END_ADDRESS        .assert * <= $8000, error, "***** Error: the install code must not exceed $8000, please make sure the DISKIO_INSTALL segment ends below $8000. *****"
.if LOAD_VIA_KERNAL_FALLBACK
    .define CHECK_RESIDENT_START_ADDRESS; no restrictions
    .define CHECK_RESIDENT_END_ADDRESS   .assert * <= $8000, error, "***** Error: The resident code must not exceed $8000, please make sure the DISKIO segment ends below $8000. *****"
.else
    .define CHECK_RESIDENT_START_ADDRESS .assert * <= $fd00, error, "***** Error: The resident code must not exceed $fd00, please make sure the DISKIO segment ends below $fd00. *****"
    .define CHECK_RESIDENT_END_ADDRESS   .assert * <= $fd00, error, "***** Error: The resident code must not exceed $fd00, please make sure the DISKIO segment ends below $fd00. *****"
.endif


.macro INIT_PAL_NTSC
            ; nothing to do
.endmacro

.macro INIT_KERNAL_SERIAL_ROUTINES
            ; pucrunch clobbers $f9, restore it
            lda #$00
            sta USE4DY
.endmacro

.macro PREPARE_DRIVE_DISTURBANCE_VALIDATION
            ; disregard drive if it is a 1551, as
            ; it is not connected to the serial bus
            lda #$01
            sta USE4DY
.endmacro

.macro BRANCH_IF_DRIVE_DOES_NOT_DISTURB_SERIAL_BUS to
            lda USE4DY; calling LISTEN will set USE4DY to $00 if 1551 at #9,
            eor #$01  ; $30 if 1551 at #8, and leave it at $01 otherwise,
            bne to    ; then drvlistn will return with $80, $b0, or $01
.endmacro


.macro ENABLE_KERNAL_SERIAL_ROUTINES
            sta TED_ROM_ENABLE
.endmacro

.macro ENABLE_KERNAL_SERIAL_ROUTINES_Y
            sta TED_ROM_ENABLE
.endmacro

.macro ENABLE_ALL_RAM
            sta TED_RAM_ENABLE
.endmacro

.macro ENABLE_ALL_RAM_Y
            sty TED_RAM_ENABLE
.endmacro

.macro GET_MEMCONFIG
            lda #ROM_IS_ENABLED
            and TED_CHARGEN_ADDR
.endmacro

.macro SET_MEMCONFIG
            .local use_ram

            sta TED_RAM_ENABLE
            beq use_ram
            sta TED_ROM_ENABLE
use_ram:
.endmacro

.macro SET_MEMCONFIG_Y
            .local use_ram

            sty TED_RAM_ENABLE
            beq use_ram
            sty TED_ROM_ENABLE
use_ram:
.endmacro


.ifndef IO_PORT_DIR_COMMON
    IO_PORT_DIR_COMMON  = IO_PORT_SERIAL_DATA_IN_INPUT | IO_PORT_SERIAL_CLK_IN_INPUT | IO_PORT_CST_MTR_OUTPUT | IO_PORT_SERIAL_ATN_OUT_OUTPUT | IO_PORT_SERIAL_CLK_OUT_OUTPUT | IO_PORT_SERIAL_DATA_OUT_OUTPUT; $0f

                                             ; effectively, this is  
                                             ; the KERNAL flag:
                                             ; 0 = input  = KERNAL,  
                                             ; 1 = output = loader   
    IO_PORT_DIR_KERNAL  = IO_PORT_DIR_COMMON | IO_PORT_CST_RD_INPUT ; $0f

    IO_PORT_DIR_OPERATE = IO_PORT_DIR_COMMON | IO_PORT_CST_RD_OUTPUT; $1f
.endif

CLOCK                   = IO_PORT_CST_MTR | (0 & IO_PORT_SERIAL_DATA_OUT) | (0 & IO_PORT_SERIAL_CLK_OUT)
CLOCK_ATN_HI            = CLOCK |      IO_PORT_SERIAL_ATN_OUT ; 1st and 3rd bit pairs; $0c
CLOCK_ATN_LO            = CLOCK | (0 & IO_PORT_SERIAL_ATN_OUT); 2nd and 4th bit pairs; $08


.macro ENABLE_WAITBUSY_KERNAL
            lda #.lobyte(~IO_PORT_SERIAL_CLK_OUT)
            and IO_PORT
            sta IO_PORT
.endmacro

.macro INIT_CLEAR_DATA_OUT_CLEAR_CLK_OUT_CLEAR_ATN_OUT
            lda #IO_PORT_CST_MTR | (0 & IO_PORT_SERIAL_DATA_OUT) | (0 & IO_PORT_SERIAL_CLK_OUT) | (0 & IO_PORT_SERIAL_ATN_OUT); $08
            sta IO_PORT
.endmacro

.ifndef BEGIN_INSTALL_DEFINED
    BEGIN_INSTALL_DEFINED = 1
    .macro BEGIN_INSTALL
            lda #IO_PORT_CST_MTR | IO_PORT_SERIAL_DATA_OUT | (0 & IO_PORT_SERIAL_CLK_OUT) | (0 & IO_PORT_SERIAL_ATN_OUT); $09
            sta IO_PORT
    .endmacro
.endif

.if LOAD_VIA_KERNAL_FALLBACK
            KERNALFILENO = $02
.endif

.macro BRANCH_IF_INSTALLED to
            lda #IO_PORT_DIR_KERNAL
            cmp IO_PORT_DIRECTION
            bne to
.endmacro

.macro BRANCH_IF_NOT_INSTALLED to
            lda #IO_PORT_DIR_KERNAL
            cmp IO_PORT_DIRECTION
            beq to
.endmacro

.ifndef PUSH_CLOCKCONFIG_AND_FORCE_SINGLE_CLOCK_DEFINED
    PUSH_CLOCKCONFIG_AND_FORCE_SINGLE_CLOCK_DEFINED = 1
    .macro PUSH_CLOCKCONFIG_AND_FORCE_SINGLE_CLOCK
            .local singleclk

            lda TED_CHARGEN_ADDR
            and #FORCE_SINGLE_CLOCK
            pha
            bne singleclk
            lda #FORCE_SINGLE_CLOCK
            php
            sei                  ; 2
            ora TED_CHARGEN_ADDR ; 4
            sta TED_CHARGEN_ADDR ; 4
            plp                  ; 4
singleclk:                       ; = 14
    .endmacro
.endif

.ifndef POP_CLOCKCONFIG_DEFINED
    POP_CLOCKCONFIG_DEFINED = 1
    .macro POP_CLOCKCONFIG
            .local singleclk

            pla
            bne singleclk
            lda #.lobyte(~FORCE_SINGLE_CLOCK)
            php
            sei                  ; 2
            and TED_CHARGEN_ADDR ; 4
            sta TED_CHARGEN_ADDR ; 4
            plp                  ; 4
singleclk:                       ; = 14
    .endmacro
.endif

.ifndef SENDBYTE_DEFINED
    SENDBYTE_DEFINED = 1

    .macro SENDBYTE
        DO_SENDBYTE IO_PORT_SERIAL_CLK_OUT, IO_PORT_SERIAL_DATA_OUT
    .endmacro

    .macro SENDBYTE_ATN
        DO_SENDBYTE IO_PORT_SERIAL_ATN_OUT, IO_PORT_SERIAL_CLK_OUT
    .endmacro

    .macro DO_SENDBYTE data, clock
            ; y is not changed

            .local sendbyte
            .local bitset

            tax
            PUSH_CLOCKCONFIG_AND_FORCE_SINGLE_CLOCK
            txa
            ldx #$07
sendbyte:   lsr
            pha
            lda IO_PORT
            ora #data
            bcs bitset
            and #.lobyte(~data)
bitset:     eor #clock
            sta IO_PORT
            pla
            dex
            bpl sendbyte
            POP_CLOCKCONFIG
    .endmacro
.endif

.ifndef SET_FLAGS_N_DATA_V_CLK_DEFINED
    SET_FLAGS_N_DATA_V_CLK_DEFINED = 1
    .macro SET_FLAGS_N_DATA_V_CLK
            bit IO_PORT    
    .endmacro
.endif

.ifndef INSTALL_IDLE_DEFINED
    INSTALL_IDLE_DEFINED = 1
    .macro INSTALL_IDLE
            lda #IO_PORT_CST_MTR | IO_PORT_SERIAL_DATA_OUT | IO_PORT_SERIAL_CLK_OUT | IO_PORT_SERIAL_ATN_OUT; $0f
            sta IO_PORT
            lda #IO_PORT_DIR_OPERATE
            sta IO_PORT_DIRECTION
    .endmacro
.endif

.macro IDLE
            ldx #IO_PORT_CST_MTR | IO_PORT_SERIAL_DATA_OUT | IO_PORT_SERIAL_CLK_OUT | IO_PORT_SERIAL_ATN_OUT; $0f
            stx IO_PORT
.endmacro

.macro WAKEUP
            lda #IO_PORT_CST_MTR | (0 & IO_PORT_SERIAL_DATA_OUT) | IO_PORT_SERIAL_CLK_OUT | (0 & IO_PORT_SERIAL_ATN_OUT); $0a
            sta IO_PORT
.endmacro

.macro CLEAR
            lda #IO_PORT_CST_MTR | (0 & IO_PORT_SERIAL_DATA_OUT) | (0 & IO_PORT_SERIAL_CLK_OUT) | IO_PORT_SERIAL_ATN_OUT; $0c
            sta IO_PORT
.endmacro

.macro SEND_BLOCK_SIGNAL
            ldy #CLOCK_ATN_LO
            sty IO_PORT
            PUSH_CLOCKCONFIG_AND_FORCE_SINGLE_CLOCK
            pha
            pla
.endmacro

.macro GETBYTE_SETUP
            ; nothing to do
.endmacro

.macro GETBYTE loop, store
            ; 16 cycles at per bitpair ~ 18 cycles at 1 MHz = 18 us
            ; PAL:  16 / 886723 Hz = 18.04 us
            ; NTSC: 16 / 894886 Hz = 17.88 us

loop:       ldx #CLOCK_ATN_HI                       ; 2
            lda IO_PORT                             ; 3
            stx IO_PORT; ATN high                   ; 3
                                                    ; = 16

            lsr                                     ; 2
            lsr                                     ; 2
            iny                                     ; 2
            beq :+                                  ; 2
            ldx #CLOCK_ATN_LO                       ; 2
            eor IO_PORT                             ; 3
            stx IO_PORT; ATN low                    ; 3
                                                    ; = 16

            lsr                                     ; 2
            lsr                                     ; 2
            nop                                     ; 2 - delay
            nop                                     ; 2 - delay
            ldx #CLOCK_ATN_HI                       ; 2
            eor IO_PORT                             ; 3
            stx IO_PORT; ATN high                   ; 3
                                                    ; = 16

            lsr                                     ; 2
            lsr                                     ; 2
            nop                                     ; 2 - delay
            eor #CLOCK_ATN_HI | (CLOCK_ATN_LO >> 2) ; 2
            ldx #CLOCK_ATN_LO                       ; 2
            eor IO_PORT                             ; 3
            stx IO_PORT; ATN low                    ; 3
                                                    ; = 16

            store                                   ; 5 - sta abs,y
            jmp loop                                ; 3
:
.endmacro

.macro STOREBYTE_ALLRAM
storebytio: sta a:$00,y
.endmacro

.macro ENDGETBLOCK
            tax; special block number (irrelevant for regular blocks)
            POP_CLOCKCONFIG
            txa; special block number (irrelevant for regular blocks)
.endmacro

.macro SET_IO_KERNAL
            INIT_CLEAR_DATA_OUT_CLEAR_CLK_OUT_CLEAR_ATN_OUT
            lda #IO_PORT_DIR_KERNAL
            sta IO_PORT_DIRECTION
.endmacro

.endif; !_HAL_C16_INC_
