
.ifndef _HAL_C64_INC_
_HAL_C64_INC_ = 1

.include "cia.inc"


.define CHECK_INSTALL_END_ADDRESS        .assert * <= $d000, error, "***** Error: the install code must not exceed $d000, please make sure the DISKIO_INSTALL segment ends below $d000. *****"
.if LOAD_VIA_KERNAL_FALLBACK
    .define CHECK_RESIDENT_START_ADDRESS; no restrictions
    .define CHECK_RESIDENT_END_ADDRESS   .assert * <= $d000, error, "***** Error: The resident code must not exceed $d000, please make sure the DISKIO segment ends below $d000. *****"
.else
    .define CHECK_RESIDENT_START_ADDRESS .assert (* <= $d000) | (* >= $e000), error, "***** Error: The resident code must not reside at $d000..$dfff, please make sure the DISKIO segment does not overlap with that memory range. *****"
    .define CHECK_RESIDENT_END_ADDRESS   .assert (* <= $d000) | (* >= $e000), error, "***** Error: The resident code must not reside at $d000..$dfff, please make sure the DISKIO segment does not overlap with that memory range. *****"
.endif


.macro INIT_PAL_NTSC
            ; this could detect NTSC with NTSC_SUPPORT disabled,
            ; but that either involves the unreliable KERNAL variable at $02a6
            ; or disabled interrupts and sprites for a check routine here.
            ; this could also modify the getbyte routine(s) and save the NTSC
            ; support overhead on PAL, but access to the resident portion(s)
            ; from the installer is rather cumbersome and unreliable
.endmacro

.macro INIT_KERNAL_SERIAL_ROUTINES
            ; nothing to do
.endmacro

.macro PREPARE_DRIVE_DISTURBANCE_VALIDATION
            ; nothing to do
.endmacro

.macro BRANCH_IF_DRIVE_DOES_NOT_DISTURB_SERIAL_BUS to
            ; nothing to do
.endmacro


.macro ENABLE_KERNAL_SERIAL_ROUTINES
            lda #MEMCONFIG_IO_KERNAL
            sta IO_PORT
.endmacro

.macro ENABLE_KERNAL_SERIAL_ROUTINES_Y
            ldy #MEMCONFIG_IO_KERNAL
            sty IO_PORT
.endmacro

.macro ENABLE_IO_SPACE
            lda #MEMCONFIG_IO
            sta IO_PORT
.endmacro

.macro ENABLE_IO_SPACE_Y
            ldy #MEMCONFIG_IO
            sty IO_PORT
.endmacro

.macro ENABLE_ALL_RAM
            lda #MEMCONFIG_ALL_RAM
            sta IO_PORT
.endmacro

.macro ENABLE_ALL_RAM_X
            ldx #MEMCONFIG_ALL_RAM
            stx IO_PORT
.endmacro

.macro ENABLE_ALL_RAM_Y
            ldy #MEMCONFIG_ALL_RAM
            sty IO_PORT
.endmacro

.macro GET_MEMCONFIG
            lda IO_PORT
.endmacro

.macro SET_MEMCONFIG
            sta IO_PORT
.endmacro

.macro SET_MEMCONFIG_X
            stx IO_PORT
.endmacro

.macro SET_MEMCONFIG_Y
            sty IO_PORT
.endmacro


.ifdef _CIA_INC_; is not defined if via.inc had been included before cia.inc

    .ifndef CIA2_DDRA_COMMON
        ; CIA2 DDRA ($DD02) definitions
        CIA2_DDRA_COMMON   = CIA_SERIAL_DATA_IN_INPUT | CIA_SERIAL_CLK_IN_INPUT | CIA_VIC2_BANK_OUTPUT; $03
                                                                                                                                   ; effectively, this is
                                                                                                                                   ; the KERNAL flag:
                                                                                                                                   ; 0 = input  = loader,
        ; DATA OUT, CLK OUT, ATN out are low, RS232_TXD is low                                                                     ; 1 = output = KERNAL
        CIA2_DDRA_KERNAL   = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_RS232_TXD_OUTPUT; $3f

        ; DATA OUT is high, CLK OUT, ATN out are low, RS232_TXD is input
        CIA2_DDRA_INSTALL  = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_INPUT  | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_RS232_TXD_INPUT ; $1b

        ; DATA OUT, CLK OUT, ATN OUT are high, RS232_TXD is input; all bits except bank bits are inputs so that $DD00 writes do not change the bus state
        CIA2_DDRA_IDLE     = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_INPUT  | CIA_SERIAL_CLK_OUT_INPUT  | CIA_SERIAL_ATN_OUT_INPUT  | CIA_RS232_TXD_INPUT ; $03

        ; CLK OUT is high, DATA OUT and ATN out are low, RS232_TXD is input
        CIA2_DDRA_WAKEUP   = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_SERIAL_CLK_OUT_INPUT  | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_RS232_TXD_INPUT ; $2b

        ; DATA OUT and CLK OUT are low, ATN out is high, RS232_TXD is input
        CIA2_DDRA_WAIT     = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_ATN_OUT_INPUT  | CIA_RS232_TXD_INPUT ; $33

        ; DATA OUT, CLK OUT, ATN out are low, RS232_TXD is input
        CIA2_DDRA_OPERATE  = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_SERIAL_ATN_OUT_OUTPUT | CIA_RS232_TXD_INPUT ; $3b
    .endif; !CIA2_DDRA_COMMON

    ; RS232 TXD set to output so this bit won't interfere with byte fetch from serial bus
    CLOCK                  = CIA2_DDRA_COMMON | CIA_SERIAL_DATA_OUT_OUTPUT | CIA_SERIAL_CLK_OUT_OUTPUT | CIA_RS232_TXD_OUTPUT; $37
    CLOCK_ATN_HI           = CLOCK | CIA_SERIAL_ATN_OUT_INPUT ; 1st and 3rd bit pairs, $37
    CLOCK_ATN_LO           = CLOCK | CIA_SERIAL_ATN_OUT_OUTPUT; 2nd and 4th bit pairs, $3f

    .macro ENABLE_WAITBUSY_KERNAL
            lda #.lobyte(~CIA_SERIAL_CLK_OUT_OUTPUT)
            php
            sei
            and CIA2_PRA
            sta CIA2_PRA
            plp
    .endmacro

    .macro INIT_CLEAR_DATA_OUT_CLEAR_CLK_OUT_CLEAR_ATN_OUT
            ; this is executed during install and openfile if IDLE_BUS_LOCK != 0
            ; hence it accesses the port register rather than the data direction register
            lda #.lobyte(~(SERIAL_DATA_OUT | SERIAL_CLK_OUT | SERIAL_ATN_OUT | RS232_TXD))
            and CIA2_PRA
            sta CIA2_PRA
    .endmacro

    .ifndef BEGIN_INSTALL_DEFINED
        BEGIN_INSTALL_DEFINED = 1
        .macro BEGIN_INSTALL
            lda #CIA2_DDRA_INSTALL
            sta CIA2_DDRA
        .endmacro
    .endif

    .macro BRANCH_IF_IDLE to
            ldx #CIA2_DDRA_IDLE
            cpx CIA2_DDRA
            beq to
    .endmacro

    .macro BRANCH_IF_NOT_IDLE to
            lda #CIA2_DDRA_IDLE
            cmp CIA2_DDRA
            bne to
    .endmacro

    .if LOAD_VIA_KERNAL_FALLBACK
            KERNALFILENO = $02
    .endif

    .macro BRANCH_IF_INSTALLED to
            lda #CIA2_DDRA_KERNAL
            cmp CIA2_DDRA
            bne to
    .endmacro

    .macro BRANCH_IF_NOT_INSTALLED to
            lda #CIA2_DDRA_KERNAL
            cmp CIA2_DDRA
            beq to
    .endmacro

    .ifndef SENDBYTE_DEFINED
        SENDBYTE_DEFINED = 1
        .macro SENDBYTE
            DO_SENDBYTE SERIAL_CLK_OUT, SERIAL_DATA_OUT
        .endmacro

        .macro SENDBYTE_ATN
            DO_SENDBYTE SERIAL_ATN_OUT, SERIAL_CLK_OUT
        .endmacro

        .macro DO_SENDBYTE data, clock
            ; y is not changed

            .local sendbyte
            .local bitset

            ldx #$07
sendbyte:   lsr
            pha
            pha; delay
            lda CIA2_DDRA
            and #.lobyte(~data)
            bcs bitset
            ora #data
bitset:     eor #clock
            sta CIA2_DDRA
            pla; delay
            pla
            dex
            bpl sendbyte
        .endmacro
    .endif

    .ifndef SET_FLAGS_N_DATA_V_CLK_DEFINED
        SET_FLAGS_N_DATA_V_CLK_DEFINED = 1
        .macro SET_FLAGS_N_DATA_V_CLK
            bit CIA2_PRA    
        .endmacro
    .endif

    .ifndef INSTALL_IDLE_DEFINED
        INSTALL_IDLE_DEFINED = 1
        .macro INSTALL_IDLE
            lda #CIA2_DDRA_IDLE
            sta CIA2_DDRA
        .endmacro
    .endif

    .macro IDLE
            ldx #CIA2_DDRA_IDLE
            stx CIA2_DDRA
    .endmacro

    .macro WAKEUP
        .if IDLE_BUS_LOCK
            ; when the loader is idle, the user is
            ; allowed to write anything to CIA2_DDRA ($DD00) -
            ; set it to a known and valid state here.
            ;php; without these, there's a race condition and thus a small
            ;sei; chance for video glitches, but with these, there's a
            INIT_CLEAR_DATA_OUT_CLEAR_CLK_OUT_CLEAR_ATN_OUT
            ;plp; chance for actual crashes with stable raster routines
        .endif
            lda #CIA2_DDRA_WAKEUP
            sta CIA2_DDRA
    .endmacro

    .macro CLEAR
            lda #CIA2_DDRA_WAIT
            sta CIA2_DDRA
    .endmacro
    
    .macro SEND_BLOCK_SIGNAL
            ldy #CIA2_DDRA_OPERATE
            sty CIA2_DDRA
            jsr retrnokclc; delay
    .endmacro

    .macro GETBYTE_SETUP
            lda #CLOCK_ATN_HI
            sta GETBYTE_CLOCK_ATN_HI
    .endmacro

    .macro GETBYTE label, store
            .local entry

            ; note: between each store to CIA2_DDRA to signal ready for the next bit pair,
            ;       10 cycles must pass before read access of CIA2_PRA:
            ;       the drive needs max. 14 cycles to respond with a loop in the form of 'bit VIA1_PRB : bpl/bmi * - 3 : sta VIA1_PRB' -
            ;       this means that 18 cycles per bit pair are the minimum

label:      sec
            bcs entry

:           lsr                                 ; 2 - -3210HHH
            lsr                                 ; 2 - --3210HH:H
.if NTSC_COMPATIBILITY
            ldx GETBYTE_CLOCK_ATN_HI ; $37      ; 3
.else
            ldx #CLOCK_ATN_HI ; $37             ; 2
.endif
            ora CIA2_PRA - CLOCK_ATN_HI,x       ; 5 - 543210HH - delay
            stx CIA2_DDRA; sync 3: set ATN high ; 4
                                                ; = 18

            ror                                 ; 2 - H543210H:H
            ror                                 ; 2 - HH543210
            ldx #CLOCK_ATN_LO ; $3f             ; 2
            sax :+ + $01                        ; 4 - --543210
.if NTSC_COMPATIBILITY
            and CIA2_PRA - CLOCK_ATN_LO,x       ; 4 - 7654.2..
.else
            and CIA2_PRA                        ; 4 - 7654.2..
.endif
            stx CIA2_DDRA; sync 4: set ATN low  ; 4
                                                ; = 18

:           ora #$00                            ; 2 - 76543210
            store                               ; 5 - sta abs,y
entry:      lax GETBYTE_CLOCK_ATN_HI ; $37      ; 3
.if NTSC_COMPATIBILITY
            adc CIA2_PRA - CLOCK_ATN_HI,x       ; 5 - 10HHH...
.else
            adc CIA2_PRA                        ; 4 - 10HHH...
.endif
            stx CIA2_DDRA; sync 1: set ATN high ; 4
                                                ; = 18

            iny                                 ; 2
            beq :+                              ; 2
            lsr                                 ; 2 - -10HHH..
            lsr                                 ; 2 - --10HHH.
            ldx #CLOCK_ATN_LO ; $3f             ; 2
.if NTSC_COMPATIBILITY
            ora CIA2_PRA - CLOCK_ATN_LO,x       ; 5 - 3210HHH.
.else
            ora CIA2_PRA                        ; 4 - 3210HHH.
.endif
            stx CIA2_DDRA; sync 2: set ATN low  ; 4
                                                ; = 18

            jmp :--                             ; 3
:
    .endmacro

    .macro STOREBYTE_ALLRAM
            ENABLE_ALL_RAM_X
storebytio: sta a:$00,y
memconfig:  ldx #$00
            SET_MEMCONFIG_X
    .endmacro

    .macro ENDGETBLOCK
            ; nothing to do
    .endmacro

    .macro SET_IO_KERNAL
            lda #CIA2_DDRA_KERNAL
            sta CIA2_DDRA
    .endmacro

.endif; !_CIA_INC_

.endif; !_HAL_C64_INC_
