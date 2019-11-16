
.include "cpu.inc"
.include "cia.inc"
.include "via.inc"

.include "drives/drivecode-common.inc"

BUFFER                = $00
SYS_SP                = $01
JOBCODESTABLE         = $02; fixed in ROM
JOBTRKSCTTABLE        = $0b; fixed in ROM - $0b..$1c
FILETRACK             = $0b
FILESECTOR            = $0c
FILENAMEHASH0         = $0d
FILENAMEHASH1         = $0e
NUMFILES              = $0f
CURRDIRBLOCKTRACK     = $10
CURRDIRBLOCKSECTOR    = $11
CYCLESTARTENDTRACK    = $12
CYCLESTARTENDSECTOR   = $13
DIRCYCLEFLAG          = $14
NEXTDIRBLOCKTRACK     = $15
NEXTDIRBLOCKSECTOR    = $16
;BLOCKBUFFERJOBTRACK  = $17; fixed in ROM - track for job at buffer 6 ($0900)
;BLOCKBUFFERJOBSECTOR = $18; fixed in ROM - sector for job at buffer 6 ($0900)
FIRSTDIRSECTOR        = $19
NEXTCONTIGUOUSBLOCK   = $1a
TEMP                  = $1b

DISKCHANGED           = $25; fixed in ROM
DRIVESTATE            = $26; fixed in ROM
DRIVEOFF              = $00; literal
OPEN_FILE_TRACK       = $4c; fixed in ROM
WRAPFILEINDEX         = $63

LED_FLAG              = $79
IRQVECTOR_LO          = $0192
IRQVECTOR_HI          = $0193
HDRS2                 = $01bc
DIRTRACK81            = $022b
OPEN_FILE_SECTOR      = $028b

STROBE_CONTROLLER     = $ff54

READ_DV               = $80
MOTOFFI_DV            = $8a
SEEK_DV               = $8c

OK_DV                 = $00

BUFFER0               = $0300
BUFFERSIZE            = $0100

BLOCKBUFFER           = $0900
TRACKOFFSET           = $00
SECTOROFFSET          = $01
SENDTABLELO           = $0a00
SENDTABLEHI           = $0b00

LINKTRACK             = BLOCKBUFFER + TRACKOFFSET
LINKSECTOR            = BLOCKBUFFER + SECTOROFFSET

BINARY_NIBBLE_MASK    = %00001111

ROMOS_MAXTRACK        = $8f; MAXTRACK81 - 1
ROMOS_MAXSECTOR       = $75; MAXSECTOR81 + 1
MAXTRACK81            = 80; literal
MAXSECTOR81           = 39; literal

.if !DISABLE_WATCHDOG
RESET_TIMERB          = $cb9f
WATCHDOG_PERIOD       = $20; 32 * 65536 cycles at 2 MHz = 1.049 s
CONTROLLERIRQPERIODFD = $4e20
    .macro INIT_CONTROLLER
            jsr initcontrl
    .endmacro
.else
    .macro INIT_CONTROLLER
    .endmacro
.endif

BUFFERINDEX           = (BLOCKBUFFER - BUFFER0) / BUFFERSIZE

            .org $0300

.export cmdfdfix0 : absolute
.export cmdfdfix1 : absolute
.export cmdfdfix2 : absolute
.if !DISABLE_WATCHDOG
.export cmdfdfix3 : absolute
.export cmdfdfix4 : absolute
.endif

drvcodebeg81: .byte .hibyte(drvcodeend81 - * + $0100 - $01); init transfer count hi-byte

SENDNIBBLETAB:
            BIT0DEST = 3
            BIT1DEST = 1
            BIT2DEST = 2
            BIT3DEST = 0

            .repeat $10, I
                .byte (((~I >> 0) & 1) << BIT0DEST) | (((~I >> 1) & 1) << BIT1DEST) | (((~I >> 2) & 1) << BIT2DEST) | (((~I >> 3) & 1) << BIT3DEST)
            .endrep

.if !DISABLE_WATCHDOG
sysirqvbuf: .byte 0, 0
.endif

filename:   ; note: this is not in the zero-page

dcodinit81: tsx
            stx SYS_SP
.if !DISABLE_WATCHDOG
            lda IRQVECTOR_LO
            sta sysirqvbuf + 0
            lda IRQVECTOR_HI
            sta sysirqvbuf + 1
.endif
            jsr motrledoff

            ldx #$00
:           txa
            and #BINARY_NIBBLE_MASK
            tay
            lda SENDNIBBLETAB,y
            sta SENDTABLELO,x
            txa
            lsr
            lsr
            lsr
            lsr
            tay
            lda SENDNIBBLETAB,y
            sta SENDTABLEHI,x
            inx
            bne :-

.if !DISABLE_WATCHDOG
            lda cmdfdfix2; 0 for FD
            beq :+
            ; watchdog initialization
            lda #$ff
            sta CIA_TA_LO
            sta CIA_TA_HI
            lda #COUNT_PHI2 | FORCE_LOAD | CONTINUOUS | TIMER_START
            sta CIA_CRA
            jsr initwatchd
            lda #CIA_CLR_INTF | EVERY_IRQ
            sta CIA_ICR
            lda #CIA_SET_INTF | TIMERB_IRQ
            sta CIA_ICR
            bne :++
:           jsr initwatchd
:
.endif
            ldx #$00
            stx NUMFILES
            lda #DATA_OUT
            sta CIA_PRB; signal idle to the host, note that ATNA response with ATN_IN low is not possible on 1581,
                       ; so a KERNAL LISTEN command cannot be detected for automatic uninstallation
:           bit CIA_PRB
            bpl :-; no watchdog

drividle:   jsr fadeled; fade off the busy led
            lda CIA_PRB
            and #ATN_IN | ATNA_OUT | CLK_OUT | CLK_IN | DATA_OUT | DATA_IN
            eor #ATN_IN |                      CLK_IN | DATA_OUT | DATA_IN
            beq drividle; wait until there is something to do
            asl; check for reset, uninstallation or custom drive code upload
            beq :+
            jmp duninstall; check for reset or uninstallation

            ; load a file

:           txa
            beq beginload; check whether the busy led has been completely faded off
            jsr busyledon; if not, turn it on
beginload:

    .if !DISABLE_WATCHDOG
            jsr enablewdog; enable watchdog, the computer might be reset while sending over a
                          ; byte, leaving the drive waiting for handshake pulses
    .endif; !DISABLE_WATCHDOG

            GET_FILENAME 1581
            ; matches against hash of filename in FILENAMEHASH0/1
            FIND_FILE 1581
            bcs filenotfnd
            lda FILENAMEHASHVAL0,x ; store hash of next file's
            sta FILENAMEHASH0      ; name for loadnext
            lda FILENAMEHASHVAL1,x ; and PREPARE_NEXT_FILE
            sta FILENAMEHASH1      ; functionality
            lda DIRTRACKS - 1,x
            tax

            ; check for illegal track or sector
            beq toillegal + $00

            dex; 79->77 cmp 79 -> bcc
            dex; 80->78 cmp 79 -> bcc
               ; 81->80 cmp 79 -> bcs
            cpx ROMOS_MAXTRACK; #MAXTRACK81 - 1
            inx
            inx
            bcs toillegal + $01
            cpy ROMOS_MAXSECTOR; #MAXSECTOR81 + 1
            bcc :+
toillegal:  sec
cmdfdfix0:  jmp illegalts; is changed to bit illegalts on FD2000/4000 to disable illegal track or sector error,
                         ; ROM variables for logical track/sector boundaries aren't known (probably around MAXTRACKFD = $54)

:           tya
            pha
            jsr busyledon
            pla
            tay; FILESECTOR
            txa; FILETRACK

            ldx #$02
            stx NEXTCONTIGUOUSBLOCK

loadblock:  sta JOBTRKSCTTABLE + (2 * BUFFERINDEX) + TRACKOFFSET
            sty JOBTRKSCTTABLE + (2 * BUFFERINDEX) + SECTOROFFSET
:           jsr getblockag
            bcs :-

           ;ldy LINKSECTOR
           ;lda LINKTRACK
            pha
            beq :+
            ldy #$ff
:           lda LINKSECTOR; the file's last block's length (last byte index)
            pha
            sty blocksize + $01
            dey
            dey
            tya
            eor #$ff
            ldx LINKTRACK
            beq :+
            lda NEXTCONTIGUOUSBLOCK
:           sta BLOCKBUFFER + $01; block length
            lda #$41
            ldx #$00
            cpx LINKTRACK; last track: bit 0 set
            bne :+
            lda #$40
:           rol
            jsr sendblock; send the block over
            inc NEXTCONTIGUOUSBLOCK
            pla; LINKSECTOR
            tay
            pla; LINKTRACK
            bne loadblock

            ; loading is finished

            clc; all ok after loading

filenotfnd: ; branches here with carry set
illegalts:  ; or illegal t or s

            jsr sendstatus

            lda BLOCKBUFFER + $00; offset 0: block index or status byte
            bmi skipprpnxt; only after successful load
            PREPARE_NEXT_FILE 1581
skipprpnxt:
            ldx #$01; turn motor and busy led off
            lda #DRIVE_LED; check if busy led is lit
            and CIA_PRA
            beq :+
            ldx #$ff; fade off the busy led, then turn motor off
:           jmp drividle

.if !DISABLE_WATCHDOG
initcontrl: lda sysirqvbuf + 0
            sta IRQVECTOR_LO
            lda sysirqvbuf + 1
            sta IRQVECTOR_HI
            lda cmdfdfix2; 0 for FD
            beq :+
            jmp RESET_TIMERB
:           lda #.lobyte(CONTROLLERIRQPERIODFD)
            sta VIA_T1C_L
            lda #.hibyte(CONTROLLERIRQPERIODFD)
            sta VIA_T1C_H
            rts
.endif

cmdfdfix1 = * + 1
cmdfdfix2 = * + 2
getdirtrk:  lda DIRTRACK81
            rts

trackseek:  tax
            dex
            stx HDRS2 + (2 * BUFFERINDEX)
            INIT_CONTROLLER
            lda #SEEK_DV
            ldx #BUFFERINDEX
            jsr STROBE_CONTROLLER

            lda DISKCHANGED
            ora diskchangd + 1
            sta diskchangd + 1

.if DISABLE_WATCHDOG
            rts
.else
            ; fall through

initwatchd: ; the i-flag is set here
            lda #.lobyte(wdoghandlr)
            sta IRQVECTOR_LO
            lda #.hibyte(wdoghandlr)
            sta IRQVECTOR_HI
            lda cmdfdfix2; 0 for FD
            beq :+
            lda #.lobyte(WATCHDOG_PERIOD)
            sta CIA_TB_LO
            lda #.hibyte(WATCHDOG_PERIOD)
            sta CIA_TB_HI
:           rts

enablewdog: lda cmdfdfix2; 0 for FD
            beq :+
            lda #COUNT_TA_UNDF | FORCE_LOAD | ONE_SHOT | TIMER_START
            sta CIA_CRB
            bit CIA_ICR
            ENABLE_WATCHDOG
            rts
:           lda #IRQ_CLEAR_FLAGS | IRQ_ALL_FLAGS
            sta VIA_IER; no IRQs from VIA
            lda #IRQ_SET_FLAGS | IRQ_TIMER_1
            sta VIA_IER; timer 1 IRQs from VIA
            lda #$ff
            sta VIA_T1C_H
            ENABLE_WATCHDOG
            rts
.endif; !DISABLE_WATCHDOG

busyledon:  lda #DRIVE_LED
            ora CIA_PRA
            ldy #$ff
            bne store_cia; jmp

fadeled:    txa
            tay
            beq ledisoff
:           nop
            bit OPC_BIT_ZP
            iny
            bne :-
            pha
            jsr busyledon
            pla
            tay
:           nop
            bit OPC_BIT_ZP
            dey
            bne :-
            dex
            bne busyledoff

motrledoff: ; turn off motor
            txa
            pha
            INIT_CONTROLLER
            lda #MOTOFFI_DV
            ldx #BUFFERINDEX
            jsr STROBE_CONTROLLER
            lda #DRIVEOFF
            sta DRIVESTATE
            pla
            tax

busyledoff: lda CIA_PRA
            and #.lobyte(~DRIVE_LED); turn off drive led
            ldy #$00
store_cia:  sta CIA_PRA
            sty LED_FLAG
ledisoff:   rts

getblock:   sta JOBTRKSCTTABLE + (2 * BUFFERINDEX) + TRACKOFFSET
            sty JOBTRKSCTTABLE + (2 * BUFFERINDEX) + SECTOROFFSET
getblockag: INIT_CONTROLLER
            lda #READ_DV
            ldx #BUFFERINDEX
            jsr STROBE_CONTROLLER

            lda DISKCHANGED
            ora diskchangd + 1
            sta diskchangd + 1

.if !DISABLE_WATCHDOG
            jsr initwatchd
.endif
            lda JOBCODESTABLE + BUFFERINDEX; FD does not return the error status in the accu
            cmp #OK_DV + 1

            ; the link track is returned last so that the z-flag
            ; is set if this block is the file's last one (see FIND_FILE)
            ldy LINKSECTOR
            ldx JOBTRKSCTTABLE + (2 * BUFFERINDEX) + SECTOROFFSET; LOADEDSECTOR
            lda LINKTRACK
            rts

duninstall: lda #$00
            sta CIA_PRB
            lda CIA_PRB
            and #ATN_IN | ATNA_OUT | CLK_OUT | CLK_IN | DATA_OUT | DATA_IN
            cmp #                                                  DATA_IN
            bne noupload

            ldx SYS_SP
            txs
            INIT_CONTROLLER

customparm = JOBTRKSCTTABLE
uploadrout = $2000 - noupload + dgetbyte

            ldx #$00
:           lda dgetbyte,x
            sta uploadrout,x
            inx
            bpl :-
            inx; $81, ldx #OPC_STA_ZPXI
            stx uploadrout + getbyterts - dgetbyte

            lda #CLK_OUT
            sta CIA_PRB; set CLK_OUT to signal ready for code upload

            ldx #$05

:           bit CIA_PRB
            bpl :-; wait for ATN_IN high

            ; get custom drive code
:           jsr dgetbyte
            sta customparm,x
            dex
            bpl :-
            jmp uploadrout

            ; must not clobber x
dgetbyte:   lda #%10000000; CLK_OUT and DATA_OUT low: drive is ready
            sta CIA_PRB
:           pha
            lda CIA_PRB
:           cmp CIA_PRB
            beq :-
            lda CIA_PRB
            asl
            pla
            ror
            bcc :--
getbyterts: rts; is changed to sta (zp,x) for custom drive code upload
            .byte customparm + $02 + 1
            inc customparm + $02
            bne :+
            inc customparm + $03
:           dec customparm + $04
            bne dgetbyte
            dec customparm + $05
            bpl dgetbyte

            jmp (customparm); execute custom drive code

noupload:   txa
            beq :+
wdoghandlr: jsr busyledon
            lda #$ff; fade off the busy led
:           pha
            jsr getdirtrk
            jsr trackseek
            pla
            tax
:           jsr fadeled
            txa
            bne :-
            ldx SYS_SP
            txs
            INIT_CONTROLLER
            rts

            FNAMEHASH 1581

            ; carry: clear = ok, set = load error
sendstatus: lda #$00
            sta blocksize + $01

            lda #diskio::status::FILE_NOT_FOUND
            bcs sendblock
            lda #diskio::status::OK
sendblock:  sta BLOCKBUFFER + $00; block index
.if !DISABLE_WATCHDOG
            jsr enablewdog
.endif
            lda #CLK_OUT; drop DATA_OUT, set CLK_OUT high as signal of presence
            sta CIA_PRB; block ready signal
waitready:  bit CIA_PRB
            bmi waitready; wait for ATN low

            ldy #$00
sendloop:
    .if !DISABLE_WATCHDOG
cmdfdfix3 = * + 1
            lda #COUNT_TA_UNDF | FORCE_LOAD | ONE_SHOT | TIMER_START ; is changed to VIA access for FD
cmdfdfix4 = * + 1
            sta CIA_CRB                                              ; 2 + 4 - reset watchdog time-out
    .endif
            ldx BLOCKBUFFER,y                                        ; 4
            lda SENDTABLELO,x                                        ; 4
                                                                     ; = 22 (+6 with watchdog)

:           bit CIA_PRB                                              ; 4
            bmi :-                                                   ; 3
            sta CIA_PRB                                              ; 4
            asl                                                      ; 2
            and #.lobyte(~ATNA_OUT)                                  ; 2
                                                                     ; = 15

:           bit CIA_PRB                                              ; 4
            bpl :-                                                   ; 3
            sta CIA_PRB                                              ; 4
            ldx BLOCKBUFFER,y                                        ; 4
            lda SENDTABLEHI,x                                        ; 4
                                                                     ; = 19

:           bit CIA_PRB                                              ; 4
            bmi :-                                                   ; 3
            sta CIA_PRB                                              ; 4
            asl                                                      ; 2
            and #.lobyte(~ATNA_OUT)                                  ; 2
blocksize:  cpy #$00                                                 ; 2
            iny                                                      ; 2
                                                                     ; = 19

:           bit CIA_PRB                                              ; 4
            bpl :-                                                   ; 3
            sta CIA_PRB                                              ; 4
            bcc sendloop                                             ; 3
                                                                     ; = 75
:           bit CIA_PRB; wait for acknowledgement
            bmi :-     ; of the last data byte

            lda #DATA_OUT; drive busy
            sta CIA_PRB

:           bit CIA_PRB; wait for ATN in high,
            bpl :-     ; acknowledgement of the block transfer

            sei; disable watchdog
            rts

drvcodeend81:

DIRBUFFSIZE      = (BLOCKBUFFER - *) / 4
DIRTRACKS        = *
DIRSECTORS       = DIRTRACKS + DIRBUFFSIZE
FILENAMEHASHVAL0 = DIRSECTORS + DIRBUFFSIZE
FILENAMEHASHVAL1 = FILENAMEHASHVAL0 + DIRBUFFSIZE

            .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"

DIRBUFFSIZE81 = DIRBUFFSIZE
.export DIRBUFFSIZE81

            .assert * <= BLOCKBUFFER, error, "***** 1581 drive code too large. *****"

            ; following code is transferred using KERNAL routines, then it is
            ; run and gets the rest of the code

            ; entry point

dinstall:   sei
            lda #CIA_ATN_IN_INPUT | WRITE_PROTECT_OUTPUT | FSM_BUS_DRIVER_DIRECTION_OUTPUT | ATNA_ENABLE_OUT_OUTPUT | CLK_OUT_OUTPUT | CLK_IN_INPUT | DATA_OUT_OUTPUT | DATA_IN_INPUT
            sta CIA_DDRB
            lda #CLK_OUT
            sta CIA_PRB

:           lda CIA_PRB; wait for DATA IN high
            lsr
instalwait: bcc :-
            ldx #.lobyte(drvcodebeg81 - $01)
dgetrout:   inx
            bne :+
            inc dgetputhi
:           
            ; must not clobber x
            lda #%10000000; CLK OUT lo: drive is ready
            sta BUFFER
            sta CIA_PRB
:           lda CIA_PRB
:           cmp CIA_PRB
            beq :-
            lda CIA_PRB
            and #CLK_IN
            cmp #CLK_IN
            ror BUFFER
            bcc :--
            lda BUFFER

dgetputhi = * + $02
            sta a:.hibyte(drvcodebeg81 - $01) << 8,x
            cpx #.lobyte(drvcodeend81 - $01)
            bne dgetrout
            dec drvcodebeg81
            bne dgetrout
            jmp dcodinit81

drvprgend81:
            .reloc
