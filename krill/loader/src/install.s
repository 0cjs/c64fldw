
.fileopt comment, "Loader install code portion"
.fileopt compiler, "CA65"
.fileopt author, "Gunnar Ruthenberg"

__NO_LOADER_SYMBOLS_IMPORT = 1
.include "loader.inc"
.include "../version.inc"

.include "cpu.inc"
.include "cia.inc"
.include "basic.inc"; for PETSCII_RETURN
.include "kernal.inc"

; including via.inc would redefine several symbols from cia.inc
VIA2_T1L_H = $1c07; this symbol is used for the fast head stepping M-W for seeking using plain KERNAL routines
VIA_T1C_H  = $1c05; this symbol is used for watchdog servicing on CMD FD

CBM1581_8  = $a6e9; the '8' in the 1581's ID string
FD2K4K_F   = $fea4; the 'f' in the FD 2000/4000's ID string


.include "hal/hal.inc"

.include "drives/drivecode-common.inc"

.importzp BLOCKDESTLO


.if ONLY_1541_AND_COMPATIBLE = 0
.import c1570fix0
.import c1570fix1

.import cmdfdfix0
.import cmdfdfix1
.import cmdfdfix2
    .if !DISABLE_WATCHDOG
.import cmdfdfix3
.import cmdfdfix4
    .endif; !DISABLE_WATCHDOG
.endif; ONLY_1541_AND_COMPATIBLE = 0


USE_GENERIC_DRIVE = 0


.macro itoa4 value
            .if (value & $0f > 9)
                .byte (value & $0f) + 'a' - 10
            .else
                .byte (value & $0f) + '0'
            .endif
.endmacro

.macro itoa1 value
            itoa4 value <> 0
.endmacro

.macro itoa8 value
            itoa4 value >> 4
            itoa4 value & $0f
.endmacro

.macro itoa16 value
            itoa8 value >> 8
            itoa8 value & $ff
.endmacro


.segment "HEADER"
.segment "EXTZP"; not used otherwise, the EXTZP segment is not
                ; optional in the o65 built-in ld65 config

.segment "DISKIO_INSTALL"

.ifdef INSTADDR
            .org INSTADDR - 2
            .word * + 2; load address
.endif

            ; unfortunately, scopes must be defined before using them,
            ; this is why the install code is moved to after the drive code
.scope cbm1541
drivecode41:
            .include "drives/drivecode1541.s"

            .export dcodinit41   : absolute
            .export drivecode41  : absolute
            .export drvcodebeg41 : absolute
            .export drvcodeend41 : absolute
.endscope

    .if ONLY_1541_AND_COMPATIBLE = 0
.scope cbm1571
drivecode71:
            .include "drives/drivecode1571.s"

            .export dcodinit71   : absolute
            .export drivecode71  : absolute
            .export drvcodebeg71 : absolute
            .export drvcodeend71 : absolute
.endscope

.scope cbm1581
drivecode81:
            .include "drives/drivecode1581.s"

            .export dcodinit81   : absolute
            .export drivecode81  : absolute
            .export drvcodebeg81 : absolute
            .export drvcodeend81 : absolute
.endscope
    .endif; ONLY_1541_AND_COMPATIBLE = 0

.export install

            ; Install the loader

            ; in:  nothing
            ; out: c - set on error
            ;      a - status
            ;      x - drive type (one of diskio::drivetype)
            ;      y - if status is diskio::status::OK, zp address of version string address
install:    lda #.lobyte(version)
            sta BLOCKDESTLO + 0
            lda #.hibyte(version)
            sta BLOCKDESTLO + 1

            BRANCH_IF_NOT_INSTALLED :+
            jmp isinstalld

:           php; I flag buffer

            INIT_PAL_NTSC
            INIT_KERNAL_SERIAL_ROUTINES

            ; try the drive as denoted by FA (current drive) first
            lda FA
            cmp #MIN_DEVICE_NO
            bcc :+
            cmp #MAX_DEVICE_NO + 1
            bcc :++
:           lda #MIN_DEVICE_NO; FA does not contain a drive address (MIN_DEVICE_NO..MAX_DEVICE_NO), try MIN_DEVICE_NO first
:
            ; find first available drive,
            ; this is done via the high-level open/read/close routines,
            ; so non-serial bus devices will also respond
            sta FA
find1stdrv: pha; device number
            lda #$00
            sta STATUS
            lda #drvchkone - drvchkon
            ldx #.lobyte(drvchkon)
            ldy #.hibyte(drvchkon)
            jsr SETNAM
            lda #COMMAND_ERROR_CHANNEL
            ldx FA
            tay
            jsr SETLFS
            jsr OPEN
            bcc drivefound; drive present

            ; drive not present, try next address
            lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            jsr CLRCHN
            ldx FA
            inx
            cpx #MAX_DEVICE_NO + 1
            bne :+
            ldx #MIN_DEVICE_NO
:           stx FA
            pla
            cmp FA
            bne find1stdrv

            plp; I flag restore
            lda #diskio::status::DEVICE_NOT_PRESENT
            ldx #diskio::drivetype::DEVICE_NONE
            ldy #BLOCKDESTLO
            sec
            rts

drivefound: pla
            ; read error channel on active drive to stop potentially blinking error LED
            ldx #COMMAND_ERROR_CHANNEL
            jsr CHKIN
:           jsr READST
            bne :+
            jsr CHRIN
            jmp :-
:           lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            jsr CLRCHN

            ; check if there is more than 1 drive on the serial bus,
            ; upload silencing routine to the passive drives in order
            ; to make sure the 2bit+ATN protocol can work alright,
            ; detection is done via the low-level serial bus routines,
            ; so non-serial bus devices won't respond
            ; (1551 on Plus/4 does respond, though, so a little extra
            ; treatment is done through the drive disturbance HAL macros)

            lda FA; active drive
            pha
            ldx #MIN_DEVICE_NO
checkbus:   stx FA
            pla
            pha
            cmp FA
            beq jmpnodrive

            lda #$00
            sta STATUS
            PREPARE_DRIVE_DISTURBANCE_VALIDATION
            jsr drvlistn
            BRANCH_IF_DRIVE_DOES_NOT_DISTURB_SERIAL_BUS jmpnodrive
            jsr READST
            bpl :+
jmpnodrive: jmp nodrive

            ; more than 1 drive on the bus or generic serial devices present
:           jsr UNLSTN

            ; upload and execute silencing routine
.if ONLY_1541_AND_COMPATIBLE = 0
            jsr chkdrvcode
            beq :+
            jmp nodrvcode

:           cpx #'8'
            beq drvsilnc81
            cpy #'f'
            beq drvsilncfd
.endif; ONLY_1541_AND_COMPATIBLE = 0

            jsr drvlistn
            ldx #$00
:           lda drvsilencc,x
            jsr IECOUT
            inx
            cpx #atnfallbck - drvsilencc
            bne :-
            jsr drvlistn - $03
            ldx #$00
:           lda atnfallbck,x
            jsr IECOUT
            inx
            cpx #atnlo - atnfallbck
            bne :-
            jsr drvlistn - $03
            ldx #$00
:           lda atnlo,x
            jsr IECOUT
            inx
            cpx #atnhi - atnlo
            bne :-
            jsr drvlistn - $03
            ldx #$00
:           lda atnhi,x
            jsr IECOUT
            inx
            cpx #atnhiend - atnhi
            bne :-
            jsr drvlistn - $03
            ldx #$00
:           lda drvsilence,x
            jsr IECOUT
            inx
            cpx #drvsilencc - drvsilence
            bne :-

.if ONLY_1541_AND_COMPATIBLE = 0
            beq slncunlstn

drvsilnc81: jsr drvlistn
            ldx #$00
:           lda drvslnc81,x
            jsr IECOUT
            inx
            cpx #drvslnc81e - drvslnc81
            bne :-
            beq slncunlstn

drvsilncfd: jsr drvlistn
            ldx #$00
:           lda drvslncfd,x
            jsr IECOUT
            inx
            cpx #drvslncfde - drvslncfd
            bne :-
.endif; ONLY_1541_AND_COMPATIBLE = 0

slncunlstn: jsr UNLSTN
            jmp nodrive

nodrvcode:  pla
            sta FA
            plp; I flag restore
            lda #diskio::status::TOO_MANY_DEVICES
            ldx #diskio::drivetype::DRIVE_GENERIC
            ldy #BLOCKDESTLO
    .if LOAD_VIA_KERNAL_FALLBACK
            clc; this is not to be regarded as an error
    .else
            sec
    .endif
            rts

nodrive:    jsr UNLSTN
            ldx FA
            inx
            cpx #MAX_DEVICE_NO + 1
            beq :+
            jmp checkbus
:           pla
            sta FA; active drive

    .if ONLY_1541_AND_COMPATIBLE = 0
            jsr chkdrvcode
            bne usegeneric

        .if USE_GENERIC_DRIVE
            jmp usegeneric
        .endif

            ; drive allows code upload and execution,
            ; check which model the drive is and upload corresponding drive code

        .ifdef cbm1541::CX500
            lda #.lobyte($fbaa)
            ldx #.hibyte($fbaa)
            jsr memreadbyt
            cmp #'C'
            bne :+
            cpx #'x'
            bne :+
            jmp is1541
:
        .endif

            ; check if running on a 1541/70/71 compatible drive
            lda #.lobyte($e5c6)
            ldx #.hibyte($e5c6)
            jsr memreadbyt

        .ifndef cbm1541::CX500
            cmp #'4'
            beq is1541
        .endif
            cmp #'7'
            bne :+
            jmp is157x
:
            ; neither 1541 nor 157x

            ; try FD2000/FD4000
            lda #.lobyte(FD2K4K_F)
            ldx #.hibyte(FD2K4K_F)
            jsr memreadbyt
            cmp #'f'
            bne check1581
            lda #OPC_BIT_ABS
            sta cmdfdfix0 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.lobyte($54); DIRTRACKFD
            sta cmdfdfix1 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.hibyte($54); DIRTRACKFD
            sta cmdfdfix2 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.lobyte($fef0)
            ldx #.hibyte($fef0)
            jsr memreadbyt
            ldy #diskio::drivetype::DRIVE_CMD_FD_2000
            cmp #'4'
            bne isfd2000
            iny; diskio::drivetype::DRIVE_CMD_FD_4000
isfd2000:   
        .if !DISABLE_WATCHDOG
            lda #$ff
            ldx #.lobyte(VIA_T1C_H)
        .endif
            jmp iscmdfd

            ; check if 1581
check1581:  lda #.lobyte(CBM1581_8)
            ldx #.hibyte(CBM1581_8)
            jsr memreadbyt
            ldy #diskio::drivetype::DRIVE_1581
            cmp #'8'
            bne usegeneric
            jmp is1581

usegeneric: ; no compatible drive found
            plp; I flag restore
            lda #diskio::status::DEVICE_INCOMPATIBLE
            ldx #diskio::drivetype::DRIVE_GENERIC
            ldy #BLOCKDESTLO
        .if LOAD_VIA_KERNAL_FALLBACK
            clc; this is not to be regarded as an error
        .else
            sec
        .endif
            rts

            ; select appropriate drive code

is1541:     ; find out if 1541 or 1541-C/1541-II
            lda #.lobyte($c002)
            ldx #.hibyte($c002)
            jsr memreadbyt
            cmp #'c'
            beq chk1541ii; branch if 'c' at $c002 (from 'COPYRIGHT' etc.)
            ; find out if 1541 or 1541-C
            lda #.lobyte($eaa3)
            ldx #.hibyte($eaa3)
            jsr memreadbyt
            ldy #diskio::drivetype::DRIVE_1541
            cmp #$ff
            beq check1541u
            ; 1541-C: no $ff at $eaa3 (but likely $fe, data direction for track 0 sensor bit)
            ldy #diskio::drivetype::DRIVE_1541_C
            beq check1541u
chk1541ii:  lda #.lobyte($e5b7)
            ldx #.hibyte($e5b7)
            jsr memreadbyt
            tax
            lda #$ff
            cpx #'c' | $80; 'CBM DOS' etc.
            bne :+; treat as 1541-II if no match, so as not to detect SpeedDOS etc. as 1541-C but 1541-II instead
            ; find out if 1541-C or 1541-II
            lda #.lobyte($eaa3)
            ldx #.hibyte($eaa3)
            jsr memreadbyt
:           ldy #diskio::drivetype::DRIVE_1541_C
            cmp #$ff
            bne check1541u; 1541-C: no $ff at $eaa3 (but likely $fe, data direction for track 0 sensor bit)
            iny; diskio::drivetype::DRIVE_1541_II: $ff at $eaa3
            ; find out if 1541U
check1541u: sty drivetype
            jsr drvlistn
            ldx #$00
:           lda drvch1541u,x
            jsr IECOUT
            inx
            cpx #drvchkued - drvch1541u
            bne :-
            jsr UNLSTN
            lda #.lobyte($0300)
            ldx #.hibyte($0300)
            jsr memreadbyt
            bmi :+; branch if 1541U
            ldy drivetype
            SKIPWORD
:           ldy #diskio::drivetype::DRIVE_1541U
            jmp selectdcod

            ; find out if 1570 or 1571
is157x:     cpx #'1' | $80; 71
            lda #OPC_BIT_ZP
            ldx #OPC_BIT_ABS; no VIA2_PRA writes to switch sides
            ldy #diskio::drivetype::DRIVE_1570
            bcc is1570
            ; 1571 or 1571CR

            ; quicker head stepping
            jsr drvlistn
            ldx #$06
:           lda drvfaststp,x
            jsr IECOUT
            dex
            bpl :-
            jsr UNLSTN

            jsr drvlistn
            ldx #$04
:           lda twosided,x
            jsr IECOUT
            dex
            bpl :-
            jsr UNLSTN

            lda #.lobyte($e5c2)
            ldx #.hibyte($e5c2)
            jsr memreadbyt
            cmp #'1'; 3.1
            lda #OPC_BCS
            ldx #OPC_STA_ABS
            ldy #diskio::drivetype::DRIVE_1571
            bcc :+
            iny; diskio::drivetype::DRIVE_1571CR
:
is1570:     sta c1570fix0 - cbm1571::drvcodebeg71 + cbm1571::drivecode71
            stx c1570fix1 - cbm1571::drvcodebeg71 + cbm1571::drivecode71

            ; fall through

is1581:     lda #OPC_JMP_ABS
            sta cmdfdfix0 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.lobyte($022b); DIRTRACK81
            sta cmdfdfix1 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            lda #.hibyte($022b); DIRTRACK81
            sta cmdfdfix2 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
        .if !DISABLE_WATCHDOG
            lda #COUNT_TA_UNDF | FORCE_LOAD | ONE_SHOT | TIMER_START
            ldx #.lobyte(CIA_CRB)
iscmdfd:    sta cmdfdfix3 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
            stx cmdfdfix4 - cbm1581::drvcodebeg81 + cbm1581::drivecode81
        .else
iscmdfd:
        .endif

selectdcod: sty drivetype
            tya
            lsr
            lsr
            lsr
            lsr
            tax
            lda dcodeselt0,x
            sta dcodesel0
            lda dcodeselt1,x
            sta dcodesel1
            lda dcodeselt2,x
            sta dcodesel2
            lda dcodeselt3,x
            sta dcodesel3
            lda dcodeselt4,x
            sta dcodesel4
            lda dcodeselt5,x
            sta dcodesel5
            lda dcodeselt6,x
            sta dcodesel6
            lda dcodeselt7,x
            sta dcodesel7
            lda dcodeselt8,x
            sta dcodesel8

    .else; ONLY_1541_AND_COMPATIBLE

            lda #diskio::drivetype::DRIVE_1541
            sta drivetype
            lda #.lobyte(cbm1541::drvcodeend41 - cbm1541::drvcodebeg41 + cbm1541::drivecode41)
            sta dcodesel0
            lda #.hibyte(cbm1541::drvcodeend41 - cbm1541::drvcodebeg41 + cbm1541::drivecode41)
            sta dcodesel1
            lda #.lobyte(cbm1541::drvprgend41 - cbm1541::drvcodeend41 + cbm1541::TRAMPOLINEOFFSET)
            sta dcodesel2
            lda #.lobyte(cbm1541::drivecode41)
            sta dcodesel3
            lda #.hibyte(cbm1541::drivecode41)
            sta dcodesel4
            lda #.hibyte(cbm1541::drvcodeend41 - cbm1541::TRAMPOLINEOFFSET)
            sta dcodesel5
            lda #.lobyte(cbm1541::drvcodeend41 - cbm1541::TRAMPOLINEOFFSET)
            sta dcodesel6
            lda #.hibyte(cbm1541::dinstall)
            sta dcodesel7
            lda #.lobyte(cbm1541::dinstall)
            sta dcodesel8

    .endif; ONLY_1541_AND_COMPATIBLE

            ; install drive-side loader code
            jsr drvlistn

            ldx #$00
install1:   sec
            stx :+ + $01
dcodesel2 = * + $01
            lda #$00
:           sbc #$00
            cmp #$23
            bcc :+
            lda #$23
:           sta drvrutmw

            ldy #$06
:           lda drvrutmw - 1,y
            jsr IECOUT
            dey
            bne :-

dcodesel0 = * + $01
dcodesel1 = * + $02
:           lda a:$00,x
            jsr IECOUT
            inx
            cpx dcodesel2
            beq :+
            iny
            cpy #$23
            bne :-
            jsr drvlistn - $03
            clc
            lda #$23
            adc drvrutmw + $02
            sta drvrutmw + $02
            bcc install1
            inc drvrutmw + $01
            bne install1

:           jsr drvlistn - $03
            ldx #$04
:           lda droutrun,x
            jsr IECOUT
            dex
            bpl :-
            jsr UNLSTN

            INIT_CLEAR_DATA_OUT_CLEAR_CLK_OUT_CLEAR_ATN_OUT
:           SET_FLAGS_N_DATA_V_CLK
            bvs :-
            BEGIN_INSTALL
:           SET_FLAGS_N_DATA_V_CLK
            bvc :-

dcodesel3 = * + $01
            ldy #$00
dcodesel4 = * + $02
fastinst:   lda a:$00,y
            SENDBYTE
            iny
            bne :+
            inc fastinst + $02
:           cpy dcodesel0
            bne fastinst
            lda fastinst + $02
            cmp dcodesel1
            bne fastinst

:           SET_FLAGS_N_DATA_V_CLK; wait for the custom drive code to signal idle
            bmi :-
            INSTALL_IDLE

            ; wait approximately 64K cycles for the silencing routines to kick in
            ldx #$00
            ldy #$40
:           dex
            bne :-; (2 + 3) * 256 = 1280
            dey
            bne :-; 64 * 1280 = 81920

            plp; I flag restore
isinstalld: lda #diskio::status::OK
drivetype = * + $01
            ldx #$00
            ldy #BLOCKDESTLO
            clc
            rts

            jsr UNLSTN
drvlistn:   lda FA
            jsr LISTEN
            lda #SA_OPENCHANNEL | COMMAND_ERROR_CHANNEL
            jmp LSTNSA

drvsilence: .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
            lda #$ff
            sta $1803; set all port pins as outputs
            ldx #.hibyte($0400)
            stx $1801
            ldx #$7f
            stx $1802; set only ATN IN as input
            stx $180e; no IRQs from VIA 1
            stx $1c0e; no IRQs from VIA 2
            ldy #$c0 ; timer 1 IRQs from VIA 2
            sty $1c0e
            ldy #$d0 ; JOBCODE_EXECUTE
            sty $01  ; JOBCODE0400
            ldy #$10 ; ATNA set, CLK OUT low, DATA OUT low
            jmp $0440
drvsilencc: .byte "m-w", .lobyte($0440), .hibyte($0440), atnfallbck - * - 1; read forward
waitactive: ldx #.hibyte($0400); CLK_IN, ATNA cleared, CLK OUT low, DATA OUT low
:           stx $1800; clear ATNA
:           bit $1800
            bpl :-
            sty $1800; set ATNA
           ;lda #$ff
            sta $1c04; timer 1 lo
            sta $1c05; timer 1 hi
:           bit $1800
            bpl :---
            bit $1c05; timer 1 hi
            bne :-
            cpx $1801
            beq * + 39; jmp ($1800)
atnfallbck: .byte "m-w", .lobyte($0440 + atnfallbck - waitactive), .hibyte($0440 + atnfallbck - waitactive), atnlo - * - 1; read forward
            ; slower fallback silence routine, this is required on at least 1541u, which does not implement VIA1 port A correctly
            inc $1803; set all port pins as inputs
:           bit $1800
            bmi :-
            stx $1800; clear ATNA
            sta $1c05; timer 1 hi
            cli
:           bit $1800
            bpl :-
            sty $1800; set ATNA
            sei
            bmi :--
atnlo:      .byte "m-w", .lobyte($0400), .hibyte($0400), atnhi - * - 1; read forward
            jmp (RESET_VECTOR)
            .byte 0
            cli
            jmp ($1800)
            .byte 0, 0, 0, 0, 0, 0, 0, 0
            stx $1800
            sta $1c05; timer 1 hi 
            jmp ($1800)
atnhi:      .byte "m-w", .lobyte($0480), .hibyte($0480), atnhiend - * - 1; read forward
            .byte 0, 0, 0, 0
            sty $1800
            jmp ($1800)
            .byte 0, 0, 0, 0, 0, 0
            sei
            jmp ($1800)
atnhiend:

.if ONLY_1541_AND_COMPATIBLE = 0

drvslnc81:  .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
           ;ldy #$00
            sty CIA_PRB
            dey
            sty CIA_TB_HI
            lda #COUNT_PHI2 | FORCE_LOAD | ONE_SHOT | TIMER_START
slnc81loop: sta CIA_CRB
:           bit CIA_PRB
slnc81sens: bpl slnc81loop
            ldx CIA_TB_HI
            bne :-
            ldx #OPC_BMI
            cpx slnc81sens - drvslnc81 + $0200
            stx slnc81sens - drvslnc81 + $0200
            bne slnc81loop
            jmp (RESET_VECTOR)
drvslnc81e:

drvslncfd:  .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
            ldx #$00
            stx $4001
            dex
slncfdloop: stx $4005
:           bit $4001
slncfdsens: bpl slncfdloop
            lda $4005
            bne :-
            lda #OPC_BMI
            cmp slncfdsens - drvslncfd + $0200
            sta slncfdsens - drvslncfd + $0200
            bne slncfdloop
            jmp (RESET_VECTOR)
drvslncfde:

            ; check if drive allows code upload and execution
chkdrvcode: lda #.lobyte($0300)
            ldx #.hibyte($0300)
            jsr memreadbyt
            eor #$ff
            sta drvchkval + 1
            jsr drvlistn
            ldx #$00
:           lda drvchkme,x
            jsr IECOUT
            inx
            cpx #drvchkmed - drvchkme
            bne :-
            jsr UNLSTN
            lda #.lobyte($0300)
            ldx #.hibyte($0300)
            jsr memreadbyt
drvchkval:  cmp #$00
            rts

drvchkmr:   .byte "m-r", $00, $00, $03; read forward
drvchkme:   .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            lda #$ff
            eor $0300
            sta $0300
            lda CBM1581_8
            sta $0301
            lda FD2K4K_F
            sta $0302
            rts
drvchkmed:

drvch1541u: .byte "m-e", .lobyte($0205), .hibyte($0205); read forward
            sei
            ldx #$ff
            stx $0300
            stx $1803; set all port pins as outputs
            lda #$a4; bit 0 may be forced to GND (1541-II) or connected to track 0 sensor (1541-C, normally 0 = not on track 0)
            sta $1801
            cmp $1801
            bne is1541u
            anc #$8a; and #imm, but no asl/rol, bit 7 of result goes to carry
            beq is1541u
            bcc is1541u
            txa
            arr #$7f; bit 6 of result goes to carry
            ror $0300
is1541u:    inc $1803; set all port pins as inputs
            cli
            rts
drvchkued:

memreadbyt: sta drvchkmr + $03
            stx drvchkmr + $04
            lda #drvchkme - drvchkmr
            ldx #.lobyte(drvchkmr)
            ldy #.hibyte(drvchkmr)
            jsr SETNAM
            lda #COMMAND_ERROR_CHANNEL
            ldx FA
            tay
            jsr SETLFS
            jsr OPEN
            bcc :+
kernalerr:  pla
            pla
            plp; I flag restore
            lda #diskio::status::GENERIC_KERNAL_ERROR
            ldx #diskio::drivetype::DEVICE_UNKNOWN
            ldy #BLOCKDESTLO
            sec
            rts
:           ldx #COMMAND_ERROR_CHANNEL
            jsr CHKIN
            jsr CHRIN
            pha
            jsr CHRIN
            pha
            jsr CHRIN
            pha
            lda #COMMAND_ERROR_CHANNEL
            jsr CLOSE
            jsr CLRCHN
            pla
            tay
            pla
            tax
            pla
            clc
            rts

drvfaststp: .byte MINSTEPSPEED, $01, .hibyte(VIA2_T1L_H), .lobyte(VIA2_T1L_H), "w-m"; read backward
twosided:   .byte "1m>0u"; read backward

dcodeselt0: .byte .lobyte(cbm1541::drvcodeend41 - cbm1541::drvcodebeg41 + cbm1541::drivecode41)
            .byte .lobyte(cbm1571::dinstall - cbm1571::drvcodebeg71 + cbm1571::drivecode71)
            .byte .lobyte(cbm1581::drvcodeend81 - cbm1581::drvcodebeg81 + cbm1581::drivecode81)
dcodeselt1: .byte .hibyte(cbm1541::drvcodeend41 - cbm1541::drvcodebeg41 + cbm1541::drivecode41)
            .byte .hibyte(cbm1571::dinstall - cbm1571::drvcodebeg71 + cbm1571::drivecode71)
            .byte .hibyte(cbm1581::drvcodeend81 - cbm1581::drvcodebeg81 + cbm1581::drivecode81)
dcodeselt2: .byte .lobyte(cbm1541::drvprgend41 - cbm1541::drvcodeend41 + cbm1541::TRAMPOLINEOFFSET)
            .byte .lobyte(cbm1571::drvprgend71 - cbm1571::dinstall)
            .byte .lobyte(cbm1581::drvprgend81 - cbm1581::drvcodeend81)
dcodeselt3: .byte .lobyte(cbm1541::drivecode41)
            .byte .lobyte(cbm1571::drivecode71)
            .byte .lobyte(cbm1581::drivecode81)
dcodeselt4: .byte .hibyte(cbm1541::drivecode41)
            .byte .hibyte(cbm1571::drivecode71)
            .byte .hibyte(cbm1581::drivecode81)
dcodeselt5: .byte .hibyte(cbm1541::drvcodeend41 - cbm1541::TRAMPOLINEOFFSET)
            .byte .hibyte(cbm1571::dinstall)
            .byte .hibyte(cbm1581::drvcodeend81)
dcodeselt6: .byte .lobyte(cbm1541::drvcodeend41 - cbm1541::TRAMPOLINEOFFSET)
            .byte .lobyte(cbm1571::dinstall)
            .byte .lobyte(cbm1581::drvcodeend81)
dcodeselt7: .byte .hibyte(cbm1541::dinstall)
            .byte .hibyte(cbm1571::dinstall)
            .byte .hibyte(cbm1581::dinstall)
dcodeselt8: .byte .lobyte(cbm1541::dinstall)
            .byte .lobyte(cbm1571::dinstall)
            .byte .lobyte(cbm1581::dinstall)

.endif; ONLY_1541_AND_COMPATIBLE = 0

drvchkon:   .byte "m-r", .lobyte($0300), .hibyte($0300)
drvchkone:

dcodesel5 = * + $01
dcodesel6 = * + $02
drvrutmw:   .byte $23, $00, $00, "w-m"; read backward
dcodesel7 = * + $00
dcodesel8 = * + $01
droutrun:   .byte $00, $00, "e-m"; read backward

version:    .byte "Krill's Loader, revision ", REPOSITORY_VERSION, PETSCII_RETURN, "config "
            itoa4 MIN_DEVICE_NO
            itoa8 MAX_DEVICE_NO
            itoa1 ONLY_1541_AND_COMPATIBLE
            .byte '.'
            itoa8 DIRTRACK
            itoa8 FILENAME_MAXLENGTH
            .byte '.'
            itoa8 MINSTEPSPEED
            itoa8 MAXSTEPSPEED
            itoa8 STEPPERACC
            .byte '.'
            itoa1 LOAD_COMPD_API
            itoa1 LOAD_RAW_API
            itoa1 NTSC_COMPATIBILITY
            itoa1 LOAD_UNDER_D000_DFFF
            itoa1 MEM_DECOMP_API
            itoa1 MEM_DECOMP_TO_API
            itoa1 LOAD_TO_API
            itoa1 END_ADDRESS_API
            itoa1 LOAD_VIA_KERNAL_FALLBACK
            itoa1 IDLE_BUS_LOCK
            itoa1 DISABLE_WATCHDOG
            .byte '.'
            itoa4 DECOMPRESSOR
            itoa4 LC_SPEED
            .byte 0

            CHECK_INSTALL_END_ADDRESS

.exportzp config_DECOMPRESSOR             = DECOMPRESSOR
.exportzp config_LC_SPEED                 = LC_SPEED
.exportzp config_LOAD_COMPD_API           = LOAD_COMPD_API
.exportzp config_LOAD_RAW_API             = LOAD_RAW_API
.exportzp config_NTSC_COMPATIBILITY       = NTSC_COMPATIBILITY
.exportzp config_UNINSTALL_API            = UNINSTALL_API
.exportzp config_LOAD_UNDER_D000_DFFF     = LOAD_UNDER_D000_DFFF
.exportzp config_MEM_DECOMP_API           = MEM_DECOMP_API
.exportzp config_MEM_DECOMP_TO_API        = MEM_DECOMP_TO_API
.exportzp config_LOAD_TO_API              = LOAD_TO_API
.exportzp config_END_ADDRESS_API          = END_ADDRESS_API
.exportzp config_LOAD_VIA_KERNAL_FALLBACK = LOAD_VIA_KERNAL_FALLBACK
.exportzp config_IDLE_BUS_LOCK            = IDLE_BUS_LOCK
.exportzp config_DIRTRACK                 = DIRTRACK
.exportzp config_FILENAME_MAXLENGTH       = FILENAME_MAXLENGTH
.exportzp config_DISABLE_WATCHDOG         = DISABLE_WATCHDOG
.exportzp config_ONLY_1541_AND_COMPATIBLE = ONLY_1541_AND_COMPATIBLE
