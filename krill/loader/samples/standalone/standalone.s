
; not compatible with final replay, as it resets the load vector after run
; run/stop check not implemented, needs closefile call

LOAD_BY_KERNAL = 0; to determine KERNAL throughput

.include "cia.inc"

.include "basic.inc"
.include "float.inc"
.include "kernal.inc"

.include "standard.inc"

.include "loader.inc"


.import openfile
.import pollblock

.importzp loader_zp_first
.importzp loader_zp_last

.import __DISKIO_LOAD__
.import __DISKIO_RUN__
.import __DISKIO_SIZE__

.import __ROM_THUNKS_LOAD__
.import __ROM_THUNKS_RUN__
.import __ROM_THUNKS_SIZE__

.import __RESIDENT_CODE_LOAD__
.import __RESIDENT_CODE_RUN__
.import __RESIDENT_CODE_SIZE__

DISKIO_SIZE_ALIGNED = ALIGN __DISKIO_SIZE__ + __RESIDENT_CODE_SIZE__, $0100
RESIDENT_OFFSET     = __RESIDENT_CODE_LOAD__ - __RESIDENT_CODE_RUN__

POINTERS            = $fe

FILE_NOT_FOUND      = $04
DEVICE_NOT_PRESENT  = $05
LOAD_ERROR          = $1d

floatbuff           = $02fb

.if PLATFORM = diskio::platform::COMMODORE_16
.include "ted.inc"
.else
.include "vic.inc"
.endif
            ldx #loader_zp_last - loader_zp_first + 2; add space for relocateable restorezp return address
:           lda loader_zp_first,x
            pha
            dex
            bpl :-

.if LOAD_BY_KERNAL = 0
            LOADER_INSTALL
            ; out: a    - status
            ;      x    - drive type
            ;      y    - zeropage address containing a pointer to a zero-terminated version string
            ;      c    - error
            bcc installed

            ; install error
            eor #$ff
            adc #$00
            sta insterror + 1

            jsr restorezp + RESIDENT_OFFSET
            jsr new

            ldx #LOAD_ERROR
            lda #$40
            ora MSGFLG
            sta MSGFLG; enable print I/O error number
insterror:  lda #0
            cmp #.lobyte($0100 - diskio::status::DEVICE_INCOMPATIBLE)
            bne :+
            ldx #devincomp - strings
            jmp printstr
:           jmp ERROR9 + 2; print I/O error number and return to basic

installed:  lda #PETSCII_RETURN
            jsr CHROUT

            lda COLOR
            pha
.if PLATFORM = diskio::platform::COMMODORE_16
            lda #COLOUR_BLUE | INTENSITY_4
.else
            lda #COLOUR_WHITE
.endif
            sta COLOR

            lda $00,y
            sta POINTERS + 0
            lda $01,y
            sta POINTERS + 1
            ldy #$00
            beq :+
printinfo:  and #$7f; lower case
            jsr CHROUT
            iny
:           lda (POINTERS),y
            bne printinfo
            pla
            sta COLOR
.endif
            lda #PETSCII_RETURN
            jsr CHROUT

            ldx #ntsc - strings
.if PLATFORM = diskio::platform::COMMODORE_16
            lda #PAL_NTSC_MASK
            and TED_CTRL2
            bne :+
.else
            lda PAL_NTSC
            beq :+
.endif
            ldx #pal - strings
:           jsr printstr

            lda #PETSCII_RETURN
            jsr CHROUT

            ldx #.lobyte(__ROM_THUNKS_SIZE__)
:           lda __ROM_THUNKS_LOAD__ - 1,x
            sta __ROM_THUNKS_RUN__ - 1,x
            dex
            bne :-

            lda #.hibyte(__DISKIO_LOAD__)
            sta copyresilp + 2
            lda #.hibyte(__DISKIO_RUN__)
            sta copyresilp + 5
            ldx #.hibyte(DISKIO_SIZE_ALIGNED)
            ldy #$00
copyresilp: lda a:.lobyte(__DISKIO_LOAD__),y
            sta a:.lobyte(__DISKIO_RUN__),y
            iny
            bne copyresilp
            inc copyresilp + 2
            inc copyresilp + 5
            dex
            bne copyresilp

.if PLATFORM = diskio::platform::COMMODORE_16
            lda #PAL_NTSC_MASK
            and TED_CTRL2
            bne :+
.else
            lda PAL_NTSC
            beq :+
.endif
            INT32TOFAC numccpal
            jmp :++
:           INT32TOFAC numccntsc
:
            ldx #.lobyte(numccps)
            ldy #.hibyte(numccps)
            jsr MOV2M

            jsr DIV10; fac#1 /= 10
            jsr DIV10; fac#1 /= 10
            ldx #.lobyte(numccpcs)
            ldy #.hibyte(numccpcs)
            jsr MOV2M

            lda FA
            sta driveno + 1

            lda ILOAD + 0
            sta prvload + 1
            lda ILOAD + 1
            sta prvload + 2
            lda #.lobyte(load)
            sta ILOAD + 0
            lda #.hibyte(load)
            sta ILOAD + 1

            lda #.lobyte(ramirq)
            sta IRQ_VECTOR + 0
            lda #.hibyte(ramirq)
            sta IRQ_VECTOR + 1
            jsr restorezp + RESIDENT_OFFSET
            jsr new

.if PLATFORM <> diskio::platform::COMMODORE_16
            lda #.lobyte(~CIA_VIC2_BANK_OUTPUT)
            ora CIA2_PRA
            sta CIA2_PRA
            lda #.lobyte(~CIA_SERIAL_DATA_IN_OUTPUT | CIA_SERIAL_CLK_IN_OUTPUT)
            sta CIA2_DDRA
.endif

            clc
            rts; return to BASIC interpreter

new:        tsx
            stx sp + 1
            lda STACK + $f9
            sta st0 + 1
            lda STACK + $fa
            sta st1 + 1
            jsr NEW
st0:        lda #0
            sta STACK + $f9
st1:        lda #0
            sta STACK + $fa
sp:         ldx #0
            txs
            rts

printstr:   lda strings,x
            bne :+
            rts
:           jsr CHROUT
            inx
            bne printstr
            rts

numccntsc:  CYCLES_PER_SECOND_NTSC
numccpal:   CYCLES_PER_SECOND_PAL

strings:
.if PLATFORM = diskio::platform::COMMODORE_16
pal:        .byte "pal", 0
ntsc:       .byte "ntsc", 0
.else
.if NTSC_COMPATIBILITY
pal:        .byte "pal - warning: ntsc_compoatibility slows"
            .byte "down pal speed", 0
ntsc:       .byte "ntsc", 0
.else
pal:        .byte "pal", 0
ntsc:       .byte "ntsc - warning: ntsc_compatibility is   "
            .byte "not enabled in loader config", 0
.endif
.endif
devincomp:  .byte "device incompatible", 0

.segment "ROM_THUNKS"

load:       jsr enableram
            jmp doload

prevload:   jsr enablerom
prvload:    jmp $0000

irq:        jsr enableram
.if PLATFORM = diskio::platform::COMMODORE_16
            lda TED_IRR
            sta TED_IRR
.endif
            jsr irqsub
            jsr enablerom
            jmp KPREND

romjsr:     stx :+ + 1
            sty :+ + 2
            jsr enablerom
:           jsr $0000
            jmp enableram

clr:        jsr enablerom
            jsr CLR
            jsr enableram
            jmp clrst0

time:       jsr enablerom

            jsr INT24TOMANTISSA
cyclesmsb:  lda #0
            sta FACHO
            jsr NORMALIZE; fac#1 = numcycles
            jsr fac1buff; store to floatbuff
            jsr MOVAF; fac#2 = fac#1

            lda #.lobyte(numccps)
            ldy #.hibyte(numccps)
            jsr FDIVM; fac#1 = fac#2 / cycles per second
            jmp :+

throughput: jsr enablerom

            jsr INT24TOMANTISSA
            jsr NORMALIZE; fac#1 = numbytes
            lda #.lobyte(numccpcs)
            ldy #.hibyte(numccpcs)
            jsr FMULT; fac#1 *= cycles per centisecond
            jsr MOVAF; fac#2 = fac#1

            lda #.lobyte(floatbuff)
            ldy #.hibyte(floatbuff)
            jsr FDIVM; fac#1 = fac#2 / numcycles
.if LOAD_BY_KERNAL = 0
            jsr FADDH; fac#1 += 0.5
.endif
            jsr DIV10; fac#1 /= 10

            jsr fac1buff; floatbuff = fac#1 = cyclespersecond * numbytes / numcycles / 1000

:           jsr FOUT
            jsr STRLIT

            jsr enableram
            jmp printfloat

speedup:    jsr enablerom
            jsr MOVFM; fac#1 = kernal_throughput
            lda #.lobyte(floatbuff)
            ldy #.hibyte(floatbuff)
            jsr FDIV; fac#1 = kb/s figure / kernal_throughput
            jsr FADDH; fac#1 += 0.5
            jsr FLPINT; fac#1 -> int (a/y)

            jmp enableram

fac1buff:   ldx #.lobyte(floatbuff)
            ldy #.hibyte(floatbuff)
            jmp MOV2M; floatbuff = numcycles

strout03:   jsr enablerom 
            jsr STROUT + 3
.if PLATFORM = diskio::platform::COMMODORE_16
enableram:  sta TED_RAM_ENABLE
            rts
enablerom:  sta TED_ROM_ENABLE
            rts
.else
enableram:  ldx #$35
            stx $01
            rts
enablerom:  pha
            lda #$37
            sta $01
            pla
            rts

dspp:       jsr enablerom
            jsr DSPP; put a char on the screen
            jmp enableram
.endif

enableromc: jsr enablerom
            cli
            rts

numccps:    .res 5
numccpcs:   .res 5

kernalthrp: .byte $7f, $47, $de, $23, $10; .39 kB/s

.segment "RESIDENT_CODE"

doload:     sta VERFCK; load/verify

.if LOAD_BY_KERNAL

.if PLATFORM = diskio::platform::COMMODORE_16
; note: time measurement doesn't work without coupled timers due to no interrupts, and timers are horribly broken in VICE
screen   = $0800
numbytes = $b45a; threeve
.else
screen   = $0400
numbytes = $c7ff; oneder
.endif
            jsr initstat
            lda VERFCK

            ldx #.lobyte(NLOAD)
            ldy #.hibyte(NLOAD)
            jsr romjsr

            php
            pha
            txa
            pha
            tya
            pha

            sec
            ldx #.hibyte(numbytes)
            ldy #.lobyte(numbytes)
            jsr getstat

            lda #PETSCII_RETURN
            jsr chrout

            lda numcycles + 0
            ora numcycles + 1
            ora numcycles + 2
            ora cyclesmsb + 1
            bne :+
            inc numcycles + 0; avoid division by 0

:           lda numcycles + 0
            ldx numcycles + 1
            ldy numcycles + 2
            sec
            jsr time
            lda #' '
            jsr chrout
            lda #'s'
            jsr chrout
            lda #PETSCII_RETURN
            jsr chrout

            lda numbyteslo
            ldx numbyteshi
            ldy #$00
            sec
            jsr throughput
            lda #' '
            jsr chrout
            lda #'k'
            jsr chrout
            lda #'b'
            jsr chrout
            lda #'/'
            jsr chrout
            lda #'s'
            jsr chrout

            lda #PETSCII_RETURN
            jsr chrout

            lda floatbuff + 0
            sta screen + 0
            lda floatbuff + 1
            sta screen + 1
            lda floatbuff + 2
            sta screen + 2
            lda floatbuff + 3
            sta screen + 3
            lda floatbuff + 4
            sta screen + 4

            pla
            tay
            pla
            tax
            pla
            plp
            jmp enablerom; return to BASIC interpreter
.endif; LOAD_BY_KERNAL

            ;lda #$7f
            ;and MSGFLG
            ;sta MSGFLG; no messages (non-direct mode)

            lda FA
            cmp #$08
            bcc jmprevload
            cmp #$30 + 1
            bcs jmprevload

            ldy #$00
            lda (FNADR),y
            cmp #'$'
            bne :++

.if PLATFORM <> diskio::platform::COMMODORE_16
            lda #CIA_VIC2_BANK_OUTPUT
            sta CIA2_DDRA
            and CIA2_PRA
            sta CIA2_PRA
.endif
            LOADER_UNINSTALL

            lda #$04; need some delay before
            ldx #$00; the drive has completed
            ldy #$00; its startup check
:           dex
            bne :-
            dey
            bne :-
            sec
            sbc #$01
            bne :-

jmprevload: lda VERFCK; load/verify
            jmp prevload

:           ldx #loader_zp_last - loader_zp_first + 2; add space for relocateable restorezp return address
:           lda $00,x
            pha
            dex
            bpl :-

            lda CINV + 0
            sta previrqlo + 1
            lda CINV + 1
            sta previrqhi + 1

            lda FA
driveno:    cmp #$00
            bne :+; only one drive can load
            lda VERFCK
            beq :++; verify is not implemented
:           sec
            lda #diskio::status::GENERIC_KERNAL_ERROR
            bcs jmperror
:
            ; convert file name
            ldy FNLEN
            cpy #FILENAME_MAXLENGTH
            bcs :+
            lda #$00
            sta filename,y
            SKIPWORD
:           ldy #FILENAME_MAXLENGTH
            dey
:           lda (FNADR),y
            sta filename,y
            dey
            bpl :-

            bit MSGFLG
            bpl :+

            ldx #.lobyte(SEARCHING); searching for <filename>
            ldy #.hibyte(SEARCHING)
            jsr romjsr
            jsr cursoron

.if PLATFORM = diskio::platform::COMMODORE_16
            lda TED_IMR
            pha
.endif
            lda CINV + 1
            cmp #.hibyte(KERNAL_ROM)
            bcc :+
            sei
.if PLATFORM = diskio::platform::COMMODORE_16
            lda #COUNTER_3_IRQ
            sta TED_IMR
.endif
            lda #.lobyte(irq)
            sta CINV + 0
            lda #.hibyte(irq)
            sta CINV + 1
            cli
:
.if PLATFORM <> diskio::platform::COMMODORE_16
            lda #CIA_VIC2_BANK_OUTPUT
            sta CIA2_DDRA
            and CIA2_PRA
            sta CIA2_PRA
.endif
            ldx #.lobyte(filename)
            ldy #.hibyte(filename)
            lda #$00
            cmp SA; secondary address
            bcc :+
            lda MEMUSS + 0
            sta loadaddrlo
            lda MEMUSS + 1
            sta loadaddrhi
:           jsr openfile; this returns immediately
            bcs jmperror

            jsr pollblock; this blocks until the file is open
            bcc :+
jmperror:   jmp error
:
            bit MSGFLG
            bpl :+

            jsr cursoroff

            ldx #.lobyte(LOADING)
            ldy #.hibyte(LOADING)
            jsr romjsr
            ; $xyzw-
            lda #' '
            jsr chrout
            lda loadaddrlo
            ldx loadaddrhi
            jsr printhex
            lda #'-'
            jsr chrout
            lda loadaddrlo
            ldx loadaddrhi
            jsr printhex

            jsr cursoron

            jsr initstat

            ; main loading loop
:           jsr pollblock
            php
            pha

            sec
            lda PNTR
            sbc #4
            tay
            lda endaddrhi
            lsr
            lsr
            lsr
            lsr
            jsr hexnib2scr
            sta (PNT),y
            iny
            lda endaddrhi
            and #$0f
            jsr hexnib2scr
            sta (PNT),y
            iny
            lda endaddrlo
            lsr
            lsr
            lsr
            lsr
            jsr hexnib2scr
            sta (PNT),y
            iny
            lda endaddrlo
            and #$0f
            jsr hexnib2scr
            sta (PNT),y

            pla
            plp
            bcc :-

            cmp #diskio::status::OK
            beq :+
            jmp error

:           bit MSGFLG
            bmi :+

            clc
            jmp noerror

:           sec
            lda endaddrlo
            sta VARTAB + 0
            sta EAL
            sbc loadaddrlo
            tay
            lda endaddrhi
            sta VARTAB + 1
            sta EAH
            sbc loadaddrhi
            tax
            jsr getstat

            tsx
            stx clrsp + 1
            lda STACK + $f9
            sta clrst0 + 1
            lda STACK + $fa
            sta clrst1 + 1
            jmp clr
clrst0:     lda #0
            sta STACK + $f9
clrst1:     lda #0
            sta STACK + $fa
clrsp:      ldx #0
            txs

            jsr cursoroff

            lda #':'
            jsr chrout
            lda #' '
            jsr chrout

            lda numcycles + 0
            ora numcycles + 1
            ora numcycles + 2
            ora cyclesmsb + 1
            bne :+
            inc numcycles + 0; avoid division by 0

:           lda numcycles + 0
            ldx numcycles + 1
            ldy numcycles + 2
            sec
            jsr time
            lda #'s'
            jsr chrout
            lda #','
            jsr chrout
            lda #' '
            jsr chrout

            lda numbyteslo
            ldx numbyteshi
            ldy #$00
            sec
            jsr throughput
            lda #'k'
            jsr chrout
            lda #'b'
            jsr chrout
            lda #'/'
            jsr chrout
            lda #'s'
            jsr chrout
            lda #','
            jsr chrout
            lda #' '
            jsr chrout

            lda #.lobyte(kernalthrp)
            ldy #.hibyte(kernalthrp)
            jsr speedup
            tya
            ldx #$ff
:           inx
            sec
            tay
            sbc #10
            bcs :-
            tya
            pha
            txa
            beq :+
            adc #'0'
            jsr chrout
:           pla
            clc
            adc #'0'
            jsr chrout
            lda #'x'
            jsr chrout

            clc
bccnoerror: bcc noerror

error:      pha
            jsr cursoroff
            pla
            sec
            ; fall through

noerror:    ; carry is clear when branching here
.if PLATFORM <> diskio::platform::COMMODORE_16
            pha
            lda #.lobyte(~CIA_VIC2_BANK_OUTPUT)
            ora CIA2_PRA
            sta CIA2_PRA
            lda #.lobyte(~CIA_SERIAL_DATA_IN_OUTPUT | CIA_SERIAL_CLK_IN_OUTPUT)
            sta CIA2_DDRA
            pla
.endif
            sei
            bit MSGFLG
            bpl :+

            tax
previrqlo:  lda #0
            sta CINV + 0
previrqhi:  lda #0
            sta CINV + 1
.if PLATFORM = diskio::platform::COMMODORE_16
            pla
            and #.lobyte(~IRQ_RASTERLINE_MSB)
            sta TED_IMR
.endif
            txa

:           jsr restorezp

            bcc loadsucces
            tax
            lda #LOAD_ERROR
            cpx #diskio::status::FILE_NOT_FOUND
            bne :+
            lda #FILE_NOT_FOUND

:           cpx #diskio::status::DEVICE_NOT_PRESENT
            bne :+

            ; switch back to KERNAL loading
.if PLATFORM <> diskio::platform::COMMODORE_16
            lda #CIA_VIC2_BANK_OUTPUT
            and CIA2_PRA
            sta CIA2_PRA
.endif
            lda prvload + 1
            sta ILOAD + 0
            lda prvload + 2
            sta ILOAD + 1
            jmp jmprevload; note: searching for <filename> is printed again

:           sec

loadsucces: ldx EAL
            ldy EAH
            jmp enableromc; return to BASIC interpreter

chrout:     ldx #.lobyte(CHROUT)
            ldy #.hibyte(CHROUT)
            jmp romjsr

printhex:   pha
            txa
            pha
            lda #'$'
            jsr chrout
            pla
            jmp :+
printhex2:  pha
            txa
:           jsr printnum
            pla
printnum:   pha
            lsr
            lsr
            lsr
            lsr
            jsr printdigit
            pla
            and #%00001111
printdigit: cmp #$0a
            bcs :+
            adc #'0'
            SKIPWORD
:           adc #'a' - $0a - $01
            jmp chrout

hexnib2scr: cmp #$0a
            bcs :+
            adc #'0'
            SKIPWORD
:           sbc #$09
            rts

initstat:
.if PLATFORM = diskio::platform::COMMODORE_16
            jsr readcycles
            sta prvcycslo
            stx prvcycsmid
            sty prvcycshi
.else
            lda #$ff
            sta CIA2_TA_LO
            sta CIA2_TB_LO
            sta CIA2_TA_HI
            sta CIA2_TB_HI
            lda #FORCE_LOAD | CONTINUOUS | COUNT_TA_UNDF | TIMER_START
            sta CIA2_CRB
            lda CIA2_CRA
            and #.lobyte(~(COUNT_CNT | ONE_SHOT))
            ora #FORCE_LOAD | CONTINUOUS | TIMER_START
            sta CIA2_CRA
.endif
            rts

            ; in: x + y - number of bytes transferred
getstat:
.if PLATFORM = diskio::platform::COMMODORE_16
            stx numbyteshi
            sty numbyteslo

            jsr diffcycles
.else
            lda #.lobyte(~TIMER_STOP)
            and CIA2_CRA
            sta CIA2_CRA
            lda #TIMER_STOP
            sta CIA2_CRB

            sec
            lda #$ff - 31; subtract overhead
            sbc CIA2_TA_LO
            sta numcycles + 0
            lda #$ff
            sbc CIA2_TA_HI
            sta numcycles + 1
            lda #$ff
            sbc CIA2_TB_LO
            sta numcycles + 2
            lda #$ff
            sbc CIA2_TB_HI
            sta cyclesmsb + 1

            stx numbyteshi
            sty numbyteslo
.endif
            rts

printfloat:
.if LOAD_BY_KERNAL = 0
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

            ; truncate to one decimal
            sta POINTERS + 0
            iny
            lda (STRDSC),y
            sta POINTERS + 1
            ldy #.lobyte(-$01)
:           iny
            cpy statlen
            bcs truncdone
            lda (POINTERS),y
            cmp #'.'
            bne :-
            iny
            iny
            cpy #$08
            bcs truncdone
            sty statlen
            tya
            ldy #STRDSC_LEN
            sta (STRDSC),y
            cpy #$08
truncdone:
.endif; LOAD_BY_KERNAL
            jmp strout03

.if PLATFORM = diskio::platform::COMMODORE_16
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

readctr:    ldy TED_COUNTER3_HI
            ldx TED_COUNTER3_LO
            cpy TED_COUNTER3_HI
            bne readctr
            rts

diffcycles: jsr readcycles

            sec
            sbc prvcycslo
            sta numcycles + 0
            txa
            sbc prvcycsmid
            sta numcycles + 1
            tya
            sbc prvcycshi
            sta numcycles + 2
            lda #$00
            sta cyclesmsb + 1

            sec
            lda numcycles + 0
adjustdiff = * + $01
            sbc #$00
            sta numcycles + 0
            bcs :+
            lda numcycles + 1
            dec numcycles + 1
            tax
            bne :+
            lda numcycles + 2
            dec numcycles + 2
            tax
            bne :+
            dec cyclesmsb + 1
:           rts

cursoron:   clc
            lda PNT + 0
            adc PNTR
            sta TED_CURSOR_LO
            lda PNT + 1
            and #$03
            adc #$00
            sta TED_CURSOR_HI
            rts
cursoroff:  lda #$ff
            sta TED_CURSOR_LO
            sta TED_CURSOR_HI
            rts
.else
cursoron:   lda #$00
            sta BLNSW
:           rts
cursoroff:  inc BLNSW
            lda BLNON     ; Flag: Cursor Status
            beq :-
            lda GDBLN     ; Character under Cursor while Cursor Inverted
            ldx GDCOL     ; Background Color under Cursor
            ldy #$00
            sty BLNON     ; Flag: Cursor Status
            jmp dspp
.endif

restorezp:  sta abuf + 1
            php
            pla
            tay

            tsx
            txa
            clc
            adc #loader_zp_last - loader_zp_first + 1
            tax
            pla
            sta a:STACK + 3,x
            pla
            sta a:STACK + 4,x
            ldx #loader_zp_first
:           pla
            sta $00,x
            inx
            cpx #loader_zp_last + 1
            bne :-

            tya
            pha
            plp
abuf:       lda #0
            rts

.if PLATFORM = diskio::platform::COMMODORE_16
irqsub:     ; handle timer underflow
            jsr readctr
            tya
            pha
            txa
            pha
            sec
            lda prevctr3lo
            stx :+ + 1
:           sbc #0
            tax
            lda prevctr3hi
            sty :+ + 1
:           sbc #0
            tay
            bne :+
            ; less than 256 cycles difference,
            ; this is likely an underflow
            inc cycles_hi
:           txa
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
            rts
.else
irqsub:     ; blink cursor: copied from $ea34 (KEY + 3) and modified
            lda BLNSW     ; Flag: Cursor blink
            bne :++
            dec BLNCT     ; Timer: Count down for Cursor blink toggle
            bne :++
            lda #$14
            sta BLNCT     ; Timer: Count down for Cursor blink toggle
            ldy PNTR      ; Cursor Column on current Line
            lsr BLNON     ; Flag: Cursor Status
            ldx GDCOL     ; Background Color under Cursor
            lda (PNT),Y   ; Pointer: Current Screen Line Address
            bcs :+
            inc BLNON     ; Flag: Cursor Status
            sta GDBLN     ; Character under Cursor while Cursor Inverted
           ;jsr SCOLOR    ; Syncronise Color Pointer
            lda PNT       ; Pointer: Current Screen Line Address
            sta USER      ; Pointer: Current Color RAM Location
            lda PNT + 1   ; Pointer: Current Screen Line Address
            and #$03
            ora #$d8
            sta USER + 1  ; Pointer: Current Color RAM Location
            lda (USER),Y  ; Pointer: Current Color RAM Location
            sta GDCOL     ; Background Color under Cursor
            ldx COLOR     ; Current Character Color code
            lda GDBLN     ; Character under Cursor while Cursor Inverted
:           eor #$80
           ;jsr dspp2     ; Print To Screen
            ldy PNTR      ; Cursor Column on current Line
            sta (PNT),Y   ; Pointer: Current Screen Line Address
            txa
            sta (USER),Y  ; Pointer: Current Color RAM Location
:           rts
.endif

ramirq:     pha
            txa
            pha
            tya
            pha

            jsr irqsub

.if PLATFORM = diskio::platform::COMMODORE_16
            lda TED_IRR
            sta TED_IRR
.else
            bit CIA1_ICR
.endif
            pla
            tay
            pla
            tax
            pla
            rti

statlen:    .res 1

.if PLATFORM = diskio::platform::COMMODORE_16
prevctr3lo: .res 1
prevctr3hi: .res 1

cycles_hi:  .res 1
cycles_mid: .res 1
cycles_lo:  .res 1

prvcycslo:  .res 1
prvcycsmid: .res 1
prvcycshi:  .res 1
.endif

numcycles:  .res 3
numbyteslo: .res 1
numbyteshi: .res 1

filename:   .res FILENAME_MAXLENGTH
