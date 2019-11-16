
.include "cpu.inc"
.include "via.inc"
.include "cia.inc"

.include "drives/drivecode-common.inc"


CURTRACK               = $00; must not be overwritten by custom drive code upload routine at duninstall

INITBUF_MAXTRK         = $03
INITBUF_TRACK_DIFF     = $04

DISKCHANGEBUFFER       = $01
DISKCHANGED            = $02
GCRBUFFER              = $03; $07 bytes
CURRSTEPSPEEDLOW       = $04
TRACKINC               = $05
GCRBUFFER0             = $0a
GCRBUFFER1             = $0b
GCRBUFFER2             = $0c
LOADEDSECTOR           = $0d
BLOCKINDEX             = $0e
NUMSECTORS             = $0f
NUMSECTORSTEMP         = GCRBUFFER
BLOCKINDEXBASE         = $10
NEXTSECTOR             = $11
SECTORTOFETCH          = $12
REQUESTEDSECTOR        = $13
MAXTRACK               = $14
MAXTRACK_A             = $15
ID0                    = $16; = ROMOS_HEADER_ID0
ID1                    = $17; = ROMOS_HEADER_ID1
FILETRACK              = $18; = ROMOS_HEADER_TRACK
FILESECTOR             = $19; = ROMOS_HEADER_SECTOR
FILENAMEHASH0          = $1a
FILENAMEHASH1          = $1b
CHECKSUM               = $1c
MAXCONFIRMEDBLOCKINDEX = $1d
MAXBLOCKINDEXPOS       = $1e
SPECULATIVEINTERLEAVE  = $1f
NUMFILEBLOCKS          = $20
TEMP                   = $21

INDEXTAB               = $22; $15 = MAXNUMSECTORS bytes
TRACKLINKTAB           = INDEXTAB + MAXNUMSECTORS; $15 = MAXNUMSECTORS bytes
LOADFILEVARS           = TRACKLINKTAB + MAXNUMSECTORS

CURRBLOCKINDEX         = LOADFILEVARS + 0
PREVBLOCKINDEX         = LOADFILEVARS + 1
NUMCONTIGUOUSBLOCKS    = LOADFILEVARS + 2
NEXTCONTIGUOUSBLOCK    = LOADFILEVARS + 3
MAXINDEXONTRACK        = LOADFILEVARS + 4
NUMBLOCKSSENT          = LOADFILEVARS + 5

FILENAME               = TRACKLINKTAB; max. $10 bytes

DIRCYCLEFLAG           = LOADFILEVARS + 6
CYCLESTARTENDSECTOR    = LOADFILEVARS + 7
CURRDIRBLOCKSECTOR     = LOADFILEVARS + 8
NEXTDIRBLOCKSECTOR     = LOADFILEVARS + 9
DIRBLOCKPOS            = LOADFILEVARS + 10
WRAPFILEINDEX          = LOADFILEVARS + 11
FILEINDEX              = LOADFILEVARS + 12
NUMFILES               = LOADFILEVARS + 13
DIRBUFFER              = LOADFILEVARS + 14

DIRBUFFSIZE            = (sendnibbletab - DIRBUFFER) / 5
DIRTRACKS              = DIRBUFFER
DIRSECTORS             = DIRTRACKS + DIRBUFFSIZE
NUMBLOCKS              = DIRSECTORS + DIRBUFFSIZE
FILENAMEHASHVAL0       = NUMBLOCKS + DIRBUFFSIZE
FILENAMEHASHVAL1       = FILENAMEHASHVAL0 + DIRBUFFSIZE

DIRBUFFSIZE71          = DIRBUFFSIZE
.export DIRBUFFSIZE71

            .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"

BLOCKBUFFER           = $0700
TRACKOFFSET           = $00
SECTOROFFSET          = $01
BLOCKSOFFSET          = $1e

ROMOS_HEADER_ID0      = $16
ROMOS_HEADER_ID1      = $17
ROMOS_HEADER_TRACK    = $18
ROMOS_HEADER_SECTOR   = $19
ROMOS_TRACK_DIFF      = $42
ROMOS_MAXTRACK        = $02ac

DECGCRTAB10ZZZ432LO   = $9f0d
DECGCRTAB3210ZZZ4LO   = $9f0f
DECGCRTAB0ZZZ4321HI   = $9f1d
DECGCRTAB210ZZZ43HI   = $9f2a
DECGCRTAB43210XXXHI   = $a00d
DECGCRTABXX43210XHI   = $a10d
DECGCRTABX43210XXLO   = $a20d
DECGCRTABXXX43210LO   = $a30d

GCRDECODEHI           = $f8a0
GCRDECODELO           = $f8c0

LINKTRACK             = BLOCKBUFFER + TRACKOFFSET
LINKSECTOR            = BLOCKBUFFER + SECTOROFFSET


BINARY_NIBBLE_MASK    = %00001111
GCR_NIBBLE_MASK       = %00011111

NUMTRACKS_SINGLESIDED = 41
NUMTRACKS_A           = 35
NUMTRACKS_B           = NUMTRACKS_SINGLESIDED
MAXTRACK71            = NUMTRACKS_A + NUMTRACKS_B
MAXNUMSECTORS         = 21

ANYSECTOR             = $ff; no sector link sanity check
ANYSECTORSANELINK     = $80; sector link sanity check

BLOCKPENDING          = %01000000
INDEXSPECULATIVE      = %00100000

TIMER                 = VIA2_T1C_H

            .org $36

.export c1570fix0 : absolute
.export c1570fix1 : absolute


drvcodebeg71:
            .byte .hibyte(dinstall - * + $0100 - $01); init transfer count hi-byte

            ; find current track number
            ; this assumes the head is on a valid half track
findtrackn: lda #ANYSECTOR; $ff: invalid track number -> no track step
            ldx #OPC_STA_ZP; $85
            tay
            jsr getblkinit; no sector link sanity check, set CURTRACK
            bcs :+

            lda GCRBUFFER + $04
            lsr
            asl GCRBUFFER + $03
            rol
            asl GCRBUFFER + $03
            rol
            asl GCRBUFFER + $03
            rol
            and #GCR_NIBBLE_MASK
            tay
            lda GCRBUFFER + $03
            jsr decodesub; track
            cmp CURTRACK; getblkstid sets CURTRACK at this stage,
                        ; the value is inferred by eoring the header checksum
                        ; and all header field values except the current track
            beq initdone
:           clc
            lda #1 << BITRATE_SHIFT
            adc VIA2_PRB
            sta VIA2_PRB; cycle through the 4 bit-rates
            bne findtrackn; jmp

initdone:   lda #OPC_EOR_ZP
            sta headerchk - $02
            lda #OPC_BNE
            sta headerchk
            lda #OPC_LDA_IMM
            sta sanitychsw
            lda #$01
            sta sanitychsw + 1
            lda #OPC_STA_ABS
            sta putbitrate
            lda #OPC_STX_ZP
            sta putnumscts
            jsr setbitrate
            jmp beginload

sendnibbletab:
            BIT0DEST = 3
            BIT1DEST = 1
            BIT2DEST = 2
            BIT3DEST = 0

            .repeat $10, I
                .byte (((~I >> 0) & 1) << BIT0DEST) | (((~I >> 1) & 1) << BIT1DEST) | (((~I >> 2) & 1) << BIT2DEST) | (((~I >> 3) & 1) << BIT3DEST)
            .endrep

SPC:        .byte INDEXSPECULATIVE

            .assert * <= $0100, error, "***** 1571 sendnibbletab too high in memory. *****"

waitsync:   lda #$ff
            sta TIMER
:           lda TIMER
            beq wsynctmout; will return $00 in the accu
            bit VIA2_PRB
            bmi :-
            bit VIA2_PRA
:           bit VIA1_PRA
            bmi :-
            lda VIA2_PRA; is never $00 but usually $52 (header) or $55 (data)
wsynctmout: rts

decodehdr:  lda GCRBUFFER + $06
            alr #(GCR_NIBBLE_MASK << 1) | 1; and + lsr
            tay
            lda GCRBUFFER + $05
            jsr decodesub - $01; checksum
            sta GCRBUFFER + $06
            lax GCRBUFFER + $02
            lsr
            lsr
            lsr
            tay
            txa
            asl GCRBUFFER + $01
            rol
            asl GCRBUFFER + $01
            rol
            and #GCR_NIBBLE_MASK
            jsr decodesub + $03; ID1
            sta GCRBUFFER + $05
            lda GCRBUFFER + $01
            lsr
            lsr
            lsr
            tay
            lda GCRBUFFER + $00; ID0
            ror
decodesub:  lsr
            lsr
            lsr
            tax
            lda GCRDECODEHI,y
            ora GCRDECODELO,x
            rts

busyledon:  lda #SYNC_MARK | BUSY_LED
            ora VIA2_PRB
            bne store_via2

fadeled:    tya
            tax
            beq fadedone
:           inx
            bne :-
            tax
            jsr busyledon
:           dex
            bne :-
            dey
            bne :+
            and #.lobyte(~MOTOR)   ; turn off motor
:           and #.lobyte(~BUSY_LED); turn off busy LED
store_via2: sta VIA2_PRB
fadedone:   sec
            rts

checkchgl:  jsr fadeled; fade off the busy LED
            bcs checkchg

            ; the routine above must not be called from the watchdog
            ; IRQ handler, as it may be overwritten on watchdog IRQ

            ; * >= $0100
stack:
            .assert stack >= $0100, error, "***** 1571 stack too low in memory. *****"

            .word $00, $00, $00, $00
stackend:   ; stacktop + 1
            .assert stackend < $0200, error, "***** 1571 stack too high in memory. *****"

:           sta DISKCHANGED; set the fetch dir flag when disks have been changed
checkchg:   ; must not change y
errorretlo: ; ID mismatch
            lax VIA2_PRB; check light sensor for disk removal
            eor DISKCHANGEBUFFER
            stx DISKCHANGEBUFFER
            and #WRITE_PROTECT
            bne :-
            ; read error: z = 1
chksumerrl: ; checksum mismatch: z = 0
            sec; operation not successful
            rts

            ; getblock calls
            ; in: y: track
            ;     a: sector, ANYSECTOR or ANYSECTORSANELINK
getblkchid: ldx #OPC_CMP_ZP; check against stored ID
getblkstid: ; store read ID, ldx #OPC_STA_ZP executed by caller
            ldy #DIRTRACK
getblkinit: stx storputid1
            dex; OPC_STA_ZP/OPC_CMP_ZP -> OPC_STY_ZP/OPC_CPY_ZP
            stx storputid0
            sta REQUESTEDSECTOR
            jsr trackseek; stores the number of blocks on
                         ; the current track in NUMSECTORS
getblkscan: ; the disk spins at approximately 300 rpm,
            ; so a revolution takes about 2,000,000 * 60 / 300 = 400,000 cycles at 2 MHz,
            ; so the timeout counter cannot be set to one revolution -
            ; it is reset upon waiting for every new sync,
            ; thus a timeout only indicates a sync-less track range
            ; (about 65536 / 400,000 * 19 = 3.11 sectors), but exclusively non-header
            ; syncs or missing sectors or similar will leave the loader spinning forever
findblkhdr: jsr waitsync
            beq checkchg; returns with carry set on time-out
chkheader:  cmp #%01010010; $52, check if the sync is followed by a sector header
            bne findblkhdr ; if not, wait for next sync mark

            ; read the sector header
            ldx #$06
getheader:  bit VIA1_PRA ; check
            bmi getheader; BYTEREADY
            lda VIA2_PRA
            sta GCRBUFFER + $00,x
            dex
            bpl getheader

            ; check if the sector header's field values match the expectations

            ; decode sector number
            asl GCRBUFFER + $04
            lda GCRBUFFER + $05
            rol
            and #GCR_NIBBLE_MASK
            tay
            lda GCRBUFFER + $04
            jsr decodesub + $00
            cmp NUMSECTORS; skip if sector number is not within range of the allowed
            bcs findblkhdr; sector numbers for the current track

            sta LOADEDSECTOR
            cmp REQUESTEDSECTOR
            beq loadblock; branch if requested sector

            ; bit:bpl:bvc won't work because the v-flag
            ; is unstable while the disk is spinning
            ldy REQUESTEDSECTOR
            iny
            beq loadblock; branch if ANYSECTOR ($ff)
            bpl findblkhdr; branch if specific sector requested, but not reached yet

            ; ANYSECTORSANELINK ($80): no specific sector requested, out-of-order sector fetch
            tax           ; current sector number
            lda INDEXTAB,x; x = current sector number
            bpl :+        ; branch if block index is known, or speculated to belong to the file
            cmp TRACKLINKTAB,x
            bne findblkhdr; branch if block index is unknown but linked block is known (the block has been loaded before already)
           ;sec
            SKIPWORD; do not branch to findblkhdr, block is not known to belong to the file
:           cmp #BLOCKPENDING; branch if the block has already been
            bcc findblkhdr   ; loaded into the computer's memory
            and #.lobyte(~(BLOCKPENDING | INDEXSPECULATIVE))
            sta CURRBLOCKINDEX
           ;sec
            ror TRACKLINKTAB,x

loadblock:  jsr decodehdr

            ; checksum block header
            tay; ID0
            eor LOADEDSECTOR
            eor GCRBUFFER + $05; ID1
            eor GCRBUFFER + $06; checksum
            sta CURTRACK; is changed to eor CURTRACK after init
headerchk:  .byte OPC_BIT_ZP, .lobyte(chksumerrl - * - $01); is changed to bne chksumerrl
                                                           ; after init, wait for next sector if
                                                           ; sector header checksum was not ok
            lda GCRBUFFER + $05; ID1
            ldx #$00; set z-flag which won't be altered by the store opcodes
storputid0: cpy ID0; cpy ID0/sty ID0
:           bne errorretlo; branch if the disk ID does not match
storputid1: cmp ID1; cmp ID1/sta ID1
            bne :-; branch if the disk ID does not match

            ; wait for data block sync
            jsr waitsync; reset the time-out timer here
            eor #%01010101; $55, check if the sync is followed by a data block
            bne chkheader ; if not, treat as new header

            tay
            sty CHECKSUM
:           bit VIA1_PRA
            bmi :-
            ldx VIA2_PRA   ; 11222223
            cpx #%11000000
            bcc findblkhdr

loaddata:   lda DECGCRTABXX43210XHI,x; x = [$00..$ff], %2222....
               ; 54 cycles

:           bit VIA1_PRA
            bmi :-
            sta GCRBUFFER0
            txa            ; 11222223
            lsr
            lda VIA2_PRA   ; 33334444
            sta GCRBUFFER1
            and #%11110000 ; 3333....
            adc #%00000000 ; 3333...3
            tax
            lda GCRBUFFER0 ; 2222....            
            ora DECGCRTAB3210ZZZ4LO,x; x = [($00..$ff) & $f1], 22223333
            sta BLOCKBUFFER + $00,y
            eor CHECKSUM
            asl GCRBUFFER1 ; 3334444.
               ; 47 cycles

:           bit VIA1_PRA
            bmi :-
            sta CHECKSUM
            lda VIA2_PRA   ; 45555566
            sta GCRBUFFER0
            asl            ; 5555566.
            lda #%00011110
            and GCRBUFFER1 ; ...4444.
            ror            ; 4...4444
            tax            
            lda DECGCRTAB0ZZZ4321HI,x; x = [($00..$ff) & $8f], 4444....
            ldx GCRBUFFER0 ; 45555566
            ora DECGCRTABX43210XXLO,x; x = [$00..$ff], 44445555
            sta BLOCKBUFFER + $01,y
               ; 48 cycles

:           bit VIA1_PRA
            bmi :-
            eor CHECKSUM
            sta CHECKSUM
            txa            ; 45555566
            and #%00000011 ; ......66
            sta GCRBUFFER0
            lda VIA2_PRA   ; 66677777
            sta GCRBUFFER1
            and #%11100000 ; 666.....
            ora GCRBUFFER0 ; 666...66
            tax
            lda DECGCRTAB210ZZZ43HI,x; x = [($00..$ff) & $e3], 6666....
            ldx GCRBUFFER1 ; 66677777
            ora DECGCRTABXXX43210LO,x; x = [$00..$ff], 66667777
               ; 46 cycles

:           bit VIA1_PRA
            bmi :-
            sta BLOCKBUFFER + $02,y
            eor CHECKSUM
            sta CHECKSUM
            ldx VIA2_PRA   ; 00000111
            lda DECGCRTAB43210XXXHI,x; x = [$00..$ff], 0000....
            sta GCRBUFFER1
            txa
            and #%00000111 ; .....111
            sta GCRBUFFER2
            iny
            iny
            iny
               ; 42 cycles

:           bit VIA1_PRA
            bmi :-
            lda VIA2_PRA   ; 11222223
            sta GCRBUFFER0
            and #%11000000 ; 11......
            ora GCRBUFFER2 ; 11...111
            tax
            lda DECGCRTAB10ZZZ432LO,x; x = [($00..$ff) & $87]; ....1111
            ora GCRBUFFER1 ; 00001111
            sta BLOCKBUFFER + $00,y
            eor CHECKSUM
            sta CHECKSUM
            ldx GCRBUFFER0 ; 11222223
            iny
            beq :+
            jmp loaddata
               ; 49 cycles

:           txa            ; 11222223
            lsr            ; .1122222
:           bit VIA1_PRA
            bmi :-
            lda VIA2_PRA   ; 33334444
            and #%11110000 ; 3333....
            adc #%00000000 ; 3333...3
            tay
            lda DECGCRTABXX43210XHI,x; x = [$00..$ff], 2222....
            ora DECGCRTAB3210ZZZ4LO,y; y = [($00..$ff) & $f1], 22223333
            eor CHECKSUM
            bne chksumerrh; branch on checksum mismatch

            ; gets here on ANYSECTORSANELINK ($80) or requested sector
            ; block link sanity check
sanitychsw: beq success; is changed to lda #$01 after init
            cmp LINKSECTOR
            lda LINKTRACK
            bne :+
            bcc success
            SKIPWORD; branch to errorrethi
:           cmp MAXTRACK; check whether track link is within the valid range
            bcs errorrethi; if not, return error
            jsr getnumscts; get number of sectors on linked track
            dex
            cpx LINKSECTOR; check whether sector link is within the valid range
            bcc errorrethi; branch if sector number too large

success:    ; the link track is returned last so that the z-flag
            ; is set if this block is the file's last one (see FIND_FILE)
            lda LINKSECTOR  ; return the loaded block's sector link sector number
            ldx LOADEDSECTOR; return the loaded block's sector number
            ldy LINKTRACK   ; return the loader block's sector link track number
            clc             ; operation successful
            rts
errorrethi: lda #$00; read error: z = 1
chksumerrh: ; checksum mismatch: z = 0
            sec             ; operation not successful
            rts

trackseek:  lda #MOTOR; turn on the motor
            jsr busyledon + $02
            ldx #$80 | (MINSTEPSPEED + 1)
trackstep:  tya; destination track
            beq setbitrate; don't do anything if invalid track
            cmp MAXTRACK
            bcs setbitrate; don't do anything if invalid track
            sec
            lda CURTRACK
            sbc MAXTRACK_A
            beq :+
            bcc :+
            sta CURTRACK; the current track is on the 2nd side,
                        ; temporarily store the 2nd side physical track number
            SKIPWORD
            .assert * >= $02a9, error, "***** 1571 watchdog IRQ vector located below $02a9. *****"
            .assert * <= $02a9, error, "***** 1571 watchdog IRQ vector located above $02a9. *****"
            .word wdoghandlr

:           sec
            tya; destination track
            sbc MAXTRACK_A
            beq :+
            bcs :++; branch if the destination track is on the 2nd side
:           clc
            tya; the destination track is on the 1st side
:           pha
            lda VIA1_PRA
            and #.lobyte(~SIDE_SELECT)
            bcc :+
            ora #SIDE_B
:
c1570fix1:  sta VIA1_PRA
            pla
            sec
            sbc CURTRACK
            sty CURTRACK
            beq setbitrate

            ; do the track jump
            ldy #$01; move up (inwards)
            sty CURRSTEPSPEEDLOW
            bcs :+
            eor #.lobyte(~$00); invert track difference
            adc #$01
            ldy #$03; move down (outwards)
:           sty TRACKINC
            asl; half-tracks
            tay

            jsr one_mhz

:           stx TIMER; reset track-step timer
            lda VIA2_PRB
            anc #.lobyte(~(SYNC_MARK | MOTOR)); clc
            adc TRACKINC
            ora #MOTOR
            sta VIA2_PRB
            txa
            dey           ; do not wait after last half-track step,
            beq setbitrate; let checksumming and sanity checks take care of the rest
headaccl:   cmp #$80 | MAXSTEPSPEED
            beq noheadacc
            pha
           ;sec
            lda CURRSTEPSPEEDLOW
            sbc #STEPPERACC
            sta CURRSTEPSPEEDLOW
            pla
            sbc #$00
noheadacc:  cpx TIMER
            beq noheadacc; wait until the counter hi-byte has decreased by 1
            dex
            bmi headaccl
            tax
            bmi :-; jmp

            ; bit-rates:
            ; tracks 31-35/66+   (17 blocks): %00 - sector interleave 3 (lowest density, slowest clock, innermost tracks)
            ; tracks 25-30/60-65 (18 blocks): %01 - sector interleave 3
            ; tracks 18-24/53-59 (19 blocks): %10 - sector interleave 3
            ; tracks  1-17/36-52 (21 blocks): %11 - sector interleave 4 (highest density, fastest clock, outermost tracks)
setbitrate: lda CURTRACK
            jsr getnumscts
putbitrate: bit VIA2_PRB  ; is set to sta VIA2_PRB after init
putnumscts: bit NUMSECTORS; is changed to stx NUMSECTORS after init

            ; fall through

two_mhz:    lda #BYTE_READY | TWO_MHZ; the accu must contain a negative number upon return
            ora VIA1_PRA
            bne :+

            ; for normal busy LED fading speed and correct head
            ; stepping speed
one_mhz:    lda #.lobyte(~TWO_MHZ)
            and VIA1_PRA
:           sta VIA1_PRA
            rts

getnumscts: tay
            sec
            sbc MAXTRACK_A
            beq :+
            bcs getnumscts
:           lda VIA2_PRB
            ora #SYNC_MARK | BITRATE_MASK; $e0
            ldx #21; number of blocks
            cpy #18
            bcc :++; bit-rate $60
            dex
            dex; 19
            cpy #25
            bcc :+ ; bit-rate $40
            dex; 18
            and #.lobyte(~(%10 << BITRATE_SHIFT)); -$40
            cpy #31
            bcc :++; bit-rate $20
            dex; 17
:           and #.lobyte(~(%01 << BITRATE_SHIFT)); -$20
genspcdone:
:           rts

            FNAMEHASH 1571

rspeculate: ldx NUMSECTORS
clearidxtb: lda INDEXTAB - 1,x
            bmi :+
            and #INDEXSPECULATIVE
            beq :+
            sec
            ror INDEXTAB - 1,x
:           dex
            bne clearidxtb
            ldx MAXBLOCKINDEXPOS

speculate:  lda BLOCKINDEXBASE
            beq :+; do not (re-)speculate for the first file track, as this likely leads to much mis-speculation and ultimately lower speed
            clc; minus one block: do not transfer alleged final block, as it may be mis-speculated and load beyond the actual file end address
            lda NUMFILEBLOCKS
            sbc BLOCKINDEXBASE
            bcc genspcdone
            cmp NUMSECTORS
            bcc :+; limit to alleged number of file blocks
            lda NUMSECTORS
:           sta TEMP
genspcloop: iny
            cpy TEMP
            bcs genspcdone
            lda TRACKLINKTAB,x
            bpl havenewidx; branch if there is a linked block on the same track
            txa
           ;clc
            adc SPECULATIVEINTERLEAVE
            SKIPWORD
specidxtkn: inx
            txa
            cmp NUMSECTORS
            bcc havenewidx
           ;sec
            beq :+
            clc; subtract one after wrap (supposedly due to large tail gap)
:           sbc NUMSECTORS
havenewidx: tax
            lda INDEXTAB,x
            bpl specidxtkn
            tya
            ora #BLOCKPENDING | INDEXSPECULATIVE
            sta INDEXTAB,x
            bne genspcloop; jmp

dgetbyte:   lda #$ff
            sta TIMER; reset watchdog time-out
            lda #.lobyte(~(ATNA_OUT | CLK_OUT | DATA_OUT)); ATNA_OUT, CLK_OUT and DATA_OUT low: drive is ready
            sta VIA1_PRB
customgbyt: lda #%10000000; CLK OUT low: drive is ready
            GETBYTE_COMMON getbatncmp, ATN_IN; $80
getbyterts: rts; is changed to sta (zp,x) for custom drive code upload
            .byte .lobyte(customparm - customgbyt + $03 + 1)
            inc .lobyte(customparm - customgbyt + $03)
            bne :+
            inc .lobyte(customparm - customgbyt + $04)
:           dec .lobyte(customparm - customgbyt + $05)
            bne customgbyt
            dec .lobyte(customparm - customgbyt + $06)
            bpl customgbyt

            .byte OPC_JMP_ABS; execute custom drive code
customparm:

noupload:   tya
            beq :+
wdoghandlr: ; watchdog handler: unknown if LED on, off, or fading: fade off LED regardless
            jsr busyledon
            lda #$ff; fade off the busy LED
:           pha
            ldy #18; ROM dir track
            jsr trackseek; ignore error (should not occur)
            pla
            tay
:           jsr fadeled
            bne :-
            jmp (RESET_VECTOR)

duninstall: ldx #$00
:           lda customgbyt,x
            sta $01,x; $00 = CURTRACK
            inx
            bpl :-
            inx; $81, ldx #OPC_STA_ZPXI
            stx .lobyte(getbyterts - customgbyt + $01)

            jsr two_mhz

            ; no watchdog, ATN_IN is cleared
            txa; $81, #ATN_IN | DATA_IN
            ldx #CLK_OUT
            stx VIA1_PRB; clear ATNA_OUT to check DATA_IN with ATN_IN clear, set CLK_OUT to signal ready for code upload

            ldx #$05
:           bit VIA1_PRB
            beq noupload; branch if ATN_IN and DATA_IN clear
            bpl :-; wait for ATN_IN high

            ; get custom drive code
:           jsr dgetbyte
            sta .lobyte(customparm - customgbyt + $01),x
            dex
            bpl :-
           ;ldx #$ff
            txs
            jmp $01

statussent: lda BLOCKBUFFER + $00; offset 0: block index or status byte
            bmi skipprpnxt; only after successful load
            PREPARE_NEXT_FILE 1571
skipprpnxt:
            lda #ATNA_OUT
            sta VIA1_PRB
            ldy #$01; turn motor and busy LED off
            lda #BUSY_LED; check if busy LED is lit
            and VIA2_PRB
            beq :+
            ldy #$ff; fade off the busy LED, then turn motor off
:
            ; upon first load, this is skipped,
            ; and beginload is executed directly
driveidle:  jsr one_mhz; 1 MHz so the LED fades at the same speed as on 1541
idleloop:   jsr checkchgl; fade off busy LED and check light sensor for disk removal
            lda VIA1_PRB
            eor #ATN_IN | ATNA_OUT | CLK_IN | DATA_IN
            beq driveidle; wait until there is something to do
            asl; check for reset, uninstallation or custom drive code upload
            bne duninstall; check for reset or uninstallation

            jsr two_mhz

            ; load a file

            tya
            beq beginload; check whether the busy LED has been completely faded off
            jsr busyledon; if not, turn it on

beginload:  lda #$ff ; reset watchdog time-out, this also clears the possibly
            sta TIMER; pending timer 1 IRQ flag
            sta PREVBLOCKINDEX
            ENABLE_WATCHDOG; enable watchdog, the computer might be reset while sending over
                           ; a byte, leaving the drive waiting for handshake pulses

            GET_FILENAME 1571

            ; matches against hash of filename in FILENAMEHASH0/1
            FIND_FILE 1571
            sta FILESECTOR; file's starting sector
            lda #diskio::status::FILE_NOT_FOUND
            bcs filenotfnd
            lda FILENAMEHASHVAL0,x ; store hash of next file's
            sta FILENAMEHASH0      ; name for loadnext
            lda FILENAMEHASHVAL1,x ; and PREPARE_NEXT_FILE
            sta FILENAMEHASH1      ; functionality

            lda #$00
trackloop:  sta BLOCKINDEXBASE
            jsr trackseek

            ; x contains the total number of blocks on this track
            lda #$80
trackinit:  sta INDEXTAB - 1,x     ; sector indices are unknown
            sta TRACKLINKTAB - 1,x ; and so are sector links
            dex
            bne trackinit
            sta MAXINDEXONTRACK

            ldx FILESECTOR
            stx MAXBLOCKINDEXPOS
            stx NEXTCONTIGUOUSBLOCK
            ldy #$00; initial block index
            sty MAXCONFIRMEDBLOCKINDEX
            sty NUMCONTIGUOUSBLOCKS
            lsr INDEXTAB,x; $40 = BLOCKPENDING | $00
            jsr speculate; build initial speculative block index table for this track

blockloop:  lda #ANYSECTORSANELINK; compare ID, block link sanity check
            sta REQUESTEDSECTOR
            jsr getblkscan
            bcc :+; branch if fetch successful
            bne blockloop; branch if checksum error
            ldx LOADEDSECTOR; load error, ID mismatch or illegal track or sector (invalid track/sector link)
            jsr idxinvalid; out-of-range index, not pending
           ;sec
            bcs blockloop; jmp

:          ;lda LINKSECTOR
           ;ldx LOADEDSECTOR
           ;ldy LINKTRACK
            cpy CURTRACK
            bne nosetspcil; branch if linked block not on current track
           ;sec
           ;lda LINKSECTOR
            sta TRACKLINKTAB,x; set block link
            sbc LOADEDSECTOR; determine likely interleave
            bcc nosetspcil
;            bcs :+
;            adc NUMSECTORS
;:
            sta SPECULATIVEINTERLEAVE
nosetspcil: jsr loadspec
            bcs blockloop

            lda NUMCONTIGUOUSBLOCKS
            adc BLOCKINDEXBASE

            ldy FILETRACK
            bne trackloop

            ; loading is finished

            tya; $00 = diskio::status::OK
filenotfnd: ldy #$00
            sty blocksize + $01; send over one byte
            jsr sendblock
            jmp statussent

idxinvalid: lda #INDEXSPECULATIVE | $1f; out-of-range index, not pending
            sta INDEXTAB,x
            sec
            rts

loadspec:   ; first file block must be loaded first
            lda CURRBLOCKINDEX
            bmi idxinvalid; branch if block conceivably not belonging to file
            beq :+
            ldy PREVBLOCKINDEX
            iny
            beq idxinvalid

:           ldx MAXBLOCKINDEXPOS
            ldy MAXCONFIRMEDBLOCKINDEX
            clc; clear mis-speculation detected flag
            bcc linkindex

            ; validate block indices according to currently-known links,
            ; entry point is linkindex
indexloop:  tax; link sector
            iny; block index
            tya
            eor INDEXTAB,x
            eor #INDEXSPECULATIVE
            beq confirmspc; branch if already-loaded speculative index matches
            eor #BLOCKPENDING
            beq confirmidx; branch if not-yet-loaded speculative index matches
            sec ; mis-speculated block index detected
confirmidx: tya
            ora #BLOCKPENDING
confirmspc: sta INDEXTAB,x
linkindex:  lda TRACKLINKTAB,x; linked sector
            bpl indexloop; branch if there is a linked block on the same track
            stx MAXBLOCKINDEXPOS
            sty MAXCONFIRMEDBLOCKINDEX

            bcc nomisspec
            jsr rspeculate; mis-speculated block indices detected: rebuild speculative block index table

nomisspec:  ldx LOADEDSECTOR
            lda INDEXTAB,x
            ldy LINKTRACK
            cpy CURTRACK
            beq notrklink
            ; block links to other track or is the file's last one
            ldy CURRBLOCKINDEX
            bit .lobyte(SPC); INDEXSPECULATIVE
            beq settrklink; branch if block was not loaded speculatively
            bit MAXINDEXONTRACK
            bpl idxinvalid; branch if track link has been set already
            iny
            cpy NUMSECTORS
            bcc idxinvalid; branch if not possibly file's last block on track
            dey
settrklink: sty MAXINDEXONTRACK; actually number of file blocks on this track minus 1
            ldy LINKTRACK
            sty FILETRACK
            ldy LINKSECTOR
            sty FILESECTOR; first sector on the next track
notrklink:
            and #.lobyte(~BLOCKPENDING)
            sta INDEXTAB,x
            cmp #INDEXSPECULATIVE
            bcc :+; branch if block index is not speculative
            and #$1f
            cmp MAXINDEXONTRACK
            beq :+
            bcs idxinvalid; do not transfer block if speculative index is out of range
:
            lda NEXTCONTIGUOUSBLOCK
            ldy NUMCONTIGUOUSBLOCKS
:           tax
            lda INDEXTAB,x
            cmp #$1f
            bcs :+; branch if block has not been confirmed and transferred already
            iny
            lda TRACKLINKTAB,x
            bpl :-; branch if there is a linked block on the same track
:           stx NEXTCONTIGUOUSBLOCK
            sty NUMCONTIGUOUSBLOCKS

            lda MAXINDEXONTRACK
            cmp NUMCONTIGUOUSBLOCKS
            bcs nostep
            lda FILETRACK
            beq nostep

            ; perform Shrydar Stepping (R)(TM) to minimise single-track stepping overhead:
            ; nudge the R/W head in the right direction, then wait a few bycles while it gains momentum,
            ; then enable the destination track's stepper magnet long before the head has reached the intermediate half-tracks magnet,
            ; relying on the head's inertia, then send over the block while the head keeps moving beyond the intermediate half-tracks stepper magnet
            ; and ultimately settles on the destination track.
            ; sending the block over takes at least 72 bycles
            ldy CURTRACK
            cpy FILETRACK
            bcs :+
            iny
            SKIPBYTE
:           dey
            ldx #$80 | SINGLESTEPSPEED
            jsr trackstep

nostep:     clc
            lda #$ff
            ldx LINKTRACK
            bne :+
            sec
            lda LINKSECTOR; the file's last block's length (last byte index)
:           sta blocksize + $01
            eor #$ff
            bcs :+; branch if file's last block: send block size
            lda BLOCKINDEXBASE
            adc NUMCONTIGUOUSBLOCKS; actually NUMCONTIGUOUSBLOCKS + 1
:           adc #$01
            sta BLOCKBUFFER + $01; block size

            ; calculate block index in host memory
            clc
            lda CURRBLOCKINDEX
            adc BLOCKINDEXBASE
            tax
            sec
            sbc PREVBLOCKINDEX
            stx PREVBLOCKINDEX
            clc
            ldx LINKTRACK; file's last block: bit 0 set
            bne :+
            sbc #$00; counter set carry on host side
           ;sec
:           rol
            eor #$80

            ; accu: block index or status byte
sendblock:  sta BLOCKBUFFER + $00; block index or status byte
            ldx #$24; here, the watchdog timer is polled manually because
                    ; an extra-long time-out period is needed since the computer may
                    ; still be busy decompressing a large chunk of data;
                    ; this is the round counter
            ldy #$ff
            sty TIMER; reset watchdog time-out, this also clears the possibly
                     ; pending timer 1 IRQ flag
            lda #ATNA_OUT | CLK_OUT; drop DATA_OUT, set CLK_OUT high as signal of presence
            sta VIA1_PRB; block ready signal
waitready:  lda TIMER; see if the watchdog barked
            bne :+
            dex      ; if yes, decrease the round counter
.if DISABLE_WATCHDOG
            beq :+
.else
            beq timeout; and trigger watchdog on time-out
.endif
            sty TIMER; reset watchdog time-out and clear IRQ flag
:           bit VIA1_PRB
            bmi waitready; wait for ATN low
            sty TIMER; reset watchdog time-out and clear possibly set IRQ flag

timeout:    ENABLE_WATCHDOG
            iny; ldy #$00
sendloop:   lda #$ff                ; 2
            sta TIMER               ; 4 - reset watchdog time-out
            lda BLOCKBUFFER,y       ; 4
            and #BINARY_NIBBLE_MASK ; 2
            tax                     ; 2
            lda sendnibbletab,x     ; 4
                                    ; = 32

:           bit VIA1_PRB            ; 4
            bmi :-                  ; 3
            sta VIA1_PRB            ; 4
            asl                     ; 2
            ora #ATNA_OUT           ; 2
                                    ; = 15

:           bit VIA1_PRB            ; 4
            bpl :-                  ; 3
            sta VIA1_PRB            ; 4
            lda BLOCKBUFFER,y       ; 4
            lsr                     ; 2
            lsr                     ; 2
            lsr                     ; 2
            lsr                     ; 2
            tax                     ; 2
            lda sendnibbletab,x     ; 4
                                    ; = 29

:           bit VIA1_PRB            ; 4
            bmi :-                  ; 3
            sta VIA1_PRB            ; 4
            asl                     ; 2
            ora #ATNA_OUT           ; 2
blocksize:  cpy #$00                ; 2
            iny                     ; 2
                                    ; = 19

:           bit VIA1_PRB            ; 4
            bpl :-                  ; 3
            sta VIA1_PRB            ; 4
            bcc sendloop            ; 3
                                    ; = 95
            lda MAXINDEXONTRACK
            cmp NUMCONTIGUOUSBLOCKS
            lda #ATNA_OUT | DATA_OUT; drive busy

:           bit VIA1_PRB; wait for ATN low,
            bmi :-      ; acknowledgement of the last data byte

            sta VIA1_PRB

:           bit VIA1_PRB; wait for ATN in high,
            bpl :-      ; acknowledgement of the block transfer

            sei; disable watchdog
            rts

            .assert * <= BLOCKBUFFER, error, "***** 1571 drive code too large. *****"

dcodinit71: ldx #.lobyte(stackend - $01)
            txs

            lda #.lobyte(~MOTOR)
            and VIA2_PRB
            sta VIA2_PRB

            lda #T1_FREE_RUNNING | PA_LATCHING_ENABLE; watchdog IRQ: count phi2 pulses, 16-bit free-running,
            sta VIA2_ACR                             ; port a latching should not be needed here
                                                     ; (IC rather than discrete logic),
                                                     ; but it is enabled just to be sure
            lda #READ_MODE | BYTE_SYNC_ENABLE
            sta VIA2_PCR

            ; before loading the first file, the current track number is
            ; retrieved by reading any block header on the disk -
            ; however, if the loader is uninstalled before loading anything,
            ; it needs the more or less correct current track number to
            ; seek to track 18
            lda ROMOS_HEADER_TRACK; $18
            sta CURTRACK; $13

            ; set seek boundaries according to number of disk sides
            lda #MAXTRACK71 + 1
            ldx #NUMTRACKS_A
            ldy INITBUF_MAXTRK
            cpy #NUMTRACKS_SINGLESIDED + 2; + 2 because + 1 would misinterpret MAXTRACK = NUMTRACKS_SINGLESIDED + 1 as indicator for double-sidedness upon reinstall
c1570fix0:  bcs :+
            lda #NUMTRACKS_SINGLESIDED + 1
            ldx #NUMTRACKS_SINGLESIDED
:           sta MAXTRACK
            stx MAXTRACK_A

            ; watchdog initialisation
            lda #IRQ_CLEAR_FLAGS | IRQ_ALL_FLAGS
            sta DISKCHANGED
            sta VIA1_IER; no IRQs from VIA 1
            sta VIA2_IER; no IRQs from VIA 2
            sta CIA_ICR; no IRQs from CIA
            bit CIA_ICR
            lda #IRQ_SET_FLAGS | IRQ_TIMER_1
.if !DISABLE_WATCHDOG
            sta VIA2_IER; timer 1 IRQs from VIA 2
.else
            bit VIA2_IER; timer 1 IRQs from VIA 2
.endif
            lda #ATNA_OUT; signal idle to the host with ATN_IN low
            sta VIA1_PRB
:           bit VIA1_PRB
            bpl :-; no watchdog

            ; fade off the busy LED if lit
            lda VIA2_PRB
            sta DISKCHANGEBUFFER; store light sensor state for disk removal detection
            and #BUSY_LED
            beq :+
            lda #$ff
:           tay
            ; before spinning up the motor and finding the current track,
            ; wait until a file is requested to be loaded at all
            jsr one_mhz
:           jsr fadeled
            lda VIA1_PRB
            eor #ATN_IN | ATNA_OUT | CLK_IN | DATA_IN
            beq :-; wait until there is something to do
            asl; check for reset, uninstallation or custom drive code upload
            beq :+
            jmp duninstall
:
            jsr two_mhz

            lda #MAXNUMSECTORS
            sta NUMSECTORS

            lda INITBUF_TRACK_DIFF
            bne dcodinitc; branch if the drive had already seeked before the loader has been started
            ; the drive was reset immediately before running the loader -
            ; step down a track: this works normally if the stepping bits are congruent with the stepper motor;
            ; however, it may happen that the bits are misaligned (opposite to the actual stepper position, bit 1
            ; reversed), this alone does not move the head but stepping makes it go into the direction opposite to
            ; the one desired when moving; the stepping down two halftracks will actually step up and step down one
            ; halftrack each and thus will end up on the same track as before, but align the stepper bits to the motor.
            ldy #$02
            sty CURTRACK
            dey
            jsr trackseek

dcodinitc:  jmp findtrackn

drvcodeend71:

            ; following code is transferred using KERNAL routines,
            ; then it is run and gets the rest of the code

            ; entry point

dinstall:   sei
            lda ROMOS_MAXTRACK
            sta INITBUF_MAXTRK
            lda ROMOS_TRACK_DIFF
            sta INITBUF_TRACK_DIFF

            lda #TWO_MHZ
            ora VIA1_PRA
            sta VIA1_PRA

            ldy #CLK_OUT
            sty VIA1_PRB
            lda #VIA_ATN_IN_INPUT | VIA_DEVICE_NUMBER_OUTPUT | VIA_ATNA_OUT_OUTPUT | VIA_CLK_OUT_OUTPUT | VIA_CLK_IN_INPUT | VIA_DATA_OUT_OUTPUT | VIA_DATA_IN_INPUT
            sta VIA1_DDRB

:           lda VIA1_PRB; wait for DATA IN high
            lsr
instalwait: bcc :-

            ldx #.lobyte(drvcodebeg71 - $01)
dgetrout:   inx
            bne :+
            inc dgetputhi
:           lda #%10000000; CLK OUT low: drive is ready
            sta VIA1_PRB
            GETBYTE_COMMON getbnoatncmp, CLK_IN; there is no watchdog while installing
dgetputhi = * + $02
            sta a:.hibyte(drvcodebeg71 - $01) << 8,x
            cpx #.lobyte(dinstall - $01)
            bne dgetrout
            dec drvcodebeg71
            bne dgetrout

            jmp dcodinit71

drvprgend71:
            .reloc
