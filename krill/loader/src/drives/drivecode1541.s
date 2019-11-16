
.include "cpu.inc"
.include "via.inc"

.include "drives/drivecode-common.inc"

; Uncomment to compile the 1541 drivecode for Century Planning Corp. CX-500/Tecmate NPH-501C, which is not
; a clone, as the VIAs are located at $3800 and $5c00 due to simplified address decoding logics, and the ROM
; including the position of the GCR encoding and decoding tables (not used here) is entirely different as well.
;CX500                  = 1

JOBCODE0300            = $00
CURTRACK               = JOBCODE0300; always positive, not interpreted as job code

JOBCODE0400            = $01
LOADEDSECTOR           = JOBCODE0400; always positive, not interpreted as job code

JOBCODE0500            = $02
NUMSECTORS             = JOBCODE0500; always positive, not interpreted as job code

JOBCODE0600            = $03
MAXCONFIRMEDBLOCKINDEX = JOBCODE0600; always positive, not interpreted as job code

LINKTRACK              = $08
LINKSECTOR             = $10
REQUESTEDSECTOR        = LINKSECTOR

DISKCHANGEBUFFER       = $13
DISKCHANGED            = $17
FILETRACK              = $18
FILESECTOR             = $19
ID0                    = $1b
ID1                    = $1f
NUMFILES               = $20
FILENAMEHASH0          = $27
FILENAMEHASH1          = $2b
BLOCKINDEXBASE         = $2f
CURRBLOCKINDEX         = $30
PREVBLOCKINDEX         = $33
NUMCONTIGUOUSBLOCKS    = $37
NEXTCONTIGUOUSBLOCK    = $3b
NUMFILEBLOCKS          = $40
MAXBLOCKINDEXPOS       = $54
SPECULATIVEINTERLEAVE  = $5c

TEMP                   = $50
CURRSTEPSPEEDLOW       = TEMP
BLOCKINDEX_STATUS      = TEMP

TEMP2                  = $5a
TRACKINC               = TEMP2
DECODETEMP             = TEMP2

; DECODETAB0 is at $11..$65
STACKBUF               = $56; 4 bytes
STACKBUFE              = $5a

CURRDIRBLOCKSECTOR     = NUMCONTIGUOUSBLOCKS
CYCLESTARTENDSECTOR    = NEXTCONTIGUOUSBLOCK
NEXTDIRBLOCKSECTOR     = $60
DIRBLOCKPOS            = $64

MAXINDEXONTRACK        = $66; must be INDEXTAB - 1, see trackinit
INDEXTAB               = $67; length MAXNUMSECTORS = $15 bytes

FILENAME               = INDEXTAB; max. $10 bytes

DIRBUFFSIZE            = 9
FILENAMEHASHVAL0       = $44; length is DIRBUFFSIZE
; FILENAMEHASHVAL1 is at $7c..$84,
; DIRTRACKS is at $85..$8d
; readloop is at $8e..$ff
FILENAMEHASHVAL1       = INDEXTAB + MAXNUMSECTORS
DIRTRACKS              = FILENAMEHASHVAL1 + DIRBUFFSIZE

DECODETABLE            = $0700; effectively starts at $0722

TRACKLINKTAB           = $0704; length MAXNUMSECTORS = $15 bytes
DIRSECTORS             = $0719
NUMBLOCKS              = $0796

TRACK_DIFF             = NUMFILEBLOCKS; must be >= $40 due to how the installation procedure is structured

ROMOS_HEADER_ID0       = $16
ROMOS_HEADER_ID1       = $17
ROMOS_HEADER_TRACK     = $18
ROMOS_HEADER_SECTOR    = $19
ROMOS_TRACK_DIFF       = $42

BINARY_NIBBLE_MASK     = %00001111

MAXNUMSECTORS          = 21
MAXTRACK41             = 41

TIMER                  = VIA2_T1C_H
TIMER2                 = VIA1_T1C_H

BLOCKPENDING           = %01000000
INDEXSPECULATIVE       = %00100000

DIRBUFFSIZE41          = DIRBUFFSIZE
.export DIRBUFFSIZE41

            .assert DIRBUFFSIZE >= 9, error, "***** Dir buffer too small. *****"

            .org $3f

drvcodebeg41:
            .byte .hibyte(drvcodeend41 - * + $0100 - $01); init transfer count hi-byte

MNS  = MAXNUMSECTORS; NUMSECTORS initial value
WRP  = WRITE_PROTECT; DISKCHANGED initial value
PBI  = $ff          ; PREVBLOCKINDEX initial value
GIL  = $08          ; SPECULATIVEINTERLEAVE initial value
XEF  = $ef

SPC  = $07; $20 = INDEXSPECULATIVE
ATA  = $0e; $10 = ATNA_OUT
EF   = $5b

T1   = TIMER
TIM  = $22
V1   = VIA1_PRB
V1B  = $28
V2   = VIA2_PRB
V2B  = $38

D0   = DECODETAB8 + 0
DT0  = $42
D1   = DECODETAB8 + 1
DT1  = $52

ST   = STACK
STP  = $3e
SD   = STACK + $fd
STD  = $4e
SF   = STACK + $ff
TOS  = $5e
BL   = STACK - $1a  ; number of file blocks in dir sector
BLP  = $62

___  = 123
            ;                         $d0 doubles as JOBCODE_JUMP to $0700 to handle the watchdog IRQ
           ;.byte      ___, MNS, ___, $d0, $60, $90, $20, ___, $40, $f0, $00, $50, $30, $10, $f0; $00 is CURTRACK
           ;.byte ___, $01, $c3, ___, $d3, $a1, $53, WRP, ___, ___, $83, ___, $93, $c1, $13, ___
           ;.byte ___, $41, <T1, >T1, $f3, $e1, $73, ___, <V1, >V1, $23, ___, $b3, $91, $33, ___
           ;.byte ___, $11, $43, PBI, $e3, $81, $63, ___, <V2, >V2, $03, ___, $a3, $d1, <ST, >ST
            .byte ___, $51, <D0, >D0, ___, ___, ___, ___, ___, ___, ___, ___, ___, $b1, <SD, >SD
            .byte ___, $31, <D1, >D1, ___, $21, ___, ___, ___, ___, ___, XEF, GIL, $f1, <SF, >SF
            .byte ___, $71, <BL, >BL, ___, $61, ___, ___, ___, ___

            ; initialisation continued from dcodinitc
            ; will be overwritten with INDEXTAB ($67..$7b) and FILENAMEHASHVAL1 ($7c..$84)

nxtbitrate: lax VIA2_PRB
            axs #$0100 - (1 << BITRATE_SHIFT)
            stx VIA2_PRB; cycle through the 4 bit-rates
findtrackn: lda #$ff; invalid track number -> no track step
            ldx #OPC_STA_ZP; $85
            tay
            jsr getblkinit; any sector, no block link sanity check, store ID
            lda STACK + $fe; track
            sta CURTRACK
            jsr loadblock
            bcs nxtbitrate
            lda CURTRACK
            jsr rawtoser
            sta CURTRACK
            jmp toloadfile

.define SUPPORT_WOBBLY_DRIVES 1

.if SUPPORT_WOBBLY_DRIVES
            ; for the tightest bitrate %11, i.e., tracks 1-17 with 21 blocks each, a quarter of the checksumming effort is
            ; shifted to after reading and decoding a block, such that there are enough spare cycles for some picky drives with
            ; massive wobble, which do not tolerate 129 cycles for the read+decode+checksumming loop but want 127 or fewer -
            ; this does not reduce speed, as on that track zone, a track is read in 4 revolutions rather than 3 as with the
            ; others, as the 3rd block after reading and transferring the current one is just missed anyways, even with
            ; same-sized inter-sector gaps and no big tail gap, leaving enough spare time for some lazy checksumming.

            ;                   bitrates %11 %10 %01 %00
readloop:   inc DECODETEMP             ;              67
            inc DECODETEMP             ;          67  72
            eor STACK + 4,x            ;      66  71  76 ;              ; this is skipped with bitrate %11, otherwise eor #$ff (byte at $0200) in first loop
ST3 = * + 1; $0103
            eor STACK + 3,x            ;  66  70  75  80
readdata:   sta .lobyte(checksum + 1)  ;  69  73  78  83
            lax VIA2_PRA               ;  73  77  82  87 ; 3:33334444   ; 2 - %11: cycle 73 is -4 in [52..77], %10: cycle 77 is -6 in [56..83]
            arr #%11110000             ;  75  79  84  89 ;   33333---   ;     %01: cycle 84 is -7 in [60..89], %00: cycle 89 is -6 in [64..95]
            tay                        ;  77  81  86  91
            lda #%00001111             ;  79  83  88  93
            sax .lobyte(decode4 + 1)   ;  82  86  91  96 ;   ----4444
            ldx VIA2_PRA               ;  86  90  95 100 ;   45555566   ; 3 - %11: cycle 86 is +8 in [78..103], %10: cycle 90 is +6 in [84..111]
            sax .lobyte(decode6 + 1)   ;  89  93  98 103 ;   ----5566   ;     %01: cycle 95 is +5 in [90..119], %00: cycle 100 is +4 in [96..127]
decode2:    lda $00                    ;  92  96 101 106 ;   --22222- -> $x0
            eor DECODETAB3,y           ;  96 100 105 110 ;   33333--- -> $0x
            pha                        ;  99 103 108 113 ;              ; first push goes to STACK + 0 = $0100
            lda #%11111100             ; 101 105 110 115
            axs #$00                   ; 103 107 112 117 ;   455555--   ; sets c
            lda DECODETAB5,x           ; 107 111 116 121 ;   455555-- -> $xx, partial
decode4:    adc $00                    ; 110 114 119 124 ;   ----4444 -> $x0, clears v
            pha                        ; 113 117 122 127
            ldx #%11100000             ; 115 119 124 129
            lda VIA2_PRA               ; 119 123 128 133 ;   66677777   ; 4 - %11: cycle 119 is -10 in [104..129], %10: cycle 123 is +11 in [112..139],
            clv                        ; 121 125 130 135 ;              ;     %01: cycle 128 is +8 in [120..149], %00: cycle 133 is +5 in [128..159]
            axs #$0100 - decoffs6      ; 123 127 132 137 ;   666-----   ; clv %11: cycle 121 is -8 in [104..129], %10: cycle 125 is +13 in [112..139], 
            and #%00011111             ; 125 129 134 139 ;   ---77777   ;     %01: cycle 130 is +10 in [120..149], %00: cycle 135 is +7 in [128..159]
            tay                        ; 127 131 136 141
                                       ;[130 140 150 160]
            ;                          all bitrates
FE = * + 1; $fe
            bvc *                      ;   2
decode6:    lda DECODETAB6,x           ;   6             ;   666-5566 -> $x0 ; x = 666-----, lsb = ----5566
            eor DECODETAB7,y           ;  10             ;   ---77777 -> $0x
            pha                        ;  13             ;              ; checksummed with eor #imm below
            sta .lobyte(checksum2 + 1) ;  16
            lax VIA2_PRA               ;  20             ;   00000111   ; 0 - cycle 20 is -5 in [0..25]
            eor #%11111000             ;  22
            sax .lobyte(decode1 + 1)   ;  25             ;   -----111
            alr #%11111000             ;  27             ;   -00000--
            tay                        ;  29
_3E = * + 1; $3e
            ldx #%00111110             ;  31
            lda VIA2_PRA               ;  35             ;   11222223   ; 1 - cycle 35 is +3 in [32..51]
            sax .lobyte(decode2 + 1)   ;  38             ;   --22222-
            alr #%11000001             ;  40             ;   -11-----:3
            tax                        ;  42
            lda DECODETAB0,y           ;  46             ;   -00000-- -> $x0 ; zeropage access
decode1:    eor DECODETAB1,x           ;  50             ;   -11--111 -> $0x ; x = -11-----, lsb = -----111
            pha                        ;  53
checksum:   eor #$00                   ;  55
checksum2:  eor #$00                   ;  57
            tsx                        ;  59
            bne readloop               ;  62             ; branch to readloop + [0, 2, 4, 7] or to * + 2
bitrateswt   = * - 1

            sta .lobyte(checksum + 1)  ;  64
V2A = * + 1
            lda VIA2_PRA               ;  68             ; 3:33334444 ; 2 - cycle 68 is +4 in [64..77]
            ror                        ;                 ;   33333444
            tay
            lda DECODETAB3 - 2,y       ;                 ;   33333--- -> $0x, -2 for 5/GCR $a shifted to 2
            ldx .lobyte(decode2 + 1)   ;                 ;   --22222-
            jmp dataread

.else; !SUPPORT_WOBBLY_DRIVES

            ; full on-the-fly GCR read+decode+checksumming across the entire disk,
            ; may be incompatible with some drives with extreme wobble,
            ; and is likely not faster than the safer method

            ;                   bitrates %11 %10 %01 %00
readloop:   inc DECODETEMP             ;              64
            inc DECODETEMP             ;          64  69
            inc DECODETEMP             ;      64  69  74
ST3 = * + 1; $0103
            eor STACK + 3,x            ;  63  68  73  78
ST2 = * + 1; $0102
            eor STACK + 2,x            ;  67  72  77  82
readdata:   sta .lobyte(checksumx + 1) ;  70  75  80  85
            lax VIA2_PRA               ;  74  79  84  89 ; 3:33334444   ; 2 - %11: cycle 74 is -3 in [52..77], %10: cycle 79 is -4 in [56..83]
            arr #%11110000             ;  76  81  86  91 ;   33333---   ;     %01: cycle 84 is -5 in [60..89], %00: cycle 89 is -6 in [64..95]
            tay                        ;  78  83  88  93
            lda #%00001111             ;  80  85  90  95
            sax .lobyte(decode4 + 1)   ;  83  88  93  98 ;   ----4444
            ldx VIA2_PRA               ;  87  92  97 102 ;   45555566   ; 3 - %11: cycle 87 is +9 in [78..103], %10: cycle 92 is +8 in [84..111]
            sax .lobyte(decode6 + 1)   ;  90  95 100 105 ;   ----5566   ;     %01: cycle 97 is +7 in [90..119], %00: cycle 102 is +6 in [96..127]
decode2:    lda $00                    ;  93  98 103 108 ;   --22222- -> $x0
            eor DECODETAB3,y           ;  97 102 107 112 ;   33333--- -> $0x
            pha                        ; 100 105 110 115 ;              ; first push goes to STACK + 0 = $0100
checksumx:  eor #$00                   ; 102 107 112 117
            sta .lobyte(checksum + 1)  ; 105 110 115 120
            lda #%11111100             ; 107 112 117 122
            axs #$00                   ; 109 114 119 124 ;   455555--   ; sets c
decode4:    lda $00                    ; 112 117 122 127 ;   ----4444 -> $x0, partial    %11: cycle 118 is -11 in [104..129], %10: cycle 123 is +11 in [112..139]
            adc DECODETAB5,x           ; 116 121 126 131 ;   455555-- -> $xx, clears v - %01: cycle 128 is +8 in [120..149], %00: cycle 133 is +5 in [128..159]
            pha                        ; 119 124 129 134 ;              ; checksummed with eor STACK + 3,x above
            lda VIA2_PRA               ; 123 128 133 138 ;   66677777   ; 4 - %11: cycle 125 is -4 in [104..129], %10: cycle 130 is -9 in [112..139]
            ldx #%11100000             ; 125 130 135 140 ;              ;     %01: cycle 135 is -14 in [120..149], %00: cycle 140 is +12 in [128..159]
            axs #$0100 - decoffs6      ; 127 132 137 142 ;   666-----   ; mask and adjust for table not at $xx00
            and #%00011111             ; 129 134 139 144 ;   ---77777
                                       ;[130 140 150 160]
            ;                          all bitrates
FE = * + 1; $fe
            bvc *                      ;   2
            tay                        ;   4
decode6:    lda DECODETAB6,x           ;   8             ;   666-5566 -> $x0 ; x = 666-----, lsb = ----5566
            eor DECODETAB7,y           ;  12             ;   ---77777 -> $0x
            pha                        ;  15             ;              ; checksummed with eor STACK + 2,x above
            lax VIA2_PRA               ;  19             ;   00000111   ; 0 - cycle 19 is -6 in [0..25]
            eor #%11111000             ;  21
            sax .lobyte(decode1 + 1)   ;  24             ;   -----111
            alr #%11111000             ;  26             ;   -00000--
            tay                        ;  28
            ldx #%00111110             ;  30
            lda VIA2_PRA               ;  34             ;   11222223   ; 1 - cycle 34 is +2 in [32..51]
            sax .lobyte(decode2 + 1)   ;  37             ;   --22222-
            alr #%11000001             ;  39             ;   -11-----:3
            tax                        ;  41
            lda DECODETAB0,y           ;  45             ;   -00000-- -> $x0 ; zeropage access
decode1:    eor DECODETAB1,x           ;  49             ;   -11--111 -> $0x ; x = -11-----, lsb = -----111
            pha                        ;  52
checksum:   eor #$00                   ;  54
            tsx                        ;  56
            bne readloop               ;  59             ; branch to readloop + [0, 2, 4, 6] or * + 2
bitrateswt   = * - 1
READLOOPBASE = .lobyte(readloop - *)

            sta .lobyte(checksum + 1)  ;  63
            ldx .lobyte(decode2 + 1)   ;  66             ;   --22222-
V2A = * + 1
            lda VIA2_PRA               ;  70             ; 3:33334444 ; 2 - cycle 70 is +6 in [64..77]
            ror                        ;                 ;   33333444
            tay
            lda $00,x                  ;                 ;   --22222- -> $x0
            jmp dataread
.endif; !SUPPORT_WOBBLY_DRIVES

            ; * = $0100

            .byte 0; track link in loaded block
            .byte 0
            .byte 0; number of blocks of file #8 in loaded dir block
stack:
            .assert stack >= (STACK + $03), error, "***** 1541 stack below $0103. *****"
            .assert stack <= (STACK + $03), error, "***** 1541 stack above $0103. *****"

            .word 0, 0
stackend:   ; stacktop + 1

            .assert stackend >= (STACK + $07), error, "***** 1541 top of stack below $0107. *****"
            .assert stackend <= (STACK + $07), error, "***** 1541 top of stack above $0107. *****"

            .byte 0; padding

dcodinitc:  lda TRACK_DIFF
            bne stepperok; branch if the drive had already seeked before the loader has been started
            ; here, the drive was apparently reset immediately before running the loader -
            ; step down a track: this works normally if the stepping bits are congruent with the stepper motor.
            ; however, it may happen that the bits are misaligned (opposite to the actual stepper position, bit 1
            ; reversed), this alone does not move the head but stepping makes it go into the direction opposite to
            ; the one desired when moving. the stepping down two halftracks will actually step up and step down one
            ; halftrack each and thus will end up on the same track as before, but align the stepper bits to the motor.
            ldy #$02
            sty CURTRACK
            dey
            jsr trackseek

stepperok:  ldy #DECTABOFFS
:           lda decodetab,y
            sta DECODETABLE,y
            iny
            bne :-

            jmp findtrackn

DECTABOFFS = $22
decodetab  = * & $ff00

decoffs0   = $0d
decoffs1   = $00
decoffs3   = $0f
decoffs5   = $04
decoffs6   = $04
decoffs7   = $77
decoffs8   = $a5

DECODETAB0 = decoffs0
DECODETAB1 = DECODETABLE + decoffs1
DECODETAB3 = DECODETABLE + decoffs3
DECODETAB5 = DECODETABLE + decoffs5
DECODETAB6 = DECODETABLE + decoffs6
DECODETAB7 = DECODETABLE + decoffs7
DECODETAB8 = DECODETABLE + decoffs8

            .assert .lobyte(*) >= DECTABOFFS, error, "***** 'decodetab' data is below in-page offset $22. *****"
            .assert .lobyte(*) <= DECTABOFFS, error, "***** 'decodetab' data is above in-page offset $22. *****"

; branch offsets for bitrateswt, used with SUPPORT_WOBBLY_DRIVES != 0
BR0 = $9d; readloop + 0
BR1 = $9f; readloop + 2
BR2 = $a1; readloop + 4
BR3 = $a4; readloop + 7
BZP = OPC_BIT_ZP; opcodes for chksumswit, used with SUPPORT_WOBBLY_DRIVES != 0
BRN = OPC_BNE
            .byte           $8f, $8b, $0f, $81, $87, $83, $9d, $87, $02, $07, $9e, $87, $06, $07
            .byte $96, $87, $03, $07, $70, $d0, $07, $0d, $99, $08, $a0, $10, $9a, $90, $20, $08
            .byte $92, $e0, $8e, $8a, $8c, $88, $8d, $89, $20, $97, $b7, $a7, $9c, $97, $b7, $a7
            .byte $94, $97, $b7, $a7, $0e, $60, $b0, $2d, $8f, $10, $80, $03, $98, $05, $70, $2c
            .byte $90, $80, $86, $82, $84, $80, $85, $24, $95, $17, $37, $27, $9b, $17, $37, $27
            .byte $93, $17, $37, $27, $c0, $30, $04, $29, $91, $80, $0d, $03, $97, $b0, $08, $28
            .byte $69, $68, $60, $0a, $6d, $6c, $64, $20, $02, $6a, $62, $10, $67, $6e, $66, $05
            .byte $61, $6b, $63, $b0, $65, $6f, $03, $b0, $30, $0d, $0f, $01, $80, $20, $0e, $2e ; $0796..$079e: 9 bytes used for DIRSECTORS
            .byte $d0, $0d, $b0, $0e, $d0, BR0, BZP, $26, $1d, $c7, $67, $47, $1e, $c7, $67, $47
            .byte $16, $c7, $67, $47, $c0, BR1, BZP, $23, $19, $a0, $0c, $01, $1a, $03, $09, $2a
            .byte $12, $06, $30, $0a, $b0, BR2, BZP, $22, $09, $d7, $f7, $e7, $1c, $d7, $f7, $e7
            .byte $14, $d7, $f7, $e7, $f0, BR3, BRN, $25, $0f, $60, $b0, $e0, $18, $08, $02, $2f
            .byte $10, $06, $90, $01, $0e, $d0, $60, $27, $15, $57, $77, $70, $1b, $57, $77, $f0
            .byte $13, $57, $77, $0d, $02, $e0, $06, $21, $11, $0c, $05, $60, $17, $30, $60, $2b

            ; * = $0200

.if SUPPORT_WOBBLY_DRIVES
            .byte $ff; see readloop, checksum eor #$ff in first loop
.endif
            ; getblock calls
            ; in: y: track
            ;     a: sector or negative value for any sector
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
            ; so a revolution takes about 1,000,000 * 60 / 300 = 200,000 cycles,
            ; so the timeout counter cannot be set to one revolution -
            ; it is reset upon waiting for every new sync,
            ; thus a timeout only indicates a sync-less track range
            ; (about 65536 / 200,000 * 19 = 6.23 sectors), but exclusively non-header
            ; syncs or missing sectors or similar will leave the loader spinning forever
findblkhdr: jsr waitsync; returns to caller of this routine upon time-out
            ; check if the sync is followed by a sector header
            bcs findblkhdr           ; 21 ; if not, wait for next sync mark
           ;ldx #$00
            stx bitrateswt           ; 24
toreaddata: bvc *                    ;  2
            txs                      ;  4 ; set stack pointer for readdata
.if SUPPORT_WOBBLY_DRIVES
            ldx _3E                  ;  7 ;  %00111110
            sax decode2 + 1          ; 10 ;   --22222-
            alr #%00000001           ; 12 ;   --------:3
            jmp readdata             ; 15 ; will jump to headerread
           ;lda VIA2_PRA             ; 22 ; 3:33334444 ; 2 - cycle 22 is -3 in [0..25]
           ;lda VIA2_PRA             ; 35 ;   45555566 ; 3 - cycle 35 is +3 in [32..51]
           ;lda VIA2_PRA             ; 68 ;   66677777 ; 4 - cycle 68 is +4 in [64..77]
           ;clv                      ; 70 ;            ;     cycle 70 is +6 in [64..77]
.else; !SUPPORT_WOBBLY_DRIVES
            nop                      ;  6
            ldx #%00111110           ;  8
            sax decode2 + 1          ; 11 ;   --22222-
            alr #%00000001           ; 13 ;   --------:3
            jmp readdata             ; 16 ; will jump to headerread
           ;lda VIA2_PRA             ; 23 ; 3:33334444 ; 2 - cycle 23 is -2 in [0..25]
           ;lda VIA2_PRA             ; 36 ;   45555566 ; 3 - cycle 36 is +4 in [32..51]
           ;adc DECODETAB5,x         ; 65 ;   455555-- -> $xx, clv - cycle 65 is +1 in [64..77]
           ;lda VIA2_PRA             ; 72 ;   66677777 ; 4 - cycle 72 is -3 in [64..77]
.endif; !SUPPORT_WOBBLY_DRIVES

headerread: ldx #stackend - stack; $04
            txs
            ; header: checksum sector track ID1 ID0
            eor (.lobyte(TOS - $04),x); STACK + $ff, sector
.if SUPPORT_WOBBLY_DRIVES
.else; !SUPPORT_WOBBLY_DRIVES
            eor STACK + $fe
.endif; !SUPPORT_WOBBLY_DRIVES
            tax                       ; checksum
            lda (.lobyte(STD - $ff),x); STACK + $fd, ID1
            inx                       ; checksum
            bne chksumerr; wait for next sector if sector header checksum was not ok

            ; z = 1
storputid0: cpy ID0; cpy ID0/sty ID0
            bne readerror; branch if the disk ID does not match
storputid1: cmp ID1; cmp ID1/sta ID1
            bne readerror; branch if the disk ID does not match

           ;ldx #$00
            lda (TOS,x)   ; STACK + $ff, sector
            jsr sertoraw
            cmp NUMSECTORS; skip if sector number is not within range of the allowed
            bcs findblkhdr; sector numbers for the current track

            sta LOADEDSECTOR
            rts; is changed to tax after init
sectorchsw: eor REQUESTEDSECTOR
            beq loadblock; branch if requested sector
            bpl findblkhdr; branch if specific sector requested, but not reached yet

            ; negative value: no specific sector requested, out-of-order sector fetch
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
            ror TRACKLINKTAB,x; mark linked block as known

loadblock:  ; wait for data block sync
            tsx
            pla; return address lo
            sta STACKBUFE - 2
            pla; return address hi
            sta STACKBUFE - 1
            txs
            jsr waitsync; returns to caller of this routine upon time-out
            bne toreaddata           ; 21 ; jmp, if not a data header, will fail on checksum

wsynctmout: pla; will return to caller
            pla; of blockload routine
            SKIPWORD
:           sta DISKCHANGED; set the new disk flag when disks have been changed
checkchg:   ; must not change y
readerror:  ; ID mismatch or illegal track or sector (invalid track/sector link)
            lax VIA2_PRB; check light sensor for disk removal
            eor DISKCHANGEBUFFER
            stx DISKCHANGEBUFFER
            and #WRITE_PROTECT
            bne :-
            ; read error: z = 1
chksumerr:  ; checksum mismatch: z = 0
            sec; operation not successful
            rts

dataread:   ; decode checksum
.if SUPPORT_WOBBLY_DRIVES
            eor $00,x                  ;     ;   --22222- -> $x0
.else; !SUPPORT_WOBBLY_DRIVES
            eor DECODETAB3 - 2,y       ;     ;   33333--- -> $0x, -2 for 5/GCR $a shifted to 2
.endif; !SUPPORT_WOBBLY_DRIVES
            tay                        ;     ;   ID0
            eor .lobyte(checksum + 1)
.if SUPPORT_WOBBLY_DRIVES
            eor STACK + 0
.endif; SUPPORT_WOBBLY_DRIVES
            tsx
            bne headerread
            eor (ST3,x); STACK + 3

.if SUPPORT_WOBBLY_DRIVES
            ; for the tightest bitrate %11, i.e., tracks 1-17 with 21 blocks each, a quarter of the checksumming effort is
            ; shifted to after reading and decoding a block, such that there are enough spare bytes for some picky drives with
            ; massive wobble, which do not tolerate 129 cycles for the read+decode+checksumming loop but want 127 or fewer
:           eor STACK + 4,x ; 4
            inx             ; 2
            inx             ; 2
            inx             ; 2
            inx             ; 2
chksumswit: bne :-          ; 3
                            ; = 15 * 64 = 960
.else; !SUPPORT_WOBBLY_DRIVES
            eor (ST2,x); STACK + 2
.endif; !SUPPORT_WOBBLY_DRIVES
            sta STACKBUF; checksum

            ; swap stack with loaded data
            ldx #stackend - stack; $04
            txs
:           ldy stack - 1,x
            lda STACKBUF - 1,x
            sta stack - 1,x
            sty STACKBUF - 1,x
            dex
            bne :-

            tay; checksum
.if SUPPORT_WOBBLY_DRIVES
.else; !SUPPORT_WOBBLY_DRIVES
            iny
.endif; !SUPPORT_WOBBLY_DRIVES
            bne chksumerr; branch on checksum mismatch

           ;ldy #$00
            jsr sertorawr
            sta LINKTRACK
            jsr sertorawd; sector link or block size
            sta LINKSECTOR
           ;clc

            ; block link sanity check
sanitychs0: lda #$02; is changed to cmp #$02 after init
            ldy LINKTRACK
sanitychs1: bcc success; is changed to beq lastblock after init
            cpy #MAXTRACK41 + 1; check whether track link is within the valid range
            bcs readerror; if not, return error
            jsr getnumscts; get number of sectors on linked track
            dex
            cpx LINKSECTOR; check whether sector link is within the valid range
lastblock:  bcc readerror; branch if sector number too large
                         ; lastblock: branch if invalid block size (0..1 = 1..2 bytes)
success:    clc             ; operation successful
           ;lda LINKSECTOR  ; return the loaded block's sector link sector number or block size
            ldx LOADEDSECTOR; return the loaded block's sector number
           ;ldy LINKTRACK   ; return the loader block's sector link track number
            rts

waitsync:   lax FE; $fe
            sta (.lobyte(TIM - $fe),x); TIMER, reset sync time-out
:           lda (.lobyte(TIM - $fe),x); TIMER
            beq wsynctmout
            lda (.lobyte(V2B - $fe),x); VIA2_PRB
            bmi :-         ; 7XX43210
.if SUPPORT_WOBBLY_DRIVES
            alr #%01100000 ; --XX----
            tay
            lda (DT0),y
            sta bitrateswt
            lda (DT1),y
:           clv
            lsr VIA2_PRA   ; reads $ff and then $52 (header) or $55 (data)
            inx
            bvc *          ;  2
            sta chksumswit ;  5
.else; !SUPPORT_WOBBLY_DRIVES
            lsr            ; -7XX4321
            lsr            ; --7XX432
            lsr            ; ---7XX43
            alr #%00001100 ; -----XX-
            adc #READLOOPBASE
            ldy stackend - 1; return address hi
:           lsr VIA2_PRA   ; reads $ff and then $52 (header) or $55 (data)
            inx
            clv
            bvc *          ;  2
            sta bitrateswt ;  5
.endif; !SUPPORT_WOBBLY_DRIVES
            bne :-         ;  7
            lda (V2A,x)    ; 11 ; 11222223, VIA2_PRA 
            clv            ; 13
            rts            ; 19

trackseek:  lda #MOTOR; turn on the motor
            jsr busyledon + $02
            ldx #$80 | (MINSTEPSPEED + 1)
trackstep:  tya; destination track
            cmp #MAXTRACK41 + 1
            bcs notrkstep; don't do anything if invalid track
            sec
            sbc CURTRACK
            beq setbitrate; branch if on same track
            sty CURTRACK
            ldy #$00; move up (inwards)
            sty CURRSTEPSPEEDLOW
            bcs :+
            eor #$ff; invert track difference
            adc #$01
            ldy #$02; move down (outwards)
:           sty TRACKINC
            asl; half-tracks
            tay
halftrack:  stx TIMER; reset track-step timer
            sec
            lda VIA2_PRB
            and #.lobyte(~(SYNC_MARK | MOTOR))
            adc TRACKINC
            ora #MOTOR
            sta VIA2_PRB
            dey           ; do not wait after last half-track step,
            beq setbitrate; let checksumming and sanity checks take care of the rest
            txa
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
            bmi halftrack; jmp

            ; bit-rates:
            ; tracks 31+   (17 blocks): %00 - sector interleave 3 (lowest density, slowest clock, innermost tracks)
            ; tracks 25-30 (18 blocks): %01 - sector interleave 3
            ; tracks 18-24 (19 blocks): %10 - sector interleave 3
            ; tracks  1-17 (21 blocks): %11 - sector interleave 4 (highest density, fastest clock, outermost tracks)
setbitrate: lda VIA2_PRB
            ora #SYNC_MARK | BITRATE_MASK; $e0
            ldy CURTRACK
            jsr getnumscts
putbitrate: .byte OPC_RTS, .lobyte(VIA2_PRB), .hibyte(VIA2_PRB); is changed to sta VIA2_PRB after init
            stx NUMSECTORS
           ;rts; fall through

getnumscts: ldx #21; number of blocks
            cpy #18
            bcc notrkstep; bit-rate $60
            dex
            dex; 19
            cpy #25
            bcc :+ ; bit-rate $40
            dex; 18
            and #.lobyte(~(%10 << BITRATE_SHIFT)); -$40
            cpy #31
            bcc notrkstep; bit-rate $20
            dex; 17
:           and #.lobyte(~(%01 << BITRATE_SHIFT)); -$20
notrkstep:  rts

busyledon:  lda #SYNC_MARK | BUSY_LED
            ora VIA2_PRB
            bne store_via2; jmp

            ; returns with x = 0
chkchgfade: jsr checkchg
fadeled:    tya
            tax
            beq fadedone
:           inx
            bne :-
            tax
            jsr busyledon
           ;lda VIA2_PRB
:           dex
            bne :-
            dey
            bne :+
            and #.lobyte(~MOTOR)   ; turn off motor
:           and #.lobyte(~BUSY_LED); turn off busy LED
store_via2: sta VIA2_PRB
fadedone:   rts

            ; the raw <-> serial mapping swaps bits 0 and 3 of both nibbles and
            ; inverts the result, so it is same for both encoding and decoding
sertorawd:  dey
sertorawr:  lda (STP),y; STACK
sertoraw:
rawtoser:   sta DECODETEMP
            lsr
            lsr
            alr #%00100010
            eor DECODETEMP; bit 3 ^ bit 0
            and #%00010001; if result is 0: both are equal (no swap required)
           ;clc
            adc #%01110111; 0 -> 7, 1 -> 8
            ora #%01100110
            eor #%10001000; 0 -> f, 1 -> 0
            eor DECODETEMP
genspcdone: rts

            FNAMEHASH 1541

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

duninstall:;ldx #$00
:           lda customgbyt,x
            sta $01,x; $00 = CURTRACK
            inx
            bpl :-
            inx; $81, ldx #OPC_STA_ZPXI
            stx .lobyte(getbyterts - customgbyt + $01)

            ; no watchdog, ATN_IN is cleared
V1B2 = $04; second instruction at customgbyt = $01 is ldy VIA1_PRB
            lda #CLK_OUT
            sta (.lobyte(V1B2 - $81),x); VIA1_PRB, clear ATNA_OUT to check DATA_IN with ATN_IN clear, set CLK_OUT to signal ready for code upload

:           txa; $81, #ATN_IN | DATA_IN
            and (.lobyte(V1B2 - $81),x); VIA1_PRB
            beq noupload; branch if ATN_IN and DATA_IN clear
            bpl :-; wait for ATN_IN high

            ; get custom drive code
            ldx #$05
:           jsr dgetbyte
            sta .lobyte(customparm - customgbyt + $01),x
            dex
            bpl :-
            sei; disable watchdog
           ;ldx #$ff
            txs
            jmp $01

noupload:   tya
            beq :+
handlewdog: ; watchdog handler: unknown if LED on, off, or fading: fade off LED regardless
            jsr busyledon
            lda #$ff; fade off the busy LED
:           pha
            ldy #18; ROM dir track
            jsr trackseek; ignore error (should not occur)
            pla
            tay
uninstfade: jsr fadeled
            bne uninstfade
            jmp (RESET_VECTOR)

statussent: lda BLOCKINDEX_STATUS
            bpl skipprpnxt; only after successful load
            PREPARE_NEXT_FILE 1541

skipprpnxt: ldy #$01; turn motor and busy LED off
            lax ATA; $10, (BUSY_LED = $08) << 1 
            sta (.lobyte(V1B - $10),x); VIA1_PRB
            lsr
            and (.lobyte(V2B - $10),x); VIA2_PRB
            beq :+; branch if busy LED is not lit
            ldy #$ff; fade off busy LED, then turn motor off
:
            ; upon first load, this is skipped,
            ; and loadfile is executed directly
driveidle:  jsr chkchgfade; check light sensor for disk removal, fade off the busy LED
           ;ldx #$00
            lda (V1B,x); VIA1_PRB
devicenum:  eor #ATN_IN | ATNA_OUT | CLK_IN | DATA_IN
            beq driveidle; wait until there is something to do
            asl; check for reset, uninstallation or custom drive code upload
            bne duninstall

            tya; LED fade counter
            beq loadfile ; check whether the busy LED has been completely faded off
            jsr busyledon ; if not, turn it on

loadfile:   GET_FILENAME 1541

            ; matches against hash of filename in FILENAMEHASH0/1
            FIND_FILE 1541; sets y to file track and a to file sector
           ;sta FILESECTOR; file's starting sector
            lda #diskio::status::FILE_NOT_FOUND
            bcc filenotfnd
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
            bpl trackinit; sets MAXINDEXONTRACK = INDEXTAB - 1 to a < 0 with x = 0

            ldx FILESECTOR
            stx MAXBLOCKINDEXPOS
            stx NEXTCONTIGUOUSBLOCK
            ldy #$00; initial block index
            sty MAXCONFIRMEDBLOCKINDEX
            sty NUMCONTIGUOUSBLOCKS
            lsr INDEXTAB,x; $40 = BLOCKPENDING | $00
            jsr speculate; build initial speculative block index table for this track

blockloop:  sec; any sector, compare ID, block link sanity check
            ror REQUESTEDSECTOR
            jsr getblkscan
            bcs blockerror; branch if block fetch not successful

           ;lda LINKSECTOR
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
nosetspcil:
            ; first file block must be loaded first
            lda CURRBLOCKINDEX
            bmi idxinvalid; branch if block conceivably not belonging to file
            beq :+
            ldy PREVBLOCKINDEX
            iny
            beq idxinvalid

:           ldx MAXBLOCKINDEXPOS
            ldy MAXCONFIRMEDBLOCKINDEX
            clc; clear mis-speculation detected flag
            bcc linkindex; jmp

blockerror: bne blockloop; branch if checksum error
            ldx LOADEDSECTOR; load error, ID mismatch or illegal track or sector (invalid track/sector link)
idxinvalid: lda #INDEXSPECULATIVE | $1f; out-of-range index, not pending
            sta INDEXTAB,x
            sec

blocksent:  bcs blockloop

           ;clc
           ;lda MAXINDEXONTRACK; actually maximum index on this track plus 1
            adc BLOCKINDEXBASE

            ldy FILETRACK
            bne trackloop

            ; loading is finished

            tya; $00 = diskio::status::OK
filenotfnd: ldy #$ff; send over one byte
            sty PREVBLOCKINDEX
            sty MAXINDEXONTRACK; status flag
            jmp sendstatus

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
            bit SPC; INDEXSPECULATIVE
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
            cmp CURTRACK
            ldy CURTRACK
            bcc :+
            iny
            SKIPBYTE
:           dey
            ldx #$80 | SINGLESTEPSPEED
            jsr trackstep

nostep:     sec
            lax BLOCKINDEXBASE
            adc NUMCONTIGUOUSBLOCKS; actually NUMCONTIGUOUSBLOCKS + 1
            ldy #$00
            cpy LINKTRACK
            bne :+; branch if not file's last block
            lda LINKSECTOR; the file's last block's length (last byte index)
            eor #$ff
            tay; blocksize
           ;sec
            adc #$01
:           sty blocksize
            jsr rawtoser
            sta STACK + $ff; block size

            ; calculate block index in host memory
           ;clc
            txa; BLOCKINDEXBASE
            adc CURRBLOCKINDEX
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
sendstatus: jsr rawtoser
            sta BLOCKINDEX_STATUS

            ; restore block buffer on stack
            ldx #stackend - stack; $04
:           lda STACKBUF - 1,x
            sta stack - 1,x
            dex
            bne :-

            tya
            eor #$ff
            tay

            ; send block ready signal and wait for the signal to begin transferring the block
            lax EF; .lobyte(~ATNA_OUT) = $ef
            sta (.lobyte(TIM - $ef),x); TIMER, poll two coupled timers: an extra-long time-out period is needed since
            sta TIMER2 ; the host computer may still be busy decompressing a large chunk of data
            lda #ATNA_OUT | CLK_OUT; drop DATA_OUT, set CLK_OUT high as signal of presence
            sta (.lobyte(V1B - $ef),x); VIA1_PRB, block ready signal
waitready:  lda VIA2_T1C_L ; see if the watchdog barked
            cmp VIA1_T1C_L
.if DISABLE_WATCHDOG
            bmi :+
:
.else
            bmi timeout    ; and trigger watchdog on time-out
.endif
            lda (.lobyte(V1B - $ef),x); VIA1_PRB
            bmi waitready; wait for ATN_IN low

            .assert .hibyte(* + 1) = .hibyte(waitready), error, "***** Page boundary crossing in block send wait loop, fatal cycle loss. *****"

            stx TIMER; reset watchdog time-out and clear possibly set IRQ flag
timeout:    ENABLE_WATCHDOG

            lda BLOCKINDEX_STATUS ;   v v
            sax VIA1_PRB  ; 4 -   465-0213
sendloop:   asl           ; 2 - 4:6570213-
            ora #ATNA_OUT ; 2 - 4:657H213-, next bit pair will be transferred with ATN high
                          ; = 18 (including 1 spare cycle)

:           bit VIA1_PRB  ; 4 - sync 1: wait for ATN high
            bpl :-        ; 2 -       v v
            sta VIA1_PRB  ; 4 - 4:657H213-
            rol           ; 2 - 6:57H213-4
            rol           ; 2 - 5:7H213-46
            rol           ; 2 - 7:H213-465
            rol           ; 2 - H:213-4657
                          ; = 18

:           bit VIA1_PRB  ; 4 - sync 2: wait for ATN low
            bmi :-        ; 2 -       v v
            sta VIA1_PRB  ; 4 -   213-4657
            asl           ; 2 -   13-4657-
            ora #ATNA_OUT ; 2 -   13-H657-, next bit pair will be transferred with ATN high
            stx TIMER     ; 4 - reset watchdog time-out
                          ; = 18

:           bit VIA1_PRB  ; 4 - sync 3: wait for ATN high
            bpl :-        ; 2 -       v v
            sta VIA1_PRB  ; 4 -   13-H657-
blocksize = * + 1
            lda STACK,y   ; 4 -   46570213
            cpy #$01      ; 2
            dey           ; 2
                          ; = 18

:           bit VIA1_PRB  ; 4 - sync 4: wait for ATN low
            bmi :-        ; 2 -       v v
            sax VIA1_PRB  ; 4 -   465-0213
            bcs sendloop  ; 3
                          ; = 72

            .assert .hibyte(* + 1) = .hibyte(sendloop), error, "***** Page boundary crossing in block send loop, fatal cycle loss. *****"

            lda #ATNA_OUT | DATA_OUT; drive busy
            sta (.lobyte(V1B - $ef),x); VIA1_PRB

:           lda (.lobyte(V1B - $ef),x); VIA1_PRB, wait for ATN_IN high,
            bpl :-      ; acknowledgement of the block transfer
            sei; disable watchdog

            ldx MAXINDEXONTRACK
            cpx NUMCONTIGUOUSBLOCKS
            inx
            beq :+
            txa
            jmp blocksent
:           jmp statussent

.if !DISABLE_WATCHDOG
CUSTOMOFFS = $2c; customparm - dgetbyte
            .res $0701 - * - CUSTOMOFFS
.endif; !DISABLE_WATCHDOG

dgetbyte:   lda #.lobyte(~(ATNA_OUT | CLK_OUT | DATA_OUT)); $e5, ATNA_OUT, CLK_OUT and DATA_OUT low: drive is ready
            sta TIMER; reset watchdog time-out
            ENABLE_WATCHDOG
            sta VIA1_PRB
customgbyt: lda #%10000000
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
.if DISABLE_WATCHDOG
            .byte OPC_JMP_ABS; execute custom drive code
customparm:
.endif; DISABLE_WATCHDOG

.if DISABLE_WATCHDOG
            .assert * <= $0704, error, "***** 1541 resident code too large. *****"
.else; !DISABLE_WATCHDOG
            .assert * >= $0700, error, "***** 1541 watchdog handler below $0700. *****"
            .assert * <= $0700, error, "***** 1541 watchdog handler above $0700. *****"

customparm = * + 1
            jmp handlewdog
.endif; !DISABLE_WATCHDOG

            ; will be overwritten with TRACKLINKTAB ($0704..$0718) and DIRTRACKS ($0719..$0721)
toloadfile: lda #OPC_TAX
            sta sectorchsw - 1
            lda #OPC_CMP_IMM
            sta sanitychs0 + 0
            lda #OPC_BEQ
            sta sanitychs1 + 0
            lda #lastblock - sanitychs1 - 2
            sta sanitychs1 + 1
            lda #OPC_STA_ABS
            sta putbitrate
            jsr setbitrate
            jmp loadfile

            .assert * <= (DECODETABLE + DECTABOFFS), error, "***** 'toloadfile' code is too large. *****"

dcodinit41: ldx #.lobyte(stackend - $01); $06
            txs

           ;lda ROMOS_TRACK_DIFF
            sta TRACK_DIFF

            lda #T1_FREE_RUNNING | PA_LATCHING_ENABLE; watchdog IRQ: count phi2 pulses, 16-bit free-running,
                                                     ; enable port A latching to grab one GCR byte at a time
                                                     ; rather than letting the GCR bitstream scroll through
                                                     ; port A (applies to 1541 and Oceanic OC-118, but not
                                                     ; 1541-II)
            sta VIA2_ACR
            lda #READ_MODE | BYTE_SYNC_ENABLE
            sta VIA2_PCR

            ; before loading the first file, the current track number is
            ; retrieved by reading any block header on the disk -
            ; however, if the loader is uninstalled before loading anything,
            ; it needs to know the more or less correct current track number
            ; in order to seek to track 18 before reset
            lda ROMOS_HEADER_TRACK; $18
            sta CURTRACK; $00

            ; watchdog initialization
            lda #$00
            sta VIA2_T1L_L; write VIA2 timer 1 low-order latch
            lda #$f0
            sta VIA1_T1L_L; write VIA1 timer 1 low-order latch
            lda #IRQ_CLEAR_FLAGS | IRQ_ALL_FLAGS; $7f
            sta VIA1_IER; no IRQs from VIA 1
            sta VIA2_IER; no IRQs from VIA 2
            lda #IRQ_SET_FLAGS | IRQ_TIMER_1
            sta VIA2_IER; timer 1 IRQs from VIA 2

            lda #ATNA_OUT; signal idle to the host with ATN_IN low
            sta VIA1_PRB
:           lda VIA1_PRB
            bpl :-; no watchdog
            and #DEVICE_NUMBER
            ora getbatncmp + 1
            sta getbatncmp + 1
            and #DEVICE_NUMBER
            ora devicenum + 1
            sta devicenum + 1

            ldx #tables420e - tables420
:           lda tables420,x
            sta $01,x
            dex
            bpl :-

            ; fade off the busy LED if lit
            lda VIA2_PRB
            sta DISKCHANGEBUFFER; store light sensor state for disk removal detection
            and #BUSY_LED
            beq :+
            lda #$ff
:           tay
            ; before spinning up the motor and finding the current track,
            ; wait until a file is requested to be loaded at all
:           jsr fadeled
            lda VIA1_PRB
            and #ATN_IN | CLK_IN | DATA_IN
            eor #ATN_IN | CLK_IN | DATA_IN
            beq :-; wait until there is something to do
            asl; check for reset, uninstallation or custom drive code upload
            beq :+
           ;ldx #$00
            jmp duninstall
:           jmp dcodinitc

tables420:  .byte      ___, MNS, ___, $d0, $60, $90, $20, ___, $40, $f0, $00, $50, $30, $10, $f0; $00 is CURTRACK
            .byte ___, $01, $c3, ___, $d3, $a1, $53, WRP, ___, ___, $83, ___, $93, $c1, $13, ___
            .byte ___, $41, <T1, >T1, $f3, $e1, $73, ___, <V1, >V1, $23, ___, $b3, $91, $33, ___
            .byte ___, $11, $43, PBI, $e3, $81, $63, ___, <V2, >V2, $03, ___, $a3, $d1, <ST, >ST
tables420e:

drvcodeend41:

            ; following code is transferred using KERNAL routines, then it is
            ; run and gets the rest of the code

TRAMPOLINEOFFSET = $24; dgetrout - dinstall

            .org * - TRAMPOLINEOFFSET

            ; entry point

dinstall:   sei
            lda #VIA_ATN_IN_INPUT | VIA_DEVICE_NUMBER_INPUT | VIA_ATNA_OUT_OUTPUT | VIA_CLK_OUT_OUTPUT | VIA_CLK_IN_INPUT | VIA_DATA_OUT_OUTPUT | VIA_DATA_IN_INPUT
            sta VIA1_DDRB

            ; cannot set device number bits to output in order to mask them for reading,
            ; as this does not work on 1541U firmwares around version 2.6, so some self-modification is required
            lda VIA1_PRB
            and #DEVICE_NUMBER
            ora getbnoatncmp + 1
            sta getbnoatncmp + 1

            lda ROMOS_TRACK_DIFF
            sta trackdiff + 1

            lda #CLK_OUT
            sta VIA1_PRB

:           lda VIA1_PRB; wait for DATA IN high
            lsr
instalwait: bcc :-

            ldx #.lobyte(drvcodebeg41 - $01)
dgetrout:   inx

            .assert * >= drvcodeend41, error, "***** 1541 trampoline too low in memory. *****"

            bne :+
            inc dgetputhi
:           lda #%10000000; CLK_OUT and DATA_OUT low: drive is ready
            sta VIA1_PRB
            GETBYTE_COMMON getbnoatncmp, CLK_IN; there is no watchdog while installing
dgetputhi = * + $02
            sta a:.hibyte(drvcodebeg41 - $01) << 8,x
            cpx #.lobyte(drvcodeend41 - $01)
            bne dgetrout
            dec drvcodebeg41
            bne dgetrout

trackdiff:  lda #0
            jmp dcodinit41

            .assert * <= $0800, error, "***** 1541 drive code too large. *****"

drvprgend41:
            .reloc
