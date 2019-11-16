
.ifndef _DRIVECODE_COMMON_INC_
_DRIVECODE_COMMON_INC_ = 1

MIN_DEVICE_NO   =  8 ; these two settings define the device number range when scanning for devices in install.s,
MAX_DEVICE_NO   = 30 ; devices beyond this range will not be recognized or useable

; 1541/41-C/41-II/41U/70/71/71CR
MINSTEPSPEED    = $18 ; min. R/W head stepping speed
MAXSTEPSPEED    = $10 ; max. R/W head stepping speed
STEPPERACC      = $0c ; R/W head stepping acceleration, smaller is slower acceleration
SINGLESTEPSPEED = $0c ; time between two consecutive half-track steps for a single-track step

.macro ENABLE_WATCHDOG
    .if DISABLE_WATCHDOG
            sei
    .else
            cli
    .endif
.endmacro

.macro GETBYTE_COMMON comparelbl, compare_value
            ; must not clobber x
            ldy VIA1_PRB
:           cpy VIA1_PRB
            beq :-
            ldy VIA1_PRB
comparelbl: cpy #compare_value
            ror
            bcc :-

            .assert .hibyte(*) = .hibyte(:-), error, "***** Page boundary crossing in getbyte loop, fatal cycle loss. *****"
.endmacro

FILENAME_X_OFFSET = $100 - FILENAME_MAXLENGTH

.macro GET_FILENAME drivetype
            ldx #FILENAME_X_OFFSET
getfilenam: jsr dgetbyte; get filename
            beq :+
            inx
    .if (.not .xmatch (drivetype, 1581))
            sta .lobyte(FILENAME - FILENAME_X_OFFSET - 1),x
    .else; 1581
            sta filename - FILENAME_X_OFFSET - 1,x
    .endif; 1581
            bmi getfilenam
:
    .if (.not .xmatch (drivetype, 1581))
            lda #ATNA_OUT | DATA_OUT; drive busy
            sta VIA1_PRB
    .else
            lda #DATA_OUT; drive busy
            sta CIA_PRB
    .endif
            sei; disable watchdog
            cpx #FILENAME_X_OFFSET ; if zero-length filename,
            beq loadnextfile       ; use hash values of next file
            jsr gethashval
            sta FILENAMEHASH1
            stx FILENAMEHASH0
loadnextfile:
.endmacro; GET_FILENAME drivetype

; matches against hash of filename in FILENAMEHASH0/1,
; sets a to file track and y to file sector

.macro FIND_FILE drivetype
            lda #OPC_BIT_ABS
findfile:   sta prpnxtfjmp

    .if (.not .xmatch (drivetype, 1581))

chknewdisk: lda #$00; BAM block number
            sta CYCLESTARTENDSECTOR
            ldx DISKCHANGED
            beq samedisk

            ; a new disk has been inserted
           ;lda #$00
            sta DIRBLOCKPOS
            ldx #OPC_STA_ZP
            jsr getblkstid; store ID, sector link sanity check
            bcs chknewdisk
           ;lda LINKSECTOR
           ;ldx LOADEDSECTOR; $00
           ;ldy LINKTRACK

    .else; 1581

            lda NUMFILES
            beq newdisk
diskchangd: lda #$00
            bne newdisk
            jmp samedisk

newdisk:    ; a new disk has been inserted
            jsr getdirtrk
            ldy #$00
            sty diskchangd + 1
            sty CYCLESTARTENDTRACK
            sty CYCLESTARTENDSECTOR
            jsr getblock
            bcs newdisk

           ;lda LINKTRACK
           ;ldx LOADEDSECTOR; $00
           ;ldy LINKSECTOR
            sty FIRSTDIRSECTOR

    .endif; 1581

            ; directory cycling: fill the directory buffer with the next file entries,
            ; this is also executed upon file not found in the currently buffered directory segment
            ; (with accu containing NEXTDIRBLOCKSECTOR's value)
           ;ldx #0; LOADEDSECTOR
nextdirseg: stx NUMFILES
            bit CYCLESTARTENDSECTOR
    .if (.xmatch (drivetype, 1541))
            bpl :+ ; branch if cycle not complete
            clc
    .else; !1541
            bpl :+ ; branch if cycle not complete
            sec
    .endif; !1541
            jmp findfilerr
:
    .if (.not .xmatch (drivetype, 1581))
            SKIPWORD
dircycle:   sta CYCLESTARTENDSECTOR
    .else; 1581
dircycle:
    .endif; 1581

    .if (.not .xmatch (drivetype, 1581))
nextdirsct: ; a = sector
            jsr getblkchid; compare ID, sector link sanity check
            bcc :+
            beq chknewdisk; read error or ID mismatch
            lda LINKSECTOR; = REQUESTEDSECTOR
            bpl nextdirsct; checksum error, retry
:          ;lda LINKSECTOR
           ;ldx LOADEDSECTOR
           ;ldy LINKTRACK
    .else; 1581
nextdirsct: ; a = track
            ; y = sector
            sta CURRDIRBLOCKTRACK
            jsr getblock
            bcs newdisk; start over on error
           ;ldy LINKSECTOR
           ;lda LINKTRACK
    .endif; 1581

            stx CURRDIRBLOCKSECTOR
    .if (.not .xmatch (drivetype, 1581))
            dey
            bpl :+; branch if not wrapping to first dir block
            lda #1; wrap around to first dir sector
:           sta NEXTDIRBLOCKSECTOR
    .else; 1581
            bne :+; branch if not wrapping to first dir block
            jsr getdirtrk
            ldy FIRSTDIRSECTOR
:           sta NEXTDIRBLOCKTRACK
            sty NEXTDIRBLOCKSECTOR
    .endif; 1581

    .if (.xmatch (drivetype, 1541))
getdirloop: ldy DIRBLOCKPOS
            dey
            dey
            ldx NUMFILES
            jsr sertorawd; get file's start track
            sta DIRTRACKS,x
            beq notafile; skip non-files denoted by track 0
            jsr sertorawd; get file's start sector
            sta DIRSECTORS,x
            lda (BLP),y; number of file blocks (lobyte)
            jsr sertoraw
            sta NUMBLOCKS,x
    .elseif (.xmatch (drivetype, 1571))
getdirloop: ldy DIRBLOCKPOS
            ldx NUMFILES
            lda BLOCKBUFFER + 3 + TRACKOFFSET,y; get file's start track
            sta .lobyte(DIRTRACKS),x
            beq notafile; skip non-files denoted by track 0
            lda BLOCKBUFFER + 3 + SECTOROFFSET,y; get file's start sector
            sta .lobyte(DIRSECTORS),x
            lda BLOCKBUFFER + BLOCKSOFFSET,y
            sta .lobyte(NUMBLOCKS),x
    .else; 1581
            ldy #$00
getdirloop: ldx NUMFILES
            lda BLOCKBUFFER + 3 + TRACKOFFSET,y; get file's start track
            sta DIRTRACKS,x
            beq notafile; skip non-files denoted by track 0
            lda BLOCKBUFFER + 3 + SECTOROFFSET,y; get file's start sector
            sta DIRSECTORS,x
    .endif; 1581

            jsr fnamehash; does not clobber y
            pha
            txa
            inc NUMFILES
            ldx NUMFILES; x is clobbered in fnamehash
            sta FILENAMEHASHVAL0 - 1,x
            pla
            sta FILENAMEHASHVAL1 - 1,x

            ; advance to next file or quit loop
            cpx #DIRBUFFSIZE
            bcs dirbuffull

notafile:   tya
            and #%11100000; 8 entries per block, $20 bytes per entry
    .if (.xmatch (drivetype, 1541))
            sta DIRBLOCKPOS
            bne getdirloop; process all entries in a dir block
    .elseif (.xmatch (drivetype, 1571))
           ;clc
            adc #$20
            sta DIRBLOCKPOS
            bcc getdirloop; process all entries in a dir block
    .else
           ;clc
            adc #$20
            tay
            bcc getdirloop; process all entries in a dir block
    .endif
            ; process next dir block
    .if (.not .xmatch (drivetype, 1581))
            lda NEXTDIRBLOCKSECTOR
            ldy CYCLESTARTENDSECTOR
            beq dircycle; set CYCLESTARTENDSECTOR
            cmp CYCLESTARTENDSECTOR
    .else; 1581
            jsr getdirtrk
            ldy NEXTDIRBLOCKSECTOR
            ldx CYCLESTARTENDTRACK
            bne :+
            sta CYCLESTARTENDTRACK
            sty CYCLESTARTENDSECTOR
            beq dircycle
:           cmp CYCLESTARTENDTRACK
            bne nextdirsct
            cpy CYCLESTARTENDSECTOR
    .endif; 1581
            bne nextdirsct

            ; cycle complete
           ;sec
            ror CYCLESTARTENDSECTOR
    .if (.not .xmatch (drivetype, 1581))
        .if DIRBUFFSIZE < 16
            ; always fill a dir buffer that holds fewer than 2 dir blocks, as if the dir is 2 blocks big,
            ; the dir buffer may otherwise only hold parts of 2 blocks despite having more space
            bne nextdirsct; jmp
        .endif
    .endif
dirbuffull: lda CURRDIRBLOCKSECTOR
            sta NEXTDIRBLOCKSECTOR
    .if (.xmatch (drivetype, 1581))
            lda CURRDIRBLOCKTRACK
            sta NEXTDIRBLOCKTRACK
    .endif; 1581

            ; the disk was not changed, or the dir has just been read
samedisk:   ldx NUMFILES; must not be 0
            beq nextdirseg
findloop:
    .if (.not .xmatch (drivetype, 1581))
            lda NEXTDIRBLOCKSECTOR
    .else; !1581
            lda NEXTDIRBLOCKTRACK
            ldy NEXTDIRBLOCKSECTOR
    .endif
            dex ; skip the last file entry to keep it as an overflow entry for load-next and PREPARE_NEXT_FILE
            beq nextdirseg; if the dir buffer does not contain the file, cycle through the directory to find it

nextfile:   lda FILENAMEHASH0
            cmp FILENAMEHASHVAL0 - 1,x
            bne findloop
            lda FILENAMEHASH1
            eor FILENAMEHASHVAL1 - 1,x
            bne findloop

            ; file found

    .if (.xmatch (drivetype, 1541))
           ;lda #$00
            sta DISKCHANGED
            jsr busyledon
            lda NUMBLOCKS - 1,x
            sta NUMFILEBLOCKS
            lda DIRSECTORS - 1,x
            sta FILESECTOR
            ldy DIRTRACKS - 1,x
           ;sec
        .if 0; not enough space in RAM
            ; check for illegal track or sector
            beq findfilerr
            cpy #MAXTRACK41 + 1
            bcs findfilerr
            stx TEMP; file index
            jsr getnumscts
            dex
            cpx FILESECTOR
            ldx TEMP; file index
           ;sec if sector is valid
            SKIPBYTE
            clc
        .endif
findfilerr:
    .elseif (.xmatch (drivetype, 1571))
           ;lda #$00
            sta DISKCHANGED

            jsr busyledon

            ; check for illegal track or sector
            sec
            stx FILEINDEX
            ldy .lobyte(DIRTRACKS - 1),x
            beq findfilerr
            cpy MAXTRACK
            bcs findfilerr
            jsr getnumscts
            stx NUMSECTORSTEMP
            ldx FILEINDEX
            lda .lobyte(NUMBLOCKS),x
            sta NUMFILEBLOCKS
            ldy .lobyte(DIRTRACKS - 1),x
            lda .lobyte(DIRSECTORS - 1),x
            bmi :+; branch if loop file
            cmp NUMSECTORSTEMP
            bcs findfilerr
            cmp #0; clear N flag
:           clc
findfilerr:
    .else; 1581
           ;lda #$00
            sta CYCLESTARTENDTRACK
            sta CYCLESTARTENDSECTOR

            lda DIRTRACKS - 1,x
            ldy DIRSECTORS - 1,x
            clc
findfilerr:
    .endif; 1581

prpnxtfjmp: jmp prepnxtfil
.endmacro; FIND_FILE drivetype

.macro PREPARE_NEXT_FILE drivetype
            lda #OPC_JMP_ABS
    .if (.xmatch (drivetype, 1541))
            bne findfile; jmp, jumps to prepnxtfil
    .else; !1541
            jmp findfile; jumps to prepnxtfil
    .endif; !1541
prepnxtfil: ; always succeeds, trackseek to track in y is valid
     .if (.xmatch (drivetype, 1571)) ; not enough space for loop file handling in 1541 RAM
            lda DIRSECTORS - 1,x
            bpl noloopfile
            dex; file index in [1..DIRBUFFSIZE] -> [0..DIRBUFFSIZE - 1]
            txa
            clc
            adc DIRSECTORS,x; negative offset to referenced file entry
            tax
            bpl :+; branch if not wrapping in dir buffer
            axs #.lobyte(-DIRBUFFSIZE)
:           tya; file track
            cmp DIRTRACKS,x
            bne :+; do not seek to referenced file, as it is not in the dir buffer
noloopfile: jsr trackseek
:
    .else
            jsr trackseek
    .endif
.endmacro; PREPARE_NEXT_FILE drivetype

.macro FNAMEHASH drivetype

            ; must not clobber y
fnamehash:  ldx #FILENAME_X_OFFSET
:
    .if (.xmatch (drivetype, 1541))
            jsr sertorawd
    .else; 1571 | 1581
            lda BLOCKBUFFER + 5,y
            iny
    .endif; 1571 | 1581
            cmp #' ' | $80; $a0 = end of filename
            beq gethashval
            inx
    .if (.not .xmatch (drivetype, 1581))
            sta .lobyte(FILENAME - FILENAME_X_OFFSET - 1),x
    .else; 1581
            sta filename - FILENAME_X_OFFSET - 1,x
    .endif; 1581
            bmi :-

            ; fall through

            ; must not change y
gethashval: stx TEMP
            txa
            clc
hashloop:
    .if (.not .xmatch (drivetype, 1581))
            adc .lobyte(FILENAME - FILENAME_X_OFFSET - 1),x
    .else; 1581
            adc filename - FILENAME_X_OFFSET - 1,x
    .endif; 1581
            adc #0
            pha
            adc TEMP
            adc #0
            sta TEMP
            pla
            dex
            cpx #FILENAME_X_OFFSET
            bne hashloop
            ldx TEMP
            rts

.endmacro; FNAMEHASH drivetype

.endif; !_DRIVECODE_COMMON_INC_
