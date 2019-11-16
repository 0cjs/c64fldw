
.fileopt comment, "Loader resident code portion"
.fileopt compiler, "CA65"
.fileopt author, "Gunnar Ruthenberg"

__NO_LOADER_SYMBOLS_IMPORT = 1
__NOIMPORTVARS = 1

.include "loader.inc"
.include "kernal.inc"

.include "cpu.inc"
.include "cia.inc"

.include "hal/hal.inc"


.segment "DISKIO_ZP" : zeropage

; all zeropage variables can be
; overwritten when the loader is idle

.macro alloc_zpvar symbol
symbol:       .res 1
    .exportzp symbol
.endmacro

.macro alloc_zpvar_2 symbol
symbol:       .res 2
    .exportzp symbol
.endmacro

loader_zp_first = *
.export loader_zp_first

alloc_zpvar loadaddrlo
alloc_zpvar loadaddrhi

.if LOADCOMPD_TO
    alloc_zpvar loadaddroffslo
    alloc_zpvar loadaddroffshi
.endif

.if HAVE_DECOMPRESSOR
    alloc_zpvar decdestlo
    alloc_zpvar decdesthi
.endif

.if END_ADDRESS_API
    alloc_zpvar endaddrlo
    alloc_zpvar endaddrhi
.endif

.if BYTESTREAM
    alloc_zpvar LOADXBUF
    alloc_zpvar LOADYBUF
    alloc_zpvar YPNTRBUF
    alloc_zpvar NEXTSTREAMBLKIDX
    alloc_zpvar STREAMBLKIDX
    .if GETCZPPOINTERS
    alloc_zpvar_2 GETCLOADADDRESS
    alloc_zpvar_2 GETCMEMADDRESS
    .endif
.endif

.if PLATFORM <> diskio::platform::COMMODORE_16
    alloc_zpvar GETBYTE_CLOCK_ATN_HI
.endif

alloc_zpvar BLOCKDESTLO
alloc_zpvar BLOCKINDEX; this one must be there after BLOCKDESTLO (used as pointer hibyte)
.assert BLOCKINDEX = BLOCKDESTLO + 1, error, "BLOCKINDEX != BLOCKDESTLO + 1"

.if (!HAVE_DECOMPRESSOR) & LOAD_VIA_KERNAL_FALLBACK
    alloc_zpvar_2 LOADDESTPTR
.endif

; decompressor
DECOMPVARS:
.exportzp DECOMPVARS

.if DECOMPRESSOR = DECOMPRESSORS::BITNAX
              .res 7
.elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER2
              .res 1
.elseif DECOMPRESSOR = DECOMPRESSORS::DOYNAX_LZ
              .res 3
.elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
              .res 8
.elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
              .res 2
.elseif DECOMPRESSOR = DECOMPRESSORS::NUCRUNCH
              .res 5
.elseif DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
              .res 3
.elseif DECOMPRESSOR = DECOMPRESSORS::SUBSIZER
              .res 8
.elseif DECOMPRESSOR = DECOMPRESSORS::TINYCRUNCH
              .res 7
.endif

.if HAVE_DECOMPRESSOR & LOAD_VIA_KERNAL_FALLBACK
    alloc_zpvar_2 LOADDESTPTR
    .exportzp LOADDESTPTR
.endif

loader_zp_last = * - 1
.export loader_zp_last

            CHECK_LOADER_ZP_ADDRESSES

.segment "DISKIO"


.if PLATFORM = diskio::platform::COMMODORE_16
KERNAL_FALLBACK_WAIT_FOR_BLOCK_READY_DELAY = $0c
.else
KERNAL_FALLBACK_WAIT_FOR_BLOCK_READY_DELAY = $07
.endif

.if GETCZPPOINTERS
getclodadr = GETCLOADADDRESS
getcmemadr = GETCMEMADDRESS
.endif

.ifdef RESIADDR
            .org RESIADDR - 2
            .word * + 2; load address
.endif

            CHECK_RESIDENT_START_ADDRESS

.if LOAD_RAW_API

.export loadraw

            ; --- load file without decompression ---
            ; in:  x/y - x: lo, y: hi to 0-terminated filename string,
            ;            zero-length file name will load next file
            ;      c - if LOAD_TO_API != 0, c = 0: load to address as stored in the file
            ;                               c = 1: load to caller-specified address (loadaddrlo/hi)
            ; out: c - set on error
            ;      a - status

            ; C-64: when LOAD_UNDER_D000_DFFF is non-0, this call assumes that the IO space at $d000 is enabled
loadraw:    jsr openfile
    .if LOAD_VIA_KERNAL_FALLBACK
            bcs openerror; only with kernal fallback because only then the call might fail
    .endif
:           jsr pollblock
            bcc :-
            cmp #diskio::status::OK + 1
    .if LOAD_VIA_KERNAL_FALLBACK
openerror:
    .endif
            rts

.endif; LOAD_RAW_API

.if LOAD_COMPD_API

.export loadcompd
    .if DISABLE_WATCHDOG
.export loadcompdopen
.export loadcompdexecute
    .endif; DISABLE_WATCHDOG

            ; --- load a compressed file ---
            ; in:  x/y - x: lo, y: hi to 0-terminated filename string,
            ;            zero-length file name will load next file
            ;      c - if LOAD_TO_API != 0, c = 0: load to address as stored in the file
            ;                               c = 1: load with caller-specified address offset (loadaddroffslo/hi)
            ; out: c - set on error
            ;      a - status

            ; C-64: when LOAD_UNDER_D000_DFFF is non-0, this call assumes that the IO space at $d000 is enabled
loadcompd:
    .if LOAD_TO_API
            bcs :+
            clc
            jsr openfile
            jmp compdfileopen
:
    .endif
            jsr loadcompdopen
compdfileopen:
    .if LOAD_VIA_KERNAL_FALLBACK
            ; only with kernal fallback because only then the call might fail
            bcc :+
            rts
:
    .endif
loadcompdexecute:
            ; throw exception on stream error
            tsx
            stx stackpntr + $01

    .if EXCEPTIONS & LOAD_RAW_API
            inc throwswtch + $01; throw exception on stream error
    .endif

    .if LOAD_VIA_KERNAL_FALLBACK
            BRANCH_IF_INSTALLED nodeploadf

            jsr decompress; calls getckernal, which sets memory configuration

            ; close the file
kernalwind: jsr getckernal
            bcc kernalwind
            bcs compdeof; jmp
nodeploadf:
    .endif; LOAD_VIA_KERNAL_FALLBACK

    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_ALL_RAM
    .endif

            jsr decompress

            ; decompression is finished

            ; handle special case that decompressing is as quick as loading,
            ; this call will fetch the loading finished status and ack loading,
            ; this also loads any remaining uncompressed blob when using Bitnax
:           lda getcmemadr + 1
            bne compdeof
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE_Y
        .endif
            jsr getnewblkx
            bcc :-

            ; loading and decompression is done
compdeof:   alr #diskio::status::OK; $00, clc = all ok

            ; fall through
maybethrow:
    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ldy memconfig + $01
            SET_MEMCONFIG_Y
    .endif

    .if LOAD_RAW_API
throwswtch: ldy #$00
            beq dontthrow
    .endif
            ; throw exception
stackpntr:  ldx #$00
            txs
dontthrow:
    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            bcs :+; skip on error
            ; return execution address in x/y
            ldx lo + $01
            ldy hi + $01
:
    .endif
            rts

.else; !LOAD_COMPD_API

    .if EXCEPTIONS
maybethrow: ldy LASTPC + $01
            beq :++
            stx :+ + $01
            ; throw exception
            ldx LASTSP
            txs
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ldx memconfig + $01
            SET_MEMCONFIG_X
        .endif
:           ldx #$00
:           rts
    .endif; EXCEPTIONS
.endif; LOAD_COMPD_API

.export openfile

openfile:
.if LOADCOMPD_TO
            lda #$00
            sta loadaddroffslo
            sta loadaddroffshi
.endif
.if LOAD_TO_API
            lda #OPC_LDA_ZP
            bcs :+
loadcompdopen:
            lda #OPC_STA_ZP
:           sta storeladrl
            sta storeladrh
.else
loadcompdopen:
.endif
            sty BLOCKINDEX; file ID parameter buffer

.if END_ADDRESS_API | BYTESTREAM
    .if HAVE_GETC
            lda #.lobyte(getcload)
            ldy #.hibyte(getcload)
            jsr puttoloadb
    .endif; HAVE_GETC

    .if HAVE_GETC & BYTESTREAM
            ldy #$ff
            sty YPNTRBUF
            iny
    .else
            ldy #$00
    .endif
    .if END_ADDRESS_API
            sty endaddrlo
            sty endaddrhi
    .endif
    .if BYTESTREAM
        .if EXCEPTIONS & LOAD_COMPD_API & LOAD_RAW_API
            sty throwswtch + $01; return errors to the caller
        .endif
            sty STREAMBLKIDX
            sty getcmemadr + 1
    .endif
.endif; END_ADDRESS_API | BYTESTREAM

.if MEM_DECOMP_TO_API
    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            lda #OPC_BIT_ZP
    .else
            lda #OPC_STA_ZP
    .endif
            sta storedadrl
            sta storedadrh
.endif

.if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            GET_MEMCONFIG
            sta memconfig + $01
            GOT_MEMCONFIG = 1
.else
            GOT_MEMCONFIG = 0
.endif

.if LOAD_VIA_KERNAL_FALLBACK

USE_WAIT_FOR_BLOCK_READY_KERNAL = 1

    .if !GOT_MEMCONFIG
            GET_MEMCONFIG
    .endif; !GOT_MEMCONFIG
            sta kernaloff + $01

            BRANCH_IF_NOT_INSTALLED ldrnotinst
            jmp nofallback

            ; loader is not installed,
            ; so load via KERNAL calls

ldrnotinst: ENABLE_KERNAL_SERIAL_ROUTINES

    .if BYTESTREAM
            lda #.lobyte(getckernal)
            ldy #.hibyte(getckernal)
            jsr puttoloadb
    .endif; BYTESTREAM
            stx namestrpos + $00

    .if USE_WAIT_FOR_BLOCK_READY_KERNAL
            ENABLE_WAITBUSY_KERNAL
    .endif

    .if BYTESTREAM
            ldy BLOCKINDEX; get buffered parameter
    .endif
            sty namestrpos + $01
            ldx #$ff
:           inx
namestrpos = * + $01
            lda a:$00,x
            bne :-
            txa
            pha; name length
            lda #KERNALFILENO
            ldx FA
            ldy #$00
            jsr SETLFS
            pla; name length
            ldx namestrpos + $00
            ldy namestrpos + $01
            jsr SETNAM
            jsr OPEN
            bcc fileopen
            tax
            lda #diskio::status::GENERIC_KERNAL_ERROR
            ldy kernaloff + $01
            SET_MEMCONFIG_Y
           ;sec; error
            rts

fileopen:   ldx #KERNALFILENO
            jsr CHKIN

            ; file not found is not detected at this point,
            ; but the kernalgbyt function will return an error
            ; when trying to get the first file data byte
            ; (i.e., after "getting" the load address),
            ; the busy led will keep flashing
    .if LOAD_TO_API
            lda #OPC_STA_ZP
            cmp storeladrl
            beq :+
            lda loadaddrlo
            sta LOADDESTPTR + $00
            lda loadaddrhi
            sta LOADDESTPTR + $01
            jsr CHRIN; skip load
            jsr CHRIN; address
            jmp kernopenok
:
    .endif
            jsr CHRIN
kernalstrl: sta LOADDESTPTR + $00
            sta loadaddrlo
            jsr CHRIN
kernalstrh: sta LOADDESTPTR + $01
            sta loadaddrhi
kernopenok:
            jmp retrnokclc

nofallback:

.endif; LOAD_VIA_KERNAL_FALLBACK

            WAKEUP

            WAIT_FOR_BLOCK_READY

            ; x contains the filename pointer lobyte parameter
            stx BLOCKDESTLO; pointer hibyte is already stored at BLOCKINDEX = BLOCKDESTLO + 1

    .if !(END_ADDRESS_API | BYTESTREAM)
            ldy #$00
    .endif
sendname:   lda (BLOCKDESTLO),y
            pha
            SENDBYTE_ATN
            GETBYTE_SETUP
            iny
            pla
            beq :+
            cpy #FILENAME_MAXLENGTH
            bne sendname
:
            ; no asynchronicity:
            ; the drive must be as quick or quicker than the host here,
            ; it must get the last data bit in time

            ; clear DATA OUT and CLK OUT so they can be polled later
            ; (when DATA IN = 0: drive busy,
            ;  when DATA IN = 1, CLK IN = 1: device not present)
            CLEAR

           ;ldx #$ff
            stx BLOCKINDEX

.if LOAD_VIA_KERNAL_FALLBACK
            ; check whether the loader is still installed
            ldx #KERNAL_FALLBACK_WAIT_FOR_BLOCK_READY_DELAY; some delay until the drive side is ready
:           dex
            bne :-
            SET_FLAGS_N_DATA_V_CLK
            bpl retrnokclc

            ; if not, try again with kernal routines
            SET_IO_KERNAL
            ldx BLOCKDESTLO + 0
            ldy BLOCKDESTLO + 1
            jmp ldrnotinst
.endif

.if HAVE_POLLBLOCK | LOAD_VIA_KERNAL_FALLBACK
retrnokclc: alr #diskio::status::OK; $00, clc = all ok
.else
retrnokclc: clc; actually only need the 2 cycles for SEND_BLOCK_SIGNAL on C-64
.endif

            ; no asynchronicity:
            ; the drive must be as quick or quicker than the computer here,
            ; it sets the busy flag which the computer polls after returning
.if PLATFORM <> diskio::platform::COMMODORE_16
polldone:
.endif
.if HAVE_POLLBLOCK = 0
pollfail:   rts

getblock:   sec; might branch to pollfail
.else; HAVE_POLLBLOCK
            rts

    .if LOAD_VIA_KERNAL_FALLBACK
kernlgberr: cmp #KERNAL_STATUS_EOF
            beq kernalbeof; carry is set on branch
            sec
            rts
    .endif

.export pollblock

pollblock:

    .if LOAD_VIA_KERNAL_FALLBACK

LOAD_UNDER_E000_FFFF = 1

            BRANCH_IF_INSTALLED getblnofbk

            ENABLE_KERNAL_SERIAL_ROUTINES

            ldx #$fe; $0100 bytes minus 2 bytes for track/sector link
kernalgblk: jsr kernalgbyt
            bcs kernlgberr

        .if LOAD_UNDER_E000_FFFF | (PLATFORM <> diskio::platform::COMMODORE_16)
            .if (!LOAD_UNDER_D000_DFFF) & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE_Y
            .else
            ENABLE_ALL_RAM_Y
            .endif
        .endif
            ldy #$00
            sta (LOADDESTPTR),y

        .if LOAD_UNDER_E000_FFFF | (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_KERNAL_SERIAL_ROUTINES_Y
        .endif

            inc LOADDESTPTR + $00
            bne :+
            inc LOADDESTPTR + $01
:           dex
            bne kernalgblk

            clc
kernalbeof: lda #diskio::status::OK
            rts
getblnofbk:

    .endif; LOAD_VIA_KERNAL_FALLBACK

            jsr getblock
            bcc retrnokclc

enteridle:  IDLE

pollfail:   sec
            rts

getblock:
.endif; HAVE_POLLBLOCK

            lda #diskio::status::DEVICE_NOT_PRESENT
            WAIT_FOR_BLOCK_READY
            SET_FLAGS_N_DATA_V_CLK; need a second read, as DATA and CLK might not switch at the same time
            bvs pollfail; branch if device not present

            SEND_BLOCK_SIGNAL; sets y

            lda #OPC_RTS; disable getblock store and loop
            jsr get1byte; get block index or error/eof code

.if END_ADDRESS_API | LOAD_UNDER_D000_DFFF | (PLATFORM = diskio::platform::COMMODORE_16)
           ;sec
            beq :+
            cmp #diskio::status::FILE_NOT_FOUND
            bcc :++
:           jmp polldone
:
.else
           ;sec
            beq :+
            cmp #diskio::status::FILE_NOT_FOUND
:           bcs polldone
.endif
            pha
            jsr getbyte; get block size
            tay
            pla
            eor #$80
            cmp #$80
            ror
            bcs :+; branch if file's last block
.if BYTESTREAM
            sty NEXTSTREAMBLKIDX
.endif
            ldy #$02; block size
:           adc BLOCKINDEX
            sta BLOCKINDEX
            clc
            bne calcaddr

            ; first block: get load address
            jsr getbyte; load address lo
.if LOADCOMPD_TO
            clc
            adc loadaddroffslo
            php
.endif; LOADCOMPD_TO
storeladrl: sta loadaddrlo; is changed to lda on load_to
            jsr getbyte; load address hi
.if LOADCOMPD_TO
            plp
            adc loadaddroffshi
            sec
.endif; LOADCOMPD_TO
storeladrh: sta loadaddrhi; is changed to lda on load_to
           ;sec

calcaddr:   ; calculate the position in memory according to the block number,
            ; this is performing: pos = loadaddr + BLOCKINDEX * 254 - 2
            sty BLOCKDESTLO
            php
            lda loadaddrlo
            sbc BLOCKINDEX
            pha
            lda loadaddrhi
            adc BLOCKINDEX
            tax
            pla
            plp
.if BYTESTREAM
            php
.endif; BYTESTREAM
            sbc BLOCKINDEX
            bcs :+
            dex
:           dex

            sec
            sbc BLOCKDESTLO
            sta storebyte + $01
            bcs :+
            dex
:           stx storebyte + $02
.if BYTESTREAM
            plp
            bcc :+
            ; first block
            sta getclodadr + 0
            stx getclodadr + 1
            sty YPNTRBUF
:
.endif; BYTESTREAM

            dey

            ; getblock loop/get1byte subroutine
.if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ; trading off speed for size: choose one of two getblock loops
            ; depending on the destination of the data to be downloaded
            lda storebyte + $02
            cmp #.hibyte($cf00)
            bcc :+
            cmp #.hibyte($e000)
            bcc getblockio
:
.endif; LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)

            lda #OPC_STA_ABSY; enable getblock store and loop
get1byte:   sta storebyte

.macro STORE
storebyte:  sta a:$00,y
.endmacro; STORE
            GETBYTE getbyte, STORE

.if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            beq gotblock; jmp

getblockio: lda storebyte + $01
            sta storebytio + $01
            lda storebyte + $02
            sta storebytio + $02

            GETBYTE getbyteio, STOREBYTE_ALLRAM
gotblock:
.endif; LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)

.if END_ADDRESS_API
            ; update end address
            ldx storebyte + $01
            cpx endaddrlo
            ldy storebyte + $02
            iny
            tya
            sbc endaddrhi
            bcc :+
            stx endaddrlo
            sty endaddrhi
:
.endif; END_ADDRESS_API

.if BYTESTREAM | END_ADDRESS_API
            clc; ok
.endif; BYTESTREAM | END_ADDRESS_API

.if PLATFORM = diskio::platform::COMMODORE_16
polldone:
.endif; PLATFORM = diskio::platform::COMMODORE_16
            ENDGETBLOCK; C-16: restore clock configuration
            rts

.if LOAD_VIA_KERNAL_FALLBACK
            ; get a byte from the file's byte-stream using the KERNAL API,
            ; sets memory configuration and buffers the y register
    .if BYTESTREAM
getckernal: sty LOADYBUF

            ENABLE_KERNAL_SERIAL_ROUTINES

            jsr kernalgbyt

        .if (!LOAD_UNDER_D000_DFFF) & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE_Y
        .else
            ENABLE_ALL_RAM_Y
        .endif

            bcc :+
            ldy kernaloff + $01; only on errors on subsequent getc calls, restore the previous
            SET_MEMCONFIG_Y    ; memory configuration which had been active before calling openfile
:
            ldy LOADYBUF
            rts
    .endif; BYTESTREAM

            ; get a byte from the file using the KERNAL API,
            ; the KERNAL ROM must be enabled
            ; in  : nothing
            ; out : a - status on error
            ;     : c - set on error
kernalgbyt: jsr READST; get KERNAL status byte
            bne kernalerr
    
    .if USE_WAIT_FOR_BLOCK_READY_KERNAL
            WAIT_FOR_BLOCK_READY_KERNAL
    .endif; USE_WAIT_FOR_BLOCK_READY_KERNAL
            jsr CHRIN
            clc
            rts

            ; EOF or error, close file
kernalerr:  pha; KERNAL status byte
            lda #KERNALFILENO
            jsr CLOSE
            jsr CLRCHN

    .if END_ADDRESS_API
            lda LOADDESTPTR + $00
            sta endaddrlo
            lda LOADDESTPTR + $01
            sta endaddrhi
    .endif; !END_ADDRESS_API

kernaloff:  lda #$00
            SET_MEMCONFIG
            pla; KERNAL status byte
.if PLATFORM = diskio::platform::COMMODORE_16
            sta STATUS; the CLRCHN call above sets STATUS ($90) to KERNAL_STATUS_ILLEGAL_TRACK_OR_SECTOR ($c0)
.endif
            cmp #KERNAL_STATUS_EOF; $40
            bne kernaloerr
           ;sec
            rts; EOF
kernaloerr: sec
            tax
            bpl :+; branch if not illegal track or sector, or device not present
            cmp #KERNAL_STATUS_ILLEGAL_TRACK_OR_SECTOR
            beq kernillts
            bne kerndevnp
:           and #KERNAL_STATUS_FILE_NOT_FOUND
            beq kerngenerc; branch if error not known, generic KERNAL error
    .if EXCEPTIONS
            lda #diskio::status::FILE_NOT_FOUND; this is also returned if the file starts on an illegal track or sector
            SKIPWORD
kernillts:  lda #diskio::status::ILLEGAL_TRACK_OR_SECTOR
            SKIPWORD
kerndevnp:  lda #diskio::status::DEVICE_NOT_PRESENT
            SKIPWORD
kerngenerc: lda #diskio::status::GENERIC_KERNAL_ERROR
            jmp maybethrow
    .else; !EXCEPTIONS
            lda #diskio::status::FILE_NOT_FOUND; this is also returned if the file starts on an illegal track or sector
            rts
kernillts:  lda #diskio::status::ILLEGAL_TRACK_OR_SECTOR
            rts
kerndevnp:  lda #diskio::status::DEVICE_NOT_PRESENT
            rts
kerngenerc: lda #diskio::status::GENERIC_KERNAL_ERROR
            rts
    .endif; !EXCEPTIONS

.endif; LOAD_VIA_KERNAL_FALLBACK

.if (BYTESTREAM | HAVE_DECOMPRESSOR) & HAVE_GETC
            ; get a byte from the file's byte-stream, read from memory

            ; C-64: when LOAD_UNDER_D000_DFFF is non-0, this call assumes that the
            ; IO space at $d000 is disabled if data is accessed at $d000..$dfff
getcmem:
getcmemadr = * + 1
            lda a:$00
            inc getcmemadr + 0
            beq :+
            rts; one extra byte for one cycle less
:           inc getcmemadr + 1
            rts
.endif; (BYTESTREAM | HAVE_DECOMPRESSOR) & HAVE_GETC

.if BYTESTREAM
            ; getcload: get a byte from the file's byte-stream, download a file block before if possible

            ; C-64: when LOAD_UNDER_D000_DFFF is non-0, this call assumes that the
            ; IO space at $d000 is disabled if data is accessed at $d000..$dfff

waitforblk:
    .if HAVE_GETC
            pla; current stream byte
    .endif
load1stblk: jsr getnewblk; LOADXBUF is set already
            bcs xbuffer; branch on error
    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE_Y
    .endif
            ldy STREAMBLKIDX
            bne chkloaded
            ; first block loaded, return to caller
            inc STREAMBLKIDX

    .if HAVE_GETC
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_ALL_RAM
        .endif
            SKIPWORD; return first file byte and maybe download another block before that
getcload:
            sty LOADYBUF

dogetcload: ldy YPNTRBUF

getclodadr = * + 1
            lda a:$00,y
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_IO_SPACE_Y
        .endif
            inc YPNTRBUF
            beq nxtstrmblk; branch to process next stream block
maybegtblk: BRANCH_IF_BLOCK_READY getnewblkx; download block as soon as possible
    .endif ; HAVE_GETC

loadbytret:
    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            ENABLE_ALL_RAM_Y
    .endif
    .if HAVE_GETC
            ldy LOADYBUF
    .endif
            rts

nxtstrmblk: ldy STREAMBLKIDX; block index and flag to skip waiting for the next block to download,
                            ; the value is increased for every loaded block and set to $ff after loading is finished
    .if HAVE_GETC
            stx LOADXBUF
    .endif
            beq load1stblk

chkloaded:
    .if HAVE_GETC
            pha; current stream byte
    .endif
            iny
            beq xbufclc; branch if last file block had been loaded already, clear carry: ok
            cpy NEXTSTREAMBLKIDX
            bcs waitforblk; branch if the next block in the stream is not yet loaded

            ; advance stream pointer
            ; this is not done after the first file block had been downloaded
           ;clc
            lda #$fe
            adc getclodadr + 0
            sta getclodadr + 0
            bcc :+
            inc getclodadr + 1
:           lda #$02
            sta YPNTRBUF
            inc STREAMBLKIDX
            bne xbufclc; jmp, clear carry: ok

.if BYTESTREAM & (DECOMPRESSOR = DECOMPRESSORS::BITNAX)
getnewblkz: lda #$00
            sta YPNTRBUF
.endif
getnewblkx: stx LOADXBUF
getnewblk:  
    .if HAVE_GETC
            pha; current stream byte
    .endif
            jsr getblock
            bcs gotstatus; branch if error or loading finished

xbufclc:    clc; ok, this is branched to
xbuf:
    .if HAVE_GETC
            pla; current stream byte
    .endif
xbuffer:    ldx LOADXBUF
    .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
            jmp loadbytret; restore memory configuration
    .else
        .if HAVE_GETC
            ldy LOADYBUF
        .endif
            rts
    .endif

            ; the status byte has been received, end loading
gotstatus:  ; switch to memory-read getc routine
    .if HAVE_GETC = 0
            ldx getclodadr + 0
            stx getcmemadr + 0; current stream buffer position lo
            ldx getclodadr + 1
            inx
            stx getcmemadr + 1; current stream buffer position hi
    .else
            pha; status
            ldx YPNTRBUF; YPNTRBUF = $00 -> add 256
            dex
            txa
           ;sec
            adc getclodadr + 0
            sta getcmemadr + 0; current stream buffer position lo
            lda #$00
            adc getclodadr + 1
            jsr setgetcmem
            pla; status
    .endif; !HAVE_GETC

            ldx #$ff
            stx STREAMBLKIDX; mark load finished

.if HAVE_POLLBLOCK
            jsr enteridle
.else; !HAVE_POLLBLOCK
            IDLE
.endif; !HAVE_POLLBLOCK

            tax; status
            ; carry is cleared upon return to signal ok
            beq xbufclc; clear carry: ok

            ; an error occured, stop loading and/or decompressing, return error to the caller,
            ; a = status
           ;sec
    .if EXCEPTIONS
            jmp maybethrow
    .else
            rts
    .endif
.endif; BYTESTREAM

.if MEM_DECOMP_API

.export memdecomp

            ; --- decompress a compressed file from memory ---
            ; in:  x/y - lo/hi of compressed file in memory
            ;      c   - if MEMDECOMP_TO_API != 0, c = 0: decompress to address as stored in the file
            ;                                      c = 1: decompress to caller-specified address (loadaddrlo/hi)

            ; out: undefined
memdecomp:  stx getcmemadr + 0
    .if BYTESTREAM
            tya
            jsr setgetcmem
    .else
            sty getcmemadr + 1
    .endif

    .if MEM_DECOMP_TO_API
        .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            lda #OPC_BIT_ZP
        .else
            lda #OPC_STA_ZP
        .endif
            bcc :+
            lda #OPC_LDA_ZP
:           sta storedadrl
            sta storedadrh
    .endif

    .if DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
            jsr decompress
            ; return the execution address in x/y
            ldx lo + $01
            ldy hi + $01
            rts
    .else
            jmp decompress
    .endif

.endif; MEM_DECOMP_API

.if UNINSTALL_API

.export uninstall

            ; --- uninstall the loader ---
            ; in:  nothing
            ; out: undefined
uninstall:  DO_UNINSTALL
            rts

.endif; UNINSTALL_API

.if HAVE_DECOMPRESSOR
    .if DECOMPRESSOR = DECOMPRESSORS::BITNAX
        .include "decompress/bitnaxdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::BYTEBOOZER2
        .include "decompress/b2decomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::DOYNAX_LZ
        .include "decompress/doynaxdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::EXOMIZER
        .include "decompress/exodecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::LEVELCRUSH
        .include "decompress/lcdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::NUCRUNCH
        .include "decompress/ncdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::PUCRUNCH
        .include "decompress/pudecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::SUBSIZER
        .include "decompress/subsizerdecomp.s"

    .elseif DECOMPRESSOR = DECOMPRESSORS::TINYCRUNCH
        .include "decompress/tcdecomp.s"

    .else
        .error "***** Error: The selected decompressor option is not implemented. *****"
    .endif
.endif; HAVE_DECOMPRESSOR

.if HAVE_GETC
setgetcmem: sta getcmemadr + 1
            lda #.lobyte(getcmem)
            ldy #.hibyte(getcmem)

            ; patch the various calls to the getchar routines,
            ; one out of five functions is used:
            ; getcmem    - get a char from memory after the whole file is loaded
            ; getcload   - get a char and before that, download a file block if possible/necessary
            ; getckernal - get a char when using the KERNAL API as fallback
puttoloadb:
    .if HAVE_DECOMPRESSOR
            SETDECOMPGETBYTE
    .endif
            rts
.endif; HAVE_GETC

            CHECK_RESIDENT_END_ADDRESS
