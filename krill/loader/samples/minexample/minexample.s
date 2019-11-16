
screen     = $5800
bitmap     = $6000

.if PLATFORM <> 16; diskio::platform::COMMODORE_16

.include "../../build/loadersymbols-c64.inc"

            .org $080d

            ; clear bitmap
            lda #>bitmap
            sta :+ + 2
            lda #$00
            tax
            ldy #>$2000
:           sta $ff00,x
            inx
            bne :-
            inc :- + 2
            dey
            bne :-

            lda #$7f ; disable KERNAL timer interrupts, as KERNAL
            sta $dc0d; routines do cli with LOAD_VIA_KERNAL_FALLBACK

            lda #$cb
            ldx #$00
:           sta screen + $00,x
            sta screen + $0100,x
            sta screen + $0200,x
            sta screen + $0300,x
            inx
            bne :-

            lda #$3b
            sta $d011
            lda #$68
            sta $d018
            lda #$02
            sta $dd00

            lda #$00
            sta $d020
            jmp load

error:      ldx #$00
:           sta $d020
            stx $d020
            jmp :-

load:       jsr install
            bcs error

loop:       ldx #<filename1
            ldy #>filename1
            jsr loadraw
            bcs error

            ldx #<filename2
            ldy #>filename2
            jsr loadraw
            bcs error

            jmp loop

filename1:  .asciiz "pic1"
filename2:  .asciiz ""; load next file after pic1

.res $1800 - *
.incbin "../../build/loader-c64.prg", 2

.res $2000 - *
.incbin "../../build/install-c64.prg", 2

.else ; PLATFORM = diskio::platform::COMMODORE_16

.include "../../build/loadersymbols-c16.inc"

.include "standard.inc"
.include "ted.inc"

one_bits   = COLOUR_DARKGREY
zero_bits  = COLOUR_MEDIUMGREY

            .org $100d

            MEMSET #bitmap, #BITMAP_SIZE, #BITMAP_BACKGROUND

            lda #$00   ; disable all interrupts,
            sta TED_IMR; as KERNAL routines do cli
            lda TED_IRR; with LOAD_VIA_KERNAL_FALLBACK
            sta TED_IRR

            MEMSET #screen, #SCREEN_SIZE, #MAKE_HIRES_INTENSITIES(one_bits, zero_bits)
            MEMSET #screen + PAD(SCREEN_SIZE), #SCREEN_SIZE, #MAKE_HIRES_COLOURS(one_bits, zero_bits)

            DISPLAY_HIRES_BITMAP bitmap, screen

            lda #COLOUR_BLACK
            sta BORDERCOLOUR
            jmp load

error:      ldx #COLOUR_BLACK
:           sta BORDERCOLOUR
            stx BORDERCOLOUR
            jmp :-

load:       jsr install
            bcs error

loop:       ldx #<filename1
            ldy #>filename1
            jsr loadraw
            bcs error

            ldx #<filename2
            ldy #>filename2
            jsr loadraw
            bcs error

            jmp loop
            
filename1:  .asciiz "pic1"
filename2:  .asciiz ""; load next file after pic1

.res $1800 - *
.incbin "../../build/loader-c16.prg", 2

.res $2000 - *
.incbin "../../build/install-c16.prg", 2

.endif ; PLATFORM = diskio::platform::COMMODORE_16
