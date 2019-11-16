
.include "standard.inc"

.include "loader.inc"

.if PLATFORM = diskio::platform::COMMODORE_16
    .include "ted.inc"
.else
    .include "vic.inc"
.endif


screen     = $5800
bitmap     = $6000

one_bits   = COLOUR_DARKGREY
zero_bits  = COLOUR_MEDIUMGREY


            MEMSET #bitmap, #BITMAP_SIZE, #BITMAP_BACKGROUND

.if PLATFORM = diskio::platform::COMMODORE_16
            lda #$00   ; disable all interrupts,
            sta TED_IMR; as KERNAL routines do cli
            lda TED_IRR; with LOAD_VIA_KERNAL_FALLBACK
            sta TED_IRR

            MEMSET #screen, #SCREEN_SIZE, #MAKE_HIRES_INTENSITIES(one_bits, zero_bits)
            MEMSET #screen + PAD(SCREEN_SIZE), #SCREEN_SIZE, #MAKE_HIRES_COLOURS(one_bits, zero_bits)
.else
            lda #CIA_CLR_INTF | EVERY_IRQ; disable KERNAL timer interrupts,
            sta CIA1_ICR                 ; as KERNAL routines do cli
            bit CIA1_ICR                 ; with LOAD_VIA_KERNAL_FALLBACK

            MEMSET #screen, #SCREEN_SIZE, #MAKE_HIRES_COLOURS(one_bits, zero_bits)
.endif

            DISPLAY_HIRES_BITMAP bitmap, screen

            lda #COLOUR_BLACK
            sta BORDERCOLOUR

            LOADER_INSTALL
            bcs error


side1file:  LOADRAW #<filename1, #>filename1; filename1 is only found on the first side
            bcc side2file; branch on success
            cmp #diskio::status::FILE_NOT_FOUND
            bne error

waitside1:  inc BORDERCOLOUR
            jmp side1file

side2file:  LOADRAW #<filename2, #>filename2; filename2 is only found on the second side
            bcc side1file; branch on success
            cmp #diskio::status::FILE_NOT_FOUND
            bne error

waitside2:  inc BORDERCOLOUR
            jmp side2file


error:      ldx #COLOUR_BLACK
:           sta BORDERCOLOUR
            stx BORDERCOLOUR
            jmp :-

filename1:  .asciiz "pic1"
filename2:  .asciiz "pic2"
