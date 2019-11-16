
decompress = decrunch

.macro SETDECOMPGETBYTE
    .if BYTESTREAM
    sta decompgetbyte + $01
    sty decompgetbyte + $02
    .endif
.endmacro

TC_BLOCK_INTERFACE = 1

    .export decrunch
    .define sbx axs

.if MEM_DECOMP_TO_API
dp=decdestlo
.else
dp=DECOMPVARS + 0 ;4
.endif
sp=DECOMPVARS + 2 ;6  ; sp must follow dp, cf init code
cs=DECOMPVARS + 4 ;8

.macro MAYBEGETBLOCK

    .if BYTESTREAM
        .if LOAD_VIA_KERNAL_FALLBACK
    lda decompgetbyte + $01
    cmp #.lobyte(getckernal)
    beq nogetblock
        .endif; LOAD_VIA_KERNAL_FALLBACK

        .if HAVE_GETC
    lda getcmemadr + 1
    bne nogetblock
        .endif
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
    ENABLE_IO_SPACE_Y
    BRANCH_IF_BLOCK_NOT_READY :+
    jsr getnewblkx
:   ENABLE_ALL_RAM
        .else
    BRANCH_IF_BLOCK_NOT_READY :+
    jsr getnewblkx
:
        .endif
nogetblock:
    .endif
.endmacro

decrunch:
.ifndef TC_BLOCK_INTERFACE
    stx sp+1
.endif

    ldy#2
init_loop:
    sta sp-2,y   ;first iter stores sp-low :D
.ifdef TC_BLOCK_INTERFACE
    ; read three blocks ahead,
    ;  - one because literal strings read up to 128 bytes past sp
    ;  - two more to absorb up to 256 blocks worth of read 254 bytes/use 256 bytes
    tya
    pha
    jsr tc_getblock
    pla
    tay

    ; loading address is only known after fetching the first block
    ldx loadaddrlo
    stx sp + 0
    lda loadaddrhi
    sta sp + 1
.endif
    lda (sp),y
    dey
    bpl init_loop
    pha

.if MEM_DECOMP_TO_API
storedadrl = * + 1
storedadrh = * + 1
    lda #$00
    cmp #OPC_STA_ZP
    beq :+
    lda dp
    bne *+4
    dec dp+1
    dec dp
    jmp :++
:   lda sp-2
    sta dp
    lda sp-1
    sta dp+1
:
.endif

.if LOADCOMPD_TO
    clc
    lda loadaddroffslo
    adc dp
    sta dp
    lda loadaddroffshi
    adc dp+1
    sta dp+1
.endif

    lda#$02
    bne update_sp

literal_run:
literal_loop:
    iny
    lda(sp),y
    sta(dp),y
    dex
    bmi literal_loop

    tya
    pha
    clc
increase_dp_by_a_and_sp_by_tos_plus_one:
    adc dp
    sta dp
    bcc :+
    inc dp+1
:
    pla
update_sp:
    sec
    adc sp
    sta sp
    bcc :+
    inc sp+1
.ifdef TC_BLOCK_INTERFACE
    jsr tc_getblock
.endif
:
next_command:
    MAYBEGETBLOCK

    ldy#0
    lax(sp),y
    beq decrunch_done
    ; literal:   x = 128+length-1
    ; near copy: a = %11xxxxxx
    ; far copy:  a|0xf8 = >(~(offset-1)), x = 8*(length-2) | (some low bits)
    asl
    bcc far_copy
    bpl literal_run

near_copy:
    ldx#$07   ; clear high byte of -ve offset. Also ensures copy_loop doesn't loop.
    .byt $f0  ; beq (not taken) to skip over the iny
far_copy:
    iny
    ; carry is set for near_copy, clear for far_copy

    lda(sp),y ;fetch second byte (or for near copy, refetch first).  This is low 8 bits of offset.
    adc dp
    sta cs
    txa
    ora#$f8
    adc dp+1
    sta cs+1
    tya
    pha  ; save opcode length to stack
    ldy#1
    lda(cs),y
    sta(dp),y

copy_loop:
    iny
    lda(cs),y
    sta(dp),y
    txa   ; spend an extra 2 cycles per byte here to save 10 in the bitfield extraction. A win on average
    sbx#8
    bpl copy_loop
    tya
    bcc increase_dp_by_a_and_sp_by_tos_plus_one ; always taken.

decrunch_done:
    pla
    iny
    sta(dp),y
    rts

.ifdef TC_BLOCK_INTERFACE
tc_getblock:
    .if BYTESTREAM
        .if LOAD_VIA_KERNAL_FALLBACK
    lda decompgetbyte + $01
    cmp #.lobyte(getckernal)
    bne nogetbytekernal
            .if PLATFORM = diskio::platform::COMMODORE_16
    jsr pollblock
    ENABLE_ALL_RAM
    rts
            .elseif LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
    ENABLE_IO_SPACE
    jsr pollblock
    ENABLE_ALL_RAM
    rts
            .else
    jmp pollblock
            .endif
nogetbytekernal:
        .endif

        .if HAVE_GETC
    lda getcmemadr + 1
    beq :+
decompgetbyte:
    jmp getcmem
:
        .endif; HAVE_GETC
        .if LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
    ENABLE_IO_SPACE_Y; nxtstrmblk performs ENABLE_ALL_RAM before returning
        .endif; LOAD_UNDER_D000_DFFF & (PLATFORM <> diskio::platform::COMMODORE_16)
    jmp nxtstrmblk
    .else; !BYTESTREAM
    rts
    .endif; !BYTESTREAM
.endif
edecrunch:
