; ByteBoozer Decruncher    /HCL May.2003
; B2 Decruncher            December 2014
; with slight modifications by Krill

;Variables..        #Bytes
zp_base	= DECOMPVARS ; -
bits		= zp_base   ;1

put		= decdestlo

.macro SETDECOMPGETBYTE
	sta decompgetbyte + $01
	sty decompgetbyte + $02
	sta decompgetbyte1 + $01
	sty decompgetbyte1 + $02
	sta decompgetbyte2 + $01
	sty decompgetbyte2 + $02
.endmacro


.FEATURE labels_without_colons, leading_dot_in_identifiers

.macro	.GetNextBit
.local DgEnd
	asl bits
	bne DgEnd
	jsr GetNewBits
DgEnd
.endmacro

.macro	.GetLen
.local GlEnd
.local GlLoop
	lda #1
GlLoop
	.GetNextBit
	bcc GlEnd
	.GetNextBit
	rol
	bpl GlLoop
GlEnd
.endmacro

decompress:
Decrunch
	jsr GetNewBits
	tya
.if LOADCOMPD_TO
	clc
	adc loadaddroffslo
	php
.endif
storedadrl:
	sta put + 0
	jsr GetNewBits
	tya
.if LOADCOMPD_TO
	plp
	adc loadaddroffshi
.endif
storedadrh:
	sta put + 1
	lda #$80
	sta bits

DLoop
	.GetNextBit
	bcs Match
Literal
	; Literal run.. get length.
	.GetLen
	sta LLen+1

	ldy #0
LLoop
decompgetbyte2:
	jsr getcmem
L1	sta (put),y
	iny
LLen	cpy #0
	bne LLoop

	clc
	tya
	adc put
	sta put
	bcc *+4
	inc put+1

	iny
	beq DLoop

	; Has to continue with a match..

Match
	; Match.. get length.
	.GetLen
	sta MLen+1

	; Length 255 -> EOF
	cmp #$ff
	beq End

	; Get num bits
	cmp #2
	lda #0
	rol
	.GetNextBit
	rol
	.GetNextBit
	rol
	tay
	lda Tab,y
	beq M8

	; Get bits < 8
M_1	.GetNextBit
	rol
	bcs M_1
	bmi MShort
M8
	; Get byte
	eor #$ff
	tay
decompgetbyte1:
	jsr getcmem
	jmp Mdone
MShort
	ldy #$ff
Mdone
	;clc
	adc put
	sta MLda+1
	tya
	adc put+1
	sta MLda+2

	ldy #$ff
MLoop	iny
MLda	lda $beef,y
	sta (put),y
MLen	cpy #0
	bne MLoop

	;sec
	tya
	adc put
	sta put
	bcc *+4
	inc put+1

	jmp DLoop

End	rts

GetNewBits
	php
	pha
decompgetbyte:
	jsr getcmem
	tay
	pla
	plp
	sty bits
	rol bits
	rts

Tab
	; Short offsets
	.byte %11011111 ; 3
	.byte %11111011 ; 6
	.byte %00000000 ; 8
	.byte %10000000 ; 10
	; Long offsets
	.byte %11101111 ; 4
	.byte %11111101 ; 7
	.byte %10000000 ; 10
	.byte %11110000 ; 13
