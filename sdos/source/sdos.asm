
        ;       SDOS (legacy) - use to be called as "!"

        ;       SDOS (v1.1) - C64 floppy speeder and disk utility
        ;       1986/2008/2016-2019 by Carroll/1570/Olessak
        ;       Public Domain (open-source and freeware)

        ;       VDOS is written by Edward Carroll (in 1986);
        ;       disassembled and modified for Jiffy protocol by 1570 (in 2008);
        ;       both sources are re-assembled by Robert Olessak (in 2016-2019).

        ;       SDOS is based on SJLOAD and VDOS. This is a common boot loader
        ;       that checks the drive and chooses among three possibilities:

        ;       1.) if JiffyDOS protocol is supported, it starts as SJLOAD;
        ;       2.) if not, it tries to start as VDOS (which is possible on
        ;           1541 and 1571 drives in C64 mode);
        ;       3.) if none of them are present, it stays at Kernal load.

        ;       Starting (in all cases):

        ;       LOAD"!*PROGRAM",8(,1) - (auto)start SDOS, fastload program
        ;       LOAD"!",8(,1) RUN - (auto)start only SDOS

        ;       (on 1581 the * does not work, so please type = instead)

        ;       Usage (after having been installed):

        ;       VERIFY - send commands, read floppy status
        ;       VERIFY"$" - display directory

        ;       LOAD"$" - load directory from last accessed device
        ;       LOAD"PROGRAM" - load program from last accessed device
        ;       LOAD"PROGRAM"(,8),1 - as above (as a machine code)
        ;       LOAD"PROGRAM"(,8),0 - as above (as a Basic code)

        ;       LOAD (without filename) => LOAD"?*",8(,1) (load first file)

        ;       The loader uses $02a7-$02ff and $cf00-$cfff as framework area
        ;       when active and $fc00-$ffff when hiding (under Kernal ROM).

        ;       The length of programs loaded to the Basic start can be about
        ;       200 blocks at most (from $0801 until $ceff) approximately.

        ;       New features:

        ;       if using LOAD"!*PROGRAM",8(,1) the program will also be autorun
        ;       holding SHIFT key skips the autorun
        ;       holding C= key forces always VDOS (also skips drive check)
        ;       holding CTRL key forces always SJLOAD (also skips drive check)
        ;       holding both C= and CTRL keys at once forces always Kernal load
        ;       screen and IRQ are disabled while loading (in all three modes)
        ;       typing 0 or 1 for device number means latest with ,0 or ,1
        ;       typing no device number at all means latest once again
        ;       number 8 is substitute for last accessed device
        ;       specifying no filename means load first file
        ;       (plus several further minor bugfixes)

        ;       at any time it can be re-installed by SYS737
        ;       at any time it can be un-installed by SYS700

        ;       memory swapping can be disabled by SYS727

        ;       (Normally the code resides in the "hidden" RAM under the Kernal
        ;       area, and is brought forth to $cc00 only during loading, then
        ;       afterwards swapped there back. So remains the $cc00-$cfff area
        ;       untouched and stays usable for any other purposes. You may
        ;       switch this feature off if you do not need it - to make things
        ;       faster, or to also use the "hidden" RAM.)

*=$0326

        ;       CHROUT and STOP vector <= Basic chaining + line number

        word    setup,$f6ed

        ;       63213 syspeek(44)*256+2248

        byte    $9e,$c2,$28,$34,$34,$29,$ac,$32,$35,$36,$aa,$32,$32,$34,$38,$00
        byte    $00,$00

        ;       (* = $033c) <- (* = $0817) <- Basic start

        ;       ----------------------------------------
        ;       SETUP init (entry point for autostart)
        ;       ----------------------------------------

setup   jsr     reset
        jsr     rwap

        ldy     #$00
        ldx     #$10

stp     lda     lcc03-$0c03,y
        sta     $c000,y
        iny
        bne     stp

        inc     stp+2
        inc     stp+5
        dex
        bne     stp

        tya
        stx     $2b
        sta     ($2b),y
        inc     $2b

        jsr     $ce6a
        jsr     drive

        jsr     $a644

        dec     $b7
        bmi     basc

        ldx     $2b
        ldy     $2c

        jsr     $ffd5
        bcs     basc

        stx     $2d
        sty     $2e

        jsr     $a533

atp     lda     #$00
        ldx     #$05
        ora     $028d
        and     #$01
        bne     basc

        stx     $c6

rtp     lda     run-1,x
        dex
        sta     $0277,x
        bne     rtp

        ;       basic warm start

basc    jmp     ($a002)

*=$0395

        ;       "run" (into keyboard buffer)

run     byte    $93,$52,$55,$4e,$0d

        ;       (* = $039a)

        ;       ----------------------------------------
        ;       SJLOAD ($cc99-$cf05) <- ($c996-$cc02)
        ;       ----------------------------------------

scc99   byte    $60
        sta     $b9
        jsr     $f3d5

scc9f   lda     $ba
        jsr     $ffb4
        lda     $b9
        jmp     $ff96

scca9   cmp     #$40
        ldx     $b7

        ;        (beq instead of jmp $cc6f)

        byte    $f0,$c0

        ;       send drive command

        lda     $ba
        bcc     sccc5

        jsr     $ffb1
        lda     #$6f
        jsr     $ff93

sccbb   lda     ($bb),y
        jsr     $ffa8
        iny
        cpy     $b7
        bcc     sccbb

sccc5   jsr     $ffae
        jmp     $cc8c

        ;       actual load routine (from below continued)
        ;       (checking if "$" and/or LOAD/VERIFY with or without filename)

scccb   lda     ($bb),y

        cmp     #$24
        sty     $d030
        bne     sccdd

        lda     $93
        bne     scd22
        sta     $b9

        jmp     ($cf73)

sccdd   ldx     $93
        bne     scca9

        ldx     $b7
        bne     sccef

        lda     #$02
        ldx     #$cd
        sta     $b7
        sta     $bb
        stx     $bc

        ;       turn off screen + IRQ (before SJLOAD call)

sccef   php
        sei
        pla
        ldy     $d011
        ldx     #$0b
        stx     $d011
        sta     $cf09
        sty     $cf10
        bne     scd25

        ;       "?*" (filename at $cd02)

scd02   byte    $3f,$2a,$00

        ;       load/verify

scd05   sta     $93

        lda     $ba
        cmp     #$08
        beq     scd15
        cmp     #$02
        bcs     scd19
        and     $b9
        sta     $b9

        ;       last accessed device

scd15   lda     $a8
        sta     $ba

scd19   ldy     #$00
        sty     $d015
        sta     $a8

        beq     scccb

scd22   jmp     $cc03

        ;       "searching for..."

scd25   jsr     $f5af

        lda     #$60
        ldx     $b9
        sta     $b9

        ;       raster synchronization (having the screen turned off)

sd1     bit     $d011
        bpl     sd1

sd2     bit     $d011
        bmi     sd2

        jsr     $f3d5

        ;       start address

        sei
        lda     $ba
        jsr     $ce07
        lda     $b9
        jsr     $ce90

        jsr     $cebb
        sta     $ae

        lda     $90
        lsr
        lsr
        bcs     lends

        jsr     $cebb
        sta     $af

        inc     $b9
        jsr     $cea0
        lda     $ba
        jsr     $ce07
        lda     $b9
        jsr     $ce90
        dec     $b9

        cpx     #$01
        ldx     #$00
        bcs     laff

        lda     $c3
        sta     $ae
        lda     $c4
        byte    $2c

laff    lda     $af

        ;       control data pointer ($cc00-$cfcf and $fc00-$ffcf swapped!)

lcontr  cmp     #$cc
laontr  sta     $af
        bcc     lxontr

        cmp     #$cf
        ora     #$30
        bcc     laontr

lxontr  txa
        bne     ltrbyt
        beq     lldbs2

        ;       file not found error

lends   jsr     $cf07
        jmp     $f704

        ;       load message

lldbs2  jsr     $f5d2

        ldy     #$00

        ;       start loading

ljlw    dex
        bne     ljlw

llnrlp  lda     #$03
        sta     $dd00

lwch1   bit     $dd00
        bvc     lwch1
        bmi     lpdrsg

ltrblk  bit     $dd00
        bpl     ltrblk

ltrbyt  sei
        lda     ($00,x)
        ldx     #$23
        lda     #$03
        stx     $dd00
        bit     $dd00
        bvc     llnrlp

        nop
        sta     $dd00
        lda     $dd00
        nop
        lsr
        lsr
        eor     $dd00
        bit     $00
        lsr
        lsr
        eor     $dd00
        bit     $00
        lsr
        lsr
        eor     $dd00
        eor     #$03
        sta     ($ae),y

        ;       increment data pointer

        inc     $ae
        bne     ltrbyt

        inc     $af
        bne     laff

        pla
        pla

lpdrsg  ldx     #$64

lwok1   bit     $dd00
        bvc     lend2
        dex
        bne     lwok1

        lda     #$42
        byte    $2c

lend2   lda     #$40
        jsr     $fe1c
        jsr     $cea0
        jsr     $f642
        bcs     lends

        ;       return from the load routine (through Kernal)

lend3   jsr     $cf07
        jmp     $f5aa

        ;       (* = $cdfe)

        lda     #$08
        ora     $dd00
        sta     $dd00
        rts

        ;       (* = $ce07) talk

ltalk   ora     #$40

        ;       (* = $ce09)

lsendb  sta     $95
        jsr     $ee97
        cmp     #$3f
        bne     lca1
        jsr     $ee85

lca1    jsr     $cdfe

        ;       (* = $ce18) send IEC byte

lwiecs  jsr     $ee8e
        jsr     $ee97
        jsr     $eeb3
        jsr     $eea9
        bcc     lcont1

        ;       device not found

        jsr     $cf07
        jmp     $edad

lcont1  jsr     $ee85

lcont4  jsr     $eea9
        bcc     lcont4

        jsr     $ee8e

        txa
        pha
        ldx     #$08

lsdbts  cmp     ($00,x)

        bit     $dd00
        bpl     ltbout

lcont5  jsr     $ee97

        ror     $95
        bcs     lci1

        jsr     $eea0

lci1    jsr     $ee85

        lda     $dd00
        and     #$df
        ora     #$10

        ;       send two bits

        sta     $dd00
        and     #$08
        beq     ltwbst

        lda     $95
        ror
        ror

        cpx     #$02
        bne     ltwbst

        ldx     #$1e

lwack1  bit     $dd00
        bpl     lwack2

        dex
        bne     lwack1
        beq     lcont6

lwack2  bit     $dd00
        bpl     lwack2

        ;       when we are here JD is present in floppy

lcont6  ldx     #$02

ltwbst  dex
        bne     lsdbts

        ldx     #$56

lcont7  dex
        beq     ltbout

        lda     $dd00
        bmi     lcont7

ltbok   pla
        tax

        rts

ltbout  pla
        tax

        ;       timeout

        jsr     $cf07
        jmp     $edb0

        ;       (* = $ce90) send secondary address

lsndsa  sta     $95
        jsr     $ce18

        lda     #$23
        sta     $dd00

lwca1   bit     $dd00
        bvs     lwca1

        rts

        ;       (* = $cea0) untalk

luntlk  jsr     $cdfe

        jsr     $ee8e
        lda     #$5f
        jsr     $ce09
        jsr     $edbe
        txa
        ldx     #$0a

ll2     dex
        bne     ll2

        tax
        jsr     $ee85
        jmp     $ee97

        ;       (* = $cebb) iecin (jumped from $ee13)

ll3     lda     $dd00
        cmp     #$40
        bcc     ll3

        sei
        nop
        cmp     ($00,x)
        cmp     ($00,x)
        lda     #$03
        nop
        nop
        sta     $dd00
        cmp     ($00,x)
        cmp     ($00,x)
        ora     $dd00
        lsr
        lsr
        nop
        ora     $dd00
        lsr
        lsr
        eor     #$03
        eor     $dd00
        lsr
        lsr
        eor     #$03
        nop
        eor     $dd00
        pha
        lda     #$23
        bit     $dd00
        sta     $dd00
        bvc     lend1
        bpl     lerr1

        pla
        lda     #$42
        jmp     $edb2

lerr1   lda     #$40
        jsr     $fe1c

lend1   pla
        clc

lcrts   rts

        ;       (* = $0607) <- (* = $cf06)

        ;       ----------------------------------------
        ;       VDOS ($cc03-$cf77)
        ;       ----------------------------------------

        ;       print directory

lcc03   jsr     $ce86
        jsr     $ce8d

        jsr     $cc54

lcc0c   stx     $a9
        jsr     $aad7

        jsr     $cc51

        jsr     $bdcd
        jsr     $ab3f

lcc1a   jsr     $cc58

        tax
        beq     lcc0c

        cmp     #$22
        bne     lcc26
        sta     $a9

lcc26   ldy     $d3
        lda     #$3a
        cpy     #$1c
        beq     lcc3e
        bcs     lcc1a

        cpy     #$18
        bcc     lcc4b
        bne     lcc47

        lda     #$2c
        cpx     #$20
        beq     lcc3e

        cpx     #$40
lcc3e   ldy     $a9
        beq     lcc1a
        bcc     lcc4b

        jsr     $ffd2

lcc47   cpx     #$3c
        bcc     lcc1a

lcc4b   txa
        jsr     $ffd2
        bcc     lcc1a

        ;       get serial byte (with error handling)

lcc51   jsr     $cc54
lcc54   jsr     $cc58
        tax

lcc58   jsr     $ffa5

        bit     $90
        bvs     lcc66

        bit     $91
        bmi     lcrts

lcc63   jsr     $aad7

lcc66   jsr     $ffab
        jsr     $f642

        jmp     $cc8c

        ;       read error channel

lcc6f   jsr     $aad7
        jsr     $ab3f
        lda     $ba
        jsr     $ffb4
        lda     #$6f
        jsr     $ff96

lcc7f   jsr     $ffa5
        jsr     $ffd2
        cmp     #$0d
        bne     lcc7f

        jsr     $ffab

lcc8c   lda     #$00
        ldx     #$fb
        sta     $90
        sta     $c6
        txs

        jmp     $02a7

        ;       (*+1 = $cc99)

lcc98   byte    $a9

        ;       control data pointer ($cb00-$cfcf and $fb00-$ffcf swapped!)

lcc99   bit     $ae
        bcc     lcc9f
        bmi     lcca3
lcc9f   ora     #$30
        sta     $af
lcca3   rts

        ;       transfer routine

lcca4   ldy     #$00

lcca6   iny
        bne     lcca6

        ldx     $ac

lccab   bit     $dd00
        bmi     lccab

        stx     $dd00

        ;       Raster synchronization (badline check):

        ;       Removing these four lines makes the overall loading on PAL
        ;       systems roughly about 10% faster (with screen disabled all the
        ;       time), however, the timing becomes unstable on NTSC ones.

lccb3   lda     $d012
        eor     #$02
        and     #$06
        beq     lccb3

lccbc   lda     $ad
        sta     $dd00
        pha
        pla
        pha
        pla
        nop
        lda     $dd00
        lsr
        lsr
        nop
        ora     $dd00
        and     #$f0
        sta     $cce5
        lda     $dd00
        lsr
        lsr
        nop
        ora     $dd00
        stx     $dd00
        lsr
        lsr
        lsr
        lsr
lcce4   ora     #$f0
        sta     $cb00,y
        iny
        bne     lccb3

        lda     $ad
        sta     $dd00

        rts

        ;       start address

lccf2   lda     $cb02
        ldy     $cb03

        ldx     $ab
        bne     lcd00

lccfc   lda     $c3
        ldy     $c4

lcd00   sta     $ae
        sty     $af

        rts

        ;       load/verify

lcd05   sta     $93

        lda     $ba
        cmp     #$08
        beq     lcd15
        cmp     #$02
        bcs     lcd19
        and     $b9
        sta     $b9

        ;       last accessed device

lcd15   lda     $a8
        sta     $ba

lcd19   ldy     #$00
        sty     $d015
        sta     $a8

        ;       (checking if "$" and/or LOAD/VERIFY with or without filename)

        lda     ($bb),y

        cmp     #$24
        bne     lcd32

        lda     $93
        bne     lcd2f
        sta     $b9

        jmp     ($cf73)

lcd2f   jmp     $cc03

lcd32   ldx     $93
        beq     lcd40

        ldx     $b7
        bne     lcd3d

        jmp     $cc6f

lcd3d   jmp     $ce46

lcd40   ldx     $b7
        bne     lcd47

        jsr     $ceb3

lcd47   jsr     $cec0

        ;       raster synchronization (having the screen turned off)

lcd4a   bit     $d011
        bpl     lcd4a

        ;       (*+1 = $cd50)

crck1   jsr     $ce86
        jsr     $ce8d

        jsr     $ffa5

        lda     $90
        lsr
        lsr
        bcc     lcd64

        ;       file not found error

        jsr     $ce1f
        jmp     $f704

        ;       load message

lcd64   jsr     $f5d2

        ldy     #$00

        ;       sending drive code to the drive

lcd69   jsr     $ce5d
        lda     #$57
        jsr     $ffa8
        tya
        jsr     $ffa8
        lda     #$06
        jsr     $ffa8
        lda     #$1e
        jsr     $ffa8
        ldx     #$1e

lcd81   lda     $ced8,y
        jsr     $ffa8
        iny
        dex
        bne     lcd81

        jsr     $ffae
        cpy     #$9a
        bcc     lcd69

        jsr     $ce5d
        lda     #$45
        jsr     $ffa8
        lda     #$00
        jsr     $ffa8
        lda     #$06
        jsr     $ffa8
        jsr     $ffae

        ;       start loading

        sei
        lda     $dd00
        and     #$03
        ora     #$04
        sta     $ad
        ora     #$10
        sta     $ac

        jsr     $cca4
        jsr     $ccf2

        ldx     #$04
        bne     lcdc4

lcdbf   jsr     $cca4

        ldx     #$02

lcdc4   lda     #$01
        sta     $d030

        jsr     $ce37

        ldy     #$00

        lda     $cb00
        beq     lcde3

        sec
        bmi     lcdf4

lcdd6   lda     $cb00,x
        sta     ($ae),y
        jsr     $ce31
        inx
        bne     lcdd6

        beq     lcdf3

lcde3   dex

lcde4   lda     $cb01,x
        sta     ($ae),y
        jsr     $ce31
        inx
        cpx     $cb01
        bcc     lcde4

        txa

lcdf3   clc

lcdf4   php

        ;       control data pointer ($cb00-$cfcf and $fb00-$ffcf swapped!)

        lda     $af

        cmp     #$fb
        bcc     lcdff

        and     #$cf
        sta     $af

lcdff   plp
        sty     $d030
        beq     lcdbf

        php
        lda     $ac
        ora     #$cc
        sta     $dd00
        jsr     $ce97
        lda     #$49
        jsr     $ffa8
        jsr     $ffa8
        plp

        ;       return from the load routine (through Kernal)

        jsr     $ce1f
        jmp     $f5aa

        ;       restore screen + IRQ (before Kernal call)

lce1f   pha

        ;       (*+1 = $ce21)

        lda     #$30
        and     #$34
        adc     #$00
        pha

        ;       (*+1 = $ce28)

        lda     #$1b
        and     #$7b
        sta     $d011
        plp
        pla

lce30   rts

        ;       increment data pointer

lce31   inc     $ae
        bne     lce37

        inc     $af

        ;       control data pointer ($cb00-$cfcf and $fb00-$ffcf swapped!)

lce37   lda     $af

        cmp     #$cb
        bcc     lce30
        cmp     #$cf
        bcs     lce30

        cmp     #$ce
        jmp     $cc99

lce46   cmp     #$40
        bcc     lce57

        ;       send drive command

lce4a   jsr     $ce97

lce4d   lda     ($bb),y
        jsr     $ffa8
        iny
        cpy     $b7
        bcc     lce4d

lce57   jsr     $ffae
        jmp     $cc8c

lce5d   jsr     $ce97
        lda     #$4d
        jsr     $ffa8
        lda     #$2d
        jmp     $ffa8

        ;       display message, install load vector, exit

lce6a   sei

lce6b   lda     #<$02e3
        ldy     #>$02e3
        sta     $0330
        sty     $0331

lce75   ldx     #$00
lce77   lda     #<$cea1
lce79   ldy     #>$cea1
        stx     $d020
        stx     $d021
        stx     $c6
        jmp     $ab1e

lce86   lda     #$60
        sta     $b9
        jsr     $f3d5

lce8d   lda     $ba
        jsr     $ffb4
        lda     $b9
        jmp     $ff96

lce97   lda     $ba
        jsr     $ffb1
        lda     #$6f
        jmp     $ff93

        ;       "sdos v1.1"

lcea1   byte    $99,$93,$0e,$53,$44,$4f,$53,$20,$56,$31,$2e,$31,$0d,$00

        byte    $00

        ;       "?*" (filename at $ceb0)

lceb0   byte    $3f,$2a,$00

lceb3   lda     #$02
        ldx     #$b0
        ldy     #$ce
        sta     $b7
        stx     $bb
        sty     $bc
        rts

        ;       turn off screen + IRQ (before VDOS call)

lcec0   php
        sei
        pla
        ldy     #$0b
        sta     $ce21
        lda     $d011
        sty     $d011
        sta     $ce28

        ;       "searching for..."

        ldx     $b9
        stx     $ab

        ;       (* = $ced5)

crck2   jmp     $f5af

        ;       (* = $ced8) drive code (in drive at $0600)

        ldy     #$00

l73a    tya
        and     #$0f
        tax
        lda     $068a,x
        sta     $0400,y
        tya
        lsr
        lsr
        lsr
        lsr
        tax
        lda     $068a,x
        sta     $0500,y
        iny
        bne     l73a

l753    lda     $18
        sta     $06
        lda     $19
        sta     $07

l75b    lda     #$b0
        sta     $00

l75f    lda     $00
        bmi     l75f

l763    lda     #$80
        sta     $00

l767    lda     $00
        bmi     l767

l76b    sei
        cmp     #$01
        beq     l775

l770    lda     #$ff
        sta     $0300

l775    lda     #$02
        sta     $1800
        asl

l77b    bit     $1800
        beq     l77b

l780    ldx     $0300,y

l783    bit     $1800
        bne     l783

l788    lda     $0500,x
        sta     $1800
        asl
        and     #$0f
        nop
        sta     $1800
        lda     $0400,x
        nop
        sta     $1800
        asl
        and     #$0f
        nop
        sta     $1800
        lda     #$04
        iny
        nop
        sta     $1800
        bne     l780

l7ac    cli
        lda     $0301
        sta     $07
        lda     $0300
        beq     l7c1

l7b7    bmi     l7c1

l7b9    cmp     $06
        sta     $06
        beq     l763

l7bf    bne     l75b

l7c1    rts

        byte    $0f,$07,$0d,$05,$0b,$03,$09,$01,$0e,$06,$0c,$04,$0a,$02,$08,$00

        ;       (* = $cf72)

crack   byte    $60
        word    $f4a5
        jmp     $ce1f

        ;       (* = $cf78)

        word    $cc98
        jsr     $cc9f

        ;       (* = $0981) <- (* = $cf7d)

        ;       ----------------------------------------
        ;       SJLOAD/VDOS helper ($ff7d-$ffcf)
        ;       ----------------------------------------

        ;       (* = $cf7d) self-modification (from VDOS to SJLOAD)

        ldy     #$04
vlp1    lda     $cf78,y
        sta     $cc04,y
        dey
        bpl     vlp1

        ldy     #$22
vlp2    lda     $ce1f,y
        sta     $cf07,y
        lda     $ce6a,y
        sta     $cf2a,y
        lda     $cea1,y
        sta     $cf4d,y
        dey
        bpl     vlp2

        ldy     #$cf
vlp3    lda     $c996-1,y
        sta     $cc99-1,y
        lda     $ca65-1,y
        sta     $cd68-1,y
        lda     $cb34-1,y
        sta     $ce37-1,y
        dey
        bne     vlp3

        ldx     #$2b
        lda     #$cf
        ldy     #$4d
        stx     $02d2
        sta     $02d3
        sty     $cf38
        sta     $cf3a

        ;       (* = $cfc8)

        inc     $cfd3
        inc     $cfd9

        ;       (* = $cfce)

        rts

        ;       (* = $cfcf) <- (* = $ffcf)

        rti

        ;       (* = $09d4) <- (* = $cfd0)

        ;       ----------------------------------------
        ;       SJLOAD/VDOS helper ($ffd0-$fff9)
        ;       ----------------------------------------

        ;       (* = $ffd0) swapping $cc00 and $fc00 (or $cb00 and $fb00)

swp     ldy     #$00

swc     lda     #$cb
        sty     $0c
        sta     $0d

swd     lda     #$fb
        sty     $0e
        sta     $0f

swy     lda     ($0c),y
        pha
        lda     ($0e),y
        sta     ($0c),y
        pla
        sta     ($0e),y

        iny
        bne     swync

        inc     $0d
        inc     $0f

swync   cpy     #$cf
        bne     swy

        lda     $0d
swynd   sbc     #$cf
        bcc     swy

        rts

        ;       (* = $fffa) <- (* = $cffa)

        word    $ffcf
        word    $ffcf
        word    $ffcf

        ;       (* = $0a04) <- (* = $d000)

        ;       ----------------------------------------
        ;       SJLOAD/VDOS helper ($02a7-$02ff)
        ;       ----------------------------------------

        ;       (jump to Basic after VERIFY)

s02a7   jsr     $02eb
        jmp     $a474

        ;       (swapping forth)

s02ad   jsr     $02eb
        php
        pha
        tya
        pha
        ldy     #$59
        cpy     $cf72
        beq     s02c2

        byte    $24

        ;       sys700 (jsr $02bc): un-install (by restoring Kernal)

s02bc   jmp     $fd15

        jsr     $02eb

        ;       It works around a Kernal bug: function $ff8a ($fd15) destroys
        ;       32 bytes at $fd30-$fd4f that need to be restored:

s02c2   lda     $cf75,y
        sta     $cd00,y
        dey
        bpl     s02c2
        bmi     s02fb

        ;       sys717 <- sys737 (re-install)

s02cd   clc
s02ce   jsr     $02ad

s02d1   jsr     $ce6b
        clc
        bcc     s02eb

        ;       sys727 (jsr $02d7): re-install with swapping disabled
        ;       (speed loader code will stay for ever fixed at $cc00)

s02d7   jsr     $02ad

s02da   lda     #$60
        sta     $02eb
        bne     s02d1

        ;       sys737 (jsr $02e1): re-install

s02e1   clc
        bcc     s02ce

        ;       (another NOP at $02e3 hidden in the operand!)

s02e4   nop

s02e5   jsr     $02ad

s02e8   jsr     $cd05

        ;       (swapping back)

s02eb   php
        sei
        pha
        tya
        pha
        lda     #$35
        sta     $01
        jsr     $ffd0
        lda     #$37
        sta     $01
s02fb   pla
        tay
        pla
        plp
        rts

        ;       (* = $0a5d)

        ;       ----------------------------------------
        ;       SETUP subroutines
        ;       ----------------------------------------

        ;       checking for modified LOAD vector

check1  ldy     $0330
        lda     $0331
        sty     crack+1
        sta     crack+2

        ;       stock, Jiffy and DTV Kernals ($f4a5 or $f730) are accepted

        cpy     #$a5
        bne     dtv
        sbc     #$f4
        beq     crock

dtv     cpy     #$30
        bne     crock
        sbc     #$f7
crock   clc
        bne     rock

        ;       CPU check ($1a = INC A on 4510 or 65816, but NOP on 6510 CPU)

        byte    $1a
        beq     chock

        ;       CPU check ($eb = NOP on 65C02, XBA on 65816, ROW $xxxx on 4510)

        byte    $eb
        lda     #$00

        ;       4510 (C65) found -> Kernal load

        tay
        bne     crock

        byte    $eb

        ;       65C02 found -> Kernal load

        tay
        beq     rock

        ;       65816 (SuperCPU) found -> turbo off (required for timing)

        sta     $d07a

        ;       checking for C= and CTRL depressed

chock   sty     $ff

check2  ldx     $ba
        cpx     #$04
rock    lda     $028d
        bcs     orck

        ora     #$06

orck    ora     atp+1
        sta     atp+1

        and     #$06
        cmp     #$06
        bne     xest

        ;       self-modification (revert to Kernal LOAD)

        pla
        pla

        ;       self-modification (from VDOS to KERNAL)

kerl    rts
        word    $cfc8

        ldy     #$a5
        lda     #$93
        sty     $ced5
        sta     $ced6

        ldy     #$05
klp     sta     $cd50,y
        lda     $cf72,y
        dey
        bpl     klp

        sta     $ced7

        bne     swap

        ;       drive reset and installing helper

reset   jsr     $ff8a

        sei
        ldy     #$00
        lda     #$80
        sty     $90
        sta     $9d
        sty     $b8
        sty     $b9

        ;       checking for * or = in filename (to load another file if so)

silp    dec     $b7
        beq     lasc

        lda     ($bb),y

        inc     $bb
        bne     silc

        inc     $bc

silc    cmp     #$2a
        beq     sasc
        cmp     #$3d
        bne     silp

sasc    inc     $b7

        ;       last accessed device

lasc    lda     $ba
        bne     haba

        lda     #$08
        sta     $ba
haba    sta     $a8

        ldx     #$58
hlp     lda     $68,x
        sta     $0c80,x
        lda     s02a7,x
        sta     $02a7,x
        dex
        bpl     hlp

        stx     $90

        jsr     check1

        lsr
xest    bne     rest

        ;       drive check with UI on the command channel (for IEC devices)

        lda     #$01
        ldy     #$0f
        jsr     $ffba

        asl
        ldx     #<uin
        ldy     #>uin
        jsr     $ffbd

        jmp     $ffc0

        ;       drive check

drive   lda     #$20
        sta     kerl

        jsr     check2

        lsr
        beq     test

        lsr
        beq     swap

        ;       self-modification (from VDOS to SJLOAD)

sjld    jsr     $cf7d

        ;       It works around a Kernal bug: function $ff8a ($fd15) destroys
        ;       32 bytes at $fd30-$fd4f that need to be restored:

swap    ldy     #$59
        sty     $cf72
swlp    lda     $cd00,y
        sta     $cf75,y
        dey
        bpl     swlp

        ;       swapping $c000 and $f000

rwap    sei
swar    jsr     swax

        lda     #$60
        sta     $ff72
        cmp     $ff72
        beq     swaq

        sta     $02eb
        sta     swp

swaq    jsr     swp

swax    lda     #$0b
        eor     swc+1
        sta     swc+1

        lda     #$0b
        eor     swd+1
        sta     swd+1

        lda     #$cf
        eor     swync+1
        sta     swync+1

        lda     #$1f
        eor     swynd+1
        sta     swynd+1

        lda     #$2f
        sta     $00

        lda     #$02
        eor     $01
        sta     $01

rest    rts

sjload  bne     sjld
vdos    bne     swap

        ;       drive test (reading back from command channel after the UI)

test    ldx     $90
        bne     xomm

        inx
        jsr     $ffc6

comp    jsr     $ffcf
cpc     sta     $0c00

        lda     $90
        bne     xomm

        inc     cpc+1
        bpl     comp

xomm    jsr     $ffe7

xlp     ldx     #$58
zlp     lda     $0c80,x
        sta     $68,x
        dex
        bpl     zlp

        stx     $aa

jiffy   inc     $aa

flp1    inx
        cpx     #$08
        bcs     nock

        ldy     cpc+1

flp2    dey
        bmi     flp1

        lda     stab1,x
        cmp     $0c00,y
        bne     flp2
        lda     stab2,x
        cmp     $0c01,y
        bne     flp2

        cpx     #$04
        bcc     jiffy

        ;       LSB of A reg: 0 - em or vi / 1 - 1541 or 1571

        txa
        lsr

        ;       $02a6: 0 - NTSC / 1 - PAL
        ;       $ff: 0 - DTV found / 1 - DTV not found (LB of load vector)

        and     $02a6
        and     $ff

        ;       $aa: 0 - Jiffy not found / 1 - Jiffy found

nock    and     $aa
        bne     sjload

        txa
        and     #$02
        bne     vdos

kernal  jmp     kerl

        ;       ji,fl,cm,ie,em,vi,54,57
        ;       (jiffy, flyer, cmd, iec, emulation, vice/virtual, 1541, 1571)

stab1   byte    $4a,$46,$43,$49,$44,$56,$35,$35
stab2   byte    $49,$4c,$4d,$45,$4d,$49,$34,$37

        ;       "ui" (soft reset for drive identification)

uin     byte    $55,$49

*=$0bed

        ;       ----------------------------------------
        ;       SETUP init (entry point for Basic)
        ;       ----------------------------------------

        ;       sys4296

        lda     $2c
        pha

        lda     #$26
        ldx     #$03
        sta     $0c
        stx     $0d

        ldy     #$16
        ldx     #$09

btp     lda     ($2b),y
        sta     ($0c),y
        iny
        bne     btp

        inc     $2c
        inc     $0d
        dex
        bne     btp

        pla
        sta     $2c

        jmp     setup

        ;       (* = $0c10)

        ;       compiled with CBM prg Studio v3.13 (by Arthur Jordison)
        ;       http://ajordison.co.uk/

        ;       Rosetta Interactive Fiction project homepage:

        ;       http://istennyila.hu/rosetta

        ;       v0.13 (c) 2012-2019 by Robert Olessak

        ;       SDOS project page:

        ;       http://istennyila.hu/sdos
