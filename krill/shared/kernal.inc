
.ifndef _KERNAL_INC_
_KERNAL_INC_ = 1

DEVICE_SCREEN                          = $03

INDEX1                                 = $22; utility pointer area
VARTAB                                 = $2d; start of BASIC variables
FRFTOP                                 = $33; pointer: bottom of string storage
FRESPC                                 = $35; utility string pointer
FACEXP                                 = $61; FAC exponent
FACHO                                  = $62; FAC mantissa
STRDSC                                 = $64; temporary string descriptor
 STRDSC_LEN                            = 0
 STRDSC_PTR                            = 1
FACSGN                                 = $66; FAC sign
STATUS                                 = $90
VERFCK                                 = $93
NUMOPENFILES                           = $98

CINV                                   = $0314
CBINV                                  = $0316


.if .defined(PLATFORM) & (PLATFORM = 16)

KERNAL_ROM                             = $8000
KERNAL_ROM_SIZE                        = $8000

BSOUR                                  = $95; char buffer for serial bus
DFLTO                                  = $99; default output device
MSGFLG                                 = $9a
EAL                                    = $9d; end address lo
EAH                                    = $9e; end address hi
BSOUR1                                 = $a8; serial Bus shift counter
FNLEN                                  = $ab; length of current file name
SA                                     = $ad; current secondary address
FA                                     = $ae; current I/O device
FNADR                                  = $af; current file name
MEMUSS                                 = $b4; load ram base
PNT                                    = $c8; current screen line address
PNTR                                   = $ca; cursor column on current line
QTSW                                   = $cb; quote switch
USE4DY                                 = $f9

IBASIN                                 = $0322; CHRIN vector
IBSOUT                                 = $0324; CHROUT vector
ILOAD                                  = $032e; LOAD vector

COLOR                                  = $053b

PALETTE                                = $07f9
PALETTE_DEFAULT                        = $ff
PALETTE_USER                           = $00

CLRSCR                                 = $d88b
SEARCHING                              = $f160
LOADING                                = $f189
ERROR9                                 = $f28b
NLOAD                                  = $f04a
KPREND                                 = $fcc3; restore registers and return from IRQ handler

.else; !(.defined(PLATFORM) & (PLATFORM = 16))

KERNAL_ROM                             = $e000
KERNAL_ROM_SIZE                        = $2000

ARG                                    = $69
BSOUR                                  = $95; char buffer for serial bus
DFLTO                                  = $9a; default output device
MSGFLG                                 = $9d
BSOUR1                                 = $a4; serial Bus shift counter
EAL                                    = $ae; end address lo
EAH                                    = $af; end address hi
FNLEN                                  = $b7; length of current file name
SA                                     = $b9; current secondary address
FA                                     = $ba; current I/O device
FNADR                                  = $bb; current file name
MEMUSS                                 = $c3; load ram base
BLNSW                                  = $cc; cursor blink enable
BLNCT                                  = $cd; count to toggle cursor
GDBLN                                  = $ce; char under cursor
BLNON                                  = $cf; on/off blink flag
PNT                                    = $d1; current screen line address
PNTR                                   = $d3; cursor column on current line
QTSW                                   = $d4; quote switch
LDTB1                                  = $d9
USER                                   = $f3; current screen line colour address

COLOR                                  = $0286
GDCOL                                  = $0287; original colour under cursor
PAL_NTSC                               = $02a6; 0: NTSC

NMINV                                  = $0318
IBASIN                                 = $0324; CHRIN vector
IBSOUT                                 = $0326; CHROUT vector
ILOAD                                  = $0330; LOAD vector

POLYX                                  = $e059
CLRSCR                                 = $e544
DSPP                                   = $ea13; rreset cursor blink count, set colour pointer, put a char on the screen
DSPP2                                  = $ea1c; put a char on the screen
SCOLOR                                 = $ea24; synchronise colour pointer
KEY                                    = $ea31; keyboard/cursor handler
KPREND                                 = $ea7e; ack timer, restore registers and return from IRQ handler
ROWSLO                                 = $ecf0
NLOAD                                  = $f4a5
SEARCHING                              = $f5af
LOADING                                = $f5d2
ERROR9                                 = $f713

.endif; !(.defined(PLATFORM) & (PLATFORM = 16))


LSTNSA                                 = $ff93
 SA_TALK                               = $20
 SA_UNTALK                             = $3f
 SA_LISTEN                             = $40
 SA_UNLISTEN                           = $5f
 SA_OPENCHANNEL                        = $60
 SA_CLOSE                              = $e0
 SA_OPEN                               = $f0
TKSA                                   = $ff96
IECIN                                  = $ffa5
IECOUT                                 = $ffa8
UNTLK                                  = $ffab
UNLSTN                                 = $ffae
LISTEN                                 = $ffb1
TALK                                   = $ffb4
READST                                 = $ffb7
 KERNAL_STATUS_ILLEGAL_TRACK_OR_SECTOR = %11000000; returned if a mid-file block is on an illegal track or sector
 KERNAL_STATUS_DEVICE_NOT_PRESENT      = %10000000; this is also returned if a file starts on an illegal track or sector
 KERNAL_STATUS_EOF                     = %01000000
 KERNAL_STATUS_FILE_NOT_FOUND          = %00000010
SETLFS                                 = $ffba
 COMMAND_ERROR_CHANNEL                 = $0f
SETNAM                                 = $ffbd
OPEN                                   = $ffc0
 KERNAL_TOOMANYFILES                   = $01
 KERNAL_FILEOPEN                       = $02
 KERNAL_FILENOTOPEN                    = $03
 KERNAL_FILENOTFOUND                   = $04
 KERNAL_DEVICENOTPRESENT               = $05
 KERNAL_NOTINPUTFILE                   = $06
 KERNAL_NOTOUTPUTFILE                  = $07
 KERNAL_MISSINGFILENAME                = $08
 KERNAL_ILLEGALDEVICENUMBER            = $09
CLOSE                                  = $ffc3
CHKIN                                  = $ffc6
CHKOUT                                 = $ffc9
CLRCHN                                 = $ffcc
CHRIN                                  = $ffcf
CHROUT                                 = $ffd2
LOAD                                   = $ffd5
SAVE                                   = $ffd8

.endif; !_KERNAL_INC_
