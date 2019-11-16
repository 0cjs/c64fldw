
D71 = d71
#D71 = d64 # use this if ONLY_1541_AND_COMPATIBLE != 0

ifeq ($(PLATFORM),)
_PLATFORM_     = c64
else ifeq ($(PLATFORM),c116)
_PLATFORM_     = c16
else ifeq ($(PLATFORM),plus4)
_PLATFORM_     = c16
else
_PLATFORM_     = $(PLATFORM)
endif

ifeq ($(USE_VICE),)
USE_VICE       = 0
endif


ifneq ($(_PLATFORM_),c64)
ifneq ($(_PLATFORM_),c16)
$(error invalid platform $(_PLATFORM_) specified)
endif
endif


ARCH           = $(shell uname | tr "[a-z]" "[A-Z]" | tr -c -d "[A-Z]")
HOME          := $(shell echo ~)

ifneq ($(findstring CYGWINNT,$(ARCH)),)
    ifeq (CYGWINNT,$(ARCH))
ARCH           = WIN32
    else
ARCH           = WIN64
    endif    
endif
ifneq ($(findstring DARWIN,$(ARCH)),)
ARCH           = MACOSX
endif


ifeq ($(_PLATFORM_),c16)
    ifeq ($(USE_VICE),0)
        ifneq ($(findstring WIN,$(ARCH)),)
USE_PLUS4EMU   = 0
USE_YAPE       = 1
        else
USE_PLUS4EMU   = 1
USE_YAPE       = 0
        endif
    else
USE_PLUS4EMU   = 0
USE_YAPE       = 0
    endif
else
USE_PLUS4EMU   = 0
USE_YAPE       = 0
endif


ifeq ($(_PLATFORM_),c16)
    ifeq ($(ARCH),MACOSX)
    # MacOSX, these programs must be installed as applications
VICE           = open /Applications/VICE/xplus4.app --args
VICE128        = $(VICE)
PLUS4EMU       = open /Applications/plus4emu.app --args # -cfg "$(HOME)/Library/Application Support/plus4emu/config/P4_64k_NTSC.cfg"
    else
VICE           = xplus4
PLUS4EMU       = plus4emu
        ifeq ($(ARCH),WIN64)
YAPE           = YapeWin64
        else
YAPE           = Yape
        endif
    endif
else
VICE           = x64sc
VICE128        = x128
endif

ifeq ($(_PLATFORM_),c16)
# for testing, VICE only
DRIVE9TYPE     = 1551
else
DRIVE9TYPE     = 0
endif

ifneq ($(USE_PLUS4EMU),0)
EMU41          = $(PLUS4EMU) floppy.a.driveType=0 floppy.b.driveType=1 -disk
EMU51          = $(PLUS4EMU) floppy.a.driveType=1 floppy.b.driveType=1 -disk # C-16 only
else
EMU41          = $(VICE) -drive8type 1541 -drive9type $(DRIVE9TYPE) -autostart
EMU51          = $(VICE) -drive8type 1551 -drive9type $(DRIVE9TYPE) -autostart # C-16 only
endif
# the following mode is only supported by VICE and YAPE
EMU81          = $(VICE) -drive8type 1581 -drive9type $(DRIVE9TYPE) -autostart
# the following modes are only supported by VICE
EMU42          = $(VICE) -drive8type 1542 -drive9type $(DRIVE9TYPE) -autostart # 1541-II
EMU70          = $(VICE) -drive8type 1570 -drive9type $(DRIVE9TYPE) -autostart
EMU71          = $(VICE) -drive8type 1571 -drive9type $(DRIVE9TYPE) -autostart
EMU7141        = $(VICE) -drive8type 1571 -drive9type $(DRIVE9TYPE) -autostart # 1571 with .d64
EMU73          = $(VICE128) -drive8type 1573 -drive9type 0 -autostart # 1571CR
EMU7341        = $(VICE128) -drive8type 1573 -drive9type 0 -autostart # 1571CR with .d64
EMU2000        = $(VICE) -drive8type 2000 -drive9type 0 -autostart
EMU4000        = $(VICE) -drive8type 4000 -drive9type 0 -autostart

ifneq ($(USE_PLUS4EMU),0)
EMU941         = $(PLUS4EMU) floppy.a.driveType=0 floppy.b.driveType=0 -disk
EMU951         = $(PLUS4EMU) floppy.a.driveType=1 floppy.b.driveType=0 -disk # C-16 only
else
EMU941         = $(VICE) -drive8type 1541 -drive9type 1541 -autostart
EMU951         = $(VICE) -drive8type 1551 -drive9type 1541 -autostart # C-16 only
endif
# the following modes are only supported by VICE
EMU942         = $(VICE) -drive8type 1542 -drive9type 1541 -autostart # 1541-II
EMU970         = $(VICE) -drive8type 1570 -drive9type 1541 -autostart
EMU971         = $(VICE) -drive8type 1571 -drive9type 1541 -autostart
EMU97141       = $(VICE) -drive8type 1571 -drive9type 1541 -autostart # 1571 with .d64
EMU973         = $(VICE128) -drive8type 1573 -drive9type 1541 -autostart # 1571CR
EMU97341       = $(VICE128) -drive8type 1573 -drive9type 1541 -autostart # 1571CR with .d64
EMU981         = $(VICE) -drive8type 1581 -drive9type 1541 -autostart
EMU92000       = $(VICE) -drive8type 2000 -drive9type 1541 -autostart
EMU94000       = $(VICE) -drive8type 4000 -drive9type 1541 -autostart


NOWORKINGCOPY  = $(shell svn info > /dev/null 2>&1; echo $$?)
SVNVERSION     = svnversion
ifeq ($(NOWORKINGCOPY),0)
VERSION        = $(shell $(SVNVERSION) | tr -d [:cntrl:])
else
VERSION        =
endif

ECHO           = echo
PRINTF         = printf
HEAD           = head

MAKE           = make
CP             = cp
MV             = mv
RM             = rm -rf
MKDIR          = mkdir
TOUCH          = touch
CAT            = cat

AS             = ca65
LD             = ld65
C1541          = c1541

CC1541         = ../../tools/cc1541


BITNAX         = ../../tools/bitnax-07a8c67/lz

B2             = ../../tools/b2/b2.exe
B2_SOURCE      = ../../tools/b2

EXO            = ../../tools/exomizer-3/src/exomizer
EXO_SOURCE     = ../../tools/exomizer-3/src

DOYNAX_LZ      = ../../tools/doynamite1.1/lz

LC             = ../../tools/wcrush/wcrush/wcrush
LCSPEED        = 6
LC_SOURCE      = ../../tools/wcrush

NC             = ../../tools/nucrunch-1.0.1/target/release/nucrunch
NC_SOURCE      = ../../tools/nucrunch-1.0.1

PU             = ../../tools/pucrunch/pucrunch
PU_SOURCE      = ../../tools/pucrunch

TC             = python ../../tools/tinycrunch_v1.2/tc_encode.py

SUBSIZER       = ../../tools/subsizer-0.7pre1/subsizer
SUBSIZER_SRC   = ../../tools/subsizer-0.7pre1/src

CONV           = ../../tools/compressedfileconverter.pl

LOADER         = ../../src
LDRBIN         = ../../build
LDRINC         = ../../include

BUILDDIR       = ../../build
INTERMDIR      = ../../build/interm

RESOURCESDIR   = ../resources


AS_FLAGS       = -I $(LDRINC) -I ../../../shared
PU_FLAGS       = -d -l 0x1000 -x 0x1000 -i 0
ifeq ($(_PLATFORM_),c16)
AS_FLAGS      += -t c16 -D PLATFORM=16
PU_FLAGS      += -c16
else
AS_FLAGS      += -t c64 -D PLATFORM=64
PU_FLAGS      += -c64 -g 0x37
endif

EXTENSION_            := prg
EXTENSION_BITNAX      := bnx
EXTENSION_BYTEBOOZER2 := b2
EXTENSION_DOYNAX_LZ   := dnx
EXTENSION_EXOMIZER    := exo
EXTENSION_LEVELCRUSH  := lc
EXTENSION_NUCRUNCH    := nc
EXTENSION_PUCRUNCH    := pu
EXTENSION_SUBSIZER    := ssz
EXTENSION_TINYCRUNCH  := tc

COMPRESSOR := $(shell grep -v 'DECOMPRESSORS::NONE' ../../include/config.inc | grep 'DECOMPRESSORS::' | sed 's/.*DECOMPRESSORS::\([^; ]*\).*/\1/')
COMPEXT    := $(EXTENSION_$(COMPRESSOR))


default: all

tellarch:
	@$(ECHO) $(ARCH)


$(BUILDDIR):
	$(MKDIR) $(BUILDDIR)

$(INTERMDIR):
	$(MKDIR) $(INTERMDIR)


SHAREDDEPS     = $(LOADER)/Makefile \
                 $(LDRINC)/config.inc

LOADERDEPS     = $(SHAREDDEPS) \
                 $(LOADER)/decompress/bitnaxdecomp.s \
                 $(LOADER)/decompress/b2decomp.s \
                 $(LOADER)/decompress/doynaxdecomp.s \
                 $(LOADER)/decompress/exodecomp.s \
                 $(LOADER)/decompress/lcdecomp.s \
                 $(LOADER)/decompress/ncdecomp.s \
                 $(LOADER)/decompress/pudecomp.s \
                 $(LOADER)/decompress/subsizerdecomp.s \
                 $(LOADER)/decompress/tcdecomp.s \
                 $(LOADER)/drives/drivecode-common.inc $(LOADER)/drives/drivecode1541.s $(LOADER)/drives/drivecode1571.s $(LOADER)/drives/drivecode1581.s \
                 $(LOADER)/hal/hal.inc $(LOADER)/hal/hal-c64.inc $(LOADER)/hal/hal-c16.inc \
                 $(LOADER)/install.s \
                 $(LOADER)/resident.s \
                 $(LDRINC)/loader.inc

DISKIMAGEDEPS := Makefile $(CC1541) \
                 $(INTERMDIR)/test-$(_PLATFORM_).prg \
                 $(INTERMDIR)/pic1.prg          $(INTERMDIR)/pic2.prg \
                 $(INTERMDIR)/pic1hi.prg        $(INTERMDIR)/pic2hi.prg \
                 $(INTERMDIR)/pic1.$(COMPEXT)   $(INTERMDIR)/pic2.$(COMPEXT) \
                 $(INTERMDIR)/pic1hi.$(COMPEXT) $(INTERMDIR)/pic2hi.$(COMPEXT)


all: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64 \
     $(BUILDDIR)/loadertest-$(_PLATFORM_).d71 \
     $(BUILDDIR)/loadertest-$(_PLATFORM_).d81


$(LDRBIN)/loader-$(_PLATFORM_).a: $(LOADERDEPS) | $(BUILDDIR) $(INTERMDIR)
	$(MAKE) -C $(LOADER) DEBUG=1 lib


../../../.svn/entries:
ifeq ($(VERSION),)
	$(MKDIR) ../../../.svn
	$(TOUCH) $@
endif

../../version.inc: ../../../.svn/entries
	$(MAKE) -C ../.. version.inc


$(INTERMDIR)/test-$(_PLATFORM_).o: test.s $(LOADERDEPS) ../../version.inc Makefile | $(BUILDDIR) $(INTERMDIR)
	$(AS) $(AS_FLAGS) -o $@ $<

$(INTERMDIR)/test-$(_PLATFORM_)-uncompressed.prg: Linkfile $(INTERMDIR)/test-$(_PLATFORM_).o $(LDRBIN)/loader-$(_PLATFORM_).a
	$(LD) -o $@ $(INTERMDIR)/test-$(_PLATFORM_).o -C Linkfile -vm -m $@.map -Ln $@.lbl $(LDRBIN)/loader-$(_PLATFORM_).a

$(INTERMDIR)/test-$(_PLATFORM_).prg: $(INTERMDIR)/test-$(_PLATFORM_)-uncompressed.prg $(PU)
	$(PU) $(PU_FLAGS) $< $@


$(INTERMDIR)/pic1hi.prg: $(RESOURCESDIR)/pic1.bin
	$(PRINTF) '\000\300' | $(CAT) - $? > $@ # octal 300 = hex c0

$(INTERMDIR)/pic2hi.prg: $(RESOURCESDIR)/pic2.bin
	$(PRINTF) '\000\300' | $(CAT) - $? > $@ # octal 300 = hex c0

$(INTERMDIR)/%.prg: $(RESOURCESDIR)/%.bin
	$(PRINTF) '\000\140' | $(CAT) - $? > $@ # octal 140 = hex 60

# Rules to compress data files
$(INTERMDIR)/%.b2: $(INTERMDIR)/%.prg $(B2)
	$(B2) $<
	$(MV) $<.b2 $@

$(INTERMDIR)/%.bnx: $(INTERMDIR)/%.prg $(BITNAX)
	$(BITNAX) --bitfire -o $@ $<

$(INTERMDIR)/%.dnx: $(INTERMDIR)/%.prg $(DOYNAX_LZ)
	$(DOYNAX_LZ) -o $@ $<

$(INTERMDIR)/%.exo: $(INTERMDIR)/%.prg $(EXO)
	$(EXO) mem -f $< -o $@

$(INTERMDIR)/%.lc: $(INTERMDIR)/%.prg $(LC)
	$(LC) $(LCSPEED) $< $@
	$(CONV) lc $< $@ $@

$(INTERMDIR)/%.nc: $(INTERMDIR)/%.prg $(NC)
	$(NC) $< --auto -o $@

$(INTERMDIR)/%.pu: $(INTERMDIR)/%.prg $(PU)
	$(PU) -c0 -x 0 $< $@

$(INTERMDIR)/%.ssz: $(INTERMDIR)/%.prg $(SUBSIZER)
	$(SUBSIZER) -m -f -o $@ $<

$(INTERMDIR)/%.tc: $(INTERMDIR)/%.prg
	$(TC) --inPlace $< $@


# top-speed settings for loading without sprites or IRQ
#	 -f 1P-PIC1.PAK     -r 18       -E -w $(INTERMDIR)/pic1.$(COMPEXT) \
#	 -f 2P-PIC2.PAK     -r 18       -E -w $(INTERMDIR)/pic2.$(COMPEXT) \
#	 -f 1B-PIC1.BIN     -r 18 -b  9 -E -w $(INTERMDIR)/pic1.prg \
#	 -f 2B-PIC2.BIN     -r 18 -b  6 -E -w $(INTERMDIR)/pic2.prg \

$(BUILDDIR)/loadertest-$(_PLATFORM_).d64: $(DISKIMAGEDEPS)
	$(CC1541) -n "NORMAL IS BORING" -i PLUSH -S -3 \
	 \
	 -f "LOADER TEST"   -s 8 -w $(INTERMDIR)/test-$(_PLATFORM_).prg \
	 \
	 -f 1B-PIC1.BIN     -r 27 -b 0             -E -w $(INTERMDIR)/pic1.prg \
	 -f 2B-PIC2.BIN     -r 27 -b 4             -E -w $(INTERMDIR)/pic2.prg \
	 -f 1P-PIC1.PAK     -r 27 -b 5             -E -w $(INTERMDIR)/pic1.$(COMPEXT) \
	 -f 2P-PIC2.PAK     -r 27                  -E -w $(INTERMDIR)/pic2.$(COMPEXT) \
	 -f LF-LOOPFILE.PRG -l 1B-PIC1.BIN \
	 \
	 -f AB-PIC1HI.BIN   -r 14 -b 2  -s -4 -F 3 -E -w $(INTERMDIR)/pic1hi.prg \
	 -f BB-PIC2HI.BIN   -r 14 -b 10 -s -4 -F 3 -E -w $(INTERMDIR)/pic2hi.prg \
	 -f AP-PIC1HI.PAK   -r 19       -s -4      -E -w $(INTERMDIR)/pic1hi.$(COMPEXT) \
	 -f BP-PIC2HI.PAK   -r 19       -s -4      -E -w $(INTERMDIR)/pic2hi.$(COMPEXT) \
	 -f LG-LOOPFILE.PRG -l AB-PIC1HI.BIN \
	 \
	$@
	@echo Using $(COMPRESSOR) $(COMPEXT)

$(BUILDDIR)/loadertest-$(_PLATFORM_).d71: $(DISKIMAGEDEPS)
	$(CC1541) -n "NORMAL IS BORING" -i PLUSH -S -3 \
	 -f "LOADER TEST"   -s 8 -w $(INTERMDIR)/test-$(_PLATFORM_).prg \
	 \
	 -f 1B-PIC1.BIN     -r 25 -b 2  -F 1 -e -c -w $(INTERMDIR)/pic1.prg \
	 -f 2B-PIC2.BIN     -r 25 -b 5  -F 1 -e -c -w $(INTERMDIR)/pic2.prg \
	 -f 1P-PIC1.PAK     -r 25 -b 6       -e -c -w $(INTERMDIR)/pic1.$(COMPEXT) \
	 -f 2P-PIC2.PAK     -r 25            -e -c -w $(INTERMDIR)/pic2.$(COMPEXT) \
	 -f LF-LOOPFILE.PRG -l 1B-PIC1.BIN \
	 \
	 -f AB-PIC1HI.BIN   -r 14 -b 2  -s -4 -e -c -w $(INTERMDIR)/pic1hi.prg \
	 -f BB-PIC2HI.BIN   -r 14 -b 10 -s -4 -e -c -w $(INTERMDIR)/pic2hi.prg \
	 -f AP-PIC1HI.PAK   -r 19       -s -4 -e -c -w $(INTERMDIR)/pic1hi.$(COMPEXT) \
	 -f BP-PIC2HI.PAK   -r 19       -s -4 -e -c -w $(INTERMDIR)/pic2hi.$(COMPEXT) \
	 -f LG-LOOPFILE.PRG -l AB-PIC1HI.BIN \
	 \
	$@
	@echo Using $(COMPRESSOR) $(COMPEXT)

$(BUILDDIR)/loadertest-$(_PLATFORM_).d81: $(DISKIMAGEDEPS)
	$(C1541) -format "normal is boring,+h" $(subst .,,$(suffix $@)) $@
	$(C1541) -attach $@ \
	 -write $(INTERMDIR)/test-$(_PLATFORM_).prg "loader test" \
	  \
	 -write $(INTERMDIR)/pic1.prg               "1b-pic1.bin" \
	 -write $(INTERMDIR)/pic2.prg               "2b-pic2.bin" \
	 -write $(INTERMDIR)/pic1hi.prg             "ab-pic1hi.bin" \
	 -write $(INTERMDIR)/pic2hi.prg             "bb-pic2hi.bin" \
	  \
	 -write $(INTERMDIR)/pic1.$(COMPEXT)        "1p-pic1.pak" \
	 -write $(INTERMDIR)/pic2.$(COMPEXT)        "2p-pic2.pak" \
	 -write $(INTERMDIR)/pic1hi.$(COMPEXT)      "ap-pic1hi.pak" \
	 -write $(INTERMDIR)/pic2hi.$(COMPEXT)      "bp-pic2hi.pak"


.PHONY: run run41 run51 run41ii run42 run70 run71 run7141 run73 run7341 run81 run2000 run4000
.PHONY: run9 run941 run951 run941ii run942 run970 run971 run97141 run973 run97341 run981 run92000 run94000

ifneq ($(USE_YAPE),0)
run run41 run51: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(YAPE) "..\..\build\loadertest-$(_PLATFORM_).d64"
else
run run41: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU41) $(realpath $^)

run51: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU51) $(realpath $^)
endif

run41ii run42: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU42) $(realpath $^)

run70: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU70) $(realpath $^)

run71: $(BUILDDIR)/loadertest-$(_PLATFORM_).$(D71)
	$(EMU71) $(realpath $^)

run7141: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU7141) $(realpath $^)

run73: $(BUILDDIR)/loadertest-$(_PLATFORM_).$(D71)
	$(EMU73) $(realpath $^)

run7341: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU7341) $(realpath $^)

ifneq ($(USE_YAPE),0)
run81: $(BUILDDIR)/loadertest-$(_PLATFORM_).d81
	$(YAPE) "..\..\build\loadertest-$(_PLATFORM_).d81"
else
run81: $(BUILDDIR)/loadertest-$(_PLATFORM_).d81
	$(EMU81) $(realpath $^)
endif

run2000: $(BUILDDIR)/loadertest-$(_PLATFORM_).d81
	$(EMU2000) $(realpath $^)

run4000: $(BUILDDIR)/loadertest-$(_PLATFORM_).d81
	$(EMU4000) $(realpath $^)

ifneq ($(USE_YAPE),0)
run9 run941 run951: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(YAPE) /DISK9:"..\..\build\loadertest-$(_PLATFORM_).d64" "..\..\build\loadertest-$(_PLATFORM_).d64"
else
run9 run941: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU941) $(realpath $^)

run951: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU951) $(realpath $^)
endif

run941ii run942: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU942) $(realpath $^)

run970: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU970) $(realpath $^)

run971: $(BUILDDIR)/loadertest-$(_PLATFORM_).$(D71)
	$(EMU971) $(realpath $^)

run97141: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU97141) $(realpath $^)

run973: $(BUILDDIR)/loadertest-$(_PLATFORM_).$(D71)
	$(EMU973) $(realpath $^)

run97341: $(BUILDDIR)/loadertest-$(_PLATFORM_).d64
	$(EMU97341) $(realpath $^)

ifneq ($(USE_YAPE),0)
run981: $(BUILDDIR)/loadertest-$(_PLATFORM_).d81
	$(YAPE) /DISK9:"..\..\build\loadertest-$(_PLATFORM_).d81" "..\..\build\loadertest-$(_PLATFORM_).d81"
else
run981: $(BUILDDIR)/loadertest-$(_PLATFORM_).d81
	$(EMU981) "$(realpath $^):loader test"
endif

run92000: $(BUILDDIR)/loadertest-$(_PLATFORM_).d81
	$(EMU92000) "$(realpath $^):loader test"

run94000: $(BUILDDIR)/loadertest-$(_PLATFORM_).d81
	$(EMU94000) "$(realpath $^):loader test"


clean:
	-$(RM) $(INTERMDIR)
	-$(RM) $(LDRBIN)/loader-c64.a
	-$(RM) $(LDRBIN)/loader-c16.a

distclean:
	-$(RM) $(BITNAX)
	-$(RM) $(B2)
	-$(RM) $(DOYNAX_LZ)
	-$(RM) $(NC)
	-$(RM) $(PU)
	-$(RM) $(SUBSIZER)
	$(MAKE) -C $(CC1541)_source clean
	$(MAKE) -C $(EXO_SOURCE) clean
	$(MAKE) -C $(LC_SOURCE) clean
	$(MAKE) -C $(LOADER) clean
	$(MAKE) -C $(NC_SOURCE) clean
	$(MAKE) -C $(SUBSIZER_SRC) clean

wipe: distclean clean


$(CC1541): $(CC1541)_source/cc1541.cpp
	$(MAKE) -C $(CC1541)_source


$(BITNAX):
	$(CC) $@.c -o $@

$(B2):
	$(MAKE) -C $(B2_SOURCE)

$(DOYNAX_LZ):
	$(CC) $@.c -o $@

$(EXO):
	$(MAKE) -C $(EXO_SOURCE)

$(LC):
	$(MAKE) -C $(LC_SOURCE)

$(NC):
	$(MAKE) -C $(NC_SOURCE)

$(PU):
	$(MAKE) -C $(PU_SOURCE)

$(SUBSIZER):
	$(MAKE) -C $(SUBSIZER_SRC)
