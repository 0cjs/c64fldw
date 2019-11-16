
NOWORKINGCOPY      = $(shell svn info > /dev/null 2>&1; echo $$?)
SVNVERSION         = svnversion
ifeq ($(NOWORKINGCOPY),0)
VERSION            = $(shell $(SVNVERSION) | tr -d [:cntrl:])
else
VERSION            =
endif

CD                 = cd
PRINTF             = printf
MKDIR              = mkdir
TOUCH              = touch
ZIP                = zip -9 --must-match
ZIP_NO_PATHS       = $(ZIP) -j
ifneq ($(VERSION),)
ZIPFILE            = loader-v$(VERSION).zip
LOADERTEST         = loadertest-v$(VERSION).zip
else
ZIPFILE            = loader.zip
LOADERTEST         = loadertest.zip
endif


SHAREDFOLDER       = shared
LOADERFOLDER       = $(notdir $(shell pwd))
DOCSFOLDER         = $(LOADERFOLDER)/docs
SRCFOLDER          = $(LOADERFOLDER)/src
INCFOLDER          = $(LOADERFOLDER)/include
SAMPLESFOLDER      = $(LOADERFOLDER)/samples
BUILDFOLDER        = $(LOADERFOLDER)/build
TOOLSFOLDER        = $(LOADERFOLDER)/tools

ZIPLIST           += $(LOADERFOLDER)/README

ZIPLIST           += $(SHAREDFOLDER)/standard.inc
ZIPLIST           += $(SHAREDFOLDER)/cpu.inc
ZIPLIST           += $(SHAREDFOLDER)/vic.inc
ZIPLIST           += $(SHAREDFOLDER)/cia.inc
ZIPLIST           += $(SHAREDFOLDER)/ted.inc
ZIPLIST           += $(SHAREDFOLDER)/via.inc
ZIPLIST           += $(SHAREDFOLDER)/kernal.inc
ZIPLIST           += $(SHAREDFOLDER)/basic.inc
ZIPLIST           += $(SHAREDFOLDER)/float.inc

ZIPLIST           += $(LOADERFOLDER)/Makefile
ZIPLIST           += $(LOADERFOLDER)/version.inc

ZIPLIST           += $(DOCSFOLDER)/GCRTiming.txt
ZIPLIST           += $(DOCSFOLDER)/Prerequisites.txt
ZIPLIST           += $(DOCSFOLDER)/Protocol.txt

DECOMPFOLDER       = decompress
DRIVESFOLDER       = drives
HALFOLDER          = hal
ZIPLIST           += $(SRCFOLDER)/Makefile
ZIPLIST           += $(SRCFOLDER)/make-loadersymbolsinc.pl
ZIPLIST           += $(SRCFOLDER)/$(DECOMPFOLDER)/b2decomp.s
ZIPLIST           += $(SRCFOLDER)/$(DECOMPFOLDER)/bitnaxdecomp.s
ZIPLIST           += $(SRCFOLDER)/$(DECOMPFOLDER)/doynaxdecomp.s
ZIPLIST           += $(SRCFOLDER)/$(DECOMPFOLDER)/exodecomp.s
ZIPLIST           += $(SRCFOLDER)/$(DECOMPFOLDER)/lcdecomp.s
ZIPLIST           += $(SRCFOLDER)/$(DECOMPFOLDER)/ncdecomp.s
ZIPLIST           += $(SRCFOLDER)/$(DECOMPFOLDER)/pudecomp.s
ZIPLIST           += $(SRCFOLDER)/$(DECOMPFOLDER)/subsizerdecomp.s
ZIPLIST           += $(SRCFOLDER)/$(DECOMPFOLDER)/tcdecomp.s
ZIPLIST           += $(SRCFOLDER)/$(DRIVESFOLDER)/drivecode-common.inc
ZIPLIST           += $(SRCFOLDER)/$(DRIVESFOLDER)/drivecode1541.s
ZIPLIST           += $(SRCFOLDER)/$(DRIVESFOLDER)/drivecode1571.s
ZIPLIST           += $(SRCFOLDER)/$(DRIVESFOLDER)/drivecode1581.s
ZIPLIST           += $(SRCFOLDER)/$(HALFOLDER)/hal.inc
ZIPLIST           += $(SRCFOLDER)/$(HALFOLDER)/hal-c64.inc
ZIPLIST           += $(SRCFOLDER)/$(HALFOLDER)/hal-c16.inc
ZIPLIST           += $(SRCFOLDER)/install.s
ZIPLIST           += $(SRCFOLDER)/resident.s

ZIPLIST           += $(INCFOLDER)/config.inc
ZIPLIST           += $(INCFOLDER)/loader.inc

MINEXAMPLEFOLDER   = minexample
RESOURCESFOLDER    = resources
STANDALONEFOLDER   = standalone
TESTFOLDER         = test
TURNDISKFOLDER     = turndisk
ZIPLIST           += $(SAMPLESFOLDER)/$(RESOURCESFOLDER)/pic1.bin
ZIPLIST           += $(SAMPLESFOLDER)/$(RESOURCESFOLDER)/pic2.bin
ZIPLIST           += $(SAMPLESFOLDER)/$(RESOURCESFOLDER)/oxyron_oneder.prg
ZIPLIST           += $(SAMPLESFOLDER)/$(RESOURCESFOLDER)/threeve.prg
ZIPLIST           += $(SAMPLESFOLDER)/$(MINEXAMPLEFOLDER)/Makefile
ZIPLIST           += $(SAMPLESFOLDER)/$(MINEXAMPLEFOLDER)/minexample.s
ZIPLIST           += $(SAMPLESFOLDER)/$(MINEXAMPLEFOLDER)/loaderconfig.inc
ZIPLIST           += $(SAMPLESFOLDER)/$(STANDALONEFOLDER)/Makefile
ZIPLIST           += $(SAMPLESFOLDER)/$(STANDALONEFOLDER)/Linkfile-c64
ZIPLIST           += $(SAMPLESFOLDER)/$(STANDALONEFOLDER)/Linkfile-c16
ZIPLIST           += $(SAMPLESFOLDER)/$(STANDALONEFOLDER)/standalone.s
ZIPLIST           += $(SAMPLESFOLDER)/$(STANDALONEFOLDER)/loaderconfig.inc
ZIPLIST           += $(SAMPLESFOLDER)/$(TESTFOLDER)/Makefile
ZIPLIST           += $(SAMPLESFOLDER)/$(TESTFOLDER)/Linkfile
ZIPLIST           += $(SAMPLESFOLDER)/$(TESTFOLDER)/test.s
ZIPLIST           += $(SAMPLESFOLDER)/$(TURNDISKFOLDER)/Makefile
ZIPLIST           += $(SAMPLESFOLDER)/$(TURNDISKFOLDER)/Linkfile
ZIPLIST           += $(SAMPLESFOLDER)/$(TURNDISKFOLDER)/turndisk.s
ZIPLIST           += $(SAMPLESFOLDER)/$(TURNDISKFOLDER)/loaderconfig.inc

ZIPLIST           += $(TOOLSFOLDER)/compressedfileconverter.pl

CC1541SRCFOLDER    = cc1541_source
ZIPLIST           += $(TOOLSFOLDER)/$(CC1541SRCFOLDER)/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(CC1541SRCFOLDER)/cc1541.cpp
ZIPLIST           += $(TOOLSFOLDER)/$(CC1541SRCFOLDER)/filefinder.cpp
ZIPLIST           += $(TOOLSFOLDER)/$(CC1541SRCFOLDER)/XGetopt.cpp
ZIPLIST           += $(TOOLSFOLDER)/$(CC1541SRCFOLDER)/XGetopt.h

B2FOLDER           = b2
ZIPLIST           += $(TOOLSFOLDER)/$(B2FOLDER)/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(B2FOLDER)/bb.c
ZIPLIST           += $(TOOLSFOLDER)/$(B2FOLDER)/bb.h
ZIPLIST           += $(TOOLSFOLDER)/$(B2FOLDER)/cruncher.c
ZIPLIST           += $(TOOLSFOLDER)/$(B2FOLDER)/cruncher.h
ZIPLIST           += $(TOOLSFOLDER)/$(B2FOLDER)/file.c
ZIPLIST           += $(TOOLSFOLDER)/$(B2FOLDER)/file.h
ZIPLIST           += $(TOOLSFOLDER)/$(B2FOLDER)/Decruncher.inc

ZIPLIST           += $(TOOLSFOLDER)/bitnax-07a8c67/lz.c

DOYNAXLZFOLDER     = doynamite1.1
ZIPLIST           += $(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/readme.txt
ZIPLIST           += $(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/decrunch.asm
ZIPLIST           += $(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/lz.c
ZIPLIST           += $(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/sfx.asm
ZIPLIST           += $(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/test.asm
ZIPLIST           += $(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/simple/decrunch.asm
ZIPLIST           += $(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/simple/lz.c
ZIPLIST           += $(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/krill/doynaxdecomp.s

EXOFOLDER          = exomizer-3
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/changelog.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exo20info.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exo30info.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exobasic10b2.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/6502emu.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/6502emu.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/asm.tab.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/asm.tab.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/asm.y
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/asm.yy
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/areatrace.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/areatrace.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/b2membuf.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/bas_main.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/bprg_actions.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/bprg_link_patch.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/bprg_renumber.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/bprg_trampoline.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/bprg.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/bprg.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/callback.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/chunkpool.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/chunkpool.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/common.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/desfx.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/desfx.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/exo_helper.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/exo_helper.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/exo_main.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/exo_raw.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/exo_util.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/exo_util.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/exodec.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/exodec.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/expr.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/expr.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/flags.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/getflag.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/getflag.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/int.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/lex.yy.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/log.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/log.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/map.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/map.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/match.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/match.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/membuf_io.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/membuf_io.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/membuf.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/membuf.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/named_buffer.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/named_buffer.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/optimal.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/optimal.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/output.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/output.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/parse.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/parse.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/pc.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/pc.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/progress.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/progress.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/radix.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/radix.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/search.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/search.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/vec.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/vec.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/src/sfxdecr.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/README_exo3.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/c64.cfg
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/data.bin
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/exodecrunch.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/exostreamdecr1.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/exostreamdecr2.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/krilldecr.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/main.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/main1.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/main2.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/mainf.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/dasm/exodecrunch.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/dasm/wrap.s
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/dasm/README_exo3.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/perf/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/exodecrs/perf/data.raw
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/exodecr.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/exodecr.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/exodecrunch.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/exodecrunch.h
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/main.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/main2.c
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/test1.bin
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/test2.bin
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/test3.bin
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/test4.bin
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/6809/exo2_final.asm
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/6809/exo2_puls.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/6809/README_exo3.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/z80/deexo_b.asm
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/z80/deexo_simple.asm
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/z80/deexo_simple_b.asm
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/z80/deexo.asm
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/z80/readme.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/z80/README_exo3.txt
ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/rawdecrs/z80/lgpl-2.1.txt
#ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/win32/exobasic.exe
#ZIPLIST           += $(TOOLSFOLDER)/$(EXOFOLDER)/win32/exomizer.exe

NCFOLDER           = nucrunch-1.0.1
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/MANIFEST
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/Cargo.toml
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/readme.txt
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/decrunch.a65
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/rdecrunch.a65
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/boot.s
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/sboot.s
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/decrunch.s
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/rdecrunch.s
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/srdecrunch.s
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/src/boot.rs
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/src/lib.rs
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/src/main.rs
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/cbmcat
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/crc8.py
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/mkrings.py
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/gentest.py
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/testbed_ca65.s
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/testbed.a65
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/rtestbed.a65
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/sea_test.s
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/startaddr
ZIPLIST           += $(TOOLSFOLDER)/$(NCFOLDER)/test/endaddr

PUFOLDER           = pucrunch
ZIPLIST           += $(TOOLSFOLDER)/$(PUFOLDER)/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(PUFOLDER)/smakefile
ZIPLIST           += $(TOOLSFOLDER)/$(PUFOLDER)/pucrunch.c
ZIPLIST           += $(TOOLSFOLDER)/$(PUFOLDER)/pucrunch.h
ZIPLIST           += $(TOOLSFOLDER)/$(PUFOLDER)/uncrunch.asm
ZIPLIST           += $(TOOLSFOLDER)/$(PUFOLDER)/uncrunch-z80.asm
ZIPLIST           += $(TOOLSFOLDER)/$(PUFOLDER)/sa_uncrunch.asm
ZIPLIST           += $(TOOLSFOLDER)/$(PUFOLDER)/cbmcombine.c

SUBSIZERFOLDER     = subsizer-0.7pre1
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/README.txt
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/LICENSE.txt
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/bitfunc.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/bitfunc.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/bits-base.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/bits-base.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/buffer.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/buffer.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/crunch_normal.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/crunch_normal.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/global.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/global.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/histogram.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/histogram.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/match.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/match.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/memory.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/memory.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/message.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/message.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/params.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/params.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/pathfinder.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/pathfinder.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/subsizer.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/universal.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/universal.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/utils.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/utils.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/decrunch_normal_dirty.asm
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/decrunch_normal.asm
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/decrunchers_data.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/decrunchers.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/detect_start.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/detect_start.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/fold.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/fold.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/generate_sfx.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/generate_sfx.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/mach_c64.c
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/mach_c64.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src/sfx/ops6502.h
ZIPLIST           += $(TOOLSFOLDER)/$(SUBSIZERFOLDER)/standalone/decrunch_normal.asm

TCFOLDER           = tinycrunch_v1.2
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/readme.txt
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/bmp.bin
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/bmp.prg
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/tc_boot.prg
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/tc_boot.s
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/tc_decode.s
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/tc_decode_f.s
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/tc_encode.py
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/test/cbmcat
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/test/mkbmp.py
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/test/test.s
ZIPLIST           += $(TOOLSFOLDER)/$(TCFOLDER)/test/testbin.s

WCRUSHFOLDER       = wcrush
ZIPLIST           += $(TOOLSFOLDER)/$(WCRUSHFOLDER)/Makefile
ZIPLIST           += $(TOOLSFOLDER)/$(WCRUSHFOLDER)/LICENSE.TXT
ZIPLIST           += $(TOOLSFOLDER)/$(WCRUSHFOLDER)/README.TXT
ZIPLIST           += $(TOOLSFOLDER)/$(WCRUSHFOLDER)/decrush/decrush.tas
ZIPLIST           += $(TOOLSFOLDER)/$(WCRUSHFOLDER)/wca/main.cpp
#ZIPLIST           += $(TOOLSFOLDER)/$(WCRUSHFOLDER)/wca/wca.exe
ZIPLIST           += $(TOOLSFOLDER)/$(WCRUSHFOLDER)/wcrush/main.cpp
#ZIPLIST           += $(TOOLSFOLDER)/$(WCRUSHFOLDER)/wcrush/wcrush.exe


DISKIMAGES         = $(BUILDFOLDER)/loadertest-c64.d64
DISKIMAGES        += $(BUILDFOLDER)/loadertest-c64.d71
DISKIMAGES        += $(BUILDFOLDER)/loadertest-c64.d81
DISKIMAGES        += $(BUILDFOLDER)/loader-c64.prg
DISKIMAGES        += $(BUILDFOLDER)/install-c64.prg
DISKIMAGES        += $(BUILDFOLDER)/loadersymbols-c64.inc

DISKIMAGES        += $(BUILDFOLDER)/loadertest-c16.d64
DISKIMAGES        += $(BUILDFOLDER)/loadertest-c16.d71
DISKIMAGES        += $(BUILDFOLDER)/loadertest-c16.d81
DISKIMAGES        += $(BUILDFOLDER)/loader-c16.prg
DISKIMAGES        += $(BUILDFOLDER)/install-c16.prg
DISKIMAGES        += $(BUILDFOLDER)/loadersymbols-c16.inc

ZIPLIST           += $(DISKIMAGES)


default: prg

loader:
	$(MAKE) -C src

prg bin binary:
	$(MAKE) -C src prg

prg-c64:
	$(MAKE) -C src PLATFORM=c64 prg

prg-c16:
	$(MAKE) -C src PLATFORM=c16 prg

run:
	$(CD) ..; $(MAKE) -C $(SAMPLESFOLDER)/$(TESTFOLDER) run

$(BUILDFOLDER)/loadertest-c64.d64:
	$(CD) ..; $(MAKE) -C $(SAMPLESFOLDER)/$(TESTFOLDER) all

$(BUILDFOLDER)/loadertest-c16.d64:
	$(CD) ..; $(MAKE) -C $(SAMPLESFOLDER)/$(TESTFOLDER) PLATFORM=c16 all

zip: prg-c64 prg-c16 $(BUILDFOLDER)/loadertest-c64.d64 $(BUILDFOLDER)/loadertest-c16.d64 version.inc
	$(CD) ..; $(ZIP) $(ZIPFILE) $(ZIPLIST)

loadertest: $(BUILDFOLDER)/loadertest-c64.d64 $(BUILDFOLDER)/loadertest-c16.d64 version.inc
	$(CD) ..; $(ZIP_NO_PATHS) $(LOADERTEST) $(DISKIMAGES)

.PHONY: tools cc1541 bitnax bb2 doynax exomizer levelcrush nucrunch pucrunch subsizer
tools: cc1541 bitnax bb2 doynax exomizer levelcrush nucrunch pucrunch subsizer

bitnax:
	$(CC) ../$(TOOLSFOLDER)/bitnax-07a8c67/lz.c -o tools/bitnax-07a8c67/lz

bb2:
	$(MAKE) -C ../$(TOOLSFOLDER)/$(B2FOLDER)

doynax:
	$(CC) ../$(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/lz.c -o ../$(TOOLSFOLDER)/$(DOYNAXLZFOLDER)/lz

exomizer:
	$(MAKE) -C ../$(TOOLSFOLDER)/$(EXOFOLDER)/src

levelcrush:
	$(MAKE) -C ../$(TOOLSFOLDER)/$(WCRUSHFOLDER)

nucrunch:	
	$(MAKE) -C ../$(TOOLSFOLDER)/$(NCFOLDER)

pucrunch:
	$(MAKE) -C ../$(TOOLSFOLDER)/$(PUFOLDER)

subsizer:
	$(MAKE) -C ../$(TOOLSFOLDER)/$(SUBSIZERFOLDER)/src

cc1541:
	$(MAKE) -C ../$(TOOLSFOLDER)/$(CC1541SRCFOLDER)

../.svn/entries:
ifeq ($(VERSION),)
	$(MKDIR) ../.svn
	$(TOUCH) $@
endif

version.inc: ../.svn/entries
ifneq ($(VERSION),)
	$(PRINTF) '%s' '.define REPOSITORY_VERSION "' > $@
	$(PRINTF) '%s' $(VERSION) >> $@
	$(PRINTF) '%s' '"' >> $@
endif
