
INSTALL     ?= 4000
RESIDENT    ?= 0200
ZP          ?= e0

export INSTALL RESIDENT ZP

ifeq ($(PLATFORM),)
_PLATFORM_   = c64
else ifeq ($(PLATFORM),c116)
_PLATFORM_   = c16
else ifeq ($(PLATFORM),plus4)
_PLATFORM_   = c16
else
_PLATFORM_   = $(PLATFORM)
endif

ifneq ($(_PLATFORM_),c64)
ifneq ($(_PLATFORM_),c16)
$(error invalid platform $(_PLATFORM_) specified)
endif
endif


TOP         ?= .

SVNVERSION   = svnversion
ifneq ($(wildcard ../../.svn/format),)
VERSION      = $(shell $(SVNVERSION) | tr -d [:cntrl:])
else
VERSION      =
endif

CD           = cd
ECHO         = echo
PRINTF       = printf
CAT          = cat
CP           = cp
MV           = mv
RM           = rm -f
MKDIR        = mkdir
RMDIR        = rmdir
TOUCH        = touch

AS           = ca65
LD           = ld65
AR           = ar65
VICE         = x64
C1541        = c1541

PU           = pucrunch
EXO          = exomizer

PERL         = perl
ZIP          = zip -9

INCDIR       = $(TOP)/../../shared
LDRINC       = $(TOP)/../include

DECOMPDIR    = $(TOP)/decompress
DRIVESDIR    = $(TOP)/drives
HALDIR       = $(TOP)/hal

BUILDDIR     = $(TOP)/../build
INTERMDIR    = $(TOP)/../build/interm

ifneq ($(EXTCONFIGPATH),)
CONFIG       = $(EXTCONFIGPATH)/loaderconfig.inc
else
CONFIG       = $(LDRINC)/config.inc
endif

INSTALLDEPS  = $(CONFIG) $(BUILDDIR) $(INTERMDIR) \
               $(DRIVESDIR)/drivecode-common.inc $(DRIVESDIR)/drivecode1541.s $(DRIVESDIR)/drivecode1571.s $(DRIVESDIR)/drivecode1581.s \
               $(HALDIR)/hal.inc $(HALDIR)/hal-c64.inc $(HALDIR)/hal-c16.inc \
               install.s \
               $(LDRINC)/loader.inc \
               $(TOP)/../version.inc Makefile

RESIDENTDEPS = $(CONFIG) $(BUILDDIR) $(INTERMDIR) \
               $(DECOMPDIR)/bitnaxdecomp.s \
               $(DECOMPDIR)/b2decomp.s \
               $(DECOMPDIR)/doynaxdecomp.s \
               $(DECOMPDIR)/exodecomp.s \
               $(DECOMPDIR)/lcdecomp.s \
               $(DECOMPDIR)/ncdecomp.s \
               $(DECOMPDIR)/pudecomp.s \
               $(DECOMPDIR)/subsizerdecomp.s \
               $(DECOMPDIR)/tcdecomp.s \
               $(HALDIR)/hal.inc $(HALDIR)/hal-c64.inc $(HALDIR)/hal-c16.inc \
               resident.s \
               $(LDRINC)/loader.inc \
               $(TOP)/../version.inc Makefile

BINDEPS      = $(BUILDDIR)/loadersymbols-$(_PLATFORM_).inc $(BUILDDIR)/loader-$(_PLATFORM_).prg $(CONFIG)

AS_FLAGS     = --cpu 6502X # undocumented opcodes
ifeq ($(_PLATFORM_),c16)
AS_FLAGS    += -t c16 -D PLATFORM=16
else
AS_FLAGS    += -t c64 -D PLATFORM=64
endif
AS_FLAGS    += -I $(TOP)/. -I $(INCDIR) -I $(LDRINC)
ifneq ($(EXTCONFIGPATH),)
AS_FLAGS    += -D EXTCONFIGPATH -I $(EXTCONFIGPATH)
endif
ifneq ($(DEBUG),)
AS_FLAGS    += -g
endif


default: prg

all: lib prg

# use these targets to build loader.prg and install.prg
prg bin binary: $(BINDEPS)
	@$(ECHO) "Usage: $(MAKE) prg INSTALL=<install hexaddress> RESIDENT=<resident hexaddress> ZP=<zp hexaddress>"
	@$(ECHO) "INSTALL=\$$$(INSTALL)"
	@$(ECHO) "RESIDENT=\$$$(RESIDENT)"
	@$(ECHO) "ZP=\$$$(ZP)"
	$(CAT) $(BUILDDIR)/loadersymbols-$(_PLATFORM_).inc

prgzip binzip binaryzip: $(BUILDDIR)/loader-$(_PLATFORM_).zip


lib: $(BUILDDIR)/loader-$(_PLATFORM_).a


../../.svn/entries:
ifeq ($(VERSION),)
	$(MKDIR) ../../.svn
	$(TOUCH) $@
endif

$(TOP)/../version.inc: ../../.svn/entries
	$(MAKE) -C $(TOP)/.. version.inc


# directory targets

$(BUILDDIR):
	$(MKDIR) $(BUILDDIR)

$(INTERMDIR):
	$(MKDIR) $(INTERMDIR)


# object targets

$(INTERMDIR)/loader-$(_PLATFORM_).o: $(RESIDENTDEPS)
	$(AS) $(AS_FLAGS) -o $@ resident.s

$(INTERMDIR)/install-$(_PLATFORM_).o: $(INSTALLDEPS)
	$(AS) $(AS_FLAGS) -I $(INTERMDIR) -o $@ install.s


# binary targets

.PHONY: $(INTERMDIR)/binary.link $(BUILDDIR)/loader-$(_PLATFORM_).prg $(INTERMDIR)/install-nonreloc-$(_PLATFORM_).o $(BUILDDIR)/install-$(_PLATFORM_).prg $(INTERMDIR)/loader-nonreloc-$(_PLATFORM_).o $(INTERMDIR)/loader.map $(BUILDDIR)/loadersymbols-$(_PLATFORM_).inc

$(INTERMDIR)/loader-nonreloc-$(_PLATFORM_).o: $(RESIDENTDEPS)
	$(AS) $(AS_FLAGS) -D RESIADDR=0x$(RESIDENT) -o $@ resident.s

$(INTERMDIR)/install-nonreloc-$(_PLATFORM_).o: $(RESIDENTDEPS)
	$(AS) $(AS_FLAGS) -D INSTADDR=0x$(INSTALL) -o $@ install.s

$(INTERMDIR)/binary.link: Makefile
	$(PERL) -e 'printf "MEMORY\
	{\
	 ZPRAM: start = \$$$(ZP), size = \$$%x;\
	 INSTALLRAM: start = \$$%x, size = \$$%x, file = \"../build/install-$(_PLATFORM_).prg\";\
	 RESIDENTRAM: start = \$$%x, size = \$$%x, file = \"../build/loader-$(_PLATFORM_).prg\";\
	}\
	\
	SEGMENTS\
	{\
	 DISKIO_ZP: load = ZPRAM, type = zp;\
	 DISKIO: load = RESIDENTRAM;\
	 DISKIO_INSTALL: load = INSTALLRAM;\
	}", 0x0100 - 0x$(ZP), 0x$(INSTALL) - 2, 0x10002 - 0x$(INSTALL), 0x$(RESIDENT) - 2, 0x10002 - 0x$(RESIDENT)' > $@

$(BUILDDIR)/loader-$(_PLATFORM_).prg: $(INTERMDIR)/loader-nonreloc-$(_PLATFORM_).o $(INTERMDIR)/install-nonreloc-$(_PLATFORM_).o $(INTERMDIR)/binary.link
	$(LD) -C $(INTERMDIR)/binary.link -vm -m $(INTERMDIR)/loader-$(_PLATFORM_).map $(INTERMDIR)/loader-nonreloc-$(_PLATFORM_).o $(INTERMDIR)/install-nonreloc-$(_PLATFORM_).o
	$(RM) a.out

$(BUILDDIR)/install-$(_PLATFORM_).prg: $(BUILDDIR)/loader-$(_PLATFORM_).prg

$(INTERMDIR)/loader-$(_PLATFORM_).map: $(BUILDDIR)/install-$(_PLATFORM_).prg

$(BUILDDIR)/loadersymbols-$(_PLATFORM_).inc: $(INTERMDIR)/loader-$(_PLATFORM_).map
	$(PERL) make-loadersymbolsinc.pl $^ > $@

$(BUILDDIR)/loader-$(_PLATFORM_).zip: $(BINDEPS)
	$(CD) $(BUILDDIR) \
	 && $(CP) $(BUILDDIR)/loader-$(_PLATFORM_).prg $(INTERMDIR)/loader.prg && $(ZIP) -j $@ $(INTERMDIR)/loader.prg \
	 && $(CP) $(BUILDDIR)/install-$(_PLATFORM_).prg $(INTERMDIR)/install.prg && $(ZIP) -j $@ $(INTERMDIR)/install.prg \
	 && $(CP) $(BUILDDIR)/loadersymbols-$(_PLATFORM_).inc $(INTERMDIR)/loadersymbols.inc && $(ZIP) -j $@ $(INTERMDIR)/loadersymbols.inc


# loader-$(_PLATFORM_).a library target

$(BUILDDIR)/loader-$(_PLATFORM_).a: $(INTERMDIR)/loader-$(_PLATFORM_).o $(INTERMDIR)/install-$(_PLATFORM_).o
	$(AR) a $@ $^


clean:
	-$(RM) \
	 $(INTERMDIR)/* \
	 $(BUILDDIR)/loader-c64.a \
	 $(BUILDDIR)/loader-c64.prg $(BUILDDIR)/install-c64.prg $(BUILDDIR)/loadersymbols-c64.inc \
	 $(BUILDDIR)/loader-c16.a \
	 $(BUILDDIR)/loader-c16.prg $(BUILDDIR)/install-c16.prg $(BUILDDIR)/loadersymbols-c16.inc \
	-$(RMDIR) $(INTERMDIR)
