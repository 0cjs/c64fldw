
ifeq ($(PLATFORM),)
_PLATFORM_   = c64
else ifeq ($(PLATFORM),c116)
_PLATFORM_   = c16
else ifeq ($(PLATFORM),plus4)
_PLATFORM_   = c16
else
_PLATFORM_   = $(PLATFORM)
endif

ifeq ($(NO_VICE),)
NO_VICE      = 0
endif


ifneq ($(_PLATFORM_),c64)
ifneq ($(_PLATFORM_),c16)
$(error invalid platform $(_PLATFORM_) specified)
endif
endif


ARCH         = $(shell uname | tr "[a-z]" "[A-Z]" | tr -c -d "[A-Z]")

ifneq ($(findstring CYGWINNT,$(ARCH)),)
    ifeq (CYGWINNT,$(ARCH))
ARCH         = WIN32
    else
ARCH         = WIN64
    endif
endif
ifneq ($(findstring DARWIN,$(ARCH)),)
ARCH         = MACOSX
endif


ifeq ($(_PLATFORM_),c16)
    ifneq ($(NO_VICE),0)
        ifneq ($(findstring WIN,$(ARCH)),)
USE_PLUS4EMU = 0
USE_YAPE     = 1
        else
USE_PLUS4EMU = 1
USE_YAPE     = 0
        endif
    else
USE_PLUS4EMU = 0
USE_YAPE     = 0
    endif
else
USE_PLUS4EMU = 0
USE_YAPE     = 0
endif


ifeq ($(_PLATFORM_),c16)
    ifeq ($(ARCH),MACOSX)
    # MacOSX, these programs must be installed as applications
VICE         = xplus4
PLUS4EMU     = /Applications/plus4emu.app/Contents/MacOS/plus4emu
    else    
VICE         = xplus4
PLUS4EMU     = plus4emu
        ifeq ($(ARCH),WIN64)
YAPE         = YapeWin64
        else
YAPE         = Yape
        endif
    endif
else
VICE         = x64
endif

ifneq ($(USE_PLUS4EMU),0)
EMU          = $(PLUS4EMU) -disk
else
EMU          = $(VICE) -drive8type 1541 -drive9type 0 -autostart
endif


ECHO         = echo
PRINTF       = printf

AS           = ca65
LD           = ld65
PU           = pucrunch
CC1541       = ../../tools/cc1541

MKDIR        = mkdir -p
RM           = rm -f
ifeq ($(ARCH),MACOSX)
RMDIR        = rmdir # XXX TODO xargs to remove .DS_Store
else
RMDIR        = rmdir
endif
CAT          = cat


.PHONY: default loader assemble link compress diskimage run clean distclean wipe
.PHONY: tellarch


BUILDDIR     = ../../build
INTERMDIR    = ../../build/interm
LOADER_SRC   = ../../src
LOADER       = $(BUILDDIR)/loader-$(_PLATFORM_).a

RESOURCESDIR = ../resources
PIC1         = $(INTERMDIR)/pic1.bin
PIC2         = $(INTERMDIR)/pic2.bin

ifeq ($(_PLATFORM_),c16)
STARTTRACK   = 25
FIRSTSECTOR  = 2
INTERLEAVE   = 3
DEMO         = $(RESOURCESDIR)/threeve.prg
DEMONAME     = "THREEVE"
else
STARTTRACK   = 18
FIRSTSECTOR  = 3
INTERLEAVE   = -3
DEMO         = $(RESOURCESDIR)/oxyron_oneder.prg
DEMONAME     = "ONEDER      /OXY"
endif

NAME         = standalone

SOURCE       = $(NAME).s
LOADERCFG    = loaderconfig.inc
ASSEMBLE     = $(INTERMDIR)/$(NAME)-$(_PLATFORM_).o
LINK         = $(INTERMDIR)/$(NAME)-uncompressed-$(_PLATFORM_).prg
COMPRESS     = $(INTERMDIR)/$(NAME)-$(_PLATFORM_).prg
DISKIMAGE    = $(BUILDDIR)/$(NAME)-$(_PLATFORM_).d64

AS_FLAGS     = -I ../../../shared -I ../../include -I $(LOADER) -D EXTCONFIGPATH --cpu 6502X
PU_FLAGS     = -d -l 0x2000 -x 0x2000 -i 0 


default: diskimage


tellarch:
	@$(ECHO) $(ARCH)


loader: $(LOADER)

$(LOADER): $(LOADERCFG)
	make -C $(LOADER_SRC) EXTCONFIGPATH=../samples/$(NAME) lib


assemble: $(ASSEMBLE)

$(ASSEMBLE): $(SOURCE) $(LOADERCFG)
	$(MKDIR) $(BUILDDIR)
	$(MKDIR) $(INTERMDIR)
ifeq ($(_PLATFORM_),c64)
	$(AS) $(AS_FLAGS) -t c64 -D PLATFORM=64 -o $@ $<
else
	$(AS) $(AS_FLAGS) -t c16 -D PLATFORM=16 -o $@ $<
endif


link: $(LINK)

$(LINK): Linkfile-$(_PLATFORM_) $(ASSEMBLE) $(LOADER)
	$(LD) -o $@ -C $^


compress: $(COMPRESS)

$(COMPRESS): $(LINK)
ifeq ($(_PLATFORM_),c64)
	$(PU) $(PU_FLAGS) -c64 -g 0x37 $^ $@
else
	$(PU) $(PU_FLAGS) -c16 $^ $@
endif


diskimage: $(DISKIMAGE)

$(DISKIMAGE): $(COMPRESS) $(DEMO) $(PIC1) $(PIC2) $(CC1541)
	$(CC1541) -n "NORMAL IS BORING" -i PLUSH -S $(INTERLEAVE) -t \
	 \
	 -f "STANDALONE" -s 8 -w $(COMPRESS) \
	 \
	 -f $(DEMONAME) -r $(STARTTRACK) -F $(FIRSTSECTOR) -w $(DEMO) \
	 -f PIC1 -r 18 -E -w $(PIC1) \
	 -f PIC2 -r 18 -E -w $(PIC2) \
	 -f LOOPFILE -l $(DEMONAME) \
	 \
	$@

ifneq ($(USE_YAPE),0)
run: $(DISKIMAGE)
	$(YAPE) "..\..\build\$(NAME)-$(_PLATFORM_).d64"
else
run: $(DISKIMAGE)
ifneq ($(USE_PLUS4EMU),0)
	$(EMU) $^
else
	$(EMU) $^:standalone
endif
endif


$(INTERMDIR)/%.bin: $(RESOURCESDIR)/%.bin
	$(PRINTF) '\000\140' | $(CAT) - $? > $@ # octal 140 = hex 60

$(CC1541): $(CC1541)_source/cc1541.cpp
	$(MAKE) -C $(CC1541)_source

clean:
	-$(RM) $(ASSEMBLE) $(LINK) $(COMPRESS) $(DISKIMAGE)
	-$(RM) -rf $(INTERMDIR)
	-$(RM) $(BUILDDIR)/loader-c64.a
	-$(RM) $(BUILDDIR)/loader-c16.a
	-$(RMDIR) $(BUILDDIR)

distclean:
	-$(MAKE) -C $(LOADER_SRC) clean
	$(MAKE) -C $(CC1541)_source clean

wipe: distclean clean
