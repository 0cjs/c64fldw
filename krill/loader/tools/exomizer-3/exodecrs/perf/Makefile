#
# Makefile for perf
#
WFLAGS = -std=c89 -Wall -Wstrict-prototypes -D_XOPEN_SOURCE=600 -pedantic
CFLAGS = $(WFLAGS) -O3 -ffast-math -fomit-frame-pointer
LDFLAGS = -s

#CFLAGS = -g -mtune=i686 $(WFLAGS)
#LDFLAGS = -g -mtune=i686

DECR_OBJS = ../exodecrunch.os
DECR_OBJS256 = ../exodecrunch.os256
DECR_OBJSi = ../exodecrunch.osi
DECR_OBJSi256 = ../exodecrunch.osi256
CRUNCHER = ../../src/exomizer mem -q $<@0x1010 -o $@
CRUNCHER256 = ../../src/exomizer mem -M256 -q $<@0x1010 -o $@

TEST_OBJS = ../testrun.o ../../src/6502emu.o ../../src/exo_util.o ../../src/log.o ../../src/areatrace.o ../../src/vec.o ../../src/membuf_io.o ../../src/membuf.o

OBJS = ../main.os $(DECR_OBJS)
OBJSi = ../main.os $(DECR_OBJSi)
OBJS256 = ../main.os $(DECR_OBJS256)
OBJSi256 = ../main.os $(DECR_OBJSi256)


.PHONY: build %.test %.stat
.PRECIOUS: %.cru %.cru256 %.prg $(TEST_OBJS) %.cru.out %.cru256.out
#.INTERMEDIATE: %.cru.out

build: data.test
#
# make <prefix>.stat shows statistics for all files that match <prefix>*.raw
# together with average statistics for all files that matched.
#
# use .stat for jsr getbit decruncher
# use .stati for inlined getbit decruncher
# use .stat256 for a decruncher with sequence length <= 256
# use .stati for inlined getbit decruncher with sequence length <= 256
#
%.stat:	test.prg ../testrun
	@$(MAKE) $(sort $(patsubst %.raw,%.cru,$(wildcard $**.raw)))
	@../testrun test.prg $(sort $(patsubst %.raw,%.cru,$(wildcard $**.raw)))

%.stati:	test.prgi ../testrun
	@$(MAKE) $(sort $(patsubst %.raw,%.crui,$(wildcard $**.raw)))
	@../testrun test.prgi $(sort $(patsubst %.raw,%.crui,$(wildcard $**.raw)))

%.stat256:	test.prg256 ../testrun
	@$(MAKE) $(sort $(patsubst %.raw,%.cru256,$(wildcard $**.raw)))
	@../testrun test.prg256 $(sort $(patsubst %.raw,%.cru256,$(wildcard $**.raw)))

%.stati256:	test.prgi256 ../testrun
	@$(MAKE) $(sort $(patsubst %.raw,%.crui256,$(wildcard $**.raw)))
	@../testrun test.prgi256 $(sort $(patsubst %.raw,%.crui256,$(wildcard $**.raw)))

../testrun: $(TEST_OBJS)
	@echo "Linking $@"
	@$(CC) $(LDFLAGS) -o $@ $(TEST_OBJS)

%.test: %.cru.out %.raw
	cmp $< $*.raw

%.testi: %.crui.out %.raw
	cmp $< $*.raw

%.test256: %.cru256.out %.raw
	cmp $< $*.raw

%.testi256: %.crui256.out %.raw
	cmp $< $*.raw

%.cru.out: test.prg %.cru ../testrun
	../testrun $< $*.cru

%.cru256.out: test.prg256 %.cru256 ../testrun
	../testrun $< $*.cru256

%.crui.out: testi.prg %.crui ../testrun
	../testrun $< $*.crui

%.crui256.out: test.prgi256 %.crui256 ../testrun
	../testrun $< $*.crui256

%.prg: $(OBJS) ../c64.cfg
	ld65 $(OBJS) -o $@ -C../c64.cfg

%.prgi: $(OBJSi) ../c64.cfg
	ld65 $(OBJSi) -o $@ -C../c64.cfg

%.prg256: $(OBJS256) ../c64.cfg
	ld65 $(OBJS256) -o $@ -C../c64.cfg

%.prgi256: $(OBJSi256) ../c64.cfg
	ld65 $(OBJSi256) -o $@ -C../c64.cfg

%.os: %.s
	ca65 $< -o $@

%.osi: %.s
	ca65 -DINLINE_GET_BITS=1 $< -o $@

%.os256: %.s
	ca65 -DMAX_SEQUENCE_LENGTH_256=1 $< -o $@

%.osi256: %.s
	ca65 -DINLINE_GET_BITS=1 -DMAX_SEQUENCE_LENGTH_256=1 $< -o $@

clean:
	$(RM) *.out *.cru* test*.prg*

%.cru: %.raw
	$(CRUNCHER)

%.cru256: %.raw
	$(CRUNCHER256)

%.crui: %.cru
	cp $< $@

%.crui256: %.cru256
	cp $< $@

%.o:	%.c
	@echo "Compiling $<"
	@$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<

%:	%.o
	@$(CC) $(LDFLAGS) $< -o $@

# cancel built in rule that disturb things
%.out: %
