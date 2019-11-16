SJLOAD Documentation
====================

This is copied from the [SJLOAD] page on the C64 Wiki.

### About

SJLOAD is a C64/C64DTV software fastloader. Its main difference from normal
fastloaders is that it only works with Jiffy-enabled drives and uses the
JiffyDOS protocol. This makes it handy for people who have a Jiffy-enabled
drive (also new hardware such as SD2IEC) but do not want to make the
hardware changes necessary for exchanging the C64 kernal.

SJLOAD speed is a bit higher than normal Jiffy since SJLOAD uses the same
protocol but a different implementation (it disables the VICII during load
etc.). With an SD2IEC, SJLOAD is about 15% faster than a normal Jiffy
kernal. Note that SJLOAD does not feature the command wedge and function
key shortcuts known from a Jiffy kernal.

SJLOAD is loosely based on VDOS (1986) by Edward Carroll. However, the fast
loading routines have been replaced completely by 1570 (contact).

### Usage

- `LOAD"!",8,1` - autostart SJLOAD
- `LOAD"!*PROGRAM",8,1` - autostart SJLOAD, fastload PROGRAM
  - Since 1581 (compatible) drives do not stop filename matching at "\*",
    use `LOAD"!=PROGRAM",8,1` on these drives.
- `LOAD"!",8:REM CHANGE DISK:RUN` - save autostarting SJLOAD to (new) disk
- `VERIFY` - read floppy status - VERIFY"",9 reads status from drive 9
- `VERIFY"command"` - send floppy command
- `VERIFY"$"` - display directory - scroll to entry and press
  `SHIFT`+`RUN/STOP` to load and run program

If a program crashes on RUN after loading it using SJLOAD, try
`RUN/STOP`+`RESTORE`  (this disables SJLOAD) before RUNning it.

### Status

SJLOAD is far from completed:

- Loading files bigger than 195 blocks makes the C64 crash
- Loading files below $0801 is not supported
- Only few tests have been made (with a C64DTV and an SD2IEC and in VICE)
- No IEC timing fixes for the C64DTV are done so the J1541/J1571+C64DTV
  combo does not work with SJLOAD (J1581 and SD2IEC do work). See here for
  a DTV solution. I (1570) probably will not work on SJLOAD any more. The
  source is there, fix it! :-)



<!-------------------------------------------------------------------->
[SJLOAD]: https://www.c64-wiki.com/wiki/SJLOAD
