c64fldw: Commodore 64 Fast Loaders and DOS Wedges
=================================================

This repository contains disk images and unpacked files (especially
source code) for various freely available floppy disk [fast
loaders][cw-fl] and [DOS wedges][cw-dw] for the Commodore 64.

You may also find further information and references in answers to the
question ["Are there any open-source C64 DOS wedges and fast
loaders?"][rc 12752] on the Retrocomputing Stack Exchange.

### Drive Support

Some fast loaders require changes to the code in the drive (either
downloaded or with a new ROM); others seem to work without this. Drive
emulators that are not "exact" (i.e., fully emulating the drive CPU
and all associated hardware) may not be able to run the drive side of
fast load code, but they may support emulation of it (detecting the
fast load protocol in use but ignoring the uploaded code).

The `doc/` subdirectory in the [sd2iec firmware source][sd2iec-fw]
describes several of the commerical fast load protocols.

- [1541 Ultimate][1541ult] (~€140) does exact drive emulation.
- [SD2IEC][] (~$50; parts ~$15) supports JiffyDOS protocol and it
  seems a few others (see the `doc/` directory in the repo). Epyx Fast
  Load has been confirmed experimentally.



<!-------------------------------------------------------------------->
[cw-dw]: https://www.c64-wiki.com/wiki/DOS_Wedge
[cw-fl]: https://www.c64-wiki.com/wiki/Fast_loader
[rc 12752]: https://retrocomputing.stackexchange.com/q/12752/7208

[1541ult]: https://www.c64-wiki.com/wiki/1541_Ultimate
[sd2iec-fw]: https://sd2iec.de/sd2iec.git
[sd2iec]: https://www.sd2iec.de/
