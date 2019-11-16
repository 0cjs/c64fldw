; build command:
; make prg INSTALL=4000 RESIDENT=0200 ZP=e0

; configuration
config_DECOMPRESSOR             = $08
config_DIRTRACK                 = $12
config_DISABLE_WATCHDOG         = $00
config_END_ADDRESS_API          = $00
config_FILENAME_MAXLENGTH       = $10
config_IDLE_BUS_LOCK            = $00
config_LC_SPEED                 = $06
config_LOAD_COMPD_API           = $01
config_LOAD_RAW_API             = $01
config_LOAD_TO_API              = $00
config_LOAD_UNDER_D000_DFFF     = $00
config_LOAD_VIA_KERNAL_FALLBACK = $00
config_MEM_DECOMP_API           = $00
config_MEM_DECOMP_TO_API        = $00
config_NTSC_COMPATIBILITY       = $00
config_ONLY_1541_AND_COMPATIBLE = $00
config_UNINSTALL_API            = $00

; zeropage
loadaddrlo                      = $e0
loader_zp_first                 = $e0
loadaddrhi                      = $e1
decdestlo                       = $e2
decdesthi                       = $e3
loader_zp_last                  = $f6

; resident
loadraw                         = $0200
loadcompd                       = $020b
openfile                        = $022b
pollblock                       = $0273
drvcodebeg81                    = $0300
dcodinit81                      = $0313
decrunch                        = $0374

; install
drivecode41                     = $4000
drivecode71                     = $47df
drivecode81                     = $4f74
install                         = $5390
