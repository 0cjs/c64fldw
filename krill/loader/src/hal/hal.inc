
.ifndef _HAL_INC_
_HAL_INC_ = 1

.if PLATFORM = diskio::platform::COMMODORE_16
    .include "hal/hal-c16.inc"
.else; PLATFORM <> diskio::platform::COMMODORE_16
    .include "hal/hal-c64.inc"
.endif; PLATFORM <> diskio::platform::COMMODORE_16


.macro DO_UNINSTALL
            SET_IO_KERNAL
:           SET_FLAGS_N_DATA_V_CLK
            bpl :-
            bvc :-
.endmacro

.macro BRANCH_IF_BLOCK_READY to
            SET_FLAGS_N_DATA_V_CLK
            bmi to
.endmacro

.macro BRANCH_IF_BLOCK_NOT_READY to
            SET_FLAGS_N_DATA_V_CLK
            bpl to
.endmacro

.macro WAIT_FOR_BLOCK_READY
:           BRANCH_IF_BLOCK_NOT_READY :-
.endmacro

.macro WAIT_FOR_BLOCK_READY_KERNAL
:           SET_FLAGS_N_DATA_V_CLK
            bvc :-
.endmacro

.endif; !_HAL_INC_
