/* autogenerated by make_exe.pl, do not edit */
#include <stdint.h>
#include "decrunchers.h"

#define XOR_MAGIC 0xa7

static uint8_t default_header[] = {
    0xb2,0xaf,0xa7,0xa7,0x39,0x95,0x97,0x91,
    0x94,0x87,0xf4,0xf2,0xe5,0xf4,0xee,0xfd,
    0xe2,0xf5,0x86,0xa7,0x07,0xa7
};
static uint8_t default_tail[] = {
    0x05,0xa3,0x87,0xdd,0xa6,0x3e,0x0b,0xa4,
    0x3f,0x8e,0xa8,0x77,0xaf,0x3e,0x93,0xa4,
    0x3e,0xcf,0xa4,0x57,0xbb,0x0e,0xa7,0x22,
    0x5e,0x19,0x0c,0xa4,0x9f,0x8d,0x81,0x5e,
    0x6d,0xb7,0x5d,0xde,0x94,0xa4,0x3e,0x93,
    0xa4,0x02,0x5e,0xde,0xc0,0xa4,0x3e,0xcf,
    0xa4,0x6f,0x67,0xe3,0x77,0x6d,0x07,0xa7,
    0xeb,0xaf,0xa6
};
static uint8_t default_decruncher[] = {
    0xa7,0xa7,0x27,0x97,0xa7,0xb7,0x87,0xa5,
    0xa3,0xa3,0xa3,0x02,0x5a,0x77,0xa5,0x61,
    0x59,0x61,0x5a,0x87,0x07,0xa6,0x36,0x5a,
    0x87,0x32,0xa6,0x17,0x49,0x6f,0x67,0xb7,
    0x57,0xa2,0x87,0x32,0xa6,0x37,0x51,0x19,
    0x0c,0xa4,0x87,0xdd,0xa6,0xde,0x94,0xa4,
    0x22,0x5e,0x0f,0xee,0x58,0xc2,0x5a,0x22,
    0x5a,0x17,0xa5,0x61,0x59,0x67,0xa7,0x57,
    0xd6,0x67,0xa3,0x37,0xa5,0x07,0xa4,0x19,
    0xa3,0xa6,0x87,0xdd,0xa6,0xde,0xa7,0xa6,
    0x0f,0x19,0x1b,0xa4,0x87,0xdd,0xa6,0xde,
    0xe3,0xa4,0x0d,0x02,0x5b,0xde,0xdf,0xa4,
    0x0f,0x9f,0x2d,0xc2,0x5a,0x22,0x5d,0x3f,
    0xc2,0x59,0x22,0x5c,0x03,0x5e,0x57,0xa0,
    0x16,0x5d,0x36,0x5a,0x2f,0x77,0x5e,0x16,
    0x5d,0x36,0x5a,0x37,0x3c,0x0e,0xa7,0x22,
    0x5b,0x47,0xa6,0x37,0xb5,0xa1,0x58,0x77,
    0xaf,0xef,0x87,0x07,0xa6,0x8d,0x22,0x58,
    0xcf,0x8d,0x81,0x5b,0x6d,0x77,0x49,0xc7,
    0xa1,0x58,0x77,0xa1,0x87,0x07,0xa6,0x8d,
    0x22,0x58,0xc7,0x0a,0x0b,0xa6,0x77,0xa4,
    0x69,0x0a,0xa6,0x69,0x0b,0xa6,0x0a,0xa7,
    0xa7,0xc7
};
static FixEntry ft_default_header[] = {
    {ftEnd, 0}
};

static FixEntry ft_default_tail[] = {
    {ftEnd, 0}
};

static FixEntry ft_default_decruncher[] = {
    {ftDecruncher, 0x00fd},
    {ftDestEnd, 0x00fd},
    {ftBufZp, 0x00ff},
    {ftEndMarkerMinusOne, 0x013b},
    {ftSrcEnd, 0x01ac},
    {ftEnd, 0}
};

static uint8_t nocli_header[] = {
    0xb2,0xaf,0xa7,0xa7,0x39,0x95,0x97,0x91,
    0x94,0x87,0xf4,0xf2,0xe5,0xf4,0xee,0xfd,
    0xe2,0xf5,0x86,0xa7,0x07,0xa7
};
static uint8_t nocli_tail[] = {
    0x05,0xa3,0x87,0xdd,0xa6,0x3e,0x0b,0xa4,
    0x3f,0x8e,0xa8,0x77,0xaf,0x3e,0x93,0xa4,
    0x3e,0xcf,0xa4,0x57,0xbb,0x0e,0xa7,0x22,
    0x5e,0x19,0x0c,0xa4,0x9f,0x8d,0x81,0x5e,
    0x6d,0xb7,0x5d,0xde,0x94,0xa4,0x3e,0x93,
    0xa4,0x02,0x5e,0xde,0xc0,0xa4,0x3e,0xcf,
    0xa4,0x6f,0x67,0xe3,0x77,0x6d,0x07,0xa7,
    0xeb,0xaf,0xa6
};
static uint8_t nocli_decruncher[] = {
    0xa7,0xa7,0x27,0x97,0xa7,0xb7,0x87,0xa5,
    0xa3,0xa3,0xa3,0x02,0x5a,0x77,0xa5,0x61,
    0x59,0x61,0x5a,0x87,0x07,0xa6,0x36,0x5a,
    0x87,0x32,0xa6,0x17,0x49,0x6f,0x67,0xb7,
    0x57,0xa2,0x87,0x32,0xa6,0x37,0x51,0x19,
    0x0c,0xa4,0x87,0xdd,0xa6,0xde,0x94,0xa4,
    0x22,0x5e,0x0f,0xee,0x58,0xc2,0x5a,0x22,
    0x5a,0x17,0xa5,0x61,0x59,0x67,0xa7,0x57,
    0xd6,0x67,0xa3,0x37,0xa5,0x07,0xa4,0x19,
    0xa3,0xa6,0x87,0xdd,0xa6,0xde,0xa7,0xa6,
    0x0f,0x19,0x1b,0xa4,0x87,0xdd,0xa6,0xde,
    0xe3,0xa4,0x0d,0x02,0x5b,0xde,0xdf,0xa4,
    0x0f,0x9f,0x2d,0xc2,0x5a,0x22,0x5d,0x3f,
    0xc2,0x59,0x22,0x5c,0x03,0x5e,0x57,0xa0,
    0x16,0x5d,0x36,0x5a,0x2f,0x77,0x5e,0x16,
    0x5d,0x36,0x5a,0x37,0x3c,0x0e,0xa7,0x22,
    0x5b,0x47,0xa6,0x37,0xb5,0xa1,0x58,0x77,
    0xaf,0xef,0x87,0x07,0xa6,0x8d,0x22,0x58,
    0xcf,0x8d,0x81,0x5b,0x6d,0x77,0x49,0xc7,
    0xa1,0x58,0x77,0xa1,0x87,0x07,0xa6,0x8d,
    0x22,0x58,0xc7,0x0a,0x0b,0xa6,0x77,0xa4,
    0x69,0x0a,0xa6,0x69,0x0b,0xa6,0x0a,0xa7,
    0xa7,0xc7
};
static FixEntry ft_nocli_header[] = {
    {ftEnd, 0}
};

static FixEntry ft_nocli_tail[] = {
    {ftEnd, 0}
};

static FixEntry ft_nocli_decruncher[] = {
    {ftDecruncher, 0x00fd},
    {ftDestEnd, 0x00fd},
    {ftBufZp, 0x00ff},
    {ftEndMarkerMinusOne, 0x013b},
    {ftSrcEnd, 0x01ac},
    {ftEnd, 0}
};

static uint8_t dirty_default_header[] = {
    0xb2,0xaf,0xa7,0xa7,0x39,0x95,0x97,0x91,
    0x94,0x87,0xf4,0xf2,0xe5,0xf4,0xee,0xfd,
    0xe2,0xf5,0x86,0xa7,0x07,0xa7
};
static uint8_t dirty_default_tail[] = {
    0x0e,0x47,0xa1,0x18,0x77,0xaf,0x0f,0x87,
    0x63,0xa7,0x8d,0x22,0x18,0x3f,0x8d,0x17,
    0x56,0x32,0xdd,0x2d,0x8e,0xa8,0x0f,0x57,
    0xb3,0x0e,0xa7,0x22,0x19,0x13,0xde,0x9f,
    0x8d,0x81,0x19,0x2f,0xb7,0x5d,0xd2,0xa6,
    0x0f,0x02,0x19,0xd2,0x92,0x32,0x91,0x33,
    0xa5,0x4f,0x47,0xe3,0x77,0x6d,0x07,0xa7,
    0xeb,0x46,0xa7
};
static uint8_t dirty_default_decruncher[] = {
    0x27,0x2b,0x47,0x46,0x45,0x02,0x6a,0x77,
    0xa5,0x61,0x69,0x61,0x6a,0x0a,0xa7,0xa7,
    0xc7,0x07,0xa7,0x57,0xae,0x1e,0xa7,0xa7,
    0x3e,0xa7,0xa7,0x2f,0x77,0x50,0x16,0x72,
    0x37,0xac,0x02,0x7f,0x77,0xa5,0x61,0x7e,
    0x61,0x7f,0x87,0x63,0xa7,0x36,0x7f,0xa1,
    0x18,0x77,0xa1,0x87,0x63,0xa7,0x8d,0x22,
    0x18,0x17,0x40,0x05,0x57,0x4f,0x57,0xaa,
    0xa1,0x18,0x77,0xa1,0x87,0x63,0xa7,0x8d,
    0x22,0x18,0x37,0x56,0xbf,0x3f,0x13,0x2e,
    0x57,0xb7,0xa1,0x18,0x77,0xaf,0xef,0x87,
    0x63,0xa7,0x8d,0x22,0x18,0xcf,0x8d,0x2f,
    0x77,0x57,0xd2,0xb6,0x22,0x76,0x0d,0xee,
    0x58,0xc2,0x7f,0x22,0x7f,0x17,0xa5,0x61,
    0x7e,0x47,0xa7,0x57,0xef,0x47,0xa3,0x37,
    0xa5,0x05,0xa4,0x12,0x67,0xa1,0x18,0x77,
    0xaf,0x0f,0x87,0x63,0xa7,0x8d,0x22,0x18,
    0x3f,0x8d,0x17,0x56,0x0d,0x0e,0xa7,0x22,
    0x19,0x13,0x2d,0x57,0xb5,0xa1,0x18,0x77,
    0xaf,0xef,0x87,0x63,0xa7,0x8d,0x22,0x18,
    0xcf,0x8d,0x81,0x19,0x2f,0x77,0x49,0xd2,
    0xb5,0x37,0xa5,0x41,0x19,0x9f,0xc2,0x7f,
    0x22,0x72,0x02,0x19,0xd2,0xe1,0xc2,0x7e,
    0x22,0x71,0xeb,0x77,0xa7
};
static FixEntry ft_dirty_default_header[] = {
    {ftEnd, 0}
};

static FixEntry ft_dirty_default_tail[] = {
    {ftEnd, 0}
};

static FixEntry ft_dirty_default_decruncher[] = {
    {ftDecruncher, 0x00bf},
    {ftBufZp, 0x00bf},
    {ftSrcEnd, 0x00cd},
    {ftDestEnd, 0x00d8},
    {ftEndMarkerMinusOne, 0x0131},
    {ftEnd, 0}
};

static uint8_t dirty_nocli_header[] = {
    0xb2,0xaf,0xa7,0xa7,0x39,0x95,0x97,0x91,
    0x94,0x87,0xf4,0xf2,0xe5,0xf4,0xee,0xfd,
    0xe2,0xf5,0x86,0xa7,0x07,0xa7
};
static uint8_t dirty_nocli_tail[] = {
    0x0e,0x47,0xa1,0x18,0x77,0xaf,0x0f,0x87,
    0x63,0xa7,0x8d,0x22,0x18,0x3f,0x8d,0x17,
    0x56,0x32,0xdd,0x2d,0x8e,0xa8,0x0f,0x57,
    0xb3,0x0e,0xa7,0x22,0x19,0x13,0xde,0x9f,
    0x8d,0x81,0x19,0x2f,0xb7,0x5d,0xd2,0xa6,
    0x0f,0x02,0x19,0xd2,0x92,0x32,0x91,0x33,
    0xa5,0x4f,0x47,0xe3,0x77,0x6d,0x07,0xa7,
    0xeb,0x46,0xa7
};
static uint8_t dirty_nocli_decruncher[] = {
    0x27,0x2b,0x47,0x46,0x45,0x02,0x6a,0x77,
    0xa5,0x61,0x69,0x61,0x6a,0x0a,0xa7,0xa7,
    0xc7,0x07,0xa7,0x57,0xae,0x1e,0xa7,0xa7,
    0x3e,0xa7,0xa7,0x2f,0x77,0x50,0x16,0x72,
    0x37,0xac,0x02,0x7f,0x77,0xa5,0x61,0x7e,
    0x61,0x7f,0x87,0x63,0xa7,0x36,0x7f,0xa1,
    0x18,0x77,0xa1,0x87,0x63,0xa7,0x8d,0x22,
    0x18,0x17,0x40,0x05,0x57,0x4f,0x57,0xaa,
    0xa1,0x18,0x77,0xa1,0x87,0x63,0xa7,0x8d,
    0x22,0x18,0x37,0x56,0xbf,0x3f,0x13,0x2e,
    0x57,0xb7,0xa1,0x18,0x77,0xaf,0xef,0x87,
    0x63,0xa7,0x8d,0x22,0x18,0xcf,0x8d,0x2f,
    0x77,0x57,0xd2,0xb6,0x22,0x76,0x0d,0xee,
    0x58,0xc2,0x7f,0x22,0x7f,0x17,0xa5,0x61,
    0x7e,0x47,0xa7,0x57,0xef,0x47,0xa3,0x37,
    0xa5,0x05,0xa4,0x12,0x67,0xa1,0x18,0x77,
    0xaf,0x0f,0x87,0x63,0xa7,0x8d,0x22,0x18,
    0x3f,0x8d,0x17,0x56,0x0d,0x0e,0xa7,0x22,
    0x19,0x13,0x2d,0x57,0xb5,0xa1,0x18,0x77,
    0xaf,0xef,0x87,0x63,0xa7,0x8d,0x22,0x18,
    0xcf,0x8d,0x81,0x19,0x2f,0x77,0x49,0xd2,
    0xb5,0x37,0xa5,0x41,0x19,0x9f,0xc2,0x7f,
    0x22,0x72,0x02,0x19,0xd2,0xe1,0xc2,0x7e,
    0x22,0x71,0xeb,0x77,0xa7
};
static FixEntry ft_dirty_nocli_header[] = {
    {ftEnd, 0}
};

static FixEntry ft_dirty_nocli_tail[] = {
    {ftEnd, 0}
};

static FixEntry ft_dirty_nocli_decruncher[] = {
    {ftDecruncher, 0x00bf},
    {ftBufZp, 0x00bf},
    {ftSrcEnd, 0x00cd},
    {ftDestEnd, 0x00d8},
    {ftEndMarkerMinusOne, 0x0131},
    {ftEnd, 0}
};

static FixStruct fs_list_decruncher[] = {
    {0x00fd, default_decruncher, sizeof(default_decruncher), ft_default_decruncher, },
    {0x00fd, nocli_decruncher, sizeof(nocli_decruncher), ft_nocli_decruncher, FLAG_NOCLI},
    {0x00bf, dirty_default_decruncher, sizeof(dirty_default_decruncher), ft_dirty_default_decruncher, FLAG_DIRTY},
    {0x00bf, dirty_nocli_decruncher, sizeof(dirty_nocli_decruncher), ft_dirty_nocli_decruncher, FLAG_NOCLI|FLAG_DIRTY},
    {0, NULL, 0, NULL, 0}
};
static FixStruct fs_list_header[] = {
    {0x0801, default_header, sizeof(default_header), ft_default_header, },
    {0x0801, nocli_header, sizeof(nocli_header), ft_nocli_header, FLAG_NOCLI},
    {0x0801, dirty_default_header, sizeof(dirty_default_header), ft_dirty_default_header, FLAG_DIRTY},
    {0x0801, dirty_nocli_header, sizeof(dirty_nocli_header), ft_dirty_nocli_header, FLAG_NOCLI|FLAG_DIRTY},
    {0, NULL, 0, NULL, 0}
};
static FixStruct fs_list_tail[] = {
    {0x1000, default_tail, sizeof(default_tail), ft_default_tail, },
    {0x1000, nocli_tail, sizeof(nocli_tail), ft_nocli_tail, FLAG_NOCLI},
    {0x1000, dirty_default_tail, sizeof(dirty_default_tail), ft_dirty_default_tail, FLAG_DIRTY},
    {0x1000, dirty_nocli_tail, sizeof(dirty_nocli_tail), ft_dirty_nocli_tail, FLAG_NOCLI|FLAG_DIRTY},
    {0, NULL, 0, NULL, 0}
};

/* eof */