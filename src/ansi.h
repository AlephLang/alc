#ifndef __ANSI_H__
#define __ANSI_H__

#include <alc/defs.h>

typedef enum {
  ANSI_COLOR_NO_CHANGE = 0,
  ANSI_COLOR_BLACK = 1,
  ANSI_COLOR_RED = 2,
  ANSI_COLOR_GREEN = 3,
  ANSI_COLOR_YELLOW = 4,
  ANSI_COLOR_BLUE = 5,
  ANSI_COLOR_MAGENTA = 6,
  ANSI_COLOR_CYAN = 7,
  ANSI_COLOR_WHITE = 8,
  ANSI_COLOR_DEFAULT = 0b1111,
} Ansi_Color;
#define ANSI_COLOR_MASK (15)
#define ANSI_COLOR_OFFSET (0)

typedef enum {
  ANSI_GRAPHICS_NO_CHANGE = 0,
  ANSI_GRAPHICS_BOLD = 1 << 4,
  ANSI_GRAPHICS_DIM = 2 << 4,
  ANSI_GRAPHICS_ITALIC = 3 << 4,
  ANSI_GRAPHICS_UNDERLINE = 4 << 4,
  ANSI_GRAPHICS_BLINK = 5 << 4,
  ANSI_GRAPHICS_INVERSE = 6 << 4,
  ANSI_GRAPHICS_HIDDEN = 7 << 4,
  ANSI_GRAPHICS_STRIKETHROUGH = 8 << 4,
} Ansi_Graphics;
#define ANSI_GRAPHICS_MASK (15 << 4)
#define ANSI_GRAPHICS_OFFSET (4)

typedef u8 Ansi_Mode;

#define ANSI_RESET ((Ansi_Mode) - 1)

const char *ansi_reset(void);
const char *ansi_color(Ansi_Color color);
const char *ansi_graphics(Ansi_Graphics graphics);

Ansi_Color ansi_get_color(Ansi_Mode mode);
Ansi_Graphics ansi_get_graphics(Ansi_Mode mode);

#endif // __ANSI_H__
