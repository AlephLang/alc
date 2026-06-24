#include "ansi.h"

static const char *_colors[] = {
  [ANSI_COLOR_NO_CHANGE >> ANSI_COLOR_OFFSET] = "",
  [ANSI_COLOR_BLACK >> ANSI_COLOR_OFFSET] = "\033[30m",
  [ANSI_COLOR_RED >> ANSI_COLOR_OFFSET] = "\033[31m",
  [ANSI_COLOR_GREEN >> ANSI_COLOR_OFFSET] = "\033[32m",
  [ANSI_COLOR_YELLOW >> ANSI_COLOR_OFFSET] = "\033[33m",
  [ANSI_COLOR_BLUE >> ANSI_COLOR_OFFSET] = "\033[34m",
  [ANSI_COLOR_MAGENTA >> ANSI_COLOR_OFFSET] = "\033[35m",
  [ANSI_COLOR_CYAN >> ANSI_COLOR_OFFSET] = "\033[36m",
  [ANSI_COLOR_WHITE >> ANSI_COLOR_OFFSET] = "\033[37m",
  [ANSI_COLOR_DEFAULT >> ANSI_COLOR_OFFSET] = "\033[39m",
};

static const char *_graphics[] = {
  [ANSI_GRAPHICS_NO_CHANGE >> ANSI_GRAPHICS_OFFSET] = "",
  [ANSI_GRAPHICS_BOLD >> ANSI_GRAPHICS_OFFSET] = "\033[1m",
  [ANSI_GRAPHICS_DIM >> ANSI_GRAPHICS_OFFSET] = "\033[2m",
  [ANSI_GRAPHICS_ITALIC >> ANSI_GRAPHICS_OFFSET] = "\033[3m",
  [ANSI_GRAPHICS_UNDERLINE >> ANSI_GRAPHICS_OFFSET] = "\033[4m",
  [ANSI_GRAPHICS_BLINK >> ANSI_GRAPHICS_OFFSET] = "\033[5m",
  [ANSI_GRAPHICS_INVERSE >> ANSI_GRAPHICS_OFFSET] = "\033[7m",
  [ANSI_GRAPHICS_HIDDEN >> ANSI_GRAPHICS_OFFSET] = "\033[8m",
  [ANSI_GRAPHICS_STRIKETHROUGH >> ANSI_GRAPHICS_OFFSET] = "\033[9m",
};

const char *ansi_reset(void)
{
  return "\033[0m";
}

const char *ansi_color(Ansi_Color color)
{
  if ((u8)color == (u8)-1)
    return "\033[0m";
  return _colors[color >> ANSI_COLOR_OFFSET];
}

const char *ansi_graphics(Ansi_Graphics graphics)
{
  if ((u8)graphics == (u8)-1)
    return "\033[0m";
  return _graphics[graphics >> ANSI_GRAPHICS_OFFSET];
}

Ansi_Color ansi_get_color(Ansi_Mode mode)
{
  return mode & ANSI_COLOR_MASK;
}

Ansi_Graphics ansi_get_graphics(Ansi_Mode mode)
{
  return mode & ANSI_GRAPHICS_MASK;
}
