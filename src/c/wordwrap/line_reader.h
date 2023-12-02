#pragma once

#include <ovbase.h>

#include "glyph.h"

struct line_reader {
  struct wstr const *text;
  struct glyph *glyphs;
  size_t linehead;
};

size_t line_reader_find_left(struct line_reader const *const lr, size_t const pos);
size_t line_reader_find_right(struct line_reader const *const lr, size_t const pos);
size_t line_reader_find_breakable_left(struct line_reader const *const lr, size_t const pos, int const break_rule);
size_t line_reader_find_breakable_right(struct line_reader const *const lr, size_t const pos, int const break_rule);
