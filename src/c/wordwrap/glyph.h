#pragma once

#include "aviutl_text.h"

enum glyph_type {
  gt_glyph,
  gt_glyph_numref,
  gt_break,
  gt_tag,
  gt_original_tag,
};

enum glyph_flags {
  gt_budoux_marked = 1,
  gt_budoux_breakable = 2,
  gt_breakable_by_wbr = 4,
  gt_not_breakable_by_nobr = 8,
};

enum glyph_original_tag_type {
  ot_unknown,
  ot_nobr_open,
  ot_nobr_close,
  ot_wbr,
};

struct glyph {
  enum glyph_type typ : 16;
  int flags : 16;
  size_t pos : 16;
  size_t len : 16;
  union {
    struct {
      wchar_t ch;
      int16_t BlackBoxX;
      int16_t ptGlyphOriginX;
      int16_t CellIncX;
    } glyph;
    struct {
      enum aviutl_text_tag_type type;
    } tag;
    struct {
      enum glyph_original_tag_type type;
    } original_tag;
  } u;
};

#ifdef WW_DEBUG
void glyph_dump(struct glyph const *const glyphs);
#endif
