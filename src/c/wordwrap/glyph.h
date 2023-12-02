#pragma once

#include "aviutl_text.h"

enum glyph_type {
  gt_glyph = 0,
  gt_break = 1,
  gt_tag = 2,
  gt_original_tag = 3,
};

enum glyph_flags {
  gt_budoux_marked = 1,
  gt_budoux_breakable = 2,
  gt_breakable_by_wbr = 4,
  gt_not_breakable_by_nobr = 8,
};

struct glyph {
  enum glyph_type typ : 2;
  int flags : 4;
  uint16_t pos;
  union {
    struct {
      int16_t BlackBoxX;
      int16_t ptGlyphOriginX;
      int16_t CellIncX;
    } metrics;
    struct {
      enum aviutl_text_tag_type type;
      uint16_t len;
    } tag;
    struct {
      uint16_t len;
    } original_tag;
  } u;
};

#ifdef WW_DEBUG
void glyph_dump(struct glyph *glyphs);
#endif
