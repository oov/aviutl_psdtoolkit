#pragma once

#include "aviutl_text.h"
#include "aviutl_text_ex.h"

enum glyph_type {
  gt_glyph,
  gt_glyph_numref,
  gt_break,
  gt_kerning,
  gt_tag,
  gt_tag_ex,
};

enum glyph_flags {
  gt_budoux_marked = 1,
  gt_budoux_breakable = 2,
  gt_breakable_by_wbr = 4,
  gt_not_breakable_by_nobr = 8,
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
      enum aviutl_text_ex_tag_type type;
      int16_t initial_unit;
      int16_t current_unit;
    } tag_ex;
    struct {
      int16_t x;
      int16_t y;
    } kerning;
  } u;
};

#ifdef WW_DEBUG
void glyph_dump(struct glyph const *const glyphs);
#endif
