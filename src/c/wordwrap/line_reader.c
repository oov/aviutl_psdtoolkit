#include "line_reader.h"

#include <ovarray.h>

#include "rule.h"

size_t line_reader_find_left(struct line_reader const *const lr, size_t const pos) {
  struct glyph const *const gmin = &lr->glyphs[lr->linehead];
  for (struct glyph const *g = &lr->glyphs[pos]; gmin <= g; --g) {
    switch (g->typ) {
    case gt_tag:
      continue;
    case gt_tag_ex:
      continue;
    case gt_break:
      return SIZE_MAX;
    case gt_kerning:
      continue;
    case gt_glyph:
    case gt_glyph_numref:
      return (size_t)(g - lr->glyphs);
    }
  }
  return SIZE_MAX;
}

size_t line_reader_find_right(struct line_reader const *const lr, size_t const pos) {
  struct glyph const *const gmax = &lr->glyphs[OV_ARRAY_LENGTH(lr->glyphs) - 1];
  for (struct glyph const *g = &lr->glyphs[pos]; g <= gmax; ++g) {
    switch (g->typ) {
    case gt_tag:
      continue;
    case gt_tag_ex:
      continue;
    case gt_break:
      return SIZE_MAX;
    case gt_kerning:
      continue;
    case gt_glyph:
    case gt_glyph_numref:
      return (size_t)(g - lr->glyphs);
    }
  }
  return SIZE_MAX;
}

static size_t find_breakable_left(struct line_reader const *const lr, size_t const pos, int const break_rule) {
  size_t cpos = line_reader_find_left(lr, pos);
  if (cpos == SIZE_MAX) {
    return SIZE_MAX;
  }
  for (;;) {
    size_t ppos = line_reader_find_left(lr, cpos - 1);
    if (ppos == SIZE_MAX) {
      return SIZE_MAX;
    }
    if (!(lr->glyphs[cpos].flags & gt_not_breakable_by_nobr) || (lr->glyphs[cpos].flags & gt_breakable_by_wbr)) {
      enum break_rule const br =
          rule_is_breakable(lr->glyphs[ppos].u.glyph.ch, lr->glyphs[cpos].u.glyph.ch, break_rule);
      if ((br == br_breakable) || (lr->glyphs[cpos].flags & gt_breakable_by_wbr) ||
          ((br == br_non_ascii_word || br == br_ascii_word) && (break_rule & br_budoux) &&
           (lr->glyphs[cpos].flags & gt_budoux_breakable))) {
        return cpos;
      }
    }
    cpos = ppos;
  }
}

static size_t find_breakable_right(struct line_reader const *const lr, size_t const pos, int const break_rule) {
  size_t cpos = line_reader_find_right(lr, pos);
  if (cpos == SIZE_MAX) {
    return SIZE_MAX;
  }
  for (;;) {
    size_t npos = line_reader_find_right(lr, cpos + 1);
    if (npos == SIZE_MAX) {
      return SIZE_MAX;
    }
    if (!(lr->glyphs[npos].flags & gt_not_breakable_by_nobr) || (lr->glyphs[npos].flags & gt_breakable_by_wbr)) {
      enum break_rule const br =
          rule_is_breakable(lr->glyphs[cpos].u.glyph.ch, lr->glyphs[npos].u.glyph.ch, break_rule);
      if ((br == br_breakable) || (lr->glyphs[npos].flags & gt_breakable_by_wbr) ||
          ((br == br_non_ascii_word || br == br_ascii_word) && (break_rule & br_budoux) &&
           (lr->glyphs[npos].flags & gt_budoux_breakable))) {
        return npos;
      }
    }
    cpos = npos;
  }
}

size_t line_reader_find_breakable(struct line_reader const *const lr, size_t const pos, int const break_rule) {
  size_t r = find_breakable_left(lr, pos, break_rule);
  if (r == SIZE_MAX) {
    r = find_breakable_right(lr, pos, break_rule);
  }
  return r;
}
