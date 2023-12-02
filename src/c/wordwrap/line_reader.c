#include "line_reader.h"

#include <ovarray.h>

#include "rule.h"

size_t line_reader_find_left(struct line_reader const *const lr, size_t const pos) {
  struct glyph const *const gmin = &lr->glyphs[lr->linehead];
  for (struct glyph const *g = &lr->glyphs[pos]; gmin <= g; --g) {
    switch (g->typ) {
    case gt_tag:
      continue;
    case gt_original_tag:
      continue;
    case gt_break:
      return SIZE_MAX;
    case gt_glyph:
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
    case gt_original_tag:
      continue;
    case gt_break:
      return SIZE_MAX;
    case gt_glyph:
      return (size_t)(g - lr->glyphs);
    }
  }
  return SIZE_MAX;
}

size_t line_reader_find_breakable_left(struct line_reader const *const lr, size_t const pos, int const break_rule) {
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
          rule_is_breakable(lr->text->ptr[lr->glyphs[ppos].pos], lr->text->ptr[lr->glyphs[cpos].pos], break_rule);
      if ((br == br_breakable) || (lr->glyphs[cpos].flags & gt_breakable_by_wbr) ||
          ((br == br_non_ascii_word || br == br_ascii_word) && (break_rule & br_budoux) &&
           (lr->glyphs[cpos].flags & gt_budoux_breakable))) {
        return cpos;
      }
    }
    cpos = ppos;
  }
}

size_t line_reader_find_breakable_right(struct line_reader const *const lr, size_t const pos, int const break_rule) {
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
          rule_is_breakable(lr->text->ptr[lr->glyphs[cpos].pos], lr->text->ptr[lr->glyphs[npos].pos], break_rule);
      if ((br == br_breakable) || (lr->glyphs[npos].flags & gt_breakable_by_wbr) ||
          ((br == br_non_ascii_word || br == br_ascii_word) && (break_rule & br_budoux) &&
           (lr->glyphs[npos].flags & gt_budoux_breakable))) {
        return npos;
      }
    }
    cpos = npos;
  }
}
