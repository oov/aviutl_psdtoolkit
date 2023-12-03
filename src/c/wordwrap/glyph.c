#include "glyph.h"

#include <ovarray.h>

#ifdef WW_DEBUG
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>

void glyph_dump(struct glyph const *const glyphs) {
  wchar_t buf[128];
  wchar_t flags[64];
  OutputDebugStringW(L"glyph_dump ----");
  for (size_t i = 0, ln = OV_ARRAY_LENGTH(glyphs); i < ln; ++i) {
    struct glyph const *const g = &glyphs[i];
    wchar_t const *t = NULL;
    switch (g->typ) {
    case gt_glyph:
      t = L"gt_glyph";
      break;
    case gt_glyph_numref:
      t = L"gt_glyph_numref";
      break;
    case gt_break:
      t = L"gt_break";
      break;
    case gt_tag:
      t = L"gt_tag";
      break;
    case gt_original_tag:
      t = L"gt_original_tag";
      break;
    }
    flags[0] = L'\0';
    if (g->flags & gt_budoux_marked) {
      wcscat(flags, L"marked ");
    }
    if (g->flags & gt_budoux_breakable) {
      wcscat(flags, L"breakable ");
    }
    if (g->flags & gt_breakable_by_wbr) {
      wcscat(flags, L"wbr ");
    }
    if (g->flags & gt_not_breakable_by_nobr) {
      wcscat(flags, L"nobr ");
    }
    wsprintfW(buf, L"  %02d pos %d / typ %ls / flags %ls", i, g->pos, t, flags);
    OutputDebugStringW(buf);
  }
}
#endif
