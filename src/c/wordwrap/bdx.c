#include "bdx.h"

#include <ovarray.h>

#ifdef WW_DEBUG
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#endif

struct bdx_marker_context {
  struct line_reader const lr;
  size_t nextpos;
  size_t **boundaries;
};

static char32_t get_char(void *const userdata) {
  struct bdx_marker_context *const ctx = userdata;
  size_t const pos = line_reader_find_right(&ctx->lr, ctx->nextpos);
  if (pos == SIZE_MAX) {
    return 0;
  }
  ctx->nextpos = pos + 1;

#ifdef WW_DEBUG
  wchar_t buf[128];
  wsprintfW(buf, L"get_char: %lc", ctx->lr.glyphs[pos].u.glyph.ch);
  OutputDebugStringW(buf);
#endif

  return ctx->lr.glyphs[pos].u.glyph.ch;
}

static bool add_boundary(size_t const boundary, void *const userdata) {
  struct bdx_marker_context *const ctx = userdata;
  error err = OV_ARRAY_PUSH(ctx->boundaries, boundary);
  if (efailed(err)) {
    ereport(err);
    return false;
  }
  return true;
}

NODISCARD error bdx_write_markers(struct glyph *const glyphs,
                                  size_t const linehead,
                                  struct budouxc *const model,
                                  size_t **const boundaries) {
  if (!glyphs || !model || !boundaries) {
    return errg(err_invalid_arugment);
  }

  struct bdx_marker_context ctx = (struct bdx_marker_context){
      .lr =
          {
              .glyphs = glyphs,
              .linehead = linehead,
          },
      .nextpos = linehead,
      .boundaries = boundaries,
  };

  OV_ARRAY_SET_LENGTH(*boundaries, 0);
  bool const r = budouxc_parse_boundaries_callback(model, get_char, add_boundary, &ctx);
  if (!r) {
    return errg(err_out_of_memory);
  }

  // set a flag to indicate detection is complete.
  glyphs[linehead].flags |= gt_budoux_marked;

  size_t pos = linehead - 1;
  size_t nch = 0;
  for (size_t i = 0; i < OV_ARRAY_LENGTH(*boundaries); ++i) {
    size_t const boundary = (*boundaries)[i];
    while (nch <= boundary) {
      pos = line_reader_find_right(&ctx.lr, pos + 1);
      if (pos == SIZE_MAX) {
        break;
      }
      ++nch;
    }
    if (pos == SIZE_MAX) {
      break;
    }

#ifdef WW_DEBUG
    wchar_t buf[128];
    wsprintfW(buf, L"bdx boundary: %d pos: %d", boundary, pos);
    OutputDebugStringW(buf);
#endif
    glyphs[pos].flags |= gt_budoux_breakable;
  }
  return eok();
}
