#include "canvas.h"

void canvas_destroy(struct canvas **const ctxpp) {
  if (!ctxpp) {
    return;
  }
  struct canvas *const ctx = *ctxpp;
  if (ctx->old_font) {
    HFONT f = SelectObject(ctx->dc, ctx->old_font);
    DeleteObject(f);
    ctx->old_font = NULL;
  }
  if (ctx->dc) {
    ReleaseDC(ctx->window, ctx->dc);
    ctx->dc = NULL;
  }
  ctx->window = NULL;
  ereport(mem_free(ctxpp));
}

NODISCARD error canvas_create(struct canvas **const ctxpp) {
  struct canvas *ctx = NULL;
  error err = mem(&ctx, 1, sizeof(struct canvas));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *ctx = (struct canvas){
      .window = GetDesktopWindow(),
      .old_font = NULL,
  };
  ctx->dc = GetDC(ctx->window);
  if (!ctx->dc) {
    err = errg(err_unexpected);
    goto cleanup;
  }
  *ctxpp = ctx;
  ctx = NULL;
cleanup:
  if (ctx) {
    canvas_destroy(&ctx);
  }
  return err;
}

NODISCARD error canvas_set_font(struct canvas *const ctx, LOGFONTW const *lf, bool const high_resolution) {
  HFONT newfont = NULL;
  error err = eok();
  if (high_resolution) {
    LOGFONTW lfx2 = *lf;
    lfx2.lfHeight = lfx2.lfHeight * 2;
    newfont = CreateFontIndirectW(&lfx2);
  } else {
    newfont = CreateFontIndirectW(lf);
  }
  if (!newfont) {
    err = errg(err_unexpected);
    goto cleanup;
  }
  if (ctx->old_font) {
    HFONT f = SelectObject(ctx->dc, newfont);
    DeleteObject(f);
  } else {
    ctx->old_font = SelectObject(ctx->dc, newfont);
  }
  newfont = NULL;
  if (!GetTextMetricsW(ctx->dc, &ctx->current_font_text_metric)) {
    err = errg(err_unexpected);
    goto cleanup;
  }
cleanup:
  if (newfont) {
    DeleteObject(newfont);
  }
  return err;
}

NODISCARD error canvas_set_initial_font(struct canvas *const ctx, LOGFONTW *const lf, bool const high_resolution) {
  error err = canvas_set_font(ctx, lf, high_resolution);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  ctx->initial_font = lf;
cleanup:
  return err;
}

static size_t mywcsnlen(wchar_t const *const s, size_t const max) {
  size_t i = 0;
  while (i < max && s[i]) {
    ++i;
  }
  return i;
}

NODISCARD error canvas_set_font_params(struct canvas *const ctx,
                                       wchar_t const *const name,
                                       size_t const size,
                                       bool const bold,
                                       bool const italic,
                                       bool const high_resolution) {
  LOGFONTW lf = {
      .lfHeight = size ? -(LONG)size : ctx->initial_font->lfHeight,
      .lfWidth = 0,
      .lfEscapement = 0,
      .lfOrientation = 0,
      // When the initial setting is bold, "<s,,I>" can be used to make it bold and italic,
      // but when the initial setting is normal, "<s,,B><s,,I>" can be used for italic only.
      .lfWeight = bold || (ctx->initial_font->lfWeight == FW_BOLD) ? FW_BOLD : FW_NORMAL,
      .lfItalic = italic || (ctx->initial_font->lfItalic == TRUE) ? TRUE : FALSE,
      .lfUnderline = FALSE,
      .lfStrikeOut = FALSE,
      .lfCharSet = DEFAULT_CHARSET,
      .lfOutPrecision = OUT_DEFAULT_PRECIS,
      .lfClipPrecision = CLIP_DEFAULT_PRECIS,
      .lfQuality = DEFAULT_QUALITY,
      .lfPitchAndFamily = DEFAULT_PITCH,
  };
  if (name) {
    size_t const len = mywcsnlen(name, LF_FACESIZE - 1);
    wcsncpy(lf.lfFaceName, name, len);
    lf.lfFaceName[len] = L'\0';
  } else {
    memcpy(lf.lfFaceName, ctx->initial_font->lfFaceName, sizeof(lf.lfFaceName));
  }
  error err = canvas_set_font(ctx, &lf, high_resolution);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

bool canvas_get_metrics(struct canvas *const ctx,
                        wchar_t const ch,
                        GLYPHMETRICS *const gm,
                        bool const monospace,
                        bool const high_resolution) {
  static MAT2 const mat = {{0, 1}, {0, 0}, {0, 0}, {0, 1}};
  bool const r = GetGlyphOutlineW(ctx->dc, ch, GGO_METRICS, gm, 0, NULL, &mat) != GDI_ERROR;
  if (!r) {
    return false;
  }
  if (gm->gmBlackBoxX == 1) {
    // Even if the font does not have a glyph, gmBlackBoxX becomes 1.
    // If the size of GGO_NATIVE is also 0, the current font does not have a glyph.
    if (GetGlyphOutlineW(ctx->dc, ch, GGO_NATIVE, gm, 0, NULL, &mat) == 0) {
      gm->gmptGlyphOrigin.x = 0;
      gm->gmBlackBoxX = (UINT)gm->gmCellIncX;
    }
  }
  if (monospace) {
    gm->gmptGlyphOrigin.x = 0;
    gm->gmBlackBoxX = (UINT)ctx->current_font_text_metric.tmMaxCharWidth;
    gm->gmCellIncX = (short)ctx->current_font_text_metric.tmMaxCharWidth;
  }
  if (high_resolution) {
    gm->gmBlackBoxX = (gm->gmBlackBoxX + 1) / 2;
    gm->gmptGlyphOrigin.x = gm->gmptGlyphOrigin.x / 2;
    gm->gmCellIncX = (gm->gmCellIncX + 1) / 2;
  }
  return true;
}
