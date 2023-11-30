#include "luafuncs_wordwrap.h"

#include <ovarray.h>
#include <ovthreads.h>
#include <ovutf.h>
#include <ovutil/win32.h>

#include <budoux-c.h>

#include "aviutl.h"
#include "aviutl_text.h"
#include "i18n.h"
#include "luautil.h"

#define WW_DEBUG 0

static inline int imax(int const a, int const b) { return a > b ? a : b; }
static inline int imin(int const a, int const b) { return a < b ? a : b; }
static inline double dmax(double const a, double const b) { return a > b ? a : b; }
static inline double dmin(double const a, double const b) { return a < b ? a : b; }
// static inline size_t zumax(size_t const a, size_t const b) { return a > b ? a : b; }
static inline size_t zumin(size_t const a, size_t const b) { return a < b ? a : b; }

/**
 * @brief Checks if the given character is not a first line character, such as "）".
 *
 * This function checks if the given character is one of the characters that should not be the first character
 * in a line of text. These characters include various types of brackets and quotation marks.
 *
 * @param ch The character to check.
 * @return Returns true if the character is not a first line character, false otherwise.
 */
static bool is_no_first_line_char(wchar_t const ch) {
  switch (ch) {
  case L',':
  case L')':
  case L'）':
  case L']':
  case L'］':
  case L'}':
  case L'｝':
  case L'､':
  case L'、':
  case L'〕':
  case L'〉':
  case L'》':
  case L'｣':
  case L'」':
  case L'』':
  case L'】':
  case L'〙':
  case L'〗':
  case L'〟':
  case L'’':
  case L'”':
  case L'｠':
  case L'»':

  case L'‐':
  case L'゠':
  case L'–':
  case L'〜':
  case L'～':

  case L'?':
  case L'？':
  case L'!':
  case L'！':
  case L'‼':
  case L'⁇':
  case L'⁈':
  case L'⁉':

  case L'·':
  case L'・':
  case L':':
  case L'：':
  case L';':
  case L'；':
  case L'/':
  case L'／':

  case L'｡':
  case L'。':
  case L'.':
  case L'．':

  case L'゛':
  case L'ﾞ':
  case L'゜':
  case L'ﾟ':
  case L'ゝ':
  case L'ゞ':
  case L'々':
  case L'〻':
  case L'ー':
  case L'ァ':
  case L'ィ':
  case L'ゥ':
  case L'ェ':
  case L'ォ':
  case L'ッ':
  case L'ャ':
  case L'ュ':
  case L'ョ':
  case L'ヮ':
  case L'ヵ':
  case L'ヶ':
  case L'ぁ':
  case L'ぃ':
  case L'ぅ':
  case L'ぇ':
  case L'ぉ':
  case L'っ':
  case L'ゃ':
  case L'ゅ':
  case L'ょ':
  case L'ゎ':
  case L'ゕ':
  case L'ゖ':
  case L'ㇰ':
  case L'ㇱ':
  case L'ㇲ':
  case L'ㇳ':
  case L'ㇴ':
  case L'ㇵ':
  case L'ㇶ':
  case L'ㇷ':
  case L'ㇸ':
  case L'ㇹ':
  // case L'ㇷ゚': // U+31F7 + U+309A
  case L'ㇺ':
  case L'ㇻ':
  case L'ㇼ':
  case L'ㇽ':
  case L'ㇾ':
  case L'ㇿ':
  case L'ｧ':
  case L'ｨ':
  case L'ｩ':
  case L'ｪ':
  case L'ｫ':
  case L'ｯ':
  case L'ｬ':
  case L'ｭ':
  case L'ｮ':
    return true;
  }
  return false;
}

/**
 * @brief Checks if the given character is not a last line character, such as "（".
 *
 * This function checks if the given character is one of the characters that should not be the last character
 * in a line of text. These characters include various types of brackets and quotation marks.
 *
 * @param ch The character to check.
 * @return Returns true if the character is not a last line character, false otherwise.
 */
static bool is_no_last_line_char(wchar_t const ch) {
  switch (ch) {
  case L'(':
  case L'（':
  case L'[':
  case L'［':
  case L'{':
  case L'｛':
  case L'〔':
  case L'〈':
  case L'《':
  case L'｢':
  case L'「':
  case L'『':
  case L'【':
  case L'〘':
  case L'〖':
  case L'〝':
  case L'‘':
  case L'“':
  case L'｟':
  case L'«':
    return true;
  }
  return false;
}

/**
 * @brief Check if the given wide character is a non-breaking character, such as "…".
 *
 * @param ch The character to check.
 * @return true if the character is a non-breaking character, false otherwise.
 */
static bool is_no_break_char(wchar_t const ch) {
  switch (ch) {
  case L'—':
  case L'…':
  case L'‥':
  case L'〳':
  case L'〴':
  case L'〵':
    return true;
  }
  return false;
}

/**
 * @brief Tests if a given character is a low surrogate in Unicode.
 *
 * @param ch The character to test.
 * @return Returns true if the character is a low surrogate, false otherwise.
 */
static bool is_surrogate_low(wchar_t const ch) { return 0xdc00 <= ch && ch < 0xe000; }

/**
 * @brief Checks if the given characters represent a signed integer.
 *
 * @param ch1 The first character, expected to be '+' or '-' for signed integers.
 * @param ch2 The second character, expected to be a digit (0-9).
 * @return Returns true if the characters represent a signed integer (e.g., '+1', '-2'), false otherwise.
 */
static bool is_signed_number(wchar_t const ch1, wchar_t const ch2) {
  return (ch1 == L'-' || ch1 == L'+') && (L'0' <= ch2 && ch2 <= L'9');
}

static bool is_graph(wchar_t const ch) { return L'!' <= ch && ch <= L'~'; }

static bool is_ascii_word(wchar_t const ch1, wchar_t const ch2) { return is_graph(ch1) && is_graph(ch2); }

static bool is_non_ascii_word(wchar_t const ch1, wchar_t const ch2) { return ch1 >= 128 && ch2 >= 128; }

enum break_rule {
  br_breakable = 0,
  br_no_first_line_char = 1,
  br_no_last_line_char = 2,
  br_no_break_char = 4,
  br_surrogate_low = 8,
  br_signed_number = 16,
  br_ascii_word = 32,
  br_non_ascii_word = 64,
  br_budoux = 128,
};

static enum break_rule is_breakable(wchar_t const ch1, wchar_t const ch2, int enabled_rules) {
  enum break_rule br = br_breakable;
  if ((enabled_rules & br_no_first_line_char) != 0 && is_no_first_line_char(ch2)) {
    br = br_no_first_line_char;
  } else if ((enabled_rules & br_no_last_line_char) != 0 && is_no_last_line_char(ch1)) {
    br = br_no_last_line_char;
  } else if ((enabled_rules & br_no_break_char) != 0 && is_no_break_char(ch2)) {
    br = br_no_break_char;
  } else if ((enabled_rules & br_surrogate_low) != 0 && is_surrogate_low(ch1)) {
    br = br_surrogate_low;
  } else if ((enabled_rules & br_signed_number) != 0 && is_signed_number(ch1, ch2)) {
    br = br_signed_number;
  } else if ((enabled_rules & br_ascii_word) != 0 && is_ascii_word(ch1, ch2)) {
    br = br_ascii_word;
  } else if ((enabled_rules & br_non_ascii_word) != 0 && is_non_ascii_word(ch1, ch2)) {
    br = br_non_ascii_word;
  }
#if WW_DEBUG
  wchar_t buf[128];
  wchar_t const *t = NULL;
  switch (br) {
  case br_breakable:
    t = L"br_breakable";
    break;
  case br_no_first_line_char:
    t = L"no_first_line_char";
    break;
  case br_no_last_line_char:
    t = L"no_last_line_char";
    break;
  case br_no_break_char:
    t = L"no_break_char";
    break;
  case br_surrogate_low:
    t = L"surrogate_low";
    break;
  case br_signed_number:
    t = L"signed_number";
    break;
  case br_ascii_word:
    t = L"ascii_word";
    break;
  case br_non_ascii_word:
    t = L"non_ascii_word";
    break;
  case br_budoux:
    t = L"budoux";
    break;
  }
  wsprintfW(buf, L"ch1: %lc ch2: %lc %ls", ch1, ch2, t);
  OutputDebugStringW(buf);
#endif
  return br;
}

enum glyph_type {
  gt_glyph = 0,
  gt_break = 1,
  gt_tag = 2,
};

enum glyph_budoux_flag {
  gt_budoux_marked = 4,
  gt_budoux_breakable = 8,
};

struct glyph {
  uint16_t typ;
  uint16_t pos;
  union {
    struct {
      int16_t BlackBoxX;
      int16_t ptGlyphOriginX;
      int16_t CellIncX;
    } metrics;
    enum aviutl_text_tag_type tag_type;
  } u;
};

static inline enum glyph_type glyph_get_type(struct glyph const *const g) { return g->typ & 3; }

struct line_reader {
  struct wstr const *text;
  struct glyph *glyphs;
  size_t linehead;
};

static size_t line_reader_find_left(struct line_reader const *const lr, size_t const pos) {
  struct glyph const *const gmin = &lr->glyphs[lr->linehead];
  for (struct glyph const *g = &lr->glyphs[pos]; gmin <= g; --g) {
    switch (glyph_get_type(g)) {
    case gt_tag:
      continue;
    case gt_break:
      return SIZE_MAX;
    case gt_glyph:
      return (size_t)(g - lr->glyphs);
    }
  }
  return SIZE_MAX;
}

static size_t line_reader_find_right(struct line_reader const *const lr, size_t const pos) {
  struct glyph const *const gmax = &lr->glyphs[OV_ARRAY_LENGTH(lr->glyphs) - 1];
  for (struct glyph const *g = &lr->glyphs[pos]; g <= gmax; ++g) {
    switch (glyph_get_type(g)) {
    case gt_tag:
      continue;
    case gt_break:
      return SIZE_MAX;
    case gt_glyph:
      return (size_t)(g - lr->glyphs);
    }
  }
  return SIZE_MAX;
}

static size_t
line_reader_find_breakable_left(struct line_reader const *const lr, size_t const pos, int const break_rule) {
  size_t cpos = line_reader_find_left(lr, pos);
  if (cpos == SIZE_MAX) {
    return SIZE_MAX;
  }
  for (;;) {
    size_t ppos = line_reader_find_left(lr, cpos - 1);
    if (ppos == SIZE_MAX) {
      return SIZE_MAX;
    }
    enum break_rule const br =
        is_breakable(lr->text->ptr[lr->glyphs[ppos].pos], lr->text->ptr[lr->glyphs[cpos].pos], break_rule);
    if ((br == br_breakable) || ((br == br_non_ascii_word || br == br_ascii_word) && (break_rule & br_budoux) &&
                                 (lr->glyphs[cpos].typ & gt_budoux_breakable))) {
      return cpos;
    }
    cpos = ppos;
  }
}

static size_t
line_reader_find_breakable_right(struct line_reader const *const lr, size_t const pos, int const break_rule) {
  size_t cpos = line_reader_find_right(lr, pos);
  if (cpos == SIZE_MAX) {
    return SIZE_MAX;
  }
  for (;;) {
    size_t npos = line_reader_find_right(lr, cpos + 1);
    if (npos == SIZE_MAX) {
      return SIZE_MAX;
    }
    enum break_rule const br =
        is_breakable(lr->text->ptr[lr->glyphs[cpos].pos], lr->text->ptr[lr->glyphs[npos].pos], break_rule);
    if ((br == br_breakable) || ((br == br_non_ascii_word || br == br_ascii_word) && (break_rule & br_budoux) &&
                                 (lr->glyphs[npos].typ & gt_budoux_breakable))) {
      return npos;
    }
    cpos = npos;
  }
}

struct bdx_marker_context {
  struct line_reader const *lr;
  size_t nextpos;
  size_t **boundaries;
};

static char32_t bdx_marker_get_char(void *userdata) {
  struct bdx_marker_context *const bmctx = userdata;
  size_t const pos = line_reader_find_right(bmctx->lr, bmctx->nextpos);
  if (pos == SIZE_MAX) {
    return 0;
  }
  bmctx->nextpos = pos + 1;

#if WW_DEBUG
  OutputDebugStringW((wchar_t[]){bmctx->lr->text->ptr[bmctx->lr->glyphs[pos].pos], L'\0'});
#endif

  return bmctx->lr->text->ptr[bmctx->lr->glyphs[pos].pos];
}

static bool bdx_marker_add_boundary(size_t const boundary, void *userdata) {
  struct bdx_marker_context *const bmctx = userdata;
  error err = OV_ARRAY_PUSH(bmctx->boundaries, boundary);
  if (efailed(err)) {
    eignore(err);
    return false;
  }
  return true;
}

static NODISCARD error write_bdx_markers(struct line_reader const *lr,
                                         struct budouxc *const model,
                                         size_t **boundaries) {
  if (!lr || !model || !boundaries) {
    return errg(err_invalid_arugment);
  }
  OV_ARRAY_SET_LENGTH(*boundaries, 0);
  bool const r = budouxc_parse_boundaries_callback(model,
                                                   bdx_marker_get_char,
                                                   bdx_marker_add_boundary,
                                                   &(struct bdx_marker_context){
                                                       .lr = lr,
                                                       .nextpos = lr->linehead,
                                                       .boundaries = boundaries,
                                                   });
  if (!r) {
    return errg(err_out_of_memory);
  }

  // set a flag to indicate detection is complete.
  lr->glyphs[lr->linehead].typ |= gt_budoux_marked;

  size_t pos = lr->linehead - 1;
  size_t nch = 0;
  for (size_t i = 0; i < OV_ARRAY_LENGTH(*boundaries); ++i) {
    size_t const boundary = (*boundaries)[i];
    while (nch <= boundary) {
      pos = line_reader_find_right(lr, pos + 1);
      ++nch;
    }
    lr->glyphs[pos].typ |= gt_budoux_breakable;
  }
  return eok();
}

static size_t find_breakpoint_norule(struct line_reader const *lr, size_t const overflow_pos) {
  if (overflow_pos > lr->linehead) {
    return overflow_pos;
  }
  return line_reader_find_right(lr, overflow_pos + 1);
}

static size_t find_breakpoint_rule(struct line_reader const *lr, size_t const overflow_pos) {
  static int const rule = br_no_first_line_char | br_no_last_line_char | br_no_break_char | br_surrogate_low |
                          br_signed_number | br_ascii_word;
  size_t r = line_reader_find_breakable_left(lr, overflow_pos, rule);
  if (r == SIZE_MAX) {
    r = line_reader_find_breakable_right(lr, overflow_pos, rule);
  }
  return r;
}

static size_t find_breakpoint_budoux(struct line_reader const *lr,
                                     size_t const overflow_pos,
                                     struct budouxc *const model,
                                     size_t **boundaries) {
  if (!(lr->glyphs[lr->linehead].typ & gt_budoux_marked)) {
    error err = write_bdx_markers(lr, model, boundaries);
    if (efailed(err)) {
      ereport(err);
      return find_breakpoint_rule(lr, overflow_pos);
    }
  }

  static int const rule = br_no_first_line_char | br_no_last_line_char | br_no_break_char | br_surrogate_low |
                          br_signed_number | br_ascii_word | br_non_ascii_word | br_budoux;
  size_t r = line_reader_find_breakable_left(lr, overflow_pos, rule);
  if (r == SIZE_MAX) {
    r = line_reader_find_breakable_right(lr, overflow_pos, rule);
  }
  return r;
}

struct canvas_context {
  LOGFONTW *initial_font;

  HWND window;
  HDC dc;
  TEXTMETRICW current_font_text_metric;
  HFONT old_font;
};

static void canvas_context_destroy(struct canvas_context **const ctxpp) {
  if (!ctxpp) {
    return;
  }
  struct canvas_context *const ctx = *ctxpp;
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

NODISCARD static error canvas_context_create(struct canvas_context **const ctxpp) {
  struct canvas_context *ctx = NULL;
  error err = mem(&ctx, 1, sizeof(struct canvas_context));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *ctx = (struct canvas_context){
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
    canvas_context_destroy(&ctx);
  }
  return err;
}

NODISCARD static error
canvas_context_set_font(struct canvas_context *const ctx, LOGFONTW const *lf, bool const high_resolution) {
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

NODISCARD static error
canvas_context_set_initial_font(struct canvas_context *const ctx, LOGFONTW *const lf, bool const high_resolution) {
  error err = canvas_context_set_font(ctx, lf, high_resolution);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  ctx->initial_font = lf;
cleanup:
  return err;
}

NODISCARD static error canvas_context_set_font_tag(struct canvas_context *const ctx,
                                                   bool const high_resolution,
                                                   struct aviutl_text_tag_font const *const font_tag) {
  LOGFONTW lf = {
      .lfHeight = (LONG)(font_tag->size ? -(LONG)font_tag->size : ctx->initial_font->lfHeight),
      .lfWidth = 0,
      .lfEscapement = 0,
      .lfOrientation = 0,
      // When the initial setting is bold, "<s,,I>" can be used to make it bold and italic,
      // but when the initial setting is normal, "<s,,B><s,,I>" can be used for italic only.
      .lfWeight = font_tag->bold || (ctx->initial_font->lfWeight == FW_BOLD) ? FW_BOLD : FW_NORMAL,
      .lfItalic = font_tag->italic || (ctx->initial_font->lfItalic == TRUE) ? TRUE : FALSE,
      .lfUnderline = FALSE,
      .lfStrikeOut = FALSE,
      .lfCharSet = DEFAULT_CHARSET,
      .lfOutPrecision = OUT_DEFAULT_PRECIS,
      .lfClipPrecision = CLIP_DEFAULT_PRECIS,
      .lfQuality = DEFAULT_QUALITY,
      .lfPitchAndFamily = DEFAULT_PITCH,
  };
  if (font_tag->name && font_tag->name_len > 0) {
    size_t const face_name_len = zumin(font_tag->name_len, LF_FACESIZE - 1);
    wcsncpy(lf.lfFaceName, font_tag->name, face_name_len);
    lf.lfFaceName[face_name_len] = L'\0';
  } else {
    memcpy(lf.lfFaceName, ctx->initial_font->lfFaceName, sizeof(lf.lfFaceName));
  }
  error err = canvas_context_set_font(ctx, &lf, high_resolution);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

static bool canvas_context_get_metrics(struct canvas_context *const ctx,
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

NODISCARD static error create_glyph_metrics_list(wchar_t const *const text,
                                                 size_t const text_len,
                                                 bool const monospace,
                                                 bool const high_resolution,
                                                 struct canvas_context *canvas,
                                                 LOGFONTW *const initial_font,
                                                 struct glyph **const g) {
  if (!text || !canvas || !g || *g) {
    return errg(err_invalid_arugment);
  }
  if (text_len == 0) {
    *g = NULL;
    return eok();
  }

  struct glyph *glyphs = NULL;
  error err = eok();

  err = canvas_context_set_initial_font(canvas, initial_font, high_resolution);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = OV_ARRAY_GROW(&glyphs, text_len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  size_t gpos = 0;
  for (size_t i = 0; i < text_len; ++i) {
    if (text[i] == L'\n') {
      glyphs[gpos++] = (struct glyph){
          .typ = gt_break,
          .pos = (uint16_t)i,
      };
      continue;
    }
    struct aviutl_text_tag tag;
    if (text[i] == L'<' && aviutl_text_parse_tag(text, text_len, i, &tag)) {
      if (tag.type == aviutl_text_tag_type_font) {
        struct aviutl_text_tag_font font;
        aviutl_text_get_font(text, &tag, &font);
        err = canvas_context_set_font_tag(canvas, high_resolution, &font);
        if (efailed(err)) {
          err = ethru(err);
          goto cleanup;
        }
      }
      glyphs[gpos++] = (struct glyph){
          .typ = gt_tag,
          .pos = (uint16_t)i,
          .u =
              {
                  .tag_type = tag.type,
              },
      };
      i += tag.len - 1;
      continue;
    }
    GLYPHMETRICS gm;
    if (!canvas_context_get_metrics(canvas, text[i], &gm, monospace, high_resolution)) {
      err = emsg_i18n(err_type_generic, err_unexpected, gettext("failed to get glyph metrics."));
      goto cleanup;
    }
    glyphs[gpos++] = (struct glyph){
        .typ = gt_glyph,
        .pos = (uint16_t)i,
        .u =
            {
                .metrics =
                    {
                        .BlackBoxX = (int16_t)gm.gmBlackBoxX,
                        .ptGlyphOriginX = (int16_t)gm.gmptGlyphOrigin.x,
                        .CellIncX = (int16_t)gm.gmCellIncX,
                    },
            },
    };
  }
  OV_ARRAY_SET_LENGTH(glyphs, gpos);
  *g = glyphs;
  glyphs = NULL;
cleanup:
  if (glyphs) {
    OV_ARRAY_DESTROY(&glyphs);
  }
  return err;
}

enum wordwrap_mode {
  wwm_disabled,
  wwm_norule,
  wwm_rule,
  wwm_budoux,
  wwm_num_items,
};

static enum wordwrap_mode int_to_wordwrap_mode(int const v) {
  if (wwm_disabled <= v && v < wwm_num_items) {
    return (enum wordwrap_mode)v;
  }
  return wwm_norule;
}

struct wordwrap_settings {
  LOGFONTW initial_font;
  enum wordwrap_mode mode;
  size_t max_width;
  int letter_spacing;
  bool monospace;
  bool high_resolution;
};

static void read_exedit_params(wchar_t *const fontname,
                               size_t *const size,
                               bool *const bold,
                               bool *const italic,
                               int *const letter_spacing,
                               bool *const monospace,
                               bool *const high_resolution) {
  if (!aviutl_exedit_is_092()) {
    return;
  }
  intptr_t h = (intptr_t)(GetModuleHandleW(L"exedit.auf"));
  if (!h) {
    return;
  }
  FILTER const **filterpp = (void *)(h + 0x1B2B10);
  FILTER const *filterp = *filterpp;
  static FILTER const *const text_filter = (void *)0x100B9878;
  if (filterp != text_filter) {
    return;
  }
  if (filterp->ex_data_size < 64) {
    // ex_data_size too small
    return;
  }
  if (filterp->track_n < 1) {
    // track_n size too small
    return;
  }
  if (filterp->check_n < 5) {
    // check_n size too small
    return;
  }

  *size = (size_t)filterp->track[0];

  BOOL const *check_values = filterp->check;
  *bold = check_values[3] != FALSE;
  *italic = check_values[4] != FALSE;

  int8_t const *const ex_data_ptr = filterp->ex_data_ptr;
  *monospace = ex_data_ptr[3] & 1;
  *letter_spacing = (int)ex_data_ptr[5];
  *high_resolution = ex_data_ptr[7] & 1;

  char const *const mbcs_font_name = (char const *)&ex_data_ptr[16];
  int wn = MultiByteToWideChar(
      CP_ACP, 0, mbcs_font_name, imin((int)(strlen(mbcs_font_name)), LF_FACESIZE), fontname, LF_FACESIZE);
  if (wn < LF_FACESIZE) {
    fontname[wn] = L'\0';
  }
}

/**
 * @brief This function reads configuration information from the Lua stack and applies it to initial_font and wws.
 *
 * @param L Lua state pointer. Can be NULL.
 * @param table_index Index of the table in the Lua stack.
 * @param wws Word wrap settings to be applied, modified based on the configuration information.
 * @return Returns the result of the operation.
 */
NODISCARD static error
initialize_params(lua_State *const L, int const table_index, struct wordwrap_settings *const wws) {
  error err = eok();
  struct wstr name = {0};

  enum wordwrap_mode mode = wwm_norule;

  wchar_t font_name[LF_FACESIZE] = L"MS UI Gothic";
  size_t font_size = 9;
  bool font_bold = false;
  bool font_italic = false;

  size_t max_width = 400;
  int letter_spacing = 0;
  bool monospace = false;
  bool high_resolution = false;

  read_exedit_params(font_name, &font_size, &font_bold, &font_italic, &letter_spacing, &monospace, &high_resolution);

  if (L) {
    if (lua_istable(L, table_index)) {
      lua_getfield(L, table_index, "font");
      if (!lua_isnil(L, -1)) {
        size_t mbcs_name_len;
        char const *const mbcs_name = lua_tolstring(L, -1, &mbcs_name_len);
        if (mbcs_name && mbcs_name_len > 0) {
          err = from_mbcs(&(struct str const){.ptr = ov_deconster_(mbcs_name), .len = mbcs_name_len}, &name);
          if (efailed(err)) {
            err = ethru(err);
            goto cleanup;
          }
          size_t const n = zumin(name.len, LF_FACESIZE - 1);
          wcsncpy(font_name, name.ptr, n);
          font_name[n] = L'\0';
        }
      }

      lua_getfield(L, table_index, "size");
      if (!lua_isnil(L, -1)) {
        size_t const v = (size_t)(lua_tointeger(L, -1));
        if (v != 0) {
          font_size = v;
        }
      }

      lua_getfield(L, table_index, "bold");
      if (!lua_isnil(L, -1)) {
        font_bold = lua_toboolean(L, -1) ? true : false;
      }

      lua_getfield(L, table_index, "italic");
      if (!lua_isnil(L, -1)) {
        font_italic = lua_toboolean(L, -1) ? true : false;
      }

      lua_getfield(L, table_index, "width");
      if (!lua_isnil(L, -1)) {
        size_t const v = (size_t)(lua_tointeger(L, -1));
        if (v != 0) {
          max_width = v;
        }
      }

      lua_getfield(L, table_index, "mode");
      if (!lua_isnil(L, -1)) {
        mode = int_to_wordwrap_mode(lua_tointeger(L, -1));
      }

      lua_getfield(L, table_index, "spacing");
      if (!lua_isnil(L, -1)) {
        int const v = lua_tointeger(L, -1);
        if (v != INT_MAX) {
          letter_spacing = imax(-100, imin(100, v));
        }
      }

      lua_getfield(L, table_index, "monospace");
      if (!lua_isnil(L, -1)) {
        monospace = lua_toboolean(L, -1) ? true : false;
      }

      lua_getfield(L, table_index, "high_resolution");
      if (!lua_isnil(L, -1)) {
        high_resolution = lua_toboolean(L, -1) ? true : false;
      }

      lua_pop(L, 9);
    } else if (lua_isnumber(L, table_index)) {
      mode = int_to_wordwrap_mode(lua_tointeger(L, table_index));
    } else {
      err = emsg_i18n(err_type_generic, err_invalid_arugment, gettext("invalid argument type."));
      goto cleanup;
    }
  }

  *wws = (struct wordwrap_settings){
      .initial_font =
          {
              .lfHeight = -(LONG)font_size,
              .lfWidth = 0,
              .lfEscapement = 0,
              .lfOrientation = 0,
              .lfWeight = font_bold ? FW_BOLD : FW_NORMAL,
              .lfItalic = font_italic ? TRUE : FALSE,
              .lfUnderline = FALSE,
              .lfStrikeOut = FALSE,
              .lfCharSet = DEFAULT_CHARSET,
              .lfOutPrecision = OUT_DEFAULT_PRECIS,
              .lfClipPrecision = CLIP_DEFAULT_PRECIS,
              .lfQuality = DEFAULT_QUALITY,
              .lfPitchAndFamily = DEFAULT_PITCH,
          },
      .mode = mode,
      .max_width = max_width,
      .letter_spacing = letter_spacing,
      .monospace = monospace,
      .high_resolution = high_resolution,
  };
  memcpy(wws->initial_font.lfFaceName, font_name, sizeof(wws->initial_font.lfFaceName));
cleanup:
  if (name.ptr) {
    eignore(sfree(&name));
  }
  return err;
}

struct cache {
  uint64_t hash;
  struct timespec used_at;
  struct str text;
};

struct cyrb64_context {
  uint32_t h1;
  uint32_t h2;
};

static void cyrb64_init(struct cyrb64_context *const ctx, uint32_t const seed) {
  ctx->h1 = 0x91eb9dc7 ^ seed;
  ctx->h2 = 0x41c6ce57 ^ seed;
}

static void cyrb64_update(struct cyrb64_context *const ctx, uint32_t const *const src, size_t const len) {
  for (size_t i = 0; i < len; ++i) {
    ctx->h1 = (ctx->h1 ^ src[i]) * 2654435761;
    ctx->h2 = (ctx->h2 ^ src[i]) * 1597334677;
  }
}

static uint64_t cyrb64_final(struct cyrb64_context const *const ctx) {
  uint32_t h1 = ctx->h1, h2 = ctx->h2;
  h1 = ((h1 ^ (h1 >> 16)) * 2246822507) ^ ((h2 ^ (h2 >> 13)) * 3266489909);
  h2 = ((h2 ^ (h2 >> 16)) * 2246822507) ^ ((h1 ^ (h1 >> 13)) * 3266489909);
  return (((uint64_t)h2) << 32) | ((uint64_t)h1);
}

static struct cache g_caches[8] = {0};

static struct cache *cache_find(uint64_t const hash) {
  for (size_t i = 0; i < sizeof(g_caches) / sizeof(g_caches[0]); ++i) {
    if (g_caches[i].hash == hash) {
      timespec_get(&g_caches[i].used_at, TIME_UTC);
      return &g_caches[i];
    }
  }
  return NULL;
}

static bool a_is_old_than_b(struct timespec const *const a, struct timespec const *const b) {
  return a->tv_sec < b->tv_sec || (a->tv_sec == b->tv_sec && a->tv_nsec < b->tv_nsec);
}

static struct cache *cache_get(uint64_t const hash) {
  size_t oldest = 0;
  for (size_t i = 0; i < sizeof(g_caches) / sizeof(g_caches[0]); ++i) {
    if (g_caches[i].hash == hash) {
      return &g_caches[i];
    }
    if (a_is_old_than_b(&g_caches[i].used_at, &g_caches[oldest].used_at)) {
      oldest = i;
    }
  }
  return &g_caches[oldest];
}

static uint64_t calc_hash(struct wordwrap_settings const *const wws,
                          char const *const text,
                          size_t const text_len,
                          char const *const model_name,
                          size_t const model_name_len) {
  struct cyrb64_context ctx = {0};
  _Static_assert(sizeof(struct wordwrap_settings) % 4 == 0,
                 "Size of struct wordwrap_settings must be a multiple of 4.");
  cyrb64_init(&ctx, 0x68f6bc15);
  cyrb64_update(&ctx, (uint32_t const *)wws, sizeof(struct wordwrap_settings) / 4);
  if (text_len > 0) {
    size_t const n = text_len / 4;
    cyrb64_update(&ctx, (void const *)text, n);
    size_t const n4 = n * 4;
    if (n4 < text_len) {
      uint8_t buf[4] = {0};
      memcpy(buf, text + n4, text_len - n4);
      cyrb64_update(&ctx, (void const *)buf, 1);
    }
  }
  if (model_name_len > 0) {
    size_t const n = model_name_len / 4;
    cyrb64_update(&ctx, (void const *)model_name, n);
    size_t const n4 = n * 4;
    if (n4 < model_name_len) {
      uint8_t buf[4] = {0};
      memcpy(buf, model_name + n4, model_name_len - n4);
      cyrb64_update(&ctx, (void const *)buf, 1);
    }
  }
  return cyrb64_final(&ctx);
}

// TODO: support manually add or remove line break points.

struct model {
  char *name;
  struct budouxc *model;
  struct timespec used_at;
};

static struct model g_models[4] = {0};

static NODISCARD error model_get(char const *const name, struct budouxc **const model) {
  if (!name || !model) {
    return errg(err_invalid_arugment);
  }

  size_t oldest = 0;
  for (size_t i = 0; i < sizeof(g_models) / sizeof(g_models[0]); ++i) {
    if (g_models[i].name && strcmp(g_models[i].name, name) == 0) {
      *model = g_models[i].model;
      return eok();
    }
    if (a_is_old_than_b(&g_models[i].used_at, &g_models[oldest].used_at)) {
      oldest = i;
    }
  }

  error err = eok();
  HANDLE h = INVALID_HANDLE_VALUE;
  char *json = NULL;

  if (g_models[oldest].model) {
    budouxc_destroy(g_models[oldest].model);
    g_models[oldest].model = NULL;
  }

  char errormsg[128];
  if (strcmp(name, "ja") == 0) {
    g_models[oldest].model = budouxc_init_embedded_ja(NULL, errormsg);
  } else if (strcmp(name, "zh_hans") == 0) {
    g_models[oldest].model = budouxc_init_embedded_zh_hans(NULL, errormsg);
  } else if (strcmp(name, "zh_hant") == 0) {
    g_models[oldest].model = budouxc_init_embedded_zh_hant(NULL, errormsg);
  } else {
    h = CreateFileA(name, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (h == INVALID_HANDLE_VALUE) {
      err =
          emsg_i18n(err_type_hresult, HRESULT_FROM_WIN32(GetLastError()), gettext("failed to open BudouX model file."));
      goto cleanup;
    }
    LARGE_INTEGER sz;
    if (!GetFileSizeEx(h, &sz)) {
      err = emsg_i18n(err_type_hresult, HRESULT_FROM_WIN32(GetLastError()), gettext("failed to get file size."));
      goto cleanup;
    }
    if (sz.HighPart != 0) {
      err = emsg_i18n(err_type_generic, err_unexpected, gettext("file size is too large."));
      goto cleanup;
    }
    err = mem(&json, sz.LowPart, 1);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (!ReadFile(h, json, sz.LowPart, &sz.LowPart, NULL)) {
      err = emsg_i18n(err_type_hresult, HRESULT_FROM_WIN32(GetLastError()), gettext("failed to read file."));
      goto cleanup;
    }
    g_models[oldest].model = budouxc_init(NULL, json, sz.LowPart, errormsg);
  }
  if (!g_models[oldest].model) {
    err = emsg_i18n(err_type_generic, err_fail, errormsg);
    goto cleanup;
  }
  err = OV_ARRAY_GROW(&g_models[oldest].name, strlen(name) + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  strcpy(g_models[oldest].name, name);
  timespec_get(&g_models[oldest].used_at, TIME_UTC);
  *model = g_models[oldest].model;
cleanup:
  if (h != INVALID_HANDLE_VALUE) {
    CloseHandle(h);
  }
  if (json) {
    ereport(mem_free(json));
  }
  return err;
}

int luafn_wordwrap(lua_State *L) {
  error err = eok();
  struct wstr text = {0};
  struct wordwrap_settings wws;
  struct canvas_context *canvas = NULL;
  struct glyph *glyphs = NULL;
  struct wstr processed = {0};
  size_t *boundaries = NULL;

  int const num_params = lua_gettop(L);
  if (num_params < 1) {
    err = emsg_i18n(err_type_generic, err_invalid_arugment, gettext("too few arguments."));
    goto cleanup;
  }

  size_t text_mbcs_len = 0;
  char const *const text_mbcs = lua_tolstring(L, 1, &text_mbcs_len);
  if (!text_mbcs) {
    err = emsg_i18n(err_type_generic, err_invalid_arugment, gettext("text is not a string type."));
    goto cleanup;
  }

  err = initialize_params(num_params >= 2 ? L : NULL, 2, &wws);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  if (wws.mode == wwm_disabled) {
    lua_pushvalue(L, 1);
    goto cleanup;
  }

  char const *model_name = "ja";
  size_t model_name_len = strlen(model_name);
  if (wws.mode == wwm_budoux && num_params >= 2 && lua_istable(L, 2)) {
    lua_getfield(L, 2, "model");
    if (lua_isstring(L, -1)) {
      model_name = lua_tolstring(L, -1, &model_name_len);
    }
  }

  uint64_t const hash = calc_hash(&wws, text_mbcs, text_mbcs_len, model_name, model_name_len);
#if WW_DEBUG
  {
    wchar_t buf[128] = {0};
    wsprintfW(buf, L"hash: %08x %08x", (int)((hash >> 32) & 0xffffffff), (int)(hash & 0xffffffff));
    OutputDebugStringW(buf);
  }
#endif
  struct cache *cache = cache_find(hash);
  if (cache) {
    lua_pushlstring(L, cache->text.ptr, cache->text.len);
    goto cleanup;
  }

  err = from_mbcs(&(struct str const){.ptr = ov_deconster_(text_mbcs), .len = text_mbcs_len}, &text);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = canvas_context_create(&canvas);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  // Regardless of where line breaks are added, the width of each character remains unchanged,
  // and eventually the width of all characters is required, so first obtain the width of all characters.
  err = create_glyph_metrics_list(
      text.ptr, text.len, wws.monospace, wws.high_resolution, canvas, &wws.initial_font, &glyphs);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = sgrow(&processed, text.len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  struct budouxc *model = NULL;
  if (wws.mode == wwm_budoux) {
    err = model_get(model_name, &model);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }

#if WW_DEBUG
  OutputDebugStringW(L"================================");
  wchar_t buf[512] = {0};
#endif

  double x = 0;
  double x_min = INT_MAX;
  double x_max = INT_MIN;
  struct line_reader lr = {.text = &text, .glyphs = glyphs, .linehead = 0};
  size_t gpos = 0;
  size_t const num_glyphs = OV_ARRAY_LENGTH(glyphs);
  while (gpos < num_glyphs) {
    struct glyph const *const g = &glyphs[gpos];
    enum glyph_type const gt = glyph_get_type(g);
    if (gt == gt_break) {
      size_t const ln = g->pos - glyphs[lr.linehead].pos + 1;
      err = sncat(&processed, text.ptr + glyphs[lr.linehead].pos, ln);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      x = 0;
      x_min = INT_MAX;
      x_max = INT_MIN;
      lr.linehead = ++gpos;
      continue;
    }
    if (gt == gt_tag) {
      // Tags other than "position" do not affect the drawing position so can be ignored.
      if (g->u.tag_type != aviutl_text_tag_type_position) {
        ++gpos;
        continue;
      }
      struct aviutl_text_tag tag;
      struct aviutl_text_tag_position tag_pos;
      aviutl_text_parse_tag(text.ptr, text.len, g->pos, &tag);
      aviutl_text_get_position(text.ptr, &tag, &tag_pos);
      if (tag_pos.x_type == aviutl_text_tag_position_type_absolute) {
        x = 0;
        x_min = INT_MAX;
        x_max = INT_MIN;
        lr.linehead = ++gpos;
        continue;
      }
      x += tag_pos.x;
      ++gpos;
      continue;
    }
    x_min = dmin(x_min, x + (double)(g->u.metrics.ptGlyphOriginX));
    x_max = dmax(x_max, x + (double)(g->u.metrics.ptGlyphOriginX + g->u.metrics.BlackBoxX));
    x += (double)(g->u.metrics.CellIncX + wws.letter_spacing);

    if (x_max - x_min <= wws.max_width) {
      ++gpos;
#if WW_DEBUG
      wcsncpy(buf, text.ptr + g->pos, 1);
      wsprintfW(&buf[1],
                L"　w: %d min: %d max: %d ox: %d bbx: %d cix: %d",
                (int)(x_max - x_min),
                (int)x_min,
                (int)x_max,
                g->u.metrics.ptGlyphOriginX,
                g->u.metrics.BlackBoxX,
                g->u.metrics.CellIncX);
      OutputDebugStringW(buf);
#endif
      continue;
    }

    // NOTE: Overflow is permitted only if the smallest unit exceeds the limit to prevent infinite loops.

    size_t break_gpos = SIZE_MAX;

    if (wws.mode == wwm_norule) {
      break_gpos = find_breakpoint_norule(&lr, gpos);
    } else if (wws.mode == wwm_rule) {
      break_gpos = find_breakpoint_rule(&lr, gpos);
    } else if (wws.mode == wwm_budoux) {
      break_gpos = find_breakpoint_budoux(&lr, gpos, model, &boundaries);
    }

    if (break_gpos == SIZE_MAX) {
#if WW_DEBUG
      OutputDebugStringW(L"== failed to wrap ==");
#endif
      ++gpos;
      continue;
    }

    size_t const break_pos = glyphs[break_gpos].pos;
    err = sncat(&processed, text.ptr + glyphs[lr.linehead].pos, break_pos - glyphs[lr.linehead].pos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = scat(&processed, L"\n");
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }

    // Skip spaces at the beginning of the next line
    gpos = break_gpos;
    while (gpos < num_glyphs) {
      if (glyph_get_type(&glyphs[gpos]) == gt_tag) {
        ++gpos;
        continue;
      }
      wchar_t const ch = text.ptr[glyphs[gpos].pos];
      if (ch == L' ') {
        ++gpos;
        continue;
      }
      break;
    }

    lr.linehead = gpos;
    x = 0;
    x_min = INT_MAX;
    x_max = INT_MIN;
  }
  if (lr.linehead < num_glyphs) {
    err = scat(&processed, text.ptr + glyphs[lr.linehead].pos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
#if WW_DEBUG
  OutputDebugStringW(processed.ptr);
#endif

  struct cache *c = cache_get(hash);
  c->hash = hash;
  timespec_get(&c->used_at, TIME_UTC);
  err = to_mbcs(&processed, &c->text);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushstring(L, c->text.ptr);
cleanup:
  if (processed.ptr) {
    eignore(sfree(&processed));
  }
  if (boundaries) {
    OV_ARRAY_DESTROY(&boundaries);
  }
  if (canvas) {
    canvas_context_destroy(&canvas);
  }
  if (glyphs) {
    OV_ARRAY_DESTROY(&glyphs);
  }
  if (text.ptr) {
    eignore(sfree(&text));
  }
  return efailed(err) ? luafn_err(L, err) : 1;
}

void cleanup_wordwrap(void) {
  for (size_t i = 0; i < sizeof(g_caches) / sizeof(g_caches[0]); ++i) {
    if (g_caches[i].text.ptr) {
      eignore(sfree(&g_caches[i].text));
    }
  }
  for (size_t i = 0; i < sizeof(g_models) / sizeof(g_models[0]); ++i) {
    if (g_models[i].name) {
      OV_ARRAY_DESTROY(&g_models[i].name);
    }
    if (g_models[i].model) {
      budouxc_destroy(g_models[i].model);
      g_models[i].model = NULL;
    }
  }
}
