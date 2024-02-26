#include "luafuncs_wordwrap.h"

#include <ovarray.h>
#include <ovbase.h>
#include <ovnum.h>
#include <ovutil/win32.h>

#include <lauxlib.h>
#include <math.h>

#include "aviutl.h"
#include "i18n.h"
#include "luautil.h"

#include "kerning/kerning.h"

#include "wordwrap/aviutl_text.h"
#include "wordwrap/bdx.h"
#include "wordwrap/bdx_cache.h"
#include "wordwrap/canvas.h"
#include "wordwrap/cyrb64.h"
#include "wordwrap/glyph.h"
#include "wordwrap/line_reader.h"
#include "wordwrap/rule.h"
#include "wordwrap/text_cache.h"

static struct kerning_context *g_kerning_ctx = NULL;

NODISCARD static error measure_glyph(struct canvas *canvas,
                                     enum glyph_type const typ,
                                     int const flags,
                                     wchar_t const ch,
                                     size_t const pos,
                                     size_t const len,
                                     bool const monospace,
                                     bool const high_resolution,
                                     struct glyph *const g) {
  error err = eok();
  GLYPHMETRICS gm;
  if (!canvas_get_metrics(canvas, ch, &gm, monospace, high_resolution)) {
    err = emsg_i18n(err_type_generic, err_unexpected, gettext("failed to get glyph metrics."));
    goto cleanup;
  }
  *g = (struct glyph){
      .typ = typ,
      .flags = flags,
      .pos = pos,
      .len = len,
      .u =
          {
              .glyph =
                  {
                      .ch = ch,
                      .BlackBoxX = (int16_t)gm.gmBlackBoxX,
                      .ptGlyphOriginX = (int16_t)gm.gmptGlyphOrigin.x,
                      .CellIncX = (int16_t)gm.gmCellIncX,
                  },
          },
  };
cleanup:
  return err;
}

NODISCARD static error add_kerning(struct canvas *canvas,
                                   bool const high_resolution,
                                   struct kerning_style const *const ks,
                                   wchar_t const ch,
                                   size_t const pos,
                                   struct glyph *const g) {
  struct point distance = {0};
  error err = kerning_calculate_distance(g_kerning_ctx, ks, canvas->dc, ch, &distance);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (high_resolution) {
    distance.x *= .5;
    distance.y *= .5;
  }
  *g = (struct glyph){
      .typ = gt_kerning,
      .pos = pos,
      .len = 0,
      .u =
          {
              .kerning =
                  {
                      .x = (int16_t)(round(distance.x)),
                      .y = (int16_t)(round(distance.y)),
                  },
          },
  };
cleanup:
  return err;
}

static size_t
parse_kerning_tag(struct kerning_style *ks, wchar_t const *const text, size_t const text_len, size_t const pos) {
  wchar_t const *const end = text + text_len;
  wchar_t const *p = text + pos;
  if (p + 6 > end) {
    return 0;
  }
  if (p[0] != L'<' || p[1] != L'k' || p[2] != L'e' || p[3] != L'r' || p[4] != L'n') {
    return 0;
  }
  p += 5;
  double distance = 0.5;
  double margin = 0.;
  enum distance_find_nearest_method method = dfnm_convex_hull;
  int sign = 0, dot = 0, i = 0, f = 0;
  size_t token = 0;
  wchar_t const *token_start = p;
  while (p < end) {
    switch (*p) {
    case L'0':
    case L'1':
    case L'2':
    case L'3':
    case L'4':
    case L'5':
    case L'6':
    case L'7':
    case L'8':
    case L'9':
      if (!dot) {
        ++i; // integer part
      } else {
        ++f; // fractional part
      }
      ++p;
      continue;
    case L'.':
      if (dot) {
        return 0; // 1..1 or 1.1. is invalid
      }
      dot = 1;
      ++p;
      continue;
    case L'+':
    case L'-':
      if (sign || i || f || dot) {
        return 0; // ++1 or 1+ is invalid
      }
      sign = *p == L'+' ? 1 : -1;
      ++p;
      continue;
    case L'>':
    case L',':
      if (dot && (!f || !i)) {
        return 0; // 1. or .1 is invalid
      }
      if (i + f > 0) {
        switch (token) {
        case 0:
          ov_atof_wchar(token_start, &distance, false);
          break;
        case 1:
          ov_atof_wchar(token_start, &margin, false);
          break;
        case 2:
          if (f || dot || sign) {
            return 0;
          } else {
            int64_t v;
            ov_atoi_wchar(token_start, &v, false);
            method = (enum distance_find_nearest_method)v;
            if (method != dfnm_convex_hull && method != dfnm_box) {
              return 0;
            }
          }
          break;
        default:
          return 0;
        }
      }
      if (*p == L'>') {
        goto cleanup;
      }
      if (token == 2) {
        return 0;
      }
      ++p;
      token_start = p;
      ++token;
      sign = 0;
      dot = 0;
      i = 0;
      f = 0;
      continue;
    default:
      return 0;
    }
  }
cleanup:
  ks->distance = distance;
  ks->margin = margin;
  ks->method = method;
  return (size_t)(p - (text + pos) + 1);
}

NODISCARD static error create_glyph_metrics_list(wchar_t const *const text,
                                                 size_t const text_len,
                                                 bool const monospace,
                                                 bool const high_resolution,
                                                 struct canvas *canvas,
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
  size_t glyphs_len = text_len;
  error err = eok();

  err = canvas_set_initial_font(canvas, initial_font, high_resolution);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = OV_ARRAY_GROW(&glyphs, glyphs_len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  size_t gpos = 0;
  int flags = 0;
  bool nobr = false;

  bool kerning = false;
  struct kerning_style ks = {0};

  struct aviutl_text_tag tag = {0};
  for (size_t i = 0; i < text_len; ++i) {
    if (text[i] == L'\n') {
      glyphs[gpos++] = (struct glyph){
          .typ = gt_break,
          .pos = i,
          .len = 1,
      };
      if (kerning) {
        kerning_reset(g_kerning_ctx);
      }
      continue;
    }
    tag.type = aviutl_text_tag_type_unknown;
    if (text[i] == L'<' || text[i] == L'&') {
      if (aviutl_text_parse_tag(text, text_len, i, &tag)) {
        if (tag.type == aviutl_text_tag_type_numcharref) {
          goto measure_glyph;
        }
        if (tag.type == aviutl_text_tag_type_font) {
          struct aviutl_text_tag_font font;
          aviutl_text_get_font(text, &tag, &font);
          err = canvas_set_font_params(canvas, font.name, font.size, font.bold, font.italic, high_resolution);
          if (efailed(err)) {
            err = ethru(err);
            goto cleanup;
          }
        }
        glyphs[gpos++] = (struct glyph){
            .typ = gt_tag,
            .pos = i,
            .len = tag.len,
            .u =
                {
                    .tag =
                        {
                            .type = tag.type,
                        },
                },
        };
        i += tag.len - 1;
        continue;
      } else if (text_len - i >= 5 && text[i + 1] == L'w' && text[i + 2] == L'b' && text[i + 3] == L'r' &&
                 text[i + 4] == L'>') {
        flags |= gt_breakable_by_wbr;
        glyphs[gpos++] = (struct glyph){
            .typ = gt_original_tag,
            .pos = i,
            .len = 5,
            .u =
                {
                    .original_tag =
                        {
                            .type = ot_wbr,
                        },
                },
        };
        i += 4;
        continue;
      } else if (text_len - i >= 6 && text[i + 1] == L'n' && text[i + 2] == L'o' && text[i + 3] == L'b' &&
                 text[i + 4] == L'r' && text[i + 5] == L'>') {
        nobr = true;
        glyphs[gpos++] = (struct glyph){
            .typ = gt_original_tag,
            .pos = i,
            .len = 6,
            .u =
                {
                    .original_tag =
                        {
                            .type = ot_nobr_open,
                        },
                },
        };
        i += 5;
        continue;
      } else if (text_len - i >= 7 && text[i + 1] == L'/' && text[i + 2] == L'n' && text[i + 3] == L'o' &&
                 text[i + 4] == L'b' && text[i + 5] == L'r' && text[i + 6] == L'>') {
        nobr = false;
        flags &= ~gt_not_breakable_by_nobr;
        glyphs[gpos++] = (struct glyph){
            .typ = gt_original_tag,
            .pos = i,
            .len = 7,
            .u =
                {
                    .original_tag =
                        {
                            .type = ot_nobr_close,
                        },
                },
        };
        i += 6;
        continue;
      } else if (text_len - i >= 6 && text[i + 1] == L'k' && text[i + 2] == L'e' && text[i + 3] == L'r' &&
                 text[i + 4] == L'n') {
        size_t const tag_len = parse_kerning_tag(&ks, text, text_len, i);
        if (tag_len) {
          glyphs[gpos++] = (struct glyph){
              .typ = gt_original_tag,
              .pos = i,
              .len = tag_len,
              .u =
                  {
                      .original_tag =
                          {
                              .type = ot_kern_open,
                          },
                  },
          };
          i += tag_len - 1;
          TEXTMETRICW tm;
          if (!GetTextMetricsW(canvas->dc, &tm)) {
            err = errg(err_fail);
            goto cleanup;
          }
          ks.margin_unit = (double)(tm.tmHeight) * .1;
          kerning = true;
          if (g_kerning_ctx) {
            kerning_reset(g_kerning_ctx);
          } else {
            err = kerning_context_create(&g_kerning_ctx);
            if (efailed(err)) {
              err = ethru(err);
              goto cleanup;
            }
          }
          continue;
        }
      } else if (text_len - i >= 7 && text[i + 1] == L'/' && text[i + 2] == L'k' && text[i + 3] == L'e' &&
                 text[i + 4] == L'r' && text[i + 5] == L'n' && text[i + 6] == L'>') {
        glyphs[gpos++] = (struct glyph){
            .typ = gt_original_tag,
            .pos = i,
            .len = 7,
            .u =
                {
                    .original_tag =
                        {
                            .type = ot_kern_close,
                        },
                },
        };
        i += 6;
        kerning = false;
        continue;
      }
    }
  measure_glyph: {
    enum glyph_type gt;
    wchar_t ch;
    size_t pos;
    size_t len;
    if (tag.type == aviutl_text_tag_type_numcharref) {
      i += tag.len - 1;
      gt = gt_glyph_numref;
      struct aviutl_text_tag_numcharref ncr;
      aviutl_text_get_numcharref(text, &tag, &ncr);
      ch = ncr.ch;
      pos = tag.pos;
      len = tag.len;
    } else {
      gt = gt_glyph;
      ch = text[i];
      pos = i;
      len = 1;
    }
    if (kerning) {
      err = add_kerning(canvas, high_resolution, &ks, ch, i, &glyphs[gpos]);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      if (glyphs[gpos].u.kerning.x != 0 || glyphs[gpos].u.kerning.y != 0) {
        ++gpos;
        err = OV_ARRAY_GROW(&glyphs, ++glyphs_len);
        if (efailed(err)) {
          err = ethru(err);
          goto cleanup;
        }
      }
    }
    err = measure_glyph(canvas, gt, flags, ch, pos, len, monospace, high_resolution, &glyphs[gpos++]);
  }
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    flags &= ~gt_breakable_by_wbr;
    if (nobr) {
      flags |= gt_not_breakable_by_nobr;
    }
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
  wwm_max,
};

static enum wordwrap_mode int_to_wordwrap_mode(int const v) {
  if (wwm_disabled <= v && v < wwm_max) {
    return (enum wordwrap_mode)v;
  }
  return wwm_norule;
}

static int wordwrap_mode_to_rule(enum wordwrap_mode const mode) {
  switch (mode) {
  case wwm_norule:
    return br_surrogate_low;
  case wwm_rule:
    return br_no_first_line_char | br_no_last_line_char | br_no_break_char | br_surrogate_low | br_signed_number |
           br_ascii_word;
  case wwm_budoux:
    return br_no_first_line_char | br_no_last_line_char | br_no_break_char | br_surrogate_low | br_signed_number |
           br_ascii_word | br_non_ascii_word | br_budoux;
  case wwm_disabled:
  case wwm_max:
    return 0;
  }
}

static inline int imax(int const a, int const b) { return a > b ? a : b; }
static inline int imin(int const a, int const b) { return a < b ? a : b; }
static inline double dmax(double const a, double const b) { return a > b ? a : b; }
static inline double dmin(double const a, double const b) { return a < b ? a : b; }
// static inline size_t zumax(size_t const a, size_t const b) { return a > b ? a : b; }
static inline size_t zumin(size_t const a, size_t const b) { return a < b ? a : b; }

struct wordwrap_settings {
  LOGFONTW initial_font;
  double adjust_last;
  double max_width;
  enum wordwrap_mode mode;
  int letter_spacing;
  bool monospace;
  bool high_resolution;
};

static void read_exedit_params(wchar_t *const fontname,
                               size_t *const size,
                               bool *const bold,
                               bool *const italic,
                               int *const letter_spacing,
                               bool *const vertical,
                               bool *const monospace,
                               bool *const high_resolution) {
  if (!aviutl_exedit_is_092()) {
    return;
  }
  intptr_t h = (intptr_t)(GetModuleHandleW(L"exedit.auf"));
  if (!h) {
    return;
  }
  FILTER const *const *const filterpp = (void *)(h + 0x1B2B10);
  FILTER const *const text_filter = (void *)(h + 0xB9878);
  FILTER const *const filterp = *filterpp;
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
  int const align = ex_data_ptr[4];
  *vertical = 9 <= align && align <= 17;
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

  double max_width = 800.;
  double adjust_last = 0.;
  int letter_spacing = 0;
  bool vertical = false;
  bool monospace = false;
  bool high_resolution = false;

  read_exedit_params(
      font_name, &font_size, &font_bold, &font_italic, &letter_spacing, &vertical, &monospace, &high_resolution);

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
        double const v = lua_tonumber(L, -1);
        if (v > 0) {
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

      lua_getfield(L, table_index, "vertical");
      if (!lua_isnil(L, -1)) {
        vertical = lua_toboolean(L, -1) ? true : false;
      }

      lua_getfield(L, table_index, "monospace");
      if (!lua_isnil(L, -1)) {
        monospace = lua_toboolean(L, -1) ? true : false;
      }

      lua_getfield(L, table_index, "high_resolution");
      if (!lua_isnil(L, -1)) {
        high_resolution = lua_toboolean(L, -1) ? true : false;
      }

      lua_getfield(L, table_index, "adjust_last");
      if (!lua_isnil(L, -1)) {
        double const v = lua_tonumber(L, -1);
        if (v > 0) {
          adjust_last = dmax(0., dmin(1., v));
        }
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
      .adjust_last = adjust_last,
  };
  if (vertical) {
    wws->initial_font.lfFaceName[0] = '@';
    memcpy(wws->initial_font.lfFaceName + 1, font_name, sizeof(wws->initial_font.lfFaceName) - 1);
  } else {
    memcpy(wws->initial_font.lfFaceName, font_name, sizeof(wws->initial_font.lfFaceName));
  }
cleanup:
  if (name.ptr) {
    eignore(sfree(&name));
  }
  return err;
}

static uint64_t calc_hash(struct wordwrap_settings const *const wws,
                          char const *const text,
                          size_t const text_len,
                          char const *const model_name,
                          size_t const model_name_len) {
  struct cyrb64 ctx = {0};
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

NODISCARD static error write_text(struct wstr const *const src,
                                  struct glyph *glyphs,
                                  size_t const linehead,
                                  size_t const end,
                                  struct wstr *const text) {
  error err = eok();
  size_t copypos = linehead, nch = 0;
  for (size_t gpos = linehead; gpos < end; ++gpos) {
    struct glyph const *const g = &glyphs[gpos];
    switch (g->typ) {
    case gt_glyph:
    case gt_glyph_numref:
    case gt_break:
    case gt_tag:
      nch += g->len;
      break;
    case gt_kerning: {
      if (g->u.kerning.x == 0 && g->u.kerning.y == 0) {
        break;
      }
      err = sncat(text, src->ptr + glyphs[copypos].pos, nch);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      wchar_t num[32], buf[32];
      buf[0] = L'\0';
      wcscat(buf, L"<p");
      if (g->u.kerning.x >= 0) {
        wcscat(buf, L"+");
      }
      wcscat(buf, ov_itoa_wchar(g->u.kerning.x, num));
      wcscat(buf, L",");
      if (g->u.kerning.y >= 0) {
        wcscat(buf, L"+");
      }
      wcscat(buf, ov_itoa_wchar(g->u.kerning.y, num));
      wcscat(buf, L">");
      err = scat(text, buf);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      copypos = gpos + 1;
      nch = 0;
    } break;
    case gt_original_tag: {
      err = sncat(text, src->ptr + glyphs[copypos].pos, nch);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      copypos = gpos + 1;
      nch = 0;
    } break;
    }
  }
  if (nch) {
    err = sncat(text, src->ptr + glyphs[copypos].pos, nch);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
cleanup:
  return err;
}

static double calc_current_line_width(struct wstr const *const text,
                                      struct line_reader const *const lr,
                                      struct wordwrap_settings const *const wws) {
  double x = 0;
  double x_min = INT_MAX;
  double x_max = INT_MIN;

  size_t gpos = lr->linehead;
  size_t const num_glyphs = OV_ARRAY_LENGTH(lr->glyphs);
  while (gpos < num_glyphs) {
    struct glyph const *const g = &lr->glyphs[gpos];
    if (g->typ == gt_break) {
      return x_max - x_min;
    }
    if (g->typ == gt_kerning) {
      x += g->u.kerning.x;
      ++gpos;
      continue;
    }
    if (g->typ == gt_tag) {
      // Tags other than "position" do not affect the drawing position so can be ignored.
      if (g->u.tag.type != aviutl_text_tag_type_position) {
        ++gpos;
        continue;
      }
      struct aviutl_text_tag tag;
      struct aviutl_text_tag_position tag_pos;
      aviutl_text_parse_tag(text->ptr, text->len, g->pos, &tag);
      aviutl_text_get_position(text->ptr, &tag, &tag_pos);
      if (tag_pos.x_type == aviutl_text_tag_position_type_absolute) {
        return x_max - x_min;
      }
      x += tag_pos.x;
      ++gpos;
      continue;
    }
    if (g->typ == gt_original_tag) {
      ++gpos;
      continue;
    }
    x_min = dmin(x_min, x + (double)(g->u.glyph.ptGlyphOriginX));
    x_max = dmax(x_max, x + (double)(g->u.glyph.ptGlyphOriginX + g->u.glyph.BlackBoxX));
    x += (double)(g->u.glyph.CellIncX + wws->letter_spacing);
    ++gpos;
    continue;
  }
  return x_max - x_min;
}

static double calc_max_width(struct wstr const *const text,
                             struct line_reader const *const lr,
                             struct wordwrap_settings const *const wws) {
  if (wws->adjust_last <= 0) {
    return wws->max_width;
  }
  double const current_line_width = calc_current_line_width(text, lr, wws);
  if (wws->max_width >= current_line_width) {
    return wws->max_width;
  }
  double const adjust_width = (wws->adjust_last + 1.) * wws->max_width;
  if (adjust_width >= current_line_width) {
    return current_line_width * (1. - wws->adjust_last);
  }
  return wws->max_width;
}

int luafn_wordwrap(lua_State *L) {
  error err = eok();
  struct wstr text = {0};
  struct wordwrap_settings wws;
  struct canvas *canvas = NULL;
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
#ifdef WW_DEBUG
  {
    wchar_t buf[128] = {0};
    wsprintfW(buf, L"hash: %08x %08x", (int)((hash >> 32) & 0xffffffff), (int)(hash & 0xffffffff));
    OutputDebugStringW(buf);
  }
#endif
  struct text_cache *cache = text_cache_get(hash);
  if (cache->hash == hash) {
    lua_pushlstring(L, cache->text.ptr, cache->text.len);
    goto cleanup;
  }

  err = from_mbcs(&(struct str const){.ptr = ov_deconster_(text_mbcs), .len = text_mbcs_len}, &text);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = canvas_create(&canvas);
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

#ifdef WW_DEBUG
  glyph_dump(glyphs);
#endif

  err = sgrow(&processed, text.len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  struct budouxc *model = NULL;
  if (wws.mode == wwm_budoux) {
    err = bdx_cache_get(model_name, &model);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }

#ifdef WW_DEBUG
  OutputDebugStringW(L"================================");
  wchar_t buf[512] = {0};
#endif

  struct line_reader lr = {.glyphs = glyphs, .linehead = 0};
  size_t gpos = 0;
  size_t const num_glyphs = OV_ARRAY_LENGTH(glyphs);
  double x = 0;
  double x_min = INT_MAX;
  double x_max = INT_MIN;
  double max_width = calc_max_width(&text, &lr, &wws);
  while (gpos < num_glyphs) {
    struct glyph const *const g = &glyphs[gpos];
    if (g->typ == gt_break) {
      err = write_text(&text, glyphs, lr.linehead, gpos + 1, &processed);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      x = 0;
      x_min = INT_MAX;
      x_max = INT_MIN;
      lr.linehead = ++gpos;
      max_width = calc_max_width(&text, &lr, &wws);
      continue;
    }
    if (g->typ == gt_kerning) {
      x += g->u.kerning.x;
      ++gpos;
      continue;
    }
    if (g->typ == gt_tag) {
      // Tags other than "position" do not affect the drawing position so can be ignored.
      if (g->u.tag.type != aviutl_text_tag_type_position) {
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
        max_width = calc_max_width(&text, &lr, &wws);
        continue;
      }
      x += tag_pos.x;
      ++gpos;
      continue;
    }
    if (g->typ == gt_original_tag) {
      ++gpos;
      continue;
    }
    x_min = dmin(x_min, x + (double)(g->u.glyph.ptGlyphOriginX));
    x_max = dmax(x_max, x + (double)(g->u.glyph.ptGlyphOriginX + g->u.glyph.BlackBoxX));
    x += (double)(g->u.glyph.CellIncX + wws.letter_spacing);

    if (x_max - x_min <= max_width) {
      ++gpos;
#ifdef WW_DEBUG
      wsprintfW(buf,
                L"%lc w: %d min: %d max: %d ox: %d bbx: %d cix: %d",
                g->u.glyph.ch,
                (int)(x_max - x_min),
                (int)x_min,
                (int)x_max,
                g->u.glyph.ptGlyphOriginX,
                g->u.glyph.BlackBoxX,
                g->u.glyph.CellIncX);
      OutputDebugStringW(buf);
#endif
      continue;
    }

    int const rule = wordwrap_mode_to_rule(wws.mode);
    if (!rule) {
      err = emsg_i18nf(
          err_type_generic, err_unexpected, NSTR("%d"), gettext("%1$d is invalid word wrap mode."), wws.mode);
      goto cleanup;
    }
    if (wws.mode == wwm_budoux && !(glyphs[lr.linehead].flags & gt_budoux_marked)) {
      err = bdx_write_markers(glyphs, lr.linehead, model, &boundaries);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
#ifdef WW_DEBUG
      glyph_dump(lr.glyphs);
#endif
    }

    size_t const break_gpos = line_reader_find_breakable(&lr, gpos, rule);
    if (break_gpos == SIZE_MAX) {
#ifdef WW_DEBUG
      OutputDebugStringW(L"== failed to wrap ==");
#endif
      ++gpos;
      continue;
    }

    err = write_text(&text, glyphs, lr.linehead, break_gpos, &processed);
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
      if (glyphs[gpos].typ == gt_kerning || glyphs[gpos].typ == gt_tag || glyphs[gpos].typ == gt_original_tag) {
        ++gpos;
        continue;
      }
      if (glyphs[gpos].u.glyph.ch == L' ') {
        ++gpos;
        continue;
      }
      break;
    }

    // if the line break point is detected again from the middle of the line,
    // the result will change unexpectedly.
    // so if it has already been detected, the same result must be reused on the next line.
    if (wws.mode == wwm_budoux && glyphs[lr.linehead].flags & gt_budoux_marked) {
      glyphs[gpos].flags |= gt_budoux_marked;
    }

    lr.linehead = gpos;
    x = 0;
    x_min = INT_MAX;
    x_max = INT_MIN;
    max_width = calc_max_width(&text, &lr, &wws);
  }
  if (lr.linehead < num_glyphs) {
    err = write_text(&text, glyphs, lr.linehead, num_glyphs, &processed);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
#ifdef WW_DEBUG
  OutputDebugStringW(processed.ptr);
#endif

  cache->hash = hash;
  err = to_mbcs(&processed, &cache->text);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushstring(L, cache->text.ptr);
cleanup:
  if (processed.ptr) {
    eignore(sfree(&processed));
  }
  if (boundaries) {
    OV_ARRAY_DESTROY(&boundaries);
  }
  if (canvas) {
    canvas_destroy(&canvas);
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
  if (g_kerning_ctx) {
    kerning_context_destroy(&g_kerning_ctx);
  }
  text_cache_cleanup();
  bdx_cache_cleanup();
}
