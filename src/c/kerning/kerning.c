#include "kerning.h"

#include <math.h>
#include <ovarray.h>

#include "convexhull.h"
#include "glyphoutline.h"
#include "kerning_pairs.h"

struct kerning_context {
  struct point pos;
  char *ggo_buffer;
  struct point *glyph;
  wchar_t cur_ch, prev_ch;
  double cur_width, prev_width;
  struct point *prev, *cur;
  struct convexhull_context *chctx;
  struct kerning_pairs *kp;
};

NODISCARD error kerning_context_create(struct kerning_context **const ctx) {
  if (!ctx || *ctx) {
    return errg(err_invalid_arugment);
  }
  struct kerning_context *c = NULL;
  error err = mem(&c, 1, sizeof(*c));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *c = (struct kerning_context){0};
  err = OV_ARRAY_GROW(&c->ggo_buffer, 4096);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = OV_ARRAY_GROW(&c->glyph, 32);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = OV_ARRAY_GROW(&c->prev, 32);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = OV_ARRAY_GROW(&c->cur, 32);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = convexhull_context_create(&c->chctx);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = kerning_pairs_create(&c->kp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *ctx = c;
cleanup:
  if (efailed(err)) {
    kerning_context_destroy(&c);
  }
  return err;
}

void kerning_context_destroy(struct kerning_context **const ctx) {
  if (!ctx || !*ctx) {
    return;
  }
  struct kerning_context *const c = *ctx;
  kerning_pairs_destroy(&c->kp);
  convexhull_context_destroy(&c->chctx);
  OV_ARRAY_DESTROY(&c->cur);
  OV_ARRAY_DESTROY(&c->prev);
  OV_ARRAY_DESTROY(&c->glyph);
  eignore(mem_free(ctx));
}

struct userdata {
  struct kerning_context *ctx;
  error *err;
};

static bool add_glyph_point(void *const userdata, enum point_op const op, struct point const pt) {
  if (op == pop_close) {
    return true;
  }
  struct userdata *const ud = userdata;
  *ud->err = OV_ARRAY_PUSH(&ud->ctx->glyph,
                           ((struct point){
                               .x = ud->ctx->pos.x + pt.x,
                               .y = ud->ctx->pos.y + pt.y,
                           }));
  if (efailed(*ud->err)) {
    return false;
  }
  return true;
}

NODISCARD static error get_glyph(
    struct kerning_context *const ctx, HDC const hdc, wchar_t const ch, TEXTMETRICW *const tm, GLYPHMETRICS *const gm) {
  error err = eok();
  if (!GetTextMetricsW(hdc, tm)) {
    err = errg(err_fail);
    goto cleanup;
  }
  static MAT2 const mat = {{0, 1}, {0, 0}, {0, 0}, {0, 1}};
  DWORD size = GetGlyphOutlineW(hdc, ch, GGO_NATIVE | GGO_UNHINTED, gm, 0, NULL, &mat);
  if (size == GDI_ERROR) {
    size = GetGlyphOutlineW(hdc, ch, GGO_METRICS, gm, 0, NULL, &mat);
    if (size != GDI_ERROR) {
      err = eok();
      goto cleanup;
    }
    err = errg(err_fail);
    goto cleanup;
  }
  if (size > OV_ARRAY_CAPACITY(ctx->ggo_buffer)) {
    err = OV_ARRAY_GROW(&ctx->ggo_buffer, size);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  size = GetGlyphOutlineW(hdc, ch, GGO_NATIVE | GGO_UNHINTED, gm, size, ctx->ggo_buffer, &mat);
  if (size == GDI_ERROR) {
    err = errg(err_fail);
    goto cleanup;
  }

  OV_ARRAY_SET_LENGTH(ctx->glyph, 0);
  if (!glyphoutline_parse(ctx->ggo_buffer,
                          size,
                          20.,
                          add_glyph_point,
                          &(struct userdata){
                              .ctx = ctx,
                              .err = &err,
                          })) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

static bool add_convex_point(void *const userdata, enum point_op const op, struct point const pt) {
  if (op == pop_close) {
    return true;
  }
  struct userdata *const ud = userdata;
  *ud->err = OV_ARRAY_PUSH(&ud->ctx->cur, pt);
  if (efailed(*ud->err)) {
    return false;
  }
  return true;
}

static struct point get_normalized_forward_vector(GLYPHMETRICS *gm) {
  double const x = gm->gmCellIncX;
  double const y = gm->gmCellIncY;
  double const l = sqrt(x * x + y * y);
  return (struct point){x / l, y / l};
}

static inline LONG lmax(LONG a, LONG b) { return a > b ? a : b; }

NODISCARD error kerning_calculate_distance(struct kerning_context *const ctx,
                                           struct kerning_style const *const ks,
                                           HDC const hdc,
                                           wchar_t const ch,
                                           struct point *const distance) {
  if (!ctx || !hdc || !ch || !distance) {
    return errg(err_invalid_arugment);
  }

  error err = eok();

  struct point *const tmp = ctx->prev;
  ctx->prev = ctx->cur;
  ctx->cur = tmp;
  OV_ARRAY_SET_LENGTH(ctx->cur, 0);
  ctx->prev_width = ctx->cur_width;
  ctx->prev_ch = ctx->cur_ch;

  TEXTMETRICW tm;
  GLYPHMETRICS gm;
  err = get_glyph(ctx, hdc, ch, &tm, &gm);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  ctx->cur_ch = ch;
  ctx->cur_width = (double)(lmax(tm.tmHeight, lmax(tm.tmAveCharWidth, tm.tmMaxCharWidth)));

  int const kern = kerning_pairs_get_kerning(ctx->kp, ctx->prev_ch, ctx->cur_ch);

  ctx->pos.x += gm.gmCellIncX;
  ctx->pos.y += gm.gmCellIncY;
  if (OV_ARRAY_LENGTH(ctx->glyph) == 0) {
    // space or etc.
    *distance = (struct point){0, 0};
    goto cleanup;
  }
  error err2 = eok();
  err = convexhull_create(ctx->chctx,
                          ctx->glyph,
                          OV_ARRAY_LENGTH(ctx->glyph),
                          add_convex_point,
                          &(struct userdata){
                              .ctx = ctx,
                              .err = &err2,
                          });
  if (efailed(err)) {
    if (eisg(err, err_abort)) {
      efree(&err);
      err = err2;
    }
    err = ethru(err);
    goto cleanup;
  }

  convexhull_sort(ctx->cur, OV_ARRAY_LENGTH(ctx->cur));

  struct point const v = get_normalized_forward_vector(&gm);

  // To ensure accurate distance calculation between characters,
  // we need to move the previous character's position down significantly.
  double const safe_width = ctx->prev_width * 10.;
  struct point const safe_distance = (struct point){v.x * safe_width, v.y * safe_width};
  for (size_t i = 0; i < OV_ARRAY_LENGTH(ctx->prev); ++i) {
    ctx->prev[i].x -= safe_distance.x;
    ctx->prev[i].y -= safe_distance.y;
  }

  if (!OV_ARRAY_LENGTH(ctx->cur) || !OV_ARRAY_LENGTH(ctx->prev)) {
    *distance = (struct point){0, 0};
    goto cleanup;
  }

  double d =
      distance_find_nearest(ks->method, ctx->prev, OV_ARRAY_LENGTH(ctx->prev), ctx->cur, OV_ARRAY_LENGTH(ctx->cur), v) -
      safe_width + (double)(kern);
  d = d * (ks->distance - 1.) + ks->margin * ks->margin_unit + (double)(kern);

  *distance = (struct point){v.x * d, v.y * d};
cleanup:
  return err;
}

void kerning_reset(struct kerning_context *const ctx) {
  if (!ctx) {
    return;
  }
  ctx->pos.x = ctx->pos.y = 0;
  OV_ARRAY_SET_LENGTH(ctx->prev, 0);
  OV_ARRAY_SET_LENGTH(ctx->cur, 0);
  ctx->prev_ch = 0;
  ctx->cur_ch = 0;
  ctx->prev_width = 0;
  ctx->cur_width = 0;
}

NODISCARD error kerning_update_font(struct kerning_context *const ctx, HDC const hdc) {
  if (!ctx || !hdc) {
    return errg(err_invalid_arugment);
  }
  error err = kerning_pairs_update_database(ctx->kp, hdc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  ctx->prev_ch = 0;
cleanup:
  return err;
}
