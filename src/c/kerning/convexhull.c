#include "convexhull.h"

#include <ovarray.h>
#include <ovbase.h>

#include <assert.h>
#include <math.h>

#include "point.h"

#define DEBUG_CONVEXHULL_ORIENTATION 0
#define DEBUG_CONVEXHULL_RESULT 0

#if DEBUG_CONVEXHULL_ORIENTATION || DEBUG_CONVEXHULL_RESULT
#  include "debug.h"
#endif

struct convexhull_context {
  struct point **ppts;
};

NODISCARD error convexhull_context_create(struct convexhull_context **const ctx) {
  if (!ctx || *ctx) {
    return errg(err_invalid_arugment);
  }
  struct convexhull_context *c = NULL;
  error err = mem(&c, 1, sizeof(*c));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *c = (struct convexhull_context){0};
  *ctx = c;
cleanup:
  return err;
}

void convexhull_context_destroy(struct convexhull_context **const ctx) {
  if (!ctx || !*ctx) {
    return;
  }
  struct convexhull_context *const c = *ctx;
  OV_ARRAY_DESTROY(&c->ppts);
  eignore(mem_free(ctx));
}

struct glyphoutline_callback_context {
  struct convexhull_context *ctx;
  error *err;
};

static int compare_points(struct point const *const a, struct point const *const b, void *const userdata) {
  (void)userdata;
  double r = a->x - b->x;
  if (r < 0.) {
    return -1;
  }
  if (r > 0.) {
    return 1;
  }
  r = a->y - b->y;
  if (r < 0.) {
    return -1;
  }
  if (r > 0.) {
    return 1;
  }
  return 0;
}

static inline double cross(struct point a, struct point b, struct point o) {
  return (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x);
}

NODISCARD error convexhull_create(struct convexhull_context *ctx,
                                  struct point *const pts,
                                  size_t const ptslen,
                                  point_op_fn const fn,
                                  void *const userdata) {
  if (!ctx || !pts || !ptslen || !fn) {
    return errg(err_invalid_arugment);
  }
  // This code uses the Andrew's monotone chain algorithm.
  // https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain
  error err = eok();
  point_sort(pts, ptslen, compare_points, NULL);

  err = OV_ARRAY_GROW(&ctx->ppts, ptslen);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  struct point **ppts = ctx->ppts;

  size_t len = 0;
  for (size_t i = 0; i < ptslen; ++i) {
    while (len >= 2 && cross(*ppts[len - 2], *ppts[len - 1], pts[i]) <= 0) {
      --len;
    }
    ppts[len++] = &pts[i];
  }
  --len;
  struct point *moveto = ppts[0];
  fn(userdata, pop_move_to, *moveto);
  for (size_t i = 1; i < len; ++i) {
    if (!fn(userdata, pop_line_to, *ppts[i])) {
      return errg(err_abort);
    }
  }

  len = 0;
  for (size_t i = ptslen - 1; i < ptslen; --i) {
    while (len >= 2 && cross(*ppts[len - 2], *ppts[len - 1], pts[i]) <= 0) {
      --len;
    }
    ppts[len++] = &pts[i];
  }
  --len;
  for (size_t i = 0; i < len; ++i) {
    if (!fn(userdata, pop_line_to, *ppts[i])) {
      return errg(err_abort);
    }
  }

  if (!fn(userdata, pop_close, *moveto)) {
    return errg(err_abort);
  }
  return eok();
}

static inline int quadrant(struct point const p) {
  if (p.x >= 0 && p.y >= 0) {
    return 1;
  }
  if (p.x <= 0 && p.y >= 0) {
    return 2;
  }
  if (p.x <= 0 && p.y <= 0) {
    return 3;
  }
  return 4;
}

static int compare_for_cw(struct point const *const a, struct point const *const b, void *userdata) {
  struct point const *const mid = userdata;
  struct point p = {a->x - mid->x, a->y - mid->y};
  struct point q = {b->x - mid->x, b->y - mid->y};
  int const one = quadrant(p);
  int const two = quadrant(q);

  if (one != two) {
    return (one < two) ? -1 : 1;
  }
  return (p.y * q.x < q.y * p.x) ? -1 : 1;
}

void convexhull_sort(struct point *const pts, size_t const ptslen) {
  struct point mid = {0};
  double const len = (double)ptslen;
  for (size_t i = 0; i < ptslen; i++) {
    mid.x += pts[i].x;
    mid.y += pts[i].y;
    pts[i].x *= len;
    pts[i].y *= len;
  }
  point_sort(pts, ptslen, compare_for_cw, &mid);
  double const lenr = 1. / len;
  for (size_t i = 0; i < ptslen; i++) {
    pts[i].x *= lenr;
    pts[i].y *= lenr;
  }
}

static inline double orientation(
#if DEBUG_CONVEXHULL_ORIENTATION
    struct point const *const a,
    size_t const llen,
    struct point const *const b,
    size_t const rlen,
#endif
    struct point const p,
    struct point const q,
    struct point const r) {
  double const o = (q.y - p.y) * (r.x - q.x) - (r.y - q.y) * (q.x - p.x);
#if DEBUG_CONVEXHULL_ORIENTATION
  create_bitmap(a, llen, b, rlen, (struct point const[]){p, q, r}, 3, o, 400, 130, L"convexhull_orientation.bmp");
  DebugBreak();
#endif
  if (o > 0.) {
    return 1.;
  }
  if (o < 0.) {
    return -1.;
  }
  return 0.;
}

static inline size_t inc(size_t const index, size_t const len) {
  size_t const r = index + 1;
  return r & -(r != len);
}

static inline size_t dec(size_t const index, size_t const len) {
  return ((index - 1) & -(index != 0)) | ((len - 1) & -(index == 0));
}

struct convexhull_tangent_indices convexhull_find_tangents(struct point const *const l,
                                                           size_t const llen,
                                                           struct point const *const r,
                                                           size_t const rlen) {
  // This code uses the algorithm from:
  // https://www.geeksforgeeks.org/tangents-two-convex-polygons/
  // However, this code contains multiple changes, so it is not implemented as is.
  size_t maxl = 0;
  for (size_t i = 1; i < llen; i++) {
    if (l[i].x > l[maxl].x) {
      maxl = i;
    }
  }
  size_t minr = 0;
  for (size_t i = 1; i < rlen; i++) {
    if (r[i].x < r[minr].x) {
      minr = i;
    }
  }

#if DEBUG_CONVEXHULL_ORIENTATION
#  define ORIENTATION(p_, q_, r_) orientation(l, llen, r, rlen, p_, q_, r_)
#else
#  define ORIENTATION(p_, q_, r_) orientation(p_, q_, r_)
#endif

  size_t sib;
  size_t upper_l_idx = maxl, upper_r_idx = minr;
  bool done = false;
  while (!done) {
    done = true;
    sib = inc(upper_l_idx, llen);
    while (ORIENTATION(r[upper_r_idx], l[upper_l_idx], l[sib]) >= 0) {
      upper_l_idx = sib;
      sib = inc(upper_l_idx, llen);
    }

    sib = dec(upper_r_idx, rlen);
    while (ORIENTATION(l[upper_l_idx], r[upper_r_idx], r[sib]) <= 0) {
      upper_r_idx = sib;
      sib = dec(upper_r_idx, rlen);
      done = false;
    }
  }
  size_t lower_l_idx = maxl, lower_r_idx = minr;
  done = false;
  while (!done) {
    done = true;
    sib = dec(lower_l_idx, llen);
    while (ORIENTATION(r[lower_r_idx], l[lower_l_idx], l[sib]) <= 0) {
      lower_l_idx = sib;
      sib = dec(lower_l_idx, llen);
    }

    sib = inc(lower_r_idx, rlen);
    while (ORIENTATION(l[lower_l_idx], r[lower_r_idx], r[sib]) >= 0) {
      lower_r_idx = sib;
      sib = inc(lower_r_idx, rlen);
      done = false;
    }
  }

#undef ORIENTATION

#if DEBUG_CONVEXHULL_ORIENTATION
  create_bitmap(l,
                llen,
                r,
                rlen,
                (struct point const[]){l[upper_l_idx], r[upper_r_idx]},
                2,
                0,
                400,
                130,
                L"convexhull_orientation.bmp");
  DebugBreak();
  create_bitmap(l,
                llen,
                r,
                rlen,
                (struct point const[]){l[lower_l_idx], r[lower_r_idx]},
                2,
                0,
                400,
                130,
                L"convexhull_orientation.bmp");
  DebugBreak();
#endif

  struct convexhull_tangent_indices const result = {
      .l_begin = upper_l_idx,
      .l_end = lower_l_idx,
      .r_begin = lower_r_idx,
      .r_end = upper_r_idx,
  };
#if DEBUG_CONVEXHULL_RESULT
  struct point pts[256];
  size_t len = 0;
  for (size_t i = result.l_begin;; i = inc(i, llen)) {
    pts[len++] = l[i];
    if (i == result.l_end) {
      break;
    }
  }
  for (size_t i = result.r_begin;; i = inc(i, rlen)) {
    pts[len++] = r[i];
    if (i == result.r_end) {
      break;
    }
  }
  pts[len++] = l[result.l_begin];
  create_bitmap(NULL, 0, NULL, 0, pts, len, 1, 400, 130, L"convexhull_result.bmp");
  DebugBreak();
#endif
  return result;
}
