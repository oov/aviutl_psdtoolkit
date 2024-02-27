#include "glyphoutline.h"

#include <math.h>
#include <stdint.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static inline int fixed_to_int(FIXED f) { return *(int const *)(void const *)&f; }

static inline FIXED int_to_fixed(int const i) { return *(FIXED const *)(void const *)&i; }

static inline FIXED fixed_avg2(FIXED a, FIXED b) { return int_to_fixed((fixed_to_int(a) + fixed_to_int(b) + 1) / 2); }

static inline FIXED fixed_avg3(FIXED a, FIXED b, FIXED c) {
  return int_to_fixed(((fixed_to_int(a) + fixed_to_int(b) + fixed_to_int(c)) * 2 + 3) / 6);
}

static inline double fixed_to_double(FIXED f) {
  static double const divider = 1. / 65536.;
  return (double)(fixed_to_int(f)) * divider;
}

static bool quadratic_bezier_interpolate(struct point const a,
                                         struct point const b,
                                         struct point const c,
                                         double const tolerance_squared,
                                         point_op_fn const opfn,
                                         void *const userdata) {
  double step = .5;
  struct point pt, prev = a;
  double s, distance_squared;

  double t = 0., t0, t1, t2;
  while (t <= 1.) {
    s = 1. - t;
    t0 = s * s;
    t1 = 2. * s * t;
    t2 = t * t;
    pt.x = a.x * t0 + b.x * t1 + c.x * t2;
    pt.y = a.y * t0 + b.y * t1 + c.y * t2;
    t0 = pt.x - prev.x;
    t1 = pt.y - prev.y;
    distance_squared = t0 * t0 + t1 * t1;

    if (distance_squared > tolerance_squared) {
      step *= .5;
      t -= step;
      continue;
    } else {
      step *= 1.25;
    }
    if (!opfn(userdata, pop_line_to, pt)) {
      return false;
    }
    prev = pt;
    t += step;
  }
  return opfn(userdata, pop_line_to, c);
}

static bool cubic_bezier_interpolate(struct point const a,
                                     struct point const b,
                                     struct point const c,
                                     struct point const d,
                                     double const tolerance_squared,
                                     point_op_fn const opfn,
                                     void *const userdata) {
  double step = .5;
  double s, distance_squared;

  struct point pt, prev = a;

  double t = 0., t0, t1, t2, t3, ss, tt;
  while (t <= 1.) {
    s = 1. - t;
    ss = s * s;
    tt = t * t;
    t0 = s * ss;
    t1 = 3. * ss * t;
    t2 = 3. * s * tt;
    t3 = tt * t;
    pt.x = a.x * t0 + b.x * t1 + c.x * t2 + d.x * t3;
    pt.y = a.y * t0 + b.y * t1 + c.y * t2 + d.y * t3;
    t0 = pt.x - prev.x;
    t1 = pt.y - prev.y;
    distance_squared = t0 * t0 + t1 * t1;

    if (distance_squared > tolerance_squared) {
      step *= .5;
      t -= step;
      continue;
    } else {
      step *= 1.25;
    }
    if (!opfn(userdata, pop_line_to, pt)) {
      return false;
    }
    prev = pt;
    t += step;
  }
  return opfn(userdata, pop_line_to, d);
}

bool glyphoutline_parse(
    void const *const p, size_t const sz, double const tolerance, point_op_fn const opfn, void *const userdata) {
  char const *glyph = p;
  char const *const glyph_end = glyph + sz;
  double const tolerance_squared = tolerance * tolerance;
  while (glyph < glyph_end) {
    TTPOLYGONHEADER const *const header = (TTPOLYGONHEADER const *)(void const *)glyph;
    char const *polyline = glyph + sizeof(TTPOLYGONHEADER);
    char const *const polyline_end = glyph + header->cb;
    struct point pt0 = {
        .x = fixed_to_double(header->pfxStart.x),
        .y = fixed_to_double(header->pfxStart.y),
    };
    if (!opfn(userdata, pop_move_to, pt0)) {
      return false;
    }
    while (polyline < polyline_end) {
      TTPOLYCURVE const *const curve = (TTPOLYCURVE const *)(void const *)polyline;
      switch (curve->wType) {
      case TT_PRIM_LINE: {
        for (WORD i = 0; i < curve->cpfx; ++i) {
          struct point const pt1 = {
              .x = fixed_to_double(curve->apfx[i].x),
              .y = fixed_to_double(curve->apfx[i].y),
          };
          if (!opfn(userdata, pop_line_to, pt1)) {
            return false;
          }
          pt0 = pt1;
        }
      } break;
      case TT_PRIM_QSPLINE: {
        for (WORD i = 0; i < curve->cpfx - 1; ++i) {
          POINTFX b = curve->apfx[i];
          POINTFX c = curve->apfx[i + 1];
          if (i < curve->cpfx - 2) {
            c.x = fixed_avg2(b.x, c.x);
            c.y = fixed_avg2(b.y, c.y);
          }
          struct point const pt1 = {
              .x = fixed_to_double(b.x),
              .y = fixed_to_double(b.y),
          };
          struct point const pt2 = {
              .x = fixed_to_double(c.x),
              .y = fixed_to_double(c.y),
          };
          if (!quadratic_bezier_interpolate(pt0, pt1, pt2, tolerance_squared, opfn, userdata)) {
            return false;
          }
          pt0 = pt2;
        }
      } break;
      case TT_PRIM_CSPLINE: {
        for (WORD i = 0; i < curve->cpfx - 2; ++i) {
          POINTFX const b = curve->apfx[i];
          POINTFX const c = curve->apfx[i + 1];
          POINTFX d = curve->apfx[i + 2];
          if (i < curve->cpfx - 3) {
            d.x = fixed_avg3(b.x, c.x, d.x);
            d.y = fixed_avg3(b.y, c.y, d.y);
          }
          struct point const pt1 = {
              .x = fixed_to_double(b.x),
              .y = fixed_to_double(b.y),
          };
          struct point const pt2 = {
              .x = fixed_to_double(c.x),
              .y = fixed_to_double(c.y),
          };
          struct point const pt3 = {
              .x = fixed_to_double(d.x),
              .y = fixed_to_double(d.y),
          };
          if (!cubic_bezier_interpolate(pt0, pt1, pt2, pt3, tolerance_squared, opfn, userdata)) {
            return false;
          }
          pt0 = pt3;
        }
      } break;
      default:
        return false;
      }
      polyline += sizeof(TTPOLYCURVE) + sizeof(POINTFX) * (curve->cpfx - 1);
    }
    pt0 = (struct point){
        .x = fixed_to_double(header->pfxStart.x),
        .y = fixed_to_double(header->pfxStart.y),
    };
    if (!opfn(userdata, pop_close, pt0)) {
      return false;
    }
    glyph += header->cb;
  }
  return true;
}
