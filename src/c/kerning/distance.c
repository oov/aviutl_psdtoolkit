#include "distance.h"

#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "convexhull.h"

static bool is_intersecting(struct point const a,
                            struct point const b,
                            struct point const ray_origin,
                            struct point const ray_direction) {
  struct point const ab = {b.x - a.x, b.y - a.y};
  double const d = -ray_direction.x * ab.y + ray_direction.y * ab.x;
  if (fabs(d) < 1e-6) {
    return false;
  }

  struct point const ao = {ray_origin.x - a.x, ray_origin.y - a.y};
  double const u = (ao.x * ray_direction.y - ao.y * ray_direction.x) / d;
  return 0 <= u && u <= 1;
}

static double calculate_distance_squared(struct point const a,
                                         struct point const b,
                                         struct point const ray_origin,
                                         struct point const ray_direction) {
  struct point const ab = {b.x - a.x, b.y - a.y};
  double const d = -ray_direction.x * ab.y + ray_direction.y * ab.x;
  if (fabs(d) < 1e-6) {
    // If d is 0, AB and ray are not intersect.
    return HUGE_VAL;
  }

  double const dd = 1. / d;

  struct point const ao = {ray_origin.x - a.x, ray_origin.y - a.y};
  double const u = (ao.x * ray_direction.y - ao.y * ray_direction.x) * dd;
  if (u < 0 || u > 1) {
    // The intersection is outside AB.
    return HUGE_VAL;
  }

  double const t = (ao.x * ab.y - ao.y * ab.x) * dd;
  struct point const intersection = {ray_origin.x + t * ray_direction.x, ray_origin.y + t * ray_direction.y};
  struct point const pt = {intersection.x - ray_origin.x, intersection.y - ray_origin.y};
  double distance_squared = pt.x * pt.x + pt.y * pt.y;
  if (t < 0) {
    // Already passed by.
    distance_squared = -distance_squared;
  }
  return distance_squared;
}

static inline size_t dec(size_t const index, size_t const len) {
  return ((index - 1) & -(index != 0)) | ((len - 1) & -(index == 0));
}

static size_t find_farthest(struct point const *const pts,
                            size_t const ptslen,
                            size_t const begin,
                            size_t const end,
                            struct point const direction) {
  size_t farthest = SIZE_MAX;
  double distance = -HUGE_VAL;
  for (size_t i = begin;; i = dec(i, ptslen)) {
    struct point pt = pts[i];
    double const d = pt.x * direction.x + pt.y * direction.y;
    if (d > distance) {
      distance = d;
      farthest = i;
    }
    if (i == end) {
      break;
    }
  }
  return farthest;
}

static double calculate_min_distance_squared(struct point const *const l,
                                             size_t const llen,
                                             struct point const *const r,
                                             size_t const rlen,
                                             struct convexhull_tangent_indices il,
                                             struct point const direction) {
  double distance_squared = HUGE_VAL;
  for (size_t i = il.l_begin;; i = dec(i, llen)) {
    struct point ipt = l[i];
    if (is_intersecting(r[il.r_begin], r[il.r_end], ipt, direction)) {
      struct point prev = r[il.r_begin], jpt;
      for (size_t j = dec(il.r_begin, rlen);; prev = jpt, j = dec(j, rlen)) {
        jpt = r[j];
        distance_squared = fmin(distance_squared, calculate_distance_squared(prev, jpt, ipt, direction));
        if (j == il.r_end) {
          break;
        }
      }
    }
    if (i == il.l_end) {
      break;
    }
  }
  return distance_squared;
}

double distance_find_nearest(enum distance_find_nearest_method method,
                             struct point const *const l,
                             size_t const llen,
                             struct point const *const r,
                             size_t const rlen,
                             struct point const forward) {
  struct point const reverse = (struct point){-forward.x, -forward.y};
  struct convexhull_tangent_indices il = convexhull_find_tangents(l, llen, r, rlen);
  if (method == dfnm_convex_hull) {
    double const lr_squared = calculate_min_distance_squared(l, llen, r, rlen, il, forward);
    double const rl_squared = calculate_min_distance_squared(r,
                                                             rlen,
                                                             l,
                                                             llen,
                                                             (struct convexhull_tangent_indices){
                                                                 .l_begin = il.r_begin,
                                                                 .l_end = il.r_end,
                                                                 .r_begin = il.l_begin,
                                                                 .r_end = il.l_end,
                                                             },
                                                             reverse);
    double const distance_squared = fmin(lr_squared, rl_squared);
    if (isfinite(distance_squared)) {
      return sqrt(distance_squared);
    }
  }
  size_t const farthest_l = find_farthest(l, llen, il.l_begin, il.l_end, forward);
  size_t const farthest_r = find_farthest(r, rlen, il.r_begin, il.r_end, reverse);
  struct point const pt_l = l[farthest_l];
  struct point const pt_r = r[farthest_r];
  struct point const d = {pt_r.x - pt_l.x, pt_r.y - pt_l.y};
  double const dot = d.x * forward.x + d.y * forward.y;
  return dot < 0 ? 0 : dot / (forward.x * forward.x + forward.y * forward.y);
}
