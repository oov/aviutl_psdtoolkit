#pragma once

#include "point.h"

enum distance_find_nearest_method {
  dfnm_convex_hull,
  dfnm_box,
};

double distance_find_nearest(enum distance_find_nearest_method method,
                             struct point const *const a,
                             size_t const alen,
                             struct point const *const b,
                             size_t const blen,
                             struct point const forward);
