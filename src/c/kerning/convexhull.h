#pragma once

#include <ovbase.h>

#include "point.h"

/**
 * @brief A context for convex hull operations.
 */
struct convexhull_context;

/**
 * @brief Create a convex hull context.
 *
 * @param ctx The context to create.
 * @return An error code.
 */
NODISCARD error convexhull_context_create(struct convexhull_context **const ctx);

/**
 * @brief Destroy a convex hull context.
 *
 * @param ctx The context to destroy.
 */
void convexhull_context_destroy(struct convexhull_context **const ctx);

/**
 * @brief Create a convex hull from the given points.
 *
 * @param ctx The context to use.
 * @param pts The points to use. This array will be modified for sorting.
 * @param ptslen The number of points.
 * @param fn The callback function to use.
 * @param userdata The userdata to pass to the callback function.
 * @return An error code.
 */
NODISCARD error convexhull_create(struct convexhull_context *ctx,
                                  struct point *const pts,
                                  size_t const ptslen,
                                  point_op_fn const fn,
                                  void *const userdata);

/**
 * @brief Sort the points of a convex hull.
 *
 * @param pts The points to sort. This array will be modified.
 * @param ptslen The number of points.
 */
void convexhull_sort(struct point *const pts, size_t const ptslen);

/**
 * @brief Used to store the indices of the tangent points of two convex hulls.
 */
struct convexhull_tangent_indices {
  size_t l_begin, l_end;
  size_t r_begin, r_end;
};

/**
 * @brief Find the indices of the tangent points of two convex hulls.
 * a and b must be sorted in clockwise order.
 *
 * @param l The first convex hull.
 * @param llen The number of points in the first convex hull.
 * @param r The second convex hull.
 * @param rlen The number of points in the second convex hull.
 * @return The indices of the tangent points.
 */
struct convexhull_tangent_indices convexhull_find_tangents(struct point const *const l,
                                                           size_t const llen,
                                                           struct point const *const r,
                                                           size_t const rlen);
