#pragma once

#include <stdbool.h>
#include <stddef.h>

/**
 * @brief A point in 2D space.
 */
struct point {
  double x;
  double y;
};

/**
 * @brief Type of the operation.
 */
enum point_op {
  pop_move_to = 0,
  pop_line_to = 1,
  pop_close = 2,
};

/**
 * @brief Callback function for each element of the obtained point.
 *
 * @param userdata User data.
 * @param op       Type of the operation.
 * @param pt       Point of the element.
 * @return true    Continue processing.
 * @return false   Abort processing.
 */
typedef bool (*point_op_fn)(void *const userdata, enum point_op const op, struct point const pt);

/**
 * @brief Comparison function for points.
 *
 * @param a The first point.
 * @param b The second point.
 * @param userdata Userdata.
 * @return int Same as strcmp.
 */
typedef int (*point_compare_fn)(struct point const *const a, struct point const *const b, void *const userdata);

/**
 * @brief Sorts an array of points.
 *
 * @param arr The array of points to sort.
 * @param n The number of points in the array.
 * @param compare The comparison function to use.
 * @param userdata Userdata to pass to the comparison function.
 */
void point_sort(struct point *const arr, size_t const n, point_compare_fn const compare, void *const userdata);
