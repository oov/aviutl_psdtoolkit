#include <ovtest.h>

#include <math.h>

#include "point.h"

static int compare(struct point const *const a, struct point const *const b, void *const userdata) {
  (void)userdata;
  if (a->x < b->x)
    return -1;
  if (a->x > b->x)
    return 1;
  return 0;
}

static void test_point_sort(void) {
  struct point points[] = {
      {
          .x = 3.0,
          .y = 4.0,
      },
      {
          .x = 1.0,
          .y = 2.0,
      },
      {
          .x = 2.0,
          .y = 3.0,
      },
  };
  struct point const expected[] = {
      {
          .x = 1.0,
          .y = 2.0,
      },
      {
          .x = 2.0,
          .y = 3.0,
      },
      {
          .x = 3.0,
          .y = 4.0,
      },
  };
  point_sort(points, 3, compare, NULL);
  for (size_t i = 0; i < 3; i++) {
    TEST_CHECK(fabs(points[i].x - expected[i].x) < 1e-6);
    TEST_CHECK(fabs(points[i].y - expected[i].y) < 1e-6);
  }
}

TEST_LIST = {
    {"test_point_sort", test_point_sort},
    {0},
};
