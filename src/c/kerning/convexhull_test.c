#include <ovtest.h>

#include <math.h>

#include "convexhull.h"

struct context {
  struct point *golden;
  enum point_op *golden_ops;
  size_t n;
};

static bool callback(void *const userdata, enum point_op const op, struct point const pt) {
  struct context *ctx = userdata;
  TEST_CHECK(fabs(ctx->golden[ctx->n].x - pt.x) < 1e-6);
  TEST_MSG("want ctx->golden[%zu].x = %f, got pt.x = %f", ctx->n, ctx->golden[ctx->n].x, pt.x);
  TEST_CHECK(fabs(ctx->golden[ctx->n].y - pt.y) < 1e-6);
  TEST_MSG("want ctx->golden[%zu].y = %f, got pt.y = %f", ctx->n, ctx->golden[ctx->n].y, pt.y);
  TEST_CHECK(ctx->golden_ops[ctx->n] == op);
  TEST_MSG("want ctx->golden_ops[%zu] = %d, got op = %d", ctx->n, ctx->golden_ops[ctx->n], op);
  ++ctx->n;
  return true;
}

static void test_convexhull_create(void) {
  struct convexhull_context *cctx = NULL;
  TEST_SUCCEEDED_F(convexhull_context_create(&cctx));
  TEST_ASSERT(cctx != NULL);

  struct point points[] = {
      {21.437500, 37.500000},
      {21.437500, 0.687500},
      {21.437500, -4.203125},
      {14.015625, -4.203125},
      {14.015625, 77.484375},
      {21.437500, 6.890625},
      {21.437500, 43.406250},
      {77.250000, -4.203125},
      {21.437500, 71.390625},
      {77.250000, 0.687500},
      {77.250000, 71.390625},
      {77.250000, 43.406250},
      {84.906250, -4.203125},
      {77.250000, 6.890625},
      {77.250000, 37.500000},
      {84.906250, 77.484375},
  };
  struct point golden_points[] = {
      {14.015625, -4.203125},
      {84.90625, -4.203125},
      {84.90625, 77.484375},
      {14.015625, 77.484375},
      {14.015625, -4.203125},
  };
  enum point_op golden_ops[] = {pop_move_to, pop_line_to, pop_line_to, pop_line_to, pop_close};
  size_t sz = sizeof(points) / sizeof(points[0]);
  struct context ctx = {
      .golden = golden_points,
      .golden_ops = golden_ops,
      .n = 0,
  };
  TEST_SUCCEEDED_F(convexhull_create(cctx, points, sz, callback, &ctx));
  size_t const golden_n = sizeof(golden_points) / sizeof(golden_points[0]);
  TEST_CHECK(ctx.n == golden_n);
  TEST_MSG("want ctx.n = %zu, got %zu", golden_n, ctx.n);
  convexhull_context_destroy(&cctx);
}

static void test_convexhull_sort(void) {
  struct point points[] = {{1.0, 1.0}, {3.0, 1.0}, {3.0, 3.0}, {1.0, 3.0}};
  convexhull_sort(points, 4);
  TEST_CHECK(fabs(points[0].x - 3.0) < 1e-6);
  TEST_CHECK(fabs(points[0].y - 3.0) < 1e-6);
  TEST_CHECK(fabs(points[1].x - 1.0) < 1e-6);
  TEST_CHECK(fabs(points[1].y - 3.0) < 1e-6);
  TEST_CHECK(fabs(points[2].x - 1.0) < 1e-6);
  TEST_CHECK(fabs(points[2].y - 1.0) < 1e-6);
  TEST_CHECK(fabs(points[3].x - 3.0) < 1e-6);
  TEST_CHECK(fabs(points[3].y - 1.0) < 1e-6);
}

static void test_convexhull_find_tangents(void) {
  struct point l0[] = {
      {74.640381, 63.572144}, {75.984375, 82.203125}, {75.273193, 85.368652}, {73.843750, 90.703125},
      {67.275391, 92.616211}, {55.781250, 94.515625}, {33.906250, 94.515625}, {26.250977, 94.282715},
      {7.343750, 93.250000},  {4.703125, 91.578125},  {3.151123, 86.556641},  {2.156250, 77.328125},
      {0.890625, 62.281250},  {0.773438, 57.578369},  {1.417725, 39.269775},  {2.164307, 19.865479},
      {3.437500, 14.828125},  {4.312500, 12.671875},  {13.997358, 13.311542}, {26.738495, 14.491005},
      {36.929688, 15.710953}, {46.499739, 17.279215}, {58.754120, 20.234407}, {68.281250, 23.328125},
      {70.808105, 33.945679}, {73.593750, 52.320313},
  };
  struct point r0[] = {
      {173.359375, 80.546875},
      {149.328125, 97.046875},
      {144.391602, 98.336670},
      {136.046875, 98.718750},
      {130.000000, 96.656250},
      {96.593750, 74.593750},
      {88.390625, 33.093750},
      {91.812500, 25.859375},
      {93.777588, 24.319824},
      {96.984375, 22.046875},
      {127.453125, 8.093750},
      {132.473877, 8.870850},
      {140.343750, 11.015625},
      {181.953125, 26.250000},
      {188.390625, 36.406250},
      {188.384766, 38.847412},
  };
#if 0
  struct point l1[] = {
      {173.359375, 80.546875},
      {149.328125, 97.046875},
      {144.391602, 98.336670},
      {136.046875, 98.718750},
      {130.000000, 96.656250},
      {96.593750, 74.593750},
      {88.390625, 33.093750},
      {91.812500, 25.859375},
      {93.777588, 24.319824},
      {96.984375, 22.046875},
      {127.453125, 8.093750},
      {132.473877, 8.870850},
      {140.343750, 11.015625},
      {181.953125, 26.250000},
      {188.390625, 36.406250},
      {188.384766, 38.847412},
  };
  struct point r1[] = {
      {282.437500, 92.359375},
      {271.921387, 93.446289},
      {254.218750, 94.515625},
      {222.187500, 96.171875},
      {217.620850, 95.872314},
      {209.921875, 95.343750},
      {205.304688, 94.899170},
      {197.765625, 93.640625},
      {188.000000, 74.984375},
      {191.189697, 19.472656},
      {192.687500, 15.312500},
      {236.640625, 13.562500},
      {244.001099, 14.090578},
      {256.117188, 15.703140},
      {263.241089, 17.015631},
      {274.718750, 19.515625},
      {286.640625, 57.203125},
  };
  struct point l2[] = {
      {0, 0},
      {1, 0},
      {1, 1},
      {0, 1},
  };
  struct point r2[] = {
      {0, 0},
      {1, 0},
      {1, 1},
      {0, 1},
  };
#endif

  struct {
    struct point *l;
    size_t l_size;
    struct point *r;
    size_t r_size;
    struct convexhull_tangent_indices indices_golden;
  } tests[] = {
      {l0, sizeof(l0) / sizeof(l0[0]), r0, sizeof(r0) / sizeof(r0[0]), {7, 17, 10, 3}},
      // reversed pattern is not supported
      // {r0, sizeof(r0) / sizeof(r0[0]), l0, sizeof(l0) / sizeof(l0[0]), {-1, -1, -1, -1}},
      // convex hulls are neary intersected, current code cannot handle this case
      // {l1, sizeof(l1) / sizeof(l1[0]), r1, sizeof(r1) / sizeof(r1[0]), {-1, -1, -1, -1}},
      // completely overlapped convex hulls pattern is not supported
      // {l2, sizeof(l2) / sizeof(l2[2]), r2, sizeof(r2) / sizeof(r2[0]), {-1, -1, -1, -1}},
  };

  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); ++i) {
    TEST_CASE_("test %zu", i);
    convexhull_sort(tests[i].l, tests[i].l_size);
    convexhull_sort(tests[i].r, tests[i].r_size);
    struct convexhull_tangent_indices const indices =
        convexhull_find_tangents(tests[i].l, tests[i].l_size, tests[i].r, tests[i].r_size);
    TEST_CHECK(indices.l_begin == tests[i].indices_golden.l_begin);
    TEST_MSG("want indices.l_begin = %zu, got %zu", tests[i].indices_golden.l_begin, indices.l_begin);
    TEST_CHECK(indices.l_end == tests[i].indices_golden.l_end);
    TEST_MSG("want indices.l_end = %zu, got %zu", tests[i].indices_golden.l_end, indices.l_end);
    TEST_CHECK(indices.r_begin == tests[i].indices_golden.r_begin);
    TEST_MSG("want indices.r_begin = %zu, got %zu", tests[i].indices_golden.r_begin, indices.r_begin);
    TEST_CHECK(indices.r_end == tests[i].indices_golden.r_end);
    TEST_MSG("want indices.r_end = %zu, got %zu", tests[i].indices_golden.r_end, indices.r_end);
  }
}

TEST_LIST = {
    {"test_convexhull_create", test_convexhull_create},
    {"test_convexhull_sort", test_convexhull_sort},
    {"test_convexhull_find_tangents", test_convexhull_find_tangents},
    {0},
};
