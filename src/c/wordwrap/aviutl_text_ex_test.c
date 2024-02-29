#include <ovtest.h>

#include "aviutl_text_ex.h"

static bool test_tag(struct aviutl_text_ex_tag const *const expect,
                     struct aviutl_text_ex_tag const *const got,
                     char const *const file,
                     int const line,
                     char const *const msg) {
  bool r = acutest_check_(expect->type == got->type && expect->pos == got->pos && expect->len == got->len &&
                              expect->value_pos[0] == got->value_pos[0] && expect->value_pos[1] == got->value_pos[1] &&
                              expect->value_pos[2] == got->value_pos[2] && expect->value_len[0] == got->value_len[0] &&
                              expect->value_len[1] == got->value_len[1] && expect->value_len[2] == got->value_len[2],
                          file,
                          line,
                          "%s",
                          msg);
  if (expect->type != got->type) {
    TEST_MSG("expect->type: %d, got->type: %d", expect->type, got->type);
  }
  if (expect->pos != got->pos) {
    TEST_MSG("expect->pos: %zu, got->pos: %zu", expect->pos, got->pos);
  }
  if (expect->len != got->len) {
    TEST_MSG("expect->len: %zu, got->len: %zu", expect->len, got->len);
  }
  if (expect->value_pos[0] != got->value_pos[0]) {
    TEST_MSG("expect->value_pos[0]: %zu, got->value_pos[0]: %zu", expect->value_pos[0], got->value_pos[0]);
  }
  if (expect->value_len[0] != got->value_len[0]) {
    TEST_MSG("expect->value_len[0]: %zu, got->value_len[0]: %zu", expect->value_len[0], got->value_len[0]);
  }
  if (expect->value_pos[1] != got->value_pos[1]) {
    TEST_MSG("expect->value_pos[1]: %zu, got->value_pos[1]: %zu", expect->value_pos[1], got->value_pos[1]);
  }
  if (expect->value_len[1] != got->value_len[1]) {
    TEST_MSG("expect->value_len[1]: %zu, got->value_len[1]: %zu", expect->value_len[1], got->value_len[1]);
  }
  if (expect->value_pos[2] != got->value_pos[2]) {
    TEST_MSG("expect->value_pos[2]: %zu, got->value_pos[2]: %zu", expect->value_pos[2], got->value_pos[2]);
  }
  if (expect->value_len[2] != got->value_len[2]) {
    TEST_MSG("expect->value_len[2]: %zu, got->value_len[2]: %zu", expect->value_len[2], got->value_len[2]);
  }
  return r;
}
#define TEST_TAG(expect, got) test_tag(expect, got, __FILE__, __LINE__, #expect " == " #got)

static inline int fcompare(double x, double y, double tolerance) {
  return (x > y + tolerance) ? 1 : (y > x + tolerance) ? -1 : 0;
}
#define fcmp(x, op, y, tolerance) ((fcompare((x), (y), (tolerance)))op 0)

static void test_aviutl_text_ex_position(void) {
  struct test_case {
    aviutl_text_ex_char const *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_ex_tag expected_tag;
    struct aviutl_text_ex_tag_position expected_pos_tag;
  } const test_cases[] = {
      {
          .text = L"<pp0,+12.345>Hello",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_position,
                  .pos = 0,
                  .len = 13,
                  .value_pos = {3, 5, SIZE_MAX},
                  .value_len = {1, 7, 0},
              },
          .expected_pos_tag =
              {
                  .x = 0,
                  .x_type = aviutl_text_ex_tag_position_type_absolute,
                  .y = 12.345,
                  .y_type = aviutl_text_ex_tag_position_type_relative,
                  .z = 0,
                  .z_type = aviutl_text_ex_tag_position_type_unknown,
              },
      },
      {
          .text = L"Hello<pp+,>Hello",
          .parse_pos = 5,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_position,
                  .pos = 5,
                  .len = 6,
                  .value_pos = {8, 10, SIZE_MAX},
                  .value_len = {1, 0, 0},
              },
          .expected_pos_tag =
              {
                  .x = 0,
                  .x_type = aviutl_text_ex_tag_position_type_relative,
                  .y = 0,
                  .y_type = aviutl_text_ex_tag_position_type_absolute,
                  .z = 0,
                  .z_type = aviutl_text_ex_tag_position_type_unknown,
              },
      },
      {.text = L"<pp>", .parse_pos = 0, .expected_result = false},
      {.text = L"<PP>", .parse_pos = 0, .expected_result = false},
      {.text = L"<pp+0>", .parse_pos = 0, .expected_result = false},
      {.text = L"<pp+0,++0>", .parse_pos = 0, .expected_result = false},
      {.text = L"<pp0, >", .parse_pos = 0, .expected_result = false},
      {.text = L"<pp+a>", .parse_pos = 0, .expected_result = false},
      {.text = L"<pp+0,+0,+0,+0>", .parse_pos = 0, .expected_result = false},
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_ex_tag tag;
    struct aviutl_text_ex_tag_position pos_tag;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_ex_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_ex_get_position(test->text, &tag, &pos_tag);
    TEST_CHECK(fcmp(pos_tag.x, ==, test->expected_pos_tag.x, 1e-12));
    TEST_CHECK(pos_tag.x_type == test->expected_pos_tag.x_type);
    TEST_CHECK(fcmp(pos_tag.y, ==, test->expected_pos_tag.y, 1e-12));
    TEST_CHECK(pos_tag.y_type == test->expected_pos_tag.y_type);
    TEST_CHECK(fcmp(pos_tag.z, ==, test->expected_pos_tag.z, 1e-12));
    TEST_CHECK(pos_tag.z_type == test->expected_pos_tag.z_type);
  }
}

static void test_aviutl_text_ex_font(void) {
  struct test_case {
    aviutl_text_ex_char const *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_ex_tag expected_tag;
    struct aviutl_text_ex_tag_font expected_font_tag;
  } const test_cases[] = {
      {
          .text = L"<ss>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_font,
                  .pos = 0,
                  .len = 4,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
          .expected_font_tag =
              {
                  .size = 1,
                  .name = NULL,
                  .name_len = 0,
                  .bold = false,
                  .italic = false,
              },
      },
      {
          .text = L"<ss1.23>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_font,
                  .pos = 0,
                  .len = 8,
                  .value_pos = {3, SIZE_MAX, SIZE_MAX},
                  .value_len = {4, 0, 0},
              },
          .expected_font_tag =
              {
                  .size = 1.23,
                  .name = NULL,
                  .name_len = 0,
                  .bold = false,
                  .italic = false,
              },
      },
      {
          .text = L"<ss123,a >",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_font,
                  .pos = 0,
                  .len = 10,
                  .value_pos = {3, 7, SIZE_MAX},
                  .value_len = {3, 2, 0},
              },
          .expected_font_tag =
              {
                  .size = 123,
                  .name = L"a ",
                  .name_len = 2,
                  .bold = false,
                  .italic = false,
              },
      },
      {
          .text = L"<ss0,<,B>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_font,
                  .pos = 0,
                  .len = 9,
                  .value_pos = {3, 5, 7},
                  .value_len = {1, 1, 1},
              },
          .expected_font_tag =
              {
                  .size = 0,
                  .name = L"<",
                  .name_len = 1,
                  .bold = true,
                  .italic = false,
              },
      },
      {
          .text = L"<ss,,IB>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_font,
                  .pos = 0,
                  .len = 8,
                  .value_pos = {3, 4, 5},
                  .value_len = {0, 0, 2},
              },
          .expected_font_tag =
              {
                  .size = 1,
                  .name = NULL,
                  .name_len = 0,
                  .bold = true,
                  .italic = true,
              },
      },
      {
          .text = L"<SS>",
          .parse_pos = 0,
          .expected_result = false,
      },
      {
          .text = L"<ss+12>",
          .parse_pos = 0,
          .expected_result = false,
      },
      {
          .text = L"<ss12,a,BIS>",
          .parse_pos = 0,
          .expected_result = false,
      },
      {
          .text = L"<ss12,a,BI >",
          .parse_pos = 0,
          .expected_result = false,
      },
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_ex_tag tag;
    struct aviutl_text_ex_tag_font font_tag;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_ex_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_ex_get_font(test->text, &tag, &font_tag);
    TEST_CHECK(fcmp(font_tag.size, ==, test->expected_font_tag.size, 1e-12));
    TEST_CHECK(wcsncmp(font_tag.name, test->expected_font_tag.name, test->expected_font_tag.name_len) == 0);
    TEST_CHECK(font_tag.name_len == test->expected_font_tag.name_len);
    TEST_CHECK(font_tag.bold == test->expected_font_tag.bold);
    TEST_CHECK(font_tag.italic == test->expected_font_tag.italic);
  }
}

static void test_aviutl_text_ex_kerning(void) {
  struct test_case {
    aviutl_text_ex_char const *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_ex_tag expected_tag;
    struct aviutl_text_ex_tag_kerning expected_kerning_tag;
  } const test_cases[] = {
      {
          .text = L"<kern>Hello",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 6,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = 100.,
                  .margin = 0.,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {
          .text = L"<kern0.1>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 9,
                  .value_pos = {5, SIZE_MAX, SIZE_MAX},
                  .value_len = {3, 0, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = .1,
                  .margin = 0.,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {
          .text = L"<kern0.1,0.1>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 13,
                  .value_pos = {5, 9, SIZE_MAX},
                  .value_len = {3, 3, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = .1,
                  .margin = .1,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {
          .text = L"<kern0.1,0.1,0>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 15,
                  .value_pos = {5, 9, 13},
                  .value_len = {3, 3, 1},
              },
          .expected_kerning_tag = {.distance = .1,
                                   .margin = .1,
                                   .method = aviutl_text_ex_tag_kerning_method_convexhull},
      },
      {
          .text = L"<kern0.1,0.1,1>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 15,
                  .value_pos = {5, 9, 13},
                  .value_len = {3, 3, 1},
              },
          .expected_kerning_tag =
              {
                  .distance = .1,
                  .margin = .1,
                  .method = aviutl_text_ex_tag_kerning_method_box,
              },
      },
      {
          .text = L"<kern,,1>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 9,
                  .value_pos = {5, 6, 7},
                  .value_len = {0, 0, 1},
              },
          .expected_kerning_tag =
              {
                  .distance = 100.,
                  .margin = 0.,
                  .method = aviutl_text_ex_tag_kerning_method_box,
              },
      },
      {
          .text = L"<kern,0.1,>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 11,
                  .value_pos = {5, 6, 10},
                  .value_len = {0, 3, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = 100.,
                  .margin = .1,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {
          .text = L"<kern.1>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 8,
                  .value_pos = {5, SIZE_MAX, SIZE_MAX},
                  .value_len = {2, 0, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = .1,
                  .margin = 0.,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {
          .text = L"<kern1.>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 8,
                  .value_pos = {5, SIZE_MAX, SIZE_MAX},
                  .value_len = {2, 0, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = 1.,
                  .margin = 0.,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {
          .text = L"<kern0.1,.1>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 12,
                  .value_pos = {5, 9, SIZE_MAX},
                  .value_len = {3, 2, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = .1,
                  .margin = .1,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {
          .text = L"<kern0.1,1.>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 12,
                  .value_pos = {5, 9, SIZE_MAX},
                  .value_len = {3, 2, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = .1,
                  .margin = 1.,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {
          .text = L"<kern0.1,+0.1>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 14,
                  .value_pos = {5, 9, SIZE_MAX},
                  .value_len = {3, 4, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = .1,
                  .margin = .1,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {
          .text = L"<kern0.1,-0.1>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning,
                  .pos = 0,
                  .len = 14,
                  .value_pos = {5, 9, SIZE_MAX},
                  .value_len = {3, 4, 0},
              },
          .expected_kerning_tag =
              {
                  .distance = .1,
                  .margin = -.1,
                  .method = aviutl_text_ex_tag_kerning_method_convexhull,
              },
      },
      {.text = L"<kern+0.1>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern-0.1>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kerna>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern1..1>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern0.1+>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern0.1,a>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern0.1,1..1>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern0.1,0.1+>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern0.1,0.1,2>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern0.1,0.1,+1>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern0.1,0.1,-1>", .parse_pos = 0, .expected_result = false},
      {.text = L"<kern,,,>", .parse_pos = 0, .expected_result = false},
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_ex_tag tag = {0};
    struct aviutl_text_ex_tag_kerning kerning_tag = {0};
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_ex_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_ex_get_kerning(test->text, &tag, &kerning_tag);
    TEST_CHECK(fcmp(kerning_tag.distance, ==, test->expected_kerning_tag.distance, 1e-12));
    TEST_CHECK(fcmp(kerning_tag.margin, ==, test->expected_kerning_tag.margin, 1e-12));
    TEST_CHECK(kerning_tag.method == test->expected_kerning_tag.method);
  }
}

static void test_aviutl_text_ex_simple_tags(void) {
  struct test_case {
    aviutl_text_ex_char const *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_ex_tag expected_tag;
  } const test_cases[] = {
      {
          .text = L"<wbr>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_wbr,
                  .pos = 0,
                  .len = 5,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
      },
      {
          .text = L"<nobr>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_nobr,
                  .pos = 0,
                  .len = 6,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
      },
      {
          .text = L"</nobr>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_nobr_close,
                  .pos = 0,
                  .len = 7,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
      },
      {
          .text = L"</kern>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_ex_tag_type_kerning_close,
                  .pos = 0,
                  .len = 7,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
      },
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct test_case const *const test = &test_cases[i];
    struct aviutl_text_ex_tag tag;
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_ex_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
  }
}

TEST_LIST = {
    {"test_aviutl_text_ex_position", test_aviutl_text_ex_position},
    {"test_aviutl_text_ex_font", test_aviutl_text_ex_font},
    {"test_aviutl_text_ex_kerning", test_aviutl_text_ex_kerning},
    {"test_aviutl_text_ex_simple_tags", test_aviutl_text_ex_simple_tags},
    {0},
};
