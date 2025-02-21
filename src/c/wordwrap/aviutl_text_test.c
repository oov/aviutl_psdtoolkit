#include <ovtest.h>

#include "aviutl_text.h"

static bool test_tag(struct aviutl_text_tag const *const expect,
                     struct aviutl_text_tag const *const got,
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
    TEST_MSG("expect->type: %u, got->type: %u", expect->type, got->type);
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

static void test_aviutl_text_numcharref(void) {
  struct test_case {
    wchar_t *text;
    bool expected_result;
    struct aviutl_text_tag expected_tag;
    struct aviutl_text_tag_numcharref expected_numcharref;
  } const test_cases[] = {
      {
          .text = L"&#1234;",
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_numcharref,
                  .pos = 0,
                  .len = 7,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {4, 0, 0},
              },
          .expected_numcharref =
              {
                  .ch = 1234,
              },
      },
      {
          .text = L"&#0;",
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_numcharref,
                  .pos = 0,
                  .len = 4,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {1, 0, 0},
              },
          .expected_numcharref =
              {
                  .ch = 0,
              },
      },
      {
          .text = L"&#;",
          .expected_result = false,
      },
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_tag tag;
    struct aviutl_text_tag_numcharref numcharref;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_parse_tag(test->text, wcslen(test->text), 0, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&(test->expected_tag), &tag);
    aviutl_text_get_numcharref(test->text, &tag, &numcharref);
    TEST_CHECK(numcharref.ch == test->expected_numcharref.ch);
  }
}

static inline int fcompare(double x, double y, double tolerance) {
  return (x > y + tolerance) ? 1 : (y > x + tolerance) ? -1 : 0;
}
#define fcmp(x, op, y, tolerance) ((fcompare((x), (y), (tolerance)))op 0)

static void test_aviutl_text_color(void) {
  struct test_case {
    wchar_t *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_tag expected_tag;
    struct aviutl_text_tag_color expected_color_tag;
  } const test_cases[] = {
      {
          .text = L"<#>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_color,
                  .pos = 0,
                  .len = 3,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
          .expected_color_tag =
              {
                  .color = {0, 0},
              },
      },
      {
          .text = L"<#FF00ff>Hello",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_color,
                  .pos = 0,
                  .len = 9,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {6, 0, 0},
              },
          .expected_color_tag =
              {
                  .color = {0xffff00ff, 0},
              },
      },
      {
          .text = L"Hello<#abcdef,00FF00>Hello",
          .parse_pos = 5,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_color,
                  .pos = 5,
                  .len = 16,
                  .value_pos = {7, 14, SIZE_MAX},
                  .value_len = {6, 6, 0},
              },
          .expected_color_tag =
              {
                  .color = {0xffabcdef, 0xff00ff00},
              },
      },
      {.text = L"Hello", .parse_pos = 0, .expected_result = false},
      {.text = L"<#FF00FF,00FF00,>", .parse_pos = 0, .expected_result = false},
      {.text = L"<#FF00FF,00FF00,0000FF>", .parse_pos = 0, .expected_result = false},
      {.text = L"<#FF00F,00FF00>", .parse_pos = 0, .expected_result = false},
      {.text = L"<#,00FF00>", .parse_pos = 0, .expected_result = false},
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_tag tag;
    struct aviutl_text_tag_color color_tag;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_get_color(test->text, &tag, &color_tag);
    TEST_CHECK(color_tag.color[0] == test->expected_color_tag.color[0]);
    TEST_CHECK(color_tag.color[1] == test->expected_color_tag.color[1]);
  }
}

static void test_aviutl_text_position(void) {
  struct test_case {
    wchar_t *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_tag expected_tag;
    struct aviutl_text_tag_position expected_pos_tag;
  } const test_cases[] = {
      {
          .text = L"<p>",
          .parse_pos = 0,
          .expected_result = false,
      },
      {
          .text = L"<p0,+12.345>Hello",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_position,
                  .pos = 0,
                  .len = 12,
                  .value_pos = {2, 4, SIZE_MAX},
                  .value_len = {1, 7, 0},
              },
          .expected_pos_tag =
              {
                  .x = 0,
                  .x_type = aviutl_text_tag_position_type_absolute,
                  .y = 12.345,
                  .y_type = aviutl_text_tag_position_type_relative,
                  .z = 0,
                  .z_type = aviutl_text_tag_position_type_unknown,
              },
      },
      {
          .text = L"Hello<p+,>Hello",
          .parse_pos = 5,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_position,
                  .pos = 5,
                  .len = 5,
                  .value_pos = {7, 9, SIZE_MAX},
                  .value_len = {1, 0, 0},
              },
          .expected_pos_tag =
              {
                  .x = 0,
                  .x_type = aviutl_text_tag_position_type_relative,
                  .y = 0,
                  .y_type = aviutl_text_tag_position_type_absolute,
                  .z = 0,
                  .z_type = aviutl_text_tag_position_type_unknown,
              },
      },
      {.text = L"<P>", .parse_pos = 0, .expected_result = false},
      {.text = L"<p+0>", .parse_pos = 0, .expected_result = false},
      {.text = L"<p+0,++0>", .parse_pos = 0, .expected_result = false},
      {.text = L"<p0, >", .parse_pos = 0, .expected_result = false},
      {.text = L"<p+a>", .parse_pos = 0, .expected_result = false},
      {.text = L"<p+0,+0,+0,+0>", .parse_pos = 0, .expected_result = false},
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_tag tag;
    struct aviutl_text_tag_position pos_tag;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_get_position(test->text, &tag, &pos_tag);
    TEST_CHECK(fcmp(pos_tag.x, ==, test->expected_pos_tag.x, 1e-12));
    TEST_CHECK(pos_tag.x_type == test->expected_pos_tag.x_type);
    TEST_CHECK(fcmp(pos_tag.y, ==, test->expected_pos_tag.y, 1e-12));
    TEST_CHECK(pos_tag.y_type == test->expected_pos_tag.y_type);
    TEST_CHECK(fcmp(pos_tag.z, ==, test->expected_pos_tag.z, 1e-12));
    TEST_CHECK(pos_tag.z_type == test->expected_pos_tag.z_type);
  }
}

static void test_aviutl_text_font(void) {
  struct test_case {
    aviutl_text_char const *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_tag expected_tag;
    struct aviutl_text_tag_font expected_font_tag;
  } const test_cases[] = {
      {
          .text = L"<s>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_font,
                  .pos = 0,
                  .len = 3,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
          .expected_font_tag =
              {
                  .size = 0,
                  .name = NULL,
                  .name_len = 0,
                  .bold = false,
                  .italic = false,
              },
      },
      {
          .text = L"<s123>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag = {.type = aviutl_text_tag_type_font,
                           .pos = 0,
                           .len = 6,
                           .value_pos = {2, SIZE_MAX, SIZE_MAX},
                           .value_len = {3, 0, 0}},
          .expected_font_tag =
              {
                  .size = 123,
                  .name = NULL,
                  .name_len = 0,
                  .bold = false,
                  .italic = false,
              },
      },
      {
          .text = L"<s123,a >",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_font,
                  .pos = 0,
                  .len = 9,
                  .value_pos = {2, 6, SIZE_MAX},
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
          .text = L"<s0,<,B>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_font,
                  .pos = 0,
                  .len = 8,
                  .value_pos = {2, 4, 6},
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
          .text = L"<s,,IB>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_font,
                  .pos = 0,
                  .len = 7,
                  .value_pos = {2, 3, 4},
                  .value_len = {0, 0, 2},
              },
          .expected_font_tag =
              {
                  .size = 0,
                  .name = NULL,
                  .name_len = 0,
                  .bold = true,
                  .italic = true,
              },
      },
      {.text = L"<S>", .parse_pos = 0, .expected_result = false},
      {.text = L"<s+12>", .parse_pos = 0, .expected_result = false},
      {.text = L"<s12.3>", .parse_pos = 0, .expected_result = false},
      {.text = L"<s12,a,BIS>", .parse_pos = 0, .expected_result = false},
      {.text = L"<s12,a,BI >", .parse_pos = 0, .expected_result = false},
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_tag tag;
    struct aviutl_text_tag_font font_tag;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_get_font(test->text, &tag, &font_tag);
    TEST_CHECK(font_tag.size == test->expected_font_tag.size);
    TEST_CHECK(wcsncmp(font_tag.name, test->expected_font_tag.name, test->expected_font_tag.name_len) == 0);
    TEST_CHECK(font_tag.name_len == test->expected_font_tag.name_len);
    TEST_CHECK(font_tag.bold == test->expected_font_tag.bold);
    TEST_CHECK(font_tag.italic == test->expected_font_tag.italic);
  }
}

static void test_aviutl_text_speed(void) {
  struct test_case {
    aviutl_text_char const *text;
    bool expected_result;
    struct aviutl_text_tag expected_tag;
    struct aviutl_text_tag_speed expected_speed_tag;
  } const test_cases[] = {
      {
          .text = L"<r>",
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_speed,
                  .pos = 0,
                  .len = 3,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
          .expected_speed_tag =
              {
                  .v = 0,
              },
      },
      {
          .text = L"<r0>",
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_speed,
                  .pos = 0,
                  .len = 4,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {1, 0, 0},
              },
          .expected_speed_tag =
              {
                  .v = 0,
              },
      },
      {
          .text = L"<r1.23>",
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_speed,
                  .pos = 0,
                  .len = 7,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {4, 0, 0},
              },
          .expected_speed_tag =
              {
                  .v = 1.23,
              },
      },
      {
          .text = L"<r1.>",
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_speed,
                  .pos = 0,
                  .len = 5,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {2, 0, 0},
              },
          .expected_speed_tag =
              {
                  .v = 1,
              },
      },
      {
          .text = L"<r.1>",
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_speed,
                  .pos = 0,
                  .len = 5,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {2, 0, 0},
              },
          .expected_speed_tag =
              {
                  .v = 0.1,
              },
      },
      {.text = L"<R>", .expected_result = false},
      {.text = L"<r1..>", .expected_result = false},
      {.text = L"<ra>", .expected_result = false},
      {.text = L"<r1 >", .expected_result = false},
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_tag tag;
    struct aviutl_text_tag_speed speed_tag;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_parse_tag(test->text, wcslen(test->text), 0, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_get_speed(test->text, &tag, &speed_tag);
    TEST_CHECK(fcmp(speed_tag.v, ==, test->expected_speed_tag.v, 1e-12));
  }
}

static void test_aviutl_text_wait(void) {
  struct test_case {
    aviutl_text_char const *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_tag expected_tag;
    struct aviutl_text_tag_wait expected_wait_tag;
  } const test_cases[] = {
      {
          .text = L"<w>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_wait,
                  .pos = 0,
                  .len = 3,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
          .expected_wait_tag =
              {
                  .v = 0,
                  .per_char = false,
              },
      },
      {
          .text = L"<w12.>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_wait,
                  .pos = 0,
                  .len = 6,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {3, 0, 0},
              },
          .expected_wait_tag =
              {
                  .v = 12,
                  .per_char = false,
              },
      },
      {
          .text = L"<w.12>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_wait,
                  .pos = 0,
                  .len = 6,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {3, 0, 0},
              },
          .expected_wait_tag =
              {
                  .v = .12,
                  .per_char = false,
              },
      },
      {
          .text = L"<w*>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_wait,
                  .pos = 0,
                  .len = 4,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {1, 0, 0},
              },
          .expected_wait_tag =
              {
                  .v = 0,
                  .per_char = true,
              },
      },
      {
          .text = L"<w*12.>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_wait,
                  .pos = 0,
                  .len = 7,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {4, 0, 0},
              },
          .expected_wait_tag =
              {
                  .v = 12.,
                  .per_char = true,
              },

      },
      {
          .text = L"<w*.12>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_wait,
                  .pos = 0,
                  .len = 7,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {4, 0, 0},
              },
          .expected_wait_tag =
              {
                  .v = 0.12,
                  .per_char = true,
              },

      },
      {
          .text = L"<w*1.234>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_wait,
                  .pos = 0,
                  .len = 9,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {6, 0, 0},
              },
          .expected_wait_tag =
              {
                  .v = 1.234,
                  .per_char = true,
              },

      },
      {.text = L"<W>", .parse_pos = 0, .expected_result = false},
      {.text = L"<w1..>", .parse_pos = 0, .expected_result = false},
      {.text = L"<w1.2.>", .parse_pos = 0, .expected_result = false},
      {.text = L"<wa>", .parse_pos = 0, .expected_result = false},
      {.text = L"<w1 >", .parse_pos = 0, .expected_result = false},
      {.text = L"<w *123.4>", .parse_pos = 0, .expected_result = false},
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_tag tag;
    struct aviutl_text_tag_wait wait_tag;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_get_wait(test->text, &tag, &wait_tag);
    TEST_CHECK(fcmp(wait_tag.v, ==, test->expected_wait_tag.v, 1e-12));
    TEST_CHECK(wait_tag.per_char == test->expected_wait_tag.per_char);
  }
}

static void test_aviutl_text_clear(void) {
  struct test_case {
    aviutl_text_char const *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_tag expected_tag;
    struct aviutl_text_tag_clear expected_clear_tag;
  } const test_cases[] = {
      {
          .text = L"<c>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_clear,
                  .pos = 0,
                  .len = 3,
                  .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
          .expected_clear_tag =
              {
                  .v = 0,
                  .per_char = false,
              },
      },
      {
          .text = L"<c12.>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_clear,
                  .pos = 0,
                  .len = 6,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {3, 0, 0},
              },
          .expected_clear_tag =
              {
                  .v = 12,
                  .per_char = false,
              },
      },
      {
          .text = L"<c.12>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_clear,
                  .pos = 0,
                  .len = 6,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {3, 0, 0},
              },
          .expected_clear_tag =
              {
                  .v = .12,
                  .per_char = false,
              },
      },
      {
          .text = L"<c*>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_clear,
                  .pos = 0,
                  .len = 4,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {1, 0, 0},
              },
          .expected_clear_tag =
              {
                  .v = 0,
                  .per_char = true,
              },
      },
      {
          .text = L"<c*12.>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_clear,
                  .pos = 0,
                  .len = 7,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {4, 0, 0},
              },
          .expected_clear_tag =
              {
                  .v = 12.,
                  .per_char = true,
              },

      },
      {
          .text = L"<c*.12>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_clear,
                  .pos = 0,
                  .len = 7,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {4, 0, 0},
              },
          .expected_clear_tag =
              {
                  .v = 0.12,
                  .per_char = true,
              },

      },
      {
          .text = L"<c*1.234>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_clear,
                  .pos = 0,
                  .len = 9,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {6, 0, 0},
              },
          .expected_clear_tag =
              {
                  .v = 1.234,
                  .per_char = true,
              },

      },
      {.text = L"<C>", .parse_pos = 0, .expected_result = false},
      {.text = L"<c1..>", .parse_pos = 0, .expected_result = false},
      {.text = L"<c1.2.>", .parse_pos = 0, .expected_result = false},
      {.text = L"<ca>", .parse_pos = 0, .expected_result = false},
      {.text = L"<c1 >", .parse_pos = 0, .expected_result = false},
      {.text = L"<c *123.4>", .parse_pos = 0, .expected_result = false},
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_tag tag;
    struct aviutl_text_tag_clear clear_tag;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_get_clear(test->text, &tag, &clear_tag);
    TEST_CHECK(fcmp(clear_tag.v, ==, test->expected_clear_tag.v, 1e-12));
    TEST_CHECK(clear_tag.per_char == test->expected_clear_tag.per_char);
  }
}

static void test_aviutl_text_script(void) {
  struct test_case {
    aviutl_text_char const *text;
    size_t parse_pos;
    bool expected_result;
    struct aviutl_text_tag expected_tag;
    size_t expected_script_ptr_offset;
    size_t expected_script_len;
  } const test_cases[] = {
      {
          .text = L"<\x3f?>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_script,
                  .pos = 0,
                  .len = 4,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {0, 0, 0},
              },
          .expected_script_ptr_offset = 2,
          .expected_script_len = 0,
      },
      {
          .text = L"<? <? ? > ?>",
          .parse_pos = 0,
          .expected_result = true,
          .expected_tag =
              {
                  .type = aviutl_text_tag_type_script,
                  .pos = 0,
                  .len = 12,
                  .value_pos = {2, SIZE_MAX, SIZE_MAX},
                  .value_len = {8, 0, 0},
              },
          .expected_script_ptr_offset = 2,
          .expected_script_len = 8,
      },
  };
  for (size_t i = 0; i < sizeof(test_cases) / sizeof(test_cases[0]); ++i) {
    struct aviutl_text_tag tag;
    struct aviutl_text_tag_script script_tag;
    struct test_case const *const test = &test_cases[i];
    TEST_CASE_("%ls", test->text);
    bool const result = aviutl_text_parse_tag(test->text, wcslen(test->text), test->parse_pos, &tag);
    if (!TEST_CHECK(result == test->expected_result) || !test->expected_result) {
      continue;
    }
    TEST_TAG(&test->expected_tag, &tag);
    aviutl_text_get_script(test->text, &tag, &script_tag);
    TEST_CHECK((size_t)(script_tag.ptr - test->text) == test->expected_script_ptr_offset);
    TEST_CHECK(script_tag.len == test->expected_script_len);
  }
}

TEST_LIST = {
    {"test_aviutl_text_numcharref", test_aviutl_text_numcharref},
    {"test_aviutl_text_color", test_aviutl_text_color},
    {"test_aviutl_text_position", test_aviutl_text_position},
    {"test_aviutl_text_font", test_aviutl_text_font},
    {"test_aviutl_text_speed", test_aviutl_text_speed},
    {"test_aviutl_text_wait", test_aviutl_text_wait},
    {"test_aviutl_text_clear", test_aviutl_text_clear},
    {"test_aviutl_text_script", test_aviutl_text_script},
    {0},
};
