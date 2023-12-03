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

static void test_aviutl_text_numcharref(void) {
  struct aviutl_text_tag tag = {0};
  struct aviutl_text_tag_numcharref numcharref = {0};
  aviutl_text_char const *text = NULL;

  text = L"&#1234;";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_numcharref,
                 .pos = 0,
                 .len = 7,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {4, 0, 0},
             }),
             &tag);
    aviutl_text_get_numcharref(text, &tag, &numcharref);
    TEST_CHECK(numcharref.ch == 1234);
  }

  text = L"&#0;";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_numcharref,
                 .pos = 0,
                 .len = 4,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {1, 0, 0},
             }),
             &tag);
    aviutl_text_get_numcharref(text, &tag, &numcharref);
    TEST_CHECK(numcharref.ch == 0);
  }

  text = L"&#;";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
}

static inline int fcompare(double x, double y, double tolerance) {
  return (x > y + tolerance) ? 1 : (y > x + tolerance) ? -1 : 0;
}
#define fcmp(x, op, y, tolerance) ((fcompare((x), (y), (tolerance)))op 0)

static void test_aviutl_text_color(void) {
  struct aviutl_text_tag tag = {0};
  struct aviutl_text_tag_color color_tag = {0};
  aviutl_text_char const *text = NULL;

  text = L"<#>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_color,
                 .pos = 0,
                 .len = 3,
                 .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                 .value_len = {0, 0, 0},
             }),
             &tag);
    aviutl_text_get_color(text, &tag, &color_tag);
    TEST_CHECK(color_tag.color[0] == 0);
    TEST_CHECK(color_tag.color[1] == 0);
  }

  text = L"<#FF00ff>Hello";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_color,
                 .pos = 0,
                 .len = 9,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {6, 0, 0},
             }),
             &tag);
    aviutl_text_get_color(text, &tag, &color_tag);
    TEST_CHECK(color_tag.color[0] == 0xffff00ff);
    TEST_CHECK(color_tag.color[1] == 0);
  }

  text = L"Hello<#abcdef,00FF00>Hello";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 5, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_color,
                 .pos = 5,
                 .len = 16,
                 .value_pos = {7, 14, SIZE_MAX},
                 .value_len = {6, 6, 0},
             }),
             &tag);
    aviutl_text_get_color(text, &tag, &color_tag);
    TEST_CHECK(color_tag.color[0] == 0xffabcdef);
    TEST_CHECK(color_tag.color[1] == 0xff00ff00);
  }

  // syntax errors
  text = L"Hello";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<#FF00FF,00FF00,>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<#FF00FF,00FF00,0000FF>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<#FF00F,00FF00>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<#,00FF00>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
}

static void test_aviutl_text_position(void) {
  struct aviutl_text_tag tag = {0};
  struct aviutl_text_tag_position pos_tag = {0};
  aviutl_text_char const *text = NULL;

  text = L"<p>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);

  text = L"<p0,+12.345>Hello";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_position,
                 .pos = 0,
                 .len = 12,
                 .value_pos = {2, 4, SIZE_MAX},
                 .value_len = {1, 7, 0},
             }),
             &tag);
    aviutl_text_get_position(text, &tag, &pos_tag);
    TEST_CHECK(fcmp(pos_tag.x, ==, 0, 1e-12));
    TEST_CHECK(pos_tag.x_type == aviutl_text_tag_position_type_absolute);
    TEST_CHECK(fcmp(pos_tag.y, ==, 12.345, 1e-12));
    TEST_CHECK(pos_tag.y_type == aviutl_text_tag_position_type_relative);
    TEST_CHECK(fcmp(pos_tag.z, ==, 0, 1e-12));
    TEST_CHECK(pos_tag.z_type == aviutl_text_tag_position_type_unknown);
  }

  text = L"Hello<p+,>Hello";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 5, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_position,
                 .pos = 5,
                 .len = 5,
                 .value_pos = {7, 9, SIZE_MAX},
                 .value_len = {1, 0, 0},
             }),
             &tag);
    aviutl_text_get_position(text, &tag, &pos_tag);
    TEST_CHECK(fcmp(pos_tag.x, ==, 0, 1e-12));
    TEST_CHECK(pos_tag.x_type == aviutl_text_tag_position_type_relative);
    TEST_CHECK(fcmp(pos_tag.y, ==, 0, 1e-12));
    TEST_CHECK(pos_tag.y_type == aviutl_text_tag_position_type_absolute);
    TEST_CHECK(fcmp(pos_tag.z, ==, 0, 1e-12));
    TEST_CHECK(pos_tag.z_type == aviutl_text_tag_position_type_unknown);
  }

  // syntax errors
  text = L"<P>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<p+0>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<p+0,++0>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<p0, >";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<p+a>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<p+0,+0,+0,+0>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
}

static void test_aviutl_text_font(void) {
  struct aviutl_text_tag tag = {0};
  struct aviutl_text_tag_font font_tag = {0};
  aviutl_text_char const *text = NULL;

  text = L"<s>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_font,
                 .pos = 0,
                 .len = 3,
                 .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                 .value_len = {0, 0, 0},
             }),
             &tag);
    aviutl_text_get_font(text, &tag, &font_tag);
    TEST_CHECK(font_tag.size == 0);
    TEST_CHECK(font_tag.name == NULL);
    TEST_CHECK(font_tag.name_len == 0);
    TEST_CHECK(font_tag.bold == false);
    TEST_CHECK(font_tag.italic == false);
  }

  text = L"<s123>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_font,
                 .pos = 0,
                 .len = 6,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {3, 0, 0},
             }),
             &tag);
    aviutl_text_get_font(text, &tag, &font_tag);
    TEST_CHECK(font_tag.size == 123);
    TEST_CHECK(font_tag.name == NULL);
    TEST_CHECK(font_tag.name_len == 0);
    TEST_CHECK(font_tag.bold == false);
    TEST_CHECK(font_tag.italic == false);
  }

  text = L"<s123,a >";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_font,
                 .pos = 0,
                 .len = 9,
                 .value_pos = {2, 6, SIZE_MAX},
                 .value_len = {3, 2, 0},
             }),
             &tag);
    aviutl_text_get_font(text, &tag, &font_tag);
    TEST_CHECK(font_tag.size == 123);
    TEST_CHECK(font_tag.name == text + 6);
    TEST_CHECK(font_tag.name_len == 2);
    TEST_CHECK(font_tag.bold == false);
    TEST_CHECK(font_tag.italic == false);
  }

  text = L"<s0,<,B>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_font,
                 .pos = 0,
                 .len = 8,
                 .value_pos = {2, 4, 6},
                 .value_len = {1, 1, 1},
             }),
             &tag);
    aviutl_text_get_font(text, &tag, &font_tag);
    TEST_CHECK(font_tag.size == 0);
    TEST_CHECK(font_tag.name == text + 4);
    TEST_CHECK(font_tag.name_len == 1);
    TEST_CHECK(font_tag.bold == true);
    TEST_CHECK(font_tag.italic == false);
  }
  text = L"<s,,IB>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_font,
                 .pos = 0,
                 .len = 7,
                 .value_pos = {2, 3, 4},
                 .value_len = {0, 0, 2},
             }),
             &tag);
    aviutl_text_get_font(text, &tag, &font_tag);
    TEST_CHECK(font_tag.size == 0);
    TEST_CHECK(font_tag.name == NULL);
    TEST_CHECK(font_tag.name_len == 0);
    TEST_CHECK(font_tag.bold == true);
    TEST_CHECK(font_tag.italic == true);
  }

  // syntax errors
  text = L"<S>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<s+12>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<s12.3>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<s12,a,BIS>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<s12,a,BI >";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
}

static void test_aviutl_text_speed(void) {
  struct aviutl_text_tag tag = {0};
  struct aviutl_text_tag_speed speed_tag = {0};
  aviutl_text_char const *text = NULL;

  text = L"<r>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_speed,
                 .pos = 0,
                 .len = 3,
                 .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                 .value_len = {0, 0, 0},
             }),
             &tag);
    aviutl_text_get_speed(text, &tag, &speed_tag);
    TEST_CHECK(fcmp(speed_tag.v, ==, 0, 1e-12));
  }

  text = L"<r0>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_speed,
                 .pos = 0,
                 .len = 4,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {1, 0, 0},
             }),
             &tag);
    aviutl_text_get_speed(text, &tag, &speed_tag);
    TEST_CHECK(fcmp(speed_tag.v, ==, 0, 1e-12));
  }

  text = L"<r1.23>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_speed,
                 .pos = 0,
                 .len = 7,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {4, 0, 0},
             }),
             &tag);
    aviutl_text_get_speed(text, &tag, &speed_tag);
    TEST_CHECK(fcmp(speed_tag.v, ==, 1.23, 1e-12));
  }

  text = L"<r1.>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_speed,
                 .pos = 0,
                 .len = 5,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {2, 0, 0},
             }),
             &tag);
    aviutl_text_get_speed(text, &tag, &speed_tag);
    TEST_CHECK(fcmp(speed_tag.v, ==, 1, 1e-12));
  }

  text = L"<r.1>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_speed,
                 .pos = 0,
                 .len = 5,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {2, 0, 0},
             }),
             &tag);
    aviutl_text_get_speed(text, &tag, &speed_tag);
    TEST_CHECK(fcmp(speed_tag.v, ==, 0.1, 1e-12));
  }

  text = L"<R>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<r1..>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<ra>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<r1 >";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
}

static void test_aviutl_text_wait(void) {
  struct aviutl_text_tag tag = {0};
  struct aviutl_text_tag_wait wait_tag = {0};
  aviutl_text_char const *text = NULL;

  text = L"<w>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_wait,
                 .pos = 0,
                 .len = 3,
                 .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                 .value_len = {0, 0, 0},
             }),
             &tag);
    aviutl_text_get_wait(text, &tag, &wait_tag);
    TEST_CHECK(fcmp(wait_tag.v, ==, 0, 1e-12));
    TEST_CHECK(wait_tag.per_char == false);
  }

  text = L"<w12.>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_wait,
                 .pos = 0,
                 .len = 6,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {3, 0, 0},
             }),
             &tag);
    aviutl_text_get_wait(text, &tag, &wait_tag);
    TEST_CHECK(fcmp(wait_tag.v, ==, 12, 1e-12));
    TEST_CHECK(wait_tag.per_char == false);
  }

  text = L"<w.12>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_wait,
                 .pos = 0,
                 .len = 6,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {3, 0, 0},
             }),
             &tag);
    aviutl_text_get_wait(text, &tag, &wait_tag);
    TEST_CHECK(fcmp(wait_tag.v, ==, 0.12, 1e-12));
    TEST_CHECK(wait_tag.per_char == false);
  }

  text = L"<w*>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_wait,
                 .pos = 0,
                 .len = 4,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {1, 0, 0},
             }),
             &tag);
    aviutl_text_get_wait(text, &tag, &wait_tag);
    TEST_CHECK(fcmp(wait_tag.v, ==, 0, 1e-12));
    TEST_CHECK(wait_tag.per_char == true);
  }

  text = L"<w*12.>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_wait,
                 .pos = 0,
                 .len = 7,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {4, 0, 0},
             }),
             &tag);
    aviutl_text_get_wait(text, &tag, &wait_tag);
    TEST_CHECK(fcmp(wait_tag.v, ==, 12, 1e-12));
    TEST_CHECK(wait_tag.per_char == true);
  }

  text = L"<w*.12>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_wait,
                 .pos = 0,
                 .len = 7,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {4, 0, 0},
             }),
             &tag);
    aviutl_text_get_wait(text, &tag, &wait_tag);
    TEST_CHECK(fcmp(wait_tag.v, ==, 0.12, 1e-12));
    TEST_CHECK(wait_tag.per_char == true);
  }

  text = L"<w*1.234>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_wait,
                 .pos = 0,
                 .len = 9,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {6, 0, 0},
             }),
             &tag);
    aviutl_text_get_wait(text, &tag, &wait_tag);
    TEST_CHECK(fcmp(wait_tag.v, ==, 1.234, 1e-12));
    TEST_CHECK(wait_tag.per_char == true);
  }

  text = L"<W>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<w1..>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<w1.2.>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<wa>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<w1 >";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<w *123.4>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
}

static void test_aviutl_text_clear(void) {
  struct aviutl_text_tag tag = {0};
  struct aviutl_text_tag_clear clear_tag = {0};
  aviutl_text_char const *text = NULL;

  text = L"<c>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_clear,
                 .pos = 0,
                 .len = 3,
                 .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
                 .value_len = {0, 0, 0},
             }),
             &tag);
    aviutl_text_get_clear(text, &tag, &clear_tag);
    TEST_CHECK(fcmp(clear_tag.v, ==, 0, 1e-12));
    TEST_CHECK(clear_tag.per_char == false);
  }

  text = L"<c12.>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_clear,
                 .pos = 0,
                 .len = 6,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {3, 0, 0},
             }),
             &tag);
    aviutl_text_get_clear(text, &tag, &clear_tag);
    TEST_CHECK(fcmp(clear_tag.v, ==, 12, 1e-12));
    TEST_CHECK(clear_tag.per_char == false);
  }

  text = L"<c.12>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_clear,
                 .pos = 0,
                 .len = 6,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {3, 0, 0},
             }),
             &tag);
    aviutl_text_get_clear(text, &tag, &clear_tag);
    TEST_CHECK(fcmp(clear_tag.v, ==, 0.12, 1e-12));
    TEST_CHECK(clear_tag.per_char == false);
  }

  text = L"<c*>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_clear,
                 .pos = 0,
                 .len = 4,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {1, 0, 0},
             }),
             &tag);
    aviutl_text_get_clear(text, &tag, &clear_tag);
    TEST_CHECK(fcmp(clear_tag.v, ==, 0, 1e-12));
    TEST_CHECK(clear_tag.per_char == true);
  }

  text = L"<c*12.>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_clear,
                 .pos = 0,
                 .len = 7,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {4, 0, 0},
             }),
             &tag);
    aviutl_text_get_clear(text, &tag, &clear_tag);
    TEST_CHECK(fcmp(clear_tag.v, ==, 12, 1e-12));
    TEST_CHECK(clear_tag.per_char == true);
  }

  text = L"<c*.12>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_clear,
                 .pos = 0,
                 .len = 7,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {4, 0, 0},
             }),
             &tag);
    aviutl_text_get_clear(text, &tag, &clear_tag);
    TEST_CHECK(fcmp(clear_tag.v, ==, 0.12, 1e-12));
    TEST_CHECK(clear_tag.per_char == true);
  }

  text = L"<c*1.234>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_clear,
                 .pos = 0,
                 .len = 9,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {6, 0, 0},
             }),
             &tag);
    aviutl_text_get_clear(text, &tag, &clear_tag);
    TEST_CHECK(fcmp(clear_tag.v, ==, 1.234, 1e-12));
    TEST_CHECK(clear_tag.per_char == true);
  }

  text = L"<C>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<c1..>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<c1.2.>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<ca>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<c1 >";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
  text = L"<c *123.4>";
  TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == false);
}

static void test_aviutl_text_script(void) {
  struct aviutl_text_tag tag = {0};
  struct aviutl_text_tag_script script_tag = {0};
  aviutl_text_char const *text = NULL;

  text = L"<\x3f?>"; // U'??>' == trigraph U'}'
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_script,
                 .pos = 0,
                 .len = 4,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {0, 0, 0},
             }),
             &tag);
    aviutl_text_get_script(text, &tag, &script_tag);
    TEST_CHECK(script_tag.ptr == text + 2);
    TEST_CHECK(script_tag.len == 0);
  }

  text = L"<? <? ? > ?>";
  if (TEST_CHECK(aviutl_text_parse_tag(text, wcslen(text), 0, &tag) == true)) {
    TEST_TAG(&((struct aviutl_text_tag){
                 .type = aviutl_text_tag_type_script,
                 .pos = 0,
                 .len = 12,
                 .value_pos = {2, SIZE_MAX, SIZE_MAX},
                 .value_len = {8, 0, 0},
             }),
             &tag);
    aviutl_text_get_script(text, &tag, &script_tag);
    TEST_CHECK(script_tag.ptr == text + 2);
    TEST_CHECK(script_tag.len == 8);
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
