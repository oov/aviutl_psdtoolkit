#include <ovtest.h>

#include "glyphoutline.h"

#include <stdio.h>

static size_t g_num_point = 0;
static enum point_op g_last_op = pop_close;

static bool callback(void *const userdata, enum point_op const op, struct point const pt) {
  if (g_num_point == 0) {
    TEST_CHECK(op == pop_move_to);
  } else {
    switch (g_last_op) {
    case pop_move_to:
      TEST_CHECK(op == pop_line_to);
      break;
    case pop_line_to:
      TEST_CHECK(op == pop_line_to || op == pop_close);
      break;
    case pop_close:
      TEST_CHECK(op == pop_move_to);
      break;
    }
  }
  TEST_ASSERT(userdata == NULL);
  (void)pt;
  ++g_num_point;
  g_last_op = op;
  return true;
}

static void test_glyphoutline_parse(void) {
  HDC hdc = CreateCompatibleDC(NULL);
  TEST_ASSERT(hdc != NULL);

  HFONT hfont = CreateFontW(-120,
                            0,
                            0,
                            0,
                            FW_DONTCARE,
                            FALSE,
                            FALSE,
                            FALSE,
                            DEFAULT_CHARSET,
                            OUT_OUTLINE_PRECIS,
                            CLIP_DEFAULT_PRECIS,
                            DEFAULT_QUALITY,
                            DEFAULT_PITCH,
                            L"Arial");
  TEST_ASSERT(hfont != NULL);

  HFONT oldfont = SelectObject(hdc, hfont);

  char buffer[4096];
  GLYPHMETRICS gm;
  MAT2 mat = {{0, 1}, {0, 0}, {0, 0}, {0, 1}};
  DWORD size = GetGlyphOutlineW(hdc, 'G', GGO_NATIVE | GGO_UNHINTED, &gm, 0, NULL, &mat);
  TEST_ASSERT(size != GDI_ERROR);
  TEST_ASSERT(size <= sizeof(buffer));

  size = GetGlyphOutlineW(hdc, 'G', GGO_NATIVE | GGO_UNHINTED, &gm, size, buffer, &mat);
  TEST_ASSERT(size != GDI_ERROR);

  bool result = glyphoutline_parse(buffer, size, 15, callback, NULL);
  TEST_CHECK(result == true);
  TEST_CHECK(g_num_point > 0);
  TEST_CHECK(g_last_op == pop_close);

  SelectObject(hdc, oldfont);
  DeleteObject(hfont);
  DeleteDC(hdc);
}

TEST_LIST = {
    {"test_glyphoutline_parse", test_glyphoutline_parse},
    {0},
};
