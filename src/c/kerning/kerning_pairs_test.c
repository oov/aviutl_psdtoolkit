#include <ovtest.h>

#include "kerning_pairs.h"

#include <stdio.h>

static void test_kerning_pairs(void) {
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

  struct kerning_pairs *kp = NULL;
  TEST_SUCCEEDED_F(kerning_pairs_create(&kp));
  TEST_ASSERT(kp != NULL);

  TEST_CHECK(kerning_pairs_get_num_pairs(kp) == 0);

  kerning_pairs_get_num_pairs(kp);
  TEST_SUCCEEDED_F(kerning_pairs_update_database(kp, hdc));
  TEST_CHECK(kerning_pairs_get_num_pairs(kp) > 0);

  TEST_CHECK(kerning_pairs_get_kerning(kp, L'A', L'W') < 0);

  kerning_pairs_destroy(&kp);
  TEST_ASSERT(kp == NULL);

  SelectObject(hdc, oldfont);
  DeleteObject(hfont);
  DeleteDC(hdc);
}

TEST_LIST = {
    {"test_kerning_pairs", test_kerning_pairs},
    {0},
};
