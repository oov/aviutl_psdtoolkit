#include <ovtest.h>

#include <ovutil/win32.h>

#include "error_ptk.h"
#include "i18n.h"

#include "luafuncs_wordwrap.c"

static inline int fcompare(double x, double y, double tolerance) {
  return (x > y + tolerance) ? 1 : (y > x + tolerance) ? -1 : 0;
}
#define fcmp(x, op, y, tolerance) ((fcompare((x), (y), (tolerance)))op 0)

static void *lua_alloc(void *const ud, void *ptr, size_t const osize, size_t const nsize) {
  (void)ud;
  (void)osize;
  if (nsize) {
    if (!ereport(mem(&ptr, nsize, 1))) {
      return NULL;
    }
    return ptr;
  }
  ereport(mem_free(&ptr));
  return NULL;
}

NODISCARD static error towstr(lua_State *const L, int const idx, struct wstr *const dest) {
  if (!L) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  char const *const s = lua_tostring(L, idx);
  if (!s) {
    return errg(err_invalid_arugment);
  }
  error err = from_mbcs(&str_unmanaged_const(s), dest);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  return eok();
}

NODISCARD static error pcall_(lua_State *const L, int const nargs, int const nresults ERR_FILEPOS_PARAMS) {
  if (lua_pcall(L, nargs, nresults, 0) == 0) {
    return eok();
  }
  struct wstr trace = {0};
  error err = towstr(L, -1, &trace);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  return error_add_(NULL, err_type_ptk, err_ptk_lua, &trace ERR_FILEPOS_VALUES_PASSTHRU);

failed:
  ereport(sfree(&trace));
  efree(&err);
  return emsg_i18n(err_type_generic, err_fail, gettext("Failed to build error message."));
}
#define pcall(L, nargs, nresults) (pcall_((L), (nargs), (nresults)ERR_FILEPOS_VALUES))

NODISCARD static error dostring(lua_State *const L, wchar_t const *const ws) {
  if (!L || !ws) {
    return errg(err_invalid_arugment);
  }
  struct str mbcs = {0};
  error err = to_mbcs(&wstr_unmanaged_const(ws), &mbcs);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  luaL_loadstring(L, mbcs.ptr);
  err = pcall(L, 0, LUA_MULTRET);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
failed:
  ereport(sfree(&mbcs));
  return eok();
}

NODISCARD static error do_initialize_params(wchar_t const *const code, struct wordwrap_settings *const wws) {
  if (!code || !wws) {
    return errg(err_invalid_arugment);
  }
  error err = eok();
  lua_State *L = NULL;
  L = lua_newstate(lua_alloc, NULL);
  if (L == NULL) {
    err = errg(err_fail);
    goto cleanup;
  }
  luaL_openlibs(L);
  err = dostring(L, code);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = initialize_params(L, 1, wws);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  if (L) {
    lua_close(L);
  }
  return err;
}

static void compare_logfontw(LOGFONTW const *const expected, LOGFONTW const *const actual) {
  TEST_CHECK(expected->lfHeight == actual->lfHeight);
  TEST_MSG("Expected lfHeight: %ld, got: %ld", expected->lfHeight, actual->lfHeight);
  TEST_CHECK(expected->lfWeight == actual->lfWeight);
  TEST_MSG("Expected lfWeight: %ld, got: %ld", expected->lfWeight, actual->lfWeight);
  TEST_CHECK(expected->lfItalic == actual->lfItalic);
  TEST_MSG("Expected lfItalic: %d, got: %d", expected->lfItalic, actual->lfItalic);
  TEST_CHECK(wcscmp(expected->lfFaceName, actual->lfFaceName) == 0);
  TEST_MSG("Expected lfFaceName: %ls, got: %ls", expected->lfFaceName, actual->lfFaceName);
}

static void compare_wordwrap_settings(struct wordwrap_settings const *const expected,
                                      struct wordwrap_settings const *const actual) {
  TEST_CHECK(expected->mode == actual->mode);
  TEST_MSG("Expected mode: %d, got: %d", expected->mode, actual->mode);
  TEST_CHECK(fcmp(expected->max_width, ==, actual->max_width, 1e-12));
  TEST_MSG("Expected max_width: %f, got: %f", expected->max_width, actual->max_width);
  TEST_CHECK(expected->letter_spacing == actual->letter_spacing);
  TEST_MSG("Expected letter_spacing: %d, got: %d", expected->letter_spacing, actual->letter_spacing);
  TEST_CHECK(expected->monospace == actual->monospace);
  TEST_MSG("Expected monospace: %d, got: %d", expected->monospace, actual->monospace);
  TEST_CHECK(expected->high_resolution == actual->high_resolution);
  TEST_MSG("Expected high_resolution: %d, got: %d", expected->high_resolution, actual->high_resolution);
}

static void test_initialize_params(void) {
  static const struct {
    wchar_t *input;
    struct wordwrap_settings wws;
  } tests[] = {
      {
          L"return 1",
          {.initial_font = {.lfHeight = -9, .lfWeight = FW_NORMAL, .lfItalic = FALSE, .lfFaceName = L"MS UI Gothic"},
           .mode = wwm_norule,
           .max_width = 800,
           .letter_spacing = 0,
           .monospace = false,
           .high_resolution = false},
      },
      {
          L"return 2",
          {.initial_font = {.lfHeight = -9, .lfWeight = FW_NORMAL, .lfItalic = FALSE, .lfFaceName = L"MS UI Gothic"},
           .mode = wwm_rule,
           .max_width = 800,
           .letter_spacing = 0,
           .monospace = false,
           .high_resolution = false},
      },
      {
          L"return 3",
          {.initial_font = {.lfHeight = -9, .lfWeight = FW_NORMAL, .lfItalic = FALSE, .lfFaceName = L"MS UI Gothic"},
           .mode = wwm_budoux,
           .max_width = 800,
           .letter_spacing = 0,
           .monospace = false,
           .high_resolution = false},
      },
      {
          L"return {font='Arial', size=12, bold=true, italic=true}",
          {.initial_font = {.lfHeight = -12, .lfWeight = FW_BOLD, .lfItalic = TRUE, .lfFaceName = L"Arial"},
           .mode = wwm_norule,
           .max_width = 800,
           .letter_spacing = 0,
           .monospace = false,
           .high_resolution = false},
      },
      {
          L"return {mode=1, width=320, spacing=-1, monospace=true, high_resolution=true}",
          {.initial_font = {.lfHeight = -9, .lfWeight = FW_NORMAL, .lfItalic = FALSE, .lfFaceName = L"MS UI Gothic"},
           .mode = wwm_norule,
           .max_width = 320,
           .letter_spacing = -1,
           .monospace = true,
           .high_resolution = true},
      },
  };
  struct wordwrap_settings wws = {0};
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    TEST_CASE_("%ls", tests[i].input);
    if (!TEST_SUCCEEDED_F(do_initialize_params(tests[i].input, &wws))) {
      goto cleanup;
    }
    compare_logfontw(&tests[i].wws.initial_font, &wws.initial_font);
    compare_wordwrap_settings(&tests[i].wws, &wws);
  }
cleanup:
  return;
}

NODISCARD static error do_wordwrap(wchar_t const *const code, struct wstr *ws) {
  if (!code || !ws) {
    return errg(err_invalid_arugment);
  }
  error err = eok();
  lua_State *L = NULL;
  L = lua_newstate(lua_alloc, NULL);
  if (L == NULL) {
    err = errg(err_fail);
    goto cleanup;
  }
  luaL_openlibs(L);
  lua_pushcfunction(L, luafn_wordwrap);
  lua_setglobal(L, "wordwrap");
  err = dostring(L, code);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = towstr(L, -1, ws);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  if (L) {
    lua_close(L);
  }
  cleanup_wordwrap();
  return err;
}

static void test_kerning(void) {
  static const struct {
    wchar_t *input;
    wchar_t *expected;
  } tests[] = {
      {L"return wordwrap('<kern>hello world</kern>', {font='Arial', size=24, width=256, mode=1})",
       L"h<p-2,+0>e<p-1,+0>l<p-2,+0>l<p-2,+0>o w<p-1,+0>o<p-1,+0>r<p-1,+0>l<p-2,+0>d"},
  };
  struct wstr ws = {0};
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    TEST_CASE_("%ls", tests[i].input);
    if (!TEST_SUCCEEDED_F(do_wordwrap(tests[i].input, &ws))) {
      goto cleanup;
    }
    TEST_CHECK(wcscmp(ws.ptr, tests[i].expected) == 0);
    TEST_MSG("expected: %ls\n     got: %ls", tests[i].expected, ws.ptr);
  }
cleanup:
  ereport(sfree(&ws));
}

static void test_position_tag(void) {
  static const struct {
    wchar_t *input;
    wchar_t *expected;
  } tests[] = {
      {
          L"return wordwrap('hello<p10,20>world', {font='Arial', size=24, width=256, mode=1})",
          L"hello<p10,20>world",
      },
      {
          L"return wordwrap('hello<p10,20,30>world', {font='Arial', size=24, width=256, mode=1})",
          L"hello<p10,20,30>world",
      },
      {
          L"return wordwrap('hello<p+10,20>world', {font='Arial', size=24, width=256, mode=1})",
          L"hello<p+10,20>world",
      },
      {
          L"return wordwrap('hello<p+10,+20>world', {font='Arial', size=24, width=256, mode=1})",
          L"hello<p+10,+20>world",
      },
      {
          L"return wordwrap('hello<p+10,20,30>world', {font='Arial', size=24, width=256, mode=1})",
          L"hello<p+10,20,30>world",
      },
      {
          L"return wordwrap('hello<p+10,+20,30>world', {font='Arial', size=24, width=256, mode=1})",
          L"hello<p+10,+20,30>world",
      },
      {
          L"return wordwrap('hello<p+10,+20,+30>world', {font='Arial', size=24, width=256, mode=1})",
          L"hello<p+10,+20,+30>world",
      },
  };
  struct wstr ws = {0};
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    TEST_CASE_("%ls", tests[i].input);
    if (!TEST_SUCCEEDED_F(do_wordwrap(tests[i].input, &ws))) {
      goto cleanup;
    }
    TEST_CHECK(wcscmp(ws.ptr, tests[i].expected) == 0);
    TEST_MSG("expected: %ls\n     got: %ls", tests[i].expected, ws.ptr);
  }
cleanup:
  ereport(sfree(&ws));
}

static void test_tag(void) {
  if (GetACP() != 932) {
    // This test is only for Japanese Windows.
    return;
  }
  static const struct {
    wchar_t *input;
    wchar_t *expected;
  } tests[] = {
      {L"return wordwrap('ov<wbr>er flow',  {font='Arial', size=9, width=16, mode=1})", L"ove\nr fl\now"},
      {L"return wordwrap('こん<nobr>にちは', {font='MS UI Gothic', size=12, width=4, mode=2})", L"こ\nん\nにちは"},
      {L"return wordwrap('<nobr>こん</nobr>にちは', {font='MS UI Gothic', size=12, width=4, mode=2})",
       L"こん\nに\nち\nは"},
      {L"return wordwrap('hello\\nworld', {font='Arial', size=9, width=32, mode=1})", L"hello\nworld"},
      {L"return wordwrap('hello\\n\\nworld', {font='Arial', size=9, width=32, mode=1})", L"hello\n\nworld"},
  };

  struct wstr ws = {0};
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    TEST_CASE_("%ls", tests[i].input);
    if (!TEST_SUCCEEDED_F(do_wordwrap(tests[i].input, &ws))) {
      goto cleanup;
    }
    TEST_CHECK(wcscmp(ws.ptr, tests[i].expected) == 0);
    TEST_MSG("expected: %ls\n     got: %ls", tests[i].expected, ws.ptr);
  }
cleanup:
  ereport(sfree(&ws));
}

static void test_wordwrap(void) {
  if (GetACP() != 932) {
    // This test is only for Japanese Windows.
    return;
  }
  struct wstr ws = {0};
  if (!TEST_SUCCEEDED_F(
          do_wordwrap(L"return wordwrap('<s64>少し長め<s>の<#ff0000>日本語テキスト<#>を用いることで、wordwrap "
                      L"が正常に動作するかをテストしています。', {width=400, size=32, spacing=5})",
                      &ws))) {
    goto cleanup;
  }
  static wchar_t const *const expected = L"<s64>少し長め<s>の<#ff0000>日本語\n"
                                         L"テキスト<#>を用いることで、wo\n"
                                         L"rdwrap が正常に動作する\n"
                                         L"かをテストしています。";
  TEST_CHECK(wcscmp(ws.ptr, expected) == 0);
  TEST_MSG("expected: %ls\n     got: %ls", expected, ws.ptr);
cleanup:
  ereport(sfree(&ws));
}

static void test_modes(void) {
  static const struct {
    wchar_t *input;
    wchar_t *expected;
  } tests[] = {
      {L"return wordwrap('over flow',  {font='Arial', size=9, width=16, mode=1})", L"ove\nr fl\now"},
      {L"return wordwrap('over flow',  {font='Arial', size=9, width=2,  mode=1})", L"o\nv\ne\nr\nf\nl\no\nw"},
      {L"return wordwrap('over flow',  {font='Arial', size=9, width=2, mode=2})", L"over\nflow"},
      {L"return wordwrap('over flow',  {font='Arial', size=9, width=16, mode=2})", L"over\nflow"},
      {L"return wordwrap('over  flow', {font='Arial', size=9, width=16, mode=2})", L"over\nflow"},
  };

  struct wstr ws = {0};

  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    TEST_CASE_("%ls", tests[i].input);
    if (!TEST_SUCCEEDED_F(do_wordwrap(tests[i].input, &ws))) {
      goto cleanup;
    }
    TEST_CHECK(wcscmp(ws.ptr, tests[i].expected) == 0);
    TEST_MSG("expected: %ls\n     got: %ls", tests[i].expected, ws.ptr);
  }

cleanup:
  ereport(sfree(&ws));
}

static void test_budoux(void) {
  static wchar_t const *const str1 = L"過ごしやすい温度とは? 春や秋はどこへ?";
  static wchar_t const *const str2 = L"The quick brown fox";
  static wchar_t const *const str3 =
      L"こんにちは、日本語を書く上で、長い文章を書くと、必ずどこかで折り返されるでしょう。";
  struct {
    wchar_t const *srcstr;
    size_t expected_found;
    size_t overflow_pos;
  } tests[] = {
      {str1, 4, 0},
      {str1, 4, 2},
      {str1, 4, 4},
      {str1, 6, 8},
      {str1, 6, 10},
      {str1, 11, 11},
      {str1, 12, 12},
      {str1, 12, 13},
      {str2, 3, 2},
      {str2, 4, 6},
      {str3, 3, 4},
  };

  char errmsg[128] = {0};
  struct budouxc *bodouxc_model = budouxc_init_embedded_ja(NULL, errmsg);
  TEST_ASSERT(bodouxc_model != NULL);

  struct glyph *glyphs = NULL;
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); ++i) {
    if (!TEST_SUCCEEDED_F(OV_ARRAY_GROW(&glyphs, wcslen(tests[i].srcstr)))) {
      return;
    }
    OV_ARRAY_SET_LENGTH(glyphs, wcslen(tests[i].srcstr));
    for (size_t j = 0; j < OV_ARRAY_LENGTH(glyphs); ++j) {
      glyphs[j] = (struct glyph){.pos = (uint16_t)j, .u.glyph.ch = tests[i].srcstr[j]};
    }
    struct line_reader lr = {
        .glyphs = glyphs,
        .linehead = 0,
    };
    size_t *boundaries = NULL;
    TEST_CASE_("#%zu | pos = %zu", i, tests[i].overflow_pos);
    if (!TEST_SUCCEEDED_F(bdx_write_markers(glyphs, 0, bodouxc_model, &boundaries))) {
      goto cleanup;
    }
    size_t const found = line_reader_find_breakable(&lr, tests[i].overflow_pos, wordwrap_mode_to_rule(wwm_budoux));
    TEST_CHECK(found == tests[i].expected_found);
    TEST_MSG("expected: %zu, got: %zu", tests[i].expected_found, found);
    OV_ARRAY_DESTROY(&boundaries);
  }
cleanup:
  if (glyphs) {
    OV_ARRAY_DESTROY(&glyphs);
  }
  budouxc_destroy(bodouxc_model);
  bodouxc_model = NULL;
}

TEST_LIST = {
    {"test_initialize_params", test_initialize_params},
    {"test_kerning", test_kerning},
    {"test_position_tag", test_position_tag},
    {"test_tag", test_tag},
    {"test_wordwrap", test_wordwrap},
    {"test_modes", test_modes},
    {"test_budoux", test_budoux},
    {NULL, NULL},
};
