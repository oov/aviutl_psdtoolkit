#include "setting.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <lua5.1/lauxlib.h>
#include <lua5.1/lualib.h>

#include "aviutl.h"
#include "util.h"
#include "version.h"

static wchar_t const *const setting_dialog_prop = L"setting_dialog";

enum {
  ID_CHK_GENERATE_LIPSYNC = 100,
  ID_CHK_GENERATE_MPSLIDER = 101,
  ID_CHK_GENERATE_SUBTITLE = 102,
  ID_CHK_MERGE_PREPS = 103,
  ID_EDT_LIPSYNC_GROUP_ID = 1000,
  ID_EDT_LIPSYNC_OFFSET = 1001,
  ID_CMB_MPSLIDER_NUMBER = 2000,
  ID_EDT_MPSLIDER_GROUP_ID = 2001,
  ID_EDT_MPSLIDER_MARGIN_LEFT = 2002,
  ID_EDT_MPSLIDER_MARGIN_RIGHT = 2003,
  ID_CMB_SUBTITLE_ENCODING = 3000,
  ID_EDT_SUBTITLE_GROUP_ID = 3001,
  ID_EDT_SUBTITLE_MARGIN_LEFT = 3002,
  ID_EDT_SUBTITLE_MARGIN_RIGHT = 3003,
  ID_CHK_FIRE_SHIFT = 104,
  ID_CHK_FIRE_WAVTXT = 105,
  ID_CHK_FIRE_EXO = 106,
};

struct setting_dialog {
  error err;
  bool generate_lipsync;
  bool generate_mpslider;
  bool generate_subtitle;
  bool merge_preps;
  int lipsync_group_id;
  int lipsync_offset;
  int mpslider_number;
  int mpslider_group_id;
  int mpslider_margin_left;
  int mpslider_margin_right;
  int subtitle_encoding;
  int subtitle_group_id;
  int subtitle_margin_left;
  int subtitle_margin_right;
  bool fire_shift;
  bool fire_wavtxt;
  bool fire_exo;
  char reserved[1];
};

static bool get_check(HWND const window, int const control_id) {
  return SendMessageW(GetDlgItem(window, control_id), BM_GETCHECK, 0, 0) == BST_CHECKED;
}
static void set_check(HWND const window, int const control_id, bool const checked) {
  SendMessageW(GetDlgItem(window, control_id), BM_SETCHECK, checked ? BST_CHECKED : BST_UNCHECKED, 0);
}
static void set_int(HWND const window, int const control_id, int const value) {
  wchar_t buf[32] = {0};
  wsprintfW(buf, L"%d", value);
  SetWindowTextW(GetDlgItem(window, control_id), buf);
}
static int get_int(HWND const window, int const control_id, int const def) {
  int64_t v = 0;
  struct wstr ws = {0};
  error err = get_window_text(GetDlgItem(window, control_id), &ws);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = atoi64(&ws, &v);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&ws));
  if (efailed(err)) {
    efree(&err);
    return def;
  }
  return (int)v;
}

static INT_PTR CALLBACK setting_dialog_wndproc(HWND const dlg,
                                               UINT const message,
                                               WPARAM const wparam,
                                               LPARAM const lparam) {
  switch (message) {
  case WM_INITDIALOG: {
    struct setting_dialog *const sd = (void *)lparam;
    SetPropW(dlg, setting_dialog_prop, (HANDLE)sd);
    SetWindowTextW(dlg, L"PSDToolKit 環境設定 " VERSION_WIDE);
    set_check(dlg, ID_CHK_GENERATE_LIPSYNC, sd->generate_lipsync);
    set_check(dlg, ID_CHK_GENERATE_MPSLIDER, sd->generate_mpslider);
    set_check(dlg, ID_CHK_GENERATE_SUBTITLE, sd->generate_subtitle);
    set_check(dlg, ID_CHK_MERGE_PREPS, sd->merge_preps);
    set_int(dlg, ID_EDT_LIPSYNC_GROUP_ID, sd->lipsync_group_id);
    set_int(dlg, ID_EDT_LIPSYNC_OFFSET, sd->lipsync_offset);
    {
      wchar_t buf[32] = {0};
      HWND const h = GetDlgItem(dlg, ID_CMB_MPSLIDER_NUMBER);
      for (int i = 1; i <= 10; ++i) {
        wsprintfW(buf, L"%d", i * 4);
        SendMessageW(h, CB_ADDSTRING, 0, (LPARAM)buf);
      }
      RECT rect = {0};
      GetWindowRect(h, &rect);
      SetWindowPos(h, 0, 0, 0, rect.right - rect.left, 300, SWP_NOMOVE | SWP_NOZORDER);
      SendMessageW(h, CB_SETCURSEL, (WPARAM)sd->mpslider_number, 0);
    }
    set_int(dlg, ID_EDT_MPSLIDER_GROUP_ID, sd->mpslider_group_id);
    set_int(dlg, ID_EDT_MPSLIDER_MARGIN_LEFT, sd->mpslider_margin_left);
    set_int(dlg, ID_EDT_MPSLIDER_MARGIN_RIGHT, sd->mpslider_margin_right);
    {
      HWND const h = GetDlgItem(dlg, ID_CMB_SUBTITLE_ENCODING);
      SendMessageW(h, CB_ADDSTRING, 0, (LPARAM)L"Shift_JIS");
      SendMessageW(h, CB_ADDSTRING, 0, (LPARAM)L"UTF-8");
      RECT rect = {0};
      GetWindowRect(h, &rect);
      SetWindowPos(h, 0, 0, 0, rect.right - rect.left, 300, SWP_NOMOVE | SWP_NOZORDER);
      SendMessageW(h, CB_SETCURSEL, (WPARAM)sd->subtitle_encoding, 0);
    }
    set_int(dlg, ID_EDT_SUBTITLE_GROUP_ID, sd->subtitle_group_id);
    set_int(dlg, ID_EDT_SUBTITLE_MARGIN_LEFT, sd->subtitle_margin_left);
    set_int(dlg, ID_EDT_SUBTITLE_MARGIN_RIGHT, sd->subtitle_margin_right);

    set_check(dlg, ID_CHK_FIRE_SHIFT, sd->fire_shift);
    set_check(dlg, ID_CHK_FIRE_WAVTXT, sd->fire_wavtxt);
    set_check(dlg, ID_CHK_FIRE_EXO, sd->fire_exo);
    return TRUE;
  }
  case WM_DESTROY:
    RemovePropW(dlg, setting_dialog_prop);
    return 0;
  case WM_COMMAND:
    switch (LOWORD(wparam)) {
    case IDOK: {
      error err = eok();
      struct setting_dialog *const sd = (void *)GetPropW(dlg, setting_dialog_prop);
      if (!sd) {
        err = errg(err_unexpected);
        goto cleanup;
      }
      sd->generate_lipsync = get_check(dlg, ID_CHK_GENERATE_LIPSYNC);
      sd->generate_mpslider = get_check(dlg, ID_CHK_GENERATE_MPSLIDER);
      sd->generate_subtitle = get_check(dlg, ID_CHK_GENERATE_SUBTITLE);
      sd->merge_preps = get_check(dlg, ID_CHK_MERGE_PREPS);

      sd->lipsync_group_id = get_int(dlg, ID_EDT_LIPSYNC_GROUP_ID, 1);
      sd->lipsync_offset = get_int(dlg, ID_EDT_LIPSYNC_OFFSET, 0);

      sd->mpslider_number = SendMessageW(GetDlgItem(dlg, ID_CMB_MPSLIDER_NUMBER), CB_GETCURSEL, 0, 0);
      sd->mpslider_group_id = get_int(dlg, ID_EDT_MPSLIDER_GROUP_ID, 1);
      sd->mpslider_margin_left = get_int(dlg, ID_EDT_MPSLIDER_MARGIN_LEFT, 0);
      sd->mpslider_margin_right = get_int(dlg, ID_EDT_MPSLIDER_MARGIN_RIGHT, 0);

      sd->subtitle_encoding = SendMessageW(GetDlgItem(dlg, ID_CMB_SUBTITLE_ENCODING), CB_GETCURSEL, 0, 0);
      sd->subtitle_group_id = get_int(dlg, ID_EDT_SUBTITLE_GROUP_ID, 1);
      sd->subtitle_margin_left = get_int(dlg, ID_EDT_SUBTITLE_MARGIN_LEFT, 0);
      sd->subtitle_margin_right = get_int(dlg, ID_EDT_SUBTITLE_MARGIN_RIGHT, 0);

      sd->fire_shift = get_check(dlg, ID_CHK_FIRE_SHIFT);
      sd->fire_wavtxt = get_check(dlg, ID_CHK_FIRE_WAVTXT);
      sd->fire_exo = get_check(dlg, ID_CHK_FIRE_EXO);
    cleanup:
      if (efailed(err)) {
        sd->err = err;
        err = NULL;
        EndDialog(dlg, 0);
        return TRUE;
      }
      EndDialog(dlg, IDOK);
      return TRUE;
    }
    case IDCANCEL:
      EndDialog(dlg, IDCANCEL);
      return TRUE;
    }
    break;
  }
  return FALSE;
}

static int read_int_prop(lua_State *const L, char const *const prop_name, int const def) {
  lua_getfield(L, -1, prop_name);
  int const r = lua_isnil(L, -1) ? def : lua_tointeger(L, -1);
  lua_pop(L, 1);
  return r;
}

static void read_props(lua_State *L, struct setting_dialog *const sd) {
  sd->generate_lipsync = read_int_prop(L, "wav_lipsync", 0) == 1;
  sd->generate_mpslider = read_int_prop(L, "wav_mpslider", 0) > 0;
  sd->generate_subtitle = read_int_prop(L, "wav_subtitle", 0) != 0;
  sd->lipsync_group_id = read_int_prop(L, "wav_lipsync_group", 1);
  sd->lipsync_offset = read_int_prop(L, "wav_lipsync_offset", 0);
  sd->mpslider_number = read_int_prop(L, "wav_mpslider", 2) - 1;
  sd->mpslider_group_id = read_int_prop(L, "wav_mpslider_group", 1);
  sd->mpslider_margin_left = read_int_prop(L, "wav_mpslider_margin_left", 0);
  sd->mpslider_margin_right = read_int_prop(L, "wav_mpslider_margin_right", 0);
  lua_getfield(L, -1, "wav_subtitle_encoding");
  sd->subtitle_encoding = (lua_isstring(L, -1) && strcmp(lua_tostring(L, -1), "utf8") == 0) ? 1 : 0;
  lua_pop(L, 1);
  sd->subtitle_group_id = read_int_prop(L, "wav_subtitle_group", 1);
  sd->subtitle_margin_left = read_int_prop(L, "wav_subtitle_margin_left", 0);
  sd->subtitle_margin_right = read_int_prop(L, "wav_subtitle_margin_right", 0);
  sd->merge_preps = read_int_prop(L, "wav_mergedprep", 0) == 1;
  sd->fire_shift = read_int_prop(L, "wav_firemode", 0) == 0;
  sd->fire_wavtxt = read_int_prop(L, "wav_firemode_wavtxt", 0) == 1;
  sd->fire_exo = read_int_prop(L, "wav_firemode_wavtxt", 0) == 1;
}

NODISCARD static error
write_int_prop(char *const buf, struct str *const text, char const *const prop_name, int const value, bool need_write) {
  if (!need_write) {
    return eok();
  }
  wsprintfA(buf, "%s = %d\r\n", prop_name, value);
  error err = scat(text, buf);
  if (efailed(err)) {
    err = ethru(err);
  }
  return err;
}

NODISCARD static error write_props(struct wstr const *const path, struct setting_dialog *const sd) {
  HANDLE file = INVALID_HANDLE_VALUE;
  struct str text = {0};
  char buf[1024] = {0};
  error err = scpy(&text, "local P = {}\r\n");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_lipsync", 1, sd->generate_lipsync);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_lipsync_group", sd->lipsync_group_id, sd->lipsync_group_id != 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_lipsync_offset", sd->lipsync_offset, sd->lipsync_offset != 0);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_mpslider", sd->mpslider_number + 1, sd->generate_mpslider);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_mpslider_group", sd->mpslider_group_id, sd->mpslider_group_id != 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err =
      write_int_prop(buf, &text, "P.wav_mpslider_margin_left", sd->mpslider_margin_left, sd->mpslider_margin_left != 0);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(
      buf, &text, "P.wav_mpslider_margin_right", sd->mpslider_margin_right, sd->mpslider_margin_right != 0);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_subtitle", 2, sd->generate_subtitle);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (sd->subtitle_encoding != 0) {
    err = scat(&text, "P.wav_subtitle_encoding = \"utf8\"\r\n");
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = write_int_prop(buf, &text, "P.wav_subtitle_group", sd->subtitle_group_id, sd->subtitle_group_id != 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err =
      write_int_prop(buf, &text, "P.wav_subtitle_margin_left", sd->subtitle_margin_left, sd->subtitle_margin_left != 0);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(
      buf, &text, "P.wav_subtitle_margin_right", sd->subtitle_margin_right, sd->subtitle_margin_right != 0);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_mergedprep", 1, sd->merge_preps);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_firemode", -1, !sd->fire_shift);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_firemode_wavtxt", 1, sd->fire_wavtxt);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_int_prop(buf, &text, "P.wav_firemode_exo", 1, sd->fire_exo);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&text, "return P\r\n");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  file = CreateFileW(
      path->ptr, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  if (file == INVALID_HANDLE_VALUE) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  DWORD written = 0;
  if (!WriteFile(file, text.ptr, text.len, &written, NULL) || written != text.len) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }

cleanup:
  if (file != INVALID_HANDLE_VALUE) {
    CloseHandle(file);
    file = NULL;
  }
  if (efailed(err)) {
    DeleteFileW(path->ptr);
  }
  ereport(sfree(&text));
  return err;
}

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

NODISCARD static error luafn_push_wstr(lua_State *const L, struct wstr const *const ws) {
  struct str s = {0};
  error err = to_mbcs(ws, &s);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  lua_pushstring(L, s.ptr);
  err = sfree(&s);
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    return err;
  }
  return eok();
}

NODISCARD error show_setting(void) {
  struct str text = {0};
  struct wstr tmp = {0};
  lua_State *L = NULL;
  HWND parent_window = NULL;
  HWND *disabled_windows = NULL;
  error err = aviutl_get_exedit_window(&parent_window);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_module_file_name(get_hinstance(), &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  size_t fnpos = 0;
  err = extract_file_name(&tmp, &fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp.ptr[fnpos] = L'\0';
  tmp.len = fnpos;
  err = scat(&tmp, L"script\\PSDToolKit\\");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  L = lua_newstate(lua_alloc, NULL);
  if (!L) {
    err = errg(err_fail);
    goto cleanup;
  }
  luaL_openlibs(L);
  lua_getglobal(L, "package");
  err = luafn_push_wstr(L, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushstring(L, "?.lua");
  lua_concat(L, 2);
  lua_setfield(L, -2, "path");
  err = luafn_push_wstr(L, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushstring(L, "?.dll");
  lua_concat(L, 2);
  lua_setfield(L, -2, "cpath");
  lua_pop(L, 1);
  lua_getglobal(L, "require");
  lua_pushstring(L, "setting-gui");
  if (lua_pcall(L, 1, 1, 0) != 0) {
    lua_newtable(L);
  }
  struct setting_dialog sd = {0};
  read_props(L, &sd);
  err = disable_family_windows(parent_window, &disabled_windows);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  INT_PTR r = DialogBoxParamW(get_hinstance(), L"SETTINGDIALOG", parent_window, setting_dialog_wndproc, (LPARAM)&sd);
  if (r == 0 || r == -1) {
    if (efailed(sd.err)) {
      err = ethru(sd.err);
      sd.err = NULL;
    } else {
      err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    }
    goto cleanup;
  }
  if (r == IDCANCEL) {
    goto cleanup;
  }
  err = scat(&tmp, L"setting-gui.lua");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_props(&tmp, &sd);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  if (disabled_windows) {
    restore_disabled_family_windows(disabled_windows);
    disabled_windows = NULL;
  }
  if (L) {
    lua_close(L);
    L = NULL;
  }
  ereport(sfree(&tmp));
  ereport(sfree(&text));
  return err;
}
