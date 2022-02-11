#include "ptklayer.h"

#include "ovnum.h"
#include "ovutil/str.h"
#include "ovutil/win32.h"

#include <commdlg.h>

#include "luastr.h"

NODISCARD static error copy_to_clipboard(HWND const owner, struct wstr *content) {
  if (!owner || !content) {
    return errg(err_invalid_arugment);
  }
  bool clipboard_open = false;
  HGLOBAL h = NULL;
  wchar_t *p = NULL;
  error err = eok();
  if (!OpenClipboard(owner)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  clipboard_open = true;
  h = GlobalAlloc(GMEM_MOVEABLE, (content->len + 1) * sizeof(wchar_t));
  if (!h) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  p = GlobalLock(h);
  if (!p) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  memcpy(p, content->ptr, content->len * sizeof(wchar_t));
  p[content->len] = L'\0';
  if (!GlobalUnlock(p)) {
    HRESULT hr = HRESULT_FROM_WIN32(GetLastError());
    if (hr != HRESULT_FROM_WIN32(NO_ERROR)) {
      ereport(errhr(HRESULT_FROM_WIN32(GetLastError())));
    }
  }
  p = NULL;
  if (!EmptyClipboard()) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  if (!SetClipboardData(CF_UNICODETEXT, h)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  h = NULL;
cleanup:
  if (p) {
    if (!GlobalUnlock(p)) {
      HRESULT hr = HRESULT_FROM_WIN32(GetLastError());
      if (hr != HRESULT_FROM_WIN32(NO_ERROR)) {
        ereport(errhr(HRESULT_FROM_WIN32(GetLastError())));
      }
    }
    p = NULL;
  }
  if (h) {
    if (GlobalFree(h)) {
      ereport(errhr(HRESULT_FROM_WIN32(GetLastError())));
    }
    h = NULL;
  }
  if (clipboard_open) {
    if (!CloseClipboard()) {
      ereport(errhr(HRESULT_FROM_WIN32(GetLastError())));
    }
    clipboard_open = false;
  }
  return err;
}

NODISCARD static error show_save_dialog(HWND const owner,
                                        struct wstr const *const title,
                                        struct wstr const *const default_filename,
                                        struct wstr *const dest) {
  enum {
    BUFFER_SIZE = 4096,
  };
  struct wstr tmp = {0};
  struct wstr dir = {0};
  error err = sgrow(&tmp, BUFFER_SIZE);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(&tmp, default_filename->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_module_file_name(get_hinstance(), &dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  size_t pos = 0;
  err = extract_file_name(&dir, &pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  dir.ptr[pos] = L'\0';
  dir.len = pos;
  err = scat(&dir, L"script");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  OPENFILENAMEW ofn = {
      .lStructSize = sizeof(OPENFILENAMEW),
      .hInstance = get_hinstance(),
      .hwndOwner = owner,
      .lpstrTitle = title->ptr,
      .lpstrFilter = L"AviUtl アニメーション効果スクリプト(*.anm)\0*.anm\0CSV ファイル(*.csv)\0*.csv\0",
      .nFilterIndex = 1,
      .lpstrDefExt = L"anm",
      .lpstrInitialDir = dir.ptr,
      .lpstrFile = tmp.ptr,
      .nMaxFile = BUFFER_SIZE - 1,
      .Flags = OFN_EXPLORER | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY | OFN_ENABLESIZING |
               OFN_OVERWRITEPROMPT,
  };
  if (!GetSaveFileNameW(&ofn)) {
    err = errg(err_abort);
    goto cleanup;
  }
  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&dir));
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error
write_to_file(struct wstr const *const path, struct wstr const *const content, UINT const code_page) {
  HANDLE h = INVALID_HANDLE_VALUE;
  struct str tmp = {0};
  error err = to_cp(code_page, content, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  h = CreateFileW(
      path->ptr, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  DWORD written = 0;
  if (code_page == CP_UTF8) {
    if (!WriteFile(h, "\xef\xbb\xbf", 3, &written, NULL)) {
      err = errhr(HRESULT_FROM_WIN32(GetLastError()));
      goto cleanup;
    }
  }
  if (!WriteFile(h, tmp.ptr, tmp.len, &written, NULL)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
cleanup:
  if (h != INVALID_HANDLE_VALUE) {
    CloseHandle(h);
    h = INVALID_HANDLE_VALUE;
  }
  ereport(sfree(&tmp));
  return err;
}

void ptklayer_free(struct ptklayer *const pi) {
  for (size_t i = 0; i < pi->len; ++i) {
    ereport(sfree(&pi->ptr[i].name));
    ereport(sfree(&pi->ptr[i].value));
  }
  ereport(afree(pi));
  ereport(sfree(&pi->slider_name));
  ereport(sfree(&pi->file_path));
}

NODISCARD error ptklayer_parse(struct str const *const names_utf8,
                               struct str const *const values_utf8,
                               struct str const *const slider_name_utf8,
                               struct str const *const file_path_utf8,
                               struct ptklayer *const pi) {
  if (!names_utf8 || !values_utf8) {
    return errg(err_invalid_arugment);
  }
  if (!pi) {
    return errg(err_null_pointer);
  }
  struct wstr n = {0};
  struct wstr v = {0};
  error err = eok();

  size_t npos = 0, vpos = 0;
  while (npos < names_utf8->len && vpos < values_utf8->len) {
    struct str const n8 = str_unmanaged_const(names_utf8->ptr + npos);
    struct str const v8 = str_unmanaged_const(values_utf8->ptr + vpos);
    err = from_utf8(&n8, &n);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = from_utf8(&v8, &v);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = apush(pi, ((struct ptklayer_item){.name = n, .value = v}));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    n = (struct wstr){0};
    v = (struct wstr){0};
    npos += n8.len + 1;
    vpos += v8.len + 1;
  }
  if (slider_name_utf8) {
    err = from_utf8(slider_name_utf8, &pi->slider_name);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = from_utf8(file_path_utf8, &pi->file_path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&v));
  ereport(sfree(&n));
  if (efailed(err)) {
    ptklayer_free(pi);
  }
  return err;
}

static inline bool is_faview(struct ptklayer const *const pi) { return pi && pi->slider_name.ptr; }

static wchar_t hex2int(wchar_t c) {
  if (L'0' <= c && c <= L'9') {
    return (c & 0xff) - L'0';
  }
  if ((L'A' <= c && c <= L'F') || (L'a' <= c && c <= L'f')) {
    return (c & 0x5f) - L'A' + 10;
  }
  return 255;
}

NODISCARD static error percent_decode(struct wstr const *const src, struct wstr *dest) {
  if (!src) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  error err = sgrow(dest, src->len + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  size_t n = 0;
  for (size_t i = 0, srclen = src->len; i < srclen; ++i) {
    wchar_t const c = src->ptr[i];
    if (c != L'%') {
      dest->ptr[n++] = c;
      continue;
    }
    if (i + 2 >= srclen) {
      dest->ptr[n++] = c;
      continue;
    }
    wchar_t const c1 = hex2int(src->ptr[i + 1]);
    wchar_t const c2 = hex2int(src->ptr[i + 2]);
    if (c1 == 255 || c2 == 255) {
      dest->ptr[n++] = c;
      continue;
    }
    // If founds "%00", skip without outputting.
    if (c1 + c2 > 0) {
      dest->ptr[n++] = (wchar_t)((c1 << 4) | c2);
    }
    i += 2;
  }
  dest->ptr[n] = L'\0';
  dest->len = n;
cleanup:
  return err;
}

NODISCARD static error layer_name_to_readable(struct wstr const *const name, struct wstr *const dest) {
  if (!name) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  struct wstr tmp = {0};
  wchar_t *p = wcsrchr(name->ptr, L'/');
  error err = scpy(&tmp, p ? p + 1 : name->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = percent_decode(&tmp, dest);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error slider_name_to_readable(struct wstr const *const name, struct wstr *const dest) {
  if (!name) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  wchar_t *p = wcsrchr(name->ptr, L'\\');
  error err = scpy(dest, p ? p + 1 : name->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD error ptklayer_get_readable_item_name(struct ptklayer const *const pi,
                                                size_t const idx,
                                                struct wstr *const dest) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  error err = eok();
  if (is_faview(pi)) {
    err = scpy(dest, pi->ptr[idx].name.ptr);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    goto cleanup;
  }
  err = layer_name_to_readable(&pi->ptr[idx].name, dest);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD static error extract_layer_name(struct wstr const *const name, size_t *const pos) {
  if (!name) {
    return errg(err_invalid_arugment);
  }
  if (!pos) {
    return errg(err_null_pointer);
  }
  wchar_t *p = wcsrchr(name->ptr, L'/');
  *pos = p != NULL ? (size_t)(p - name->ptr + 1) : 0;
  return eok();
}

NODISCARD static error parent_layer_name_to_readable(struct wstr const *const name, struct wstr *const dest) {
  if (!name) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  struct wstr tmp = {0};
  size_t pos = 0;
  error err = extract_layer_name(name, &pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sncpy(&tmp, name->ptr, pos == 0 ? name->len : pos - 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = layer_name_to_readable(&tmp, dest);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

NODISCARD error ptklayer_get_readable_group_name(struct ptklayer const *const pi,
                                                 size_t const idx,
                                                 struct wstr *const dest) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  error err = eok();
  if (is_faview(pi)) {
    err = slider_name_to_readable(&pi->slider_name, dest);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    goto cleanup;
  }
  err = parent_layer_name_to_readable(&pi->ptr[idx].name, dest);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD static error
file_path_to_default_save_name(struct ptklayer *const pi, size_t const idx, struct wstr *const dest) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  struct wstr tmp = {0};
  struct wstr tmp2 = {0};
  wchar_t *p = wcschr(pi->file_path.ptr, L'|');
  error err = sncpy(&tmp, pi->file_path.ptr, p != NULL ? (size_t)(p - pi->file_path.ptr) : pi->file_path.len);
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
  size_t extpos = 0;
  err = extract_file_extension(&tmp, &extpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp.ptr[extpos] = L'\0';
  tmp.len = extpos;
  err = ptklayer_get_readable_group_name(pi, idx, &tmp2);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sanitize(&tmp2);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scatm(&tmp, L"-", tmp2.ptr, L".anm");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(dest, tmp.ptr + fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp2));
  ereport(sfree(&tmp));
  return err;
}

NODISCARD error ptklayer_send_item(struct ptklayer *const pi, HWND const target, size_t const idx) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  error err = eok();
  if (!SetWindowTextW(target, pi->ptr[idx].value.ptr)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD error ptklayer_copy_to_clipboard_item(struct ptklayer *const pi,
                                                HWND const clipboard_owner,
                                                size_t const idx) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  error err = copy_to_clipboard(clipboard_owner, &pi->ptr[idx].value);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

struct size_t_array {
  size_t *ptr;
  size_t len;
  size_t cap;
};

NODISCARD static error enumerate_siblings(struct ptklayer *const pi, size_t const idx, struct size_t_array *a) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  error err = eok();
  if (is_faview(pi)) {
    for (size_t i = 0; i < pi->len; ++i) {
      err = apush(a, i);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
    }
    goto cleanup;
  }
  size_t pos = 0;
  err = extract_layer_name(&pi->ptr[idx].name, &pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (pos == 0) {
    err = errg(err_unexpected);
    goto cleanup;
  }
  for (size_t i = 0; i < pi->len; ++i) {
    if (wcsncmp(pi->ptr[i].name.ptr, pi->ptr[idx].name.ptr, pos) != 0) {
      continue;
    }
    err = apush(a, i);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
cleanup:
  return err;
}

NODISCARD static error build_siblings_script(struct ptklayer *const pi, size_t const idx, struct wstr *const dest) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  struct wstr tmp = {0};
  struct wstr tmp2 = {0};
  wchar_t tmpbuf[32];
  struct size_t_array sibling_indices = {0};
  error err = enumerate_siblings(pi, idx, &sibling_indices);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = ptklayer_get_readable_group_name(pi, idx, &tmp2);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sreplace_all(&tmp2, L",", L"_");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpym(&tmp,
              L"--track0:",
              tmp2.ptr,
              L",0,",
              ovbase_utoa_wchar((uint64_t)sibling_indices.len, tmpbuf),
              L",0,1\r\n",
              L"local values= {\r\n");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  for (size_t i = 0; i < sibling_indices.len; ++i) {
    err = luastr_encode(&pi->ptr[sibling_indices.ptr[i]].value, &tmp2);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = scatm(&tmp, L"  ", tmp2.ptr, L",\r\n");
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = scatm(&tmp, L"}\r\n", L"PSD:addstate(values, obj.track0)\r\n");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(afree(&sibling_indices));
  ereport(sfree(&tmp2));
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error encode_csv(struct wstr *const ws) {
  struct wstr tmp = {0};
  error err = eok();
  if (!wcschr(ws->ptr, L',')) {
    goto cleanup;
  }
  err = sreplace_all(ws, L"\"", L"\"\"");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpym(&tmp, L"\"", ws->ptr, L"\"");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(ws, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error build_siblings_csv(struct ptklayer *const pi, size_t const idx, struct wstr *const dest) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  struct wstr tmp = {0};
  struct wstr tmp2 = {0};
  struct size_t_array sibling_indices = {0};
  error err = enumerate_siblings(pi, idx, &sibling_indices);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  for (size_t i = 0; i < sibling_indices.len; ++i) {
    err = ptklayer_get_readable_item_name(pi, sibling_indices.ptr[i], &tmp2);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = encode_csv(&tmp2);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = scat(&tmp, tmp2.ptr);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    struct ptklayer_item *item = pi->ptr + sibling_indices.ptr[i];
    err = scpy(&tmp2, item->value.ptr);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = encode_csv(&tmp2);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = scatm(&tmp, L",", tmp2.ptr, L"\r\n");
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(afree(&sibling_indices));
  ereport(sfree(&tmp2));
  ereport(sfree(&tmp));
  return err;
}

NODISCARD error ptklayer_copy_to_clipboard_siblings(struct ptklayer *const pi, HWND const owner, size_t const idx) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  struct wstr tmp = {0};
  error err = build_siblings_script(pi, idx, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = copy_to_clipboard(owner, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

NODISCARD error ptklayer_save_to_file_siblings(struct ptklayer *const pi, HWND const window, size_t const idx) {
  if (!pi || idx >= pi->len) {
    return errg(err_invalid_arugment);
  }
  struct wstr tmp = {0};
  struct wstr path = {0};
  error err = file_path_to_default_save_name(pi, idx, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  HWND *disabled_windows = NULL;
  err = disable_family_windows(window, &disabled_windows);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = show_save_dialog(window, &wstr_unmanaged_const(L"ファイルに保存"), &tmp, &path);
  restore_disabled_family_windows(disabled_windows);
  if (efailed(err)) {
    if (eisg(err, err_abort)) {
      efree(&err);
      goto cleanup;
    }
    err = ethru(err);
    goto cleanup;
  }
  if (wcsicmp(path.ptr + path.len - 4, L".csv") == 0) {
    err = build_siblings_csv(pi, idx, &tmp);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = write_to_file(&path, &tmp, CP_UTF8);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  } else {
    err = build_siblings_script(pi, idx, &tmp);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = write_to_file(&path, &tmp, CP_ACP);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
cleanup:
  ereport(sfree(&path));
  ereport(sfree(&tmp));
  return err;
}
