#pragma once

#include "3rd/base.c/include/base.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

struct ptklayer_item {
  struct wstr name;
  struct wstr value;
};

struct ptklayer {
  struct ptklayer_item *ptr;
  size_t len;
  size_t cap;

  struct wstr slider_name;
  struct wstr file_path;
};

void ptklayer_free(struct ptklayer *const pi);
NODISCARD error ptklayer_parse(struct str const *const names_utf8,
                               struct str const *const values_utf8,
                               struct str const *const slider_name_utf8,
                               struct str const *const file_path_utf8,
                               struct ptklayer *const pi);
NODISCARD error ptklayer_get_readable_item_name(struct ptklayer const *const pi,
                                                size_t const idx,
                                                struct wstr *const dest);
NODISCARD error ptklayer_get_readable_group_name(struct ptklayer const *const pi,
                                                 size_t const idx,
                                                 struct wstr *const dest);
NODISCARD error ptklayer_send_item(struct ptklayer *const pi, HWND const target, size_t const idx);
NODISCARD error ptklayer_copy_to_clipboard_item(struct ptklayer *const pi,
                                                HWND const clipboard_owner,
                                                size_t const idx);
NODISCARD error ptklayer_copy_to_clipboard_siblings(struct ptklayer *const pi, HWND const owner, size_t const idx);
NODISCARD error ptklayer_save_to_file_siblings(struct ptklayer *const pi, HWND const window, size_t const idx);
