#pragma once

#include <stdbool.h>
#include <stdint.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "ovbase.h"

void set_hinstance(HINSTANCE const h);
HINSTANCE get_hinstance(void);

NODISCARD error disable_family_windows(HWND const exclude, HWND **const disabled_windows);
void restore_disabled_family_windows(HWND *const disabled_windows);
int message_box(HWND const window, wchar_t const *const msg, wchar_t const *const title, UINT const flags);

NODISCARD error set_client_size(HWND const window, LONG const width, LONG const height);

NODISCARD error atoi64(struct wstr const *const s, int64_t *const dest);
NODISCARD error atou64(struct wstr const *const s, uint64_t *const dest);
NODISCARD error utoa64(uint64_t const v, struct wstr *const dest);
NODISCARD error sanitize(struct wstr *const ws);

NODISCARD error include_trailing_path_delimiter(struct wstr *const ws);
NODISCARD error exclude_trailing_path_delimiter(struct wstr *const ws);
NODISCARD error extract_file_name(struct wstr const *const src, size_t *const pos);
NODISCARD error extract_file_extension(struct wstr const *const src, size_t *const pos);

NODISCARD error get_window_text(HWND const window, struct wstr *const dest);
NODISCARD error get_module_file_name(HINSTANCE const hinst, struct wstr *const dest);
NODISCARD error get_temp_dir(struct wstr *const dest);
NODISCARD error get_long_path_name(struct wstr const *const src, struct wstr *const dest);
NODISCARD error file_exists(struct wstr const *const path, bool *const exists);
NODISCARD error file_contains(struct wstr const *const dir, struct wstr const *const file, bool *const contains);
NODISCARD error is_same_file(struct wstr const *const file1, struct wstr const *const file2, bool *const is_same);
NODISCARD error is_same_dir(struct wstr const *const dir1, struct wstr const *const dir2, bool *const is_same);

NODISCARD error create_unique_temp_file(wchar_t const *const base_filename,
                                        wchar_t const *const ext,
                                        void *const data,
                                        size_t const datalen,
                                        struct wstr *const dest);
NODISCARD error create_unique_file(wchar_t const *const base_fullpath,
                                   wchar_t const *const ext,
                                   DWORD const file_attributes,
                                   void *const data,
                                   size_t const datalen,
                                   struct wstr *const dest);
NODISCARD error delete_file(struct wstr const *const path);

NODISCARD error from_cp(UINT const code_page, struct str const *const src, struct wstr *const dest);
NODISCARD error from_mbcs(struct str const *const src, struct wstr *const dest);
NODISCARD error from_utf8(struct str const *const src, struct wstr *const dest);
NODISCARD error to_cp(UINT const code_page, struct wstr const *const src, struct str *const dest);
NODISCARD error to_mbcs(struct wstr const *const src, struct str *const dest);
NODISCARD error to_utf8(struct wstr const *const src, struct str *const dest);
