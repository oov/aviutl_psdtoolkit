#pragma once

#include "3rd/base.c/include/base.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

NODISCARD error find_exedit_multi_line_text(struct wstr const *const included_text,
                                            HWND *const window,
                                            HWND *const edit_control);

struct multi_parameter_dialog {
  HWND window;
  size_t len;
  HWND label[40];
  HWND edit[40];
};

NODISCARD error find_exedit_multi_parameter_dialog(struct multi_parameter_dialog *dlg);
