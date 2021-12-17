#include "find.h"

#include "util.h"

static HWND find_extended_filter_class_from_single_window_wrapper_child(HWND parent) {
  HWND h = 0;
  for (;;) {
    h = FindWindowExA(parent, h, "ExtendedFilterClass", NULL);
    if (h == 0) {
      return 0;
    }
    if (!IsWindowVisible(h)) {
      continue;
    }
    return find_extended_filter_class_from_single_window_wrapper_child(h);
  }
}

static HWND find_extended_filter_class_from_single_window_wrapper(void) {
  DWORD const pid = GetCurrentProcessId();
  HWND h = 0;
  for (;;) {
    h = FindWindowExA(0, h, "AVIUTLSINGLEWINDOW", NULL);
    if (h == 0) {
      return 0;
    }
    DWORD p = 0;
    GetWindowThreadProcessId(h, &p);
    if (p != pid) {
      continue;
    }
    if (!IsWindowVisible(h)) {
      continue;
    }
    return find_extended_filter_class_from_single_window_wrapper_child(h);
  }
}

static HWND find_extended_filter_window(void) {
  DWORD const pid = GetCurrentProcessId();
  HWND h = NULL;
  for (;;) {
    h = FindWindowExA(NULL, h, "ExtendedFilterClass", NULL);
    if (h == 0) {
      return 0;
    }
    DWORD p = 0;
    GetWindowThreadProcessId(h, &p);
    if (p != pid) {
      continue;
    }
    if (!IsWindowVisible(h)) {
      continue;
    }
    return h;
  }
}

static HWND find_control(HWND parent, HWND prev, wchar_t const *const class, RECT *r) {
  HWND h = prev;
  for (;;) {
    h = FindWindowExW(parent, h, class, NULL);
    if (!h) {
      return 0;
    }
    if (r) {
      RECT rect = {0};
      if (!GetWindowRect(h, &rect)) {
        continue;
      }
      POINT pt = {.x = (rect.left + rect.right) / 2, .y = (rect.top + rect.bottom) / 2};
      if (!ScreenToClient(parent, &pt)) {
        continue;
      }
      if (r->left < pt.x || pt.x >= r->right || r->top < pt.y || pt.y >= r->bottom) {
        continue;
      }
    }
    return h;
  }
}

NODISCARD error find_exedit_multi_line_text(struct wstr const *const included_text,
                                            HWND *const window,
                                            HWND *const edit_control) {
  if (!included_text) {
    return errg(err_invalid_arugment);
  }
  if (!window || !edit_control) {
    return errg(err_null_pointer);
  }
  struct wstr tmp = {0};
  error err = eok();
  HWND wnd = find_extended_filter_class_from_single_window_wrapper();
  if (!wnd) {
    wnd = find_extended_filter_window();
    if (!wnd) {
      err = errg(err_not_found);
      goto cleanup;
    }
  }
  HWND h = 0;
  for (;;) {
    h = find_control(wnd, h, L"Edit", NULL);
    if (!h) {
      err = errg(err_not_found);
      goto cleanup;
    }
    if (!IsWindowVisible(h)) {
      continue;
    }
    if ((GetWindowLongW(h, GWL_STYLE) & (ES_MULTILINE | ES_WANTRETURN)) != (ES_MULTILINE | ES_WANTRETURN)) {
      continue;
    }
    err = get_window_text(h, &tmp);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    ptrdiff_t pos = 0;
    err = sstr(&tmp, included_text->ptr, &pos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (pos == -1) {
      continue;
    }
    *window = wnd;
    *edit_control = h;
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

static HWND find_dialog(HWND prev) {
  DWORD const pid = GetCurrentProcessId();
  HWND h = prev;
  for (;;) {
    h = FindWindowExA(NULL, h, "#32770", NULL);
    if (h == 0) {
      return 0;
    }
    DWORD p = 0;
    GetWindowThreadProcessId(h, &p);
    if (p != pid) {
      continue;
    }
    if (!IsWindowVisible(h)) {
      continue;
    }
    return h;
  }
}

struct control {
  HWND label;
  HWND edit;
  RECT edit_rect;
};
struct controls {
  struct control *ptr;
  size_t len;
  size_t cap;
};

static int control_compare(void const *const a, void const *const b) {
  struct control const *const aa = a;
  struct control const *const bb = b;
  return aa->edit_rect.top < bb->edit_rect.top ? -1 : aa->edit_rect.top > bb->edit_rect.top ? 1 : 0;
}

NODISCARD error find_exedit_multi_parameter_dialog(struct multi_parameter_dialog *dlg) {
  struct controls controls = {0};
  HWND h = 0, label = 0, edit = 0;
  int ctrl_id = 0;
  POINT pt = {0};
  RECT wnd_rect = {0}, label_rect = {0}, edit_rect = {0};
  LONG y = 0;
  error err = eok();
  for (;;) {
    h = find_dialog(h);
    if (!h) {
      err = errg(err_not_found);
      goto cleanup;
    }
    if (!GetClientRect(h, &wnd_rect)) {
      continue;
    }
    if (!ScreenToClient(h, &pt)) {
      continue;
    }
    controls.len = 0;
    for (;;) {
      label = FindWindowExW(h, label, L"Static", NULL);
      if (!label) {
        break;
      }
      ctrl_id = GetDlgCtrlID(label);
      // Control ID seem to be assigned between 131-170 for labels.
      if (ctrl_id < 131 || ctrl_id > 170) {
        continue;
      }
      if (!IsWindowVisible(label)) {
        continue;
      }
      edit = GetDlgItem(h, ctrl_id - 131 + 171);
      if (!edit) {
        continue;
      }
      if (!IsWindowVisible(edit)) {
        continue;
      }
      if (!GetWindowRect(label, &label_rect)) {
        continue;
      }
      if (!GetWindowRect(edit, &edit_rect)) {
        continue;
      }
      label_rect.left += pt.x;
      label_rect.right += pt.x;
      label_rect.top += pt.y;
      label_rect.bottom += pt.y;
      edit_rect.left += pt.x;
      edit_rect.right += pt.x;
      edit_rect.top += pt.y;
      edit_rect.bottom += pt.y;
      y = (edit_rect.top + edit_rect.bottom) / 2;
      if (y < label_rect.top || label_rect.bottom < y || edit_rect.left < label_rect.right ||
          wnd_rect.right - wnd_rect.left < edit_rect.left) {
        continue;
      }
      err = apush(&controls, ((struct control){.label = label, .edit = edit, .edit_rect = edit_rect}));
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
    }
    if (controls.len) {
      qsort(controls.ptr, controls.len, sizeof(controls.ptr[0]), control_compare);
      break;
    }
  }
  dlg->window = h;
  dlg->len = controls.len;
  for (size_t i = 0; i < controls.len; ++i) {
    dlg->label[i] = controls.ptr[i].label;
    dlg->edit[i] = controls.ptr[i].edit;
  }
cleanup:
  ereport(afree(&controls));
  return err;
}
