#include "popupmenu.h"

void popup_menu_free(struct popup_menu *pm) {
  for (size_t i = 0; i < pm->len; ++i) {
    ereport(sfree(&pm->ptr[i].caption));
  }
  ereport(afree(pm));
}

NODISCARD error popup_menu_show(HWND parent, HINSTANCE hinstance, struct popup_menu *pm, int *selected) {
  WNDCLASSW wc = {
      .hInstance = hinstance,
      .lpszClassName = L"PSDToolKit_Dummy_Window",
      .lpfnWndProc = DefWindowProcW,
  };
  HWND dummy_window = 0;
  HMENU menu = 0;
  error err = eok();
  ATOM atom = RegisterClassW(&wc);
  if (!atom) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  dummy_window = CreateWindowExW(0,
                                 wc.lpszClassName,
                                 NULL,
                                 WS_OVERLAPPEDWINDOW,
                                 CW_USEDEFAULT,
                                 CW_USEDEFAULT,
                                 CW_USEDEFAULT,
                                 CW_USEDEFAULT,
                                 HWND_MESSAGE,
                                 0,
                                 wc.hInstance,
                                 NULL);
  if (!dummy_window) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  menu = CreatePopupMenu();
  if (!menu) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  for (size_t i = 0; i < pm->len; ++i) {
    struct popup_menu_item const *const pmi = pm->ptr + i;
    if (pmi->id) {
      if (!AppendMenuW(menu, MF_ENABLED | MF_STRING, (UINT_PTR)pmi->id, pmi->caption.ptr)) {
        err = errhr(HRESULT_FROM_WIN32(GetLastError()));
        goto cleanup;
      }
    } else {
      if (!AppendMenuW(menu, MF_ENABLED | MF_SEPARATOR, 0, NULL)) {
        err = errhr(HRESULT_FROM_WIN32(GetLastError()));
        goto cleanup;
      }
    }
  }
  DWORD const thid = GetCurrentThreadId();
  DWORD const ptkthid = GetWindowThreadProcessId(parent, NULL);
  AttachThreadInput(thid, ptkthid, TRUE);
  SetForegroundWindow(dummy_window);
  AttachThreadInput(thid, ptkthid, FALSE);
  POINT pt = {0};
  GetCursorPos(&pt);
  *selected = (int)(TrackPopupMenu(
      menu, TPM_TOPALIGN | TPM_LEFTALIGN | TPM_RETURNCMD | TPM_RIGHTBUTTON, pt.x, pt.y, 0, dummy_window, NULL));
cleanup:
  if (menu) {
    DestroyMenu(menu);
  }
  if (dummy_window) {
    DestroyWindow(dummy_window);
  }
  if (atom) {
    UnregisterClassW(wc.lpszClassName, wc.hInstance);
  }
  return err;
}
