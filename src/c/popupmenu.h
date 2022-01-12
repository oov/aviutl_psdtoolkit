#pragma once

#include "ovbase.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

struct popup_menu_item {
  struct wstr caption;
  int id;
};

struct popup_menu {
  struct popup_menu_item *ptr;
  size_t len;
  size_t cap;
};

void popup_menu_free(struct popup_menu *pm);
NODISCARD error popup_menu_show(HWND parent, HINSTANCE hinstance, struct popup_menu *pm, int *selected);
