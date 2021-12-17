#pragma once

#include "3rd/base.c/include/base.h"

NODISCARD error luastr_find_assign(struct wstr *s, struct wstr const *const name, size_t *const pos, size_t *const len);
NODISCARD error luastr_encode(struct wstr const *const src, struct wstr *dest);
NODISCARD error luastr_decode(struct wstr const *const src, struct wstr *dest);
