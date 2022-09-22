#pragma once

#include "ovbase.h"

NODISCARD error luastr_find_assign(struct wstr *s, struct wstr const *const name, size_t *const pos, size_t *const len);
NODISCARD error luastr_find_assign_number(struct wstr *s, struct wstr const *const name, int64_t *const value);
NODISCARD error luastr_encode(struct wstr const *const src, struct wstr *dest);
NODISCARD error luastr_decode(struct wstr const *const src, struct wstr *dest);
