#include "luastr.h"

NODISCARD error luastr_find_assign(struct wstr *s,
                                   struct wstr const *const name,
                                   size_t *const pos,
                                   size_t *const len) {
  struct wstr tmp = {0};
  ptrdiff_t start_pos = 0;
  error err = scpym(&tmp, name->ptr, L"=\"");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sstr(s, tmp.ptr, &start_pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (start_pos == -1) {
    err = errg(err_not_found);
    goto cleanup;
  }
  for (size_t i = (size_t)start_pos + tmp.len, l = s->len; i < l; ++i) {
    if (s->ptr[i] == L'"' && s->ptr[i - 1] != L'\\') {
      *pos = (size_t)start_pos;
      *len = i + 1 - (size_t)start_pos;
      goto cleanup;
    }
  }
  err = errg(err_not_found);
cleanup:
  ereport(sfree(&tmp));
  return err;
}

NODISCARD error luastr_encode(struct wstr const *const src, struct wstr *dest) {
  if (!src || !src->ptr) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  struct wstr tmp = {0};
  size_t cap = src->len + 3;
  error err = sgrow(&tmp, cap);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp.ptr[tmp.len++] = L'"';
  wchar_t c = 0;
  for (size_t i = 0; i < src->len; ++i) {
    switch (src->ptr[i]) {
    case 0x07:
      c = 'a';
      break;
    case 0x08:
      c = 'b';
      break;
    case 0x09:
      c = 't';
      break;
    case 0x0a:
      c = 'n';
      break;
    case 0x0b:
      c = 'v';
      break;
    case 0x0c:
      c = 'f';
      break;
    case 0x0d:
      c = 'r';
      break;
    case 0x22:
      c = 0x22;
      break;
    case 0x27:
      c = 0x27;
      break;
    case 0x5b:
      c = 0x5b;
      break;
    case 0x5c:
      c = 0x5c;
      break;
    case 0x5d:
      c = 0x5d;
      break;
    default:
      tmp.ptr[tmp.len++] = src->ptr[i];
      continue;
    }
    err = sgrow(&tmp, ++cap);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    tmp.ptr[tmp.len++] = L'\\';
    tmp.ptr[tmp.len++] = c;
  }
  tmp.ptr[tmp.len++] = L'"';
  tmp.ptr[tmp.len++] = L'\0';
  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

NODISCARD error luastr_decode(struct wstr const *const src, struct wstr *dest) {
  if (!src || !src->ptr || !src->len) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  size_t const len = src->len - 1;
  if (src->ptr[0] != L'"' || src->ptr[len] != L'"') {
    return errg(err_unexpected);
  }
  struct wstr tmp = {0};
  size_t cap = src->len - 2 + 1;
  error err = sgrow(&tmp, cap);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  wchar_t c = 0;
  for (size_t i = 1; i < len; ++i) {
    if (src->ptr[i] != L'\\') {
      tmp.ptr[tmp.len++] = src->ptr[i];
      continue;
    }
    switch (src->ptr[i + 1]) {
    case L'a':
      c = 0x07;
      break;
    case L'b':
      c = 0x08;
      break;
    case L't':
      c = 0x09;
      break;
    case L'n':
      c = 0x0a;
      break;
    case L'v':
      c = 0x0b;
      break;
    case L'f':
      c = 0x0c;
      break;
    case L'r':
      c = 0x0d;
      break;
    case 0x22:
      c = 0x22;
      break;
    case 0x27:
      c = 0x27;
      break;
    case 0x5b:
      c = 0x5b;
      break;
    case 0x5c:
      c = 0x5c;
      break;
    case 0x5d:
      c = 0x5d;
      break;
    default:
      tmp.ptr[tmp.len++] = L'\\';
      continue;
    }
    tmp.ptr[tmp.len++] = c;
    ++i;
  }
  tmp.ptr[tmp.len++] = L'\0';
  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}
