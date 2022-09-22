#include "luastr.h"

#include "ovnum.h"

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

NODISCARD error luastr_find_assign_number(struct wstr *s, struct wstr const *const name, int64_t *const value) {
  struct wstr tmp = {0};
  ptrdiff_t start_pos = 0;
  ptrdiff_t phase = 0;
  size_t token_pos = 0;
  size_t token_len = 0;
  error err = sstr(s, name->ptr, &start_pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (start_pos == -1) {
    err = errg(err_not_found);
    goto cleanup;
  }
  for (size_t i = (size_t)start_pos + name->len, l = s->len; i < l; ++i) {
    wchar_t const c = s->ptr[i];
    switch (phase) {
    case 0: // finding L'='
      switch (c) {
      case L' ':
      case L'\t':
      case L'\r':
      case L'\n':
        continue;
      case L'=':
        ++phase;
        continue;
      default:
        err = errg(err_fail);
        goto cleanup;
      }
    case 1: // finding first number
      switch (c) {
      case L' ':
      case L'\t':
      case L'\r':
      case L'\n':
        continue;
      case L'0':
      case L'1':
      case L'2':
      case L'3':
      case L'4':
      case L'5':
      case L'6':
      case L'7':
      case L'8':
      case L'9':
      case L'-':
        token_pos = i;
        ++phase;
        continue;
      default:
        err = errg(err_fail);
        goto cleanup;
      }
    case 2: // finding last number
      switch (c) {
      case L'0':
      case L'1':
      case L'2':
      case L'3':
      case L'4':
      case L'5':
      case L'6':
      case L'7':
      case L'8':
      case L'9':
        continue;
      case L' ':
      case L'\t':
      case L'\r':
      case L'\n':
        token_len = i - token_pos;
        ++phase;
        goto parse;
      default:
        err = errg(err_fail);
        goto cleanup;
      }
    }
  }
  err = errg(err_not_found);
  goto cleanup;

parse:
  if (phase != 3 || token_len == 0) { // failsafe
    err = errg(err_fail);
    goto cleanup;
  }
  err = sncpy(&tmp, s->ptr + token_pos, token_len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  int64_t v = 0;
  if (!ov_atoi(tmp.ptr, &v, true)) {
    err = errg(err_fail);
    goto cleanup;
  }
  *value = v;

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
    case 0x5c:
      c = 0x5c;
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
    case 0x5c:
      c = 0x5c;
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
