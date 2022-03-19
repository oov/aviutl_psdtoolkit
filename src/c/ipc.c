#include "ipc.h"

#include "ovthreads.h"
#include "ovutil/str.h"
#include "ovutil/win32.h"

#include "error_ptk.h"

#ifndef NDEBUG
static void ods(wchar_t const *fmt, ...) {
  wchar_t s[1024], f[1024];
  va_list p;
  va_start(p, fmt);
  f[0] = L'\0';
  lstrcatW(f, L"ptk: ");
  lstrcatW(f, fmt);
  wvsprintfW(s, f, p);
  OutputDebugStringW(s);
  va_end(p);
}
#endif

struct ipc {
  struct ipc_options options;

  HANDLE process;
  HANDLE pipe_in_w;
  HANDLE pipe_out_r;

  thrd_t thread;

  mtx_t mtx;
  struct cndvar c2s, s2c;
  error reply;
  bool thread_running;
  char reserved[3];
};

NODISCARD error ipc_init(struct ipc **const ipc, struct ipc_options const *const opt) {
  if (!opt || !opt->cmdline.ptr || !opt->cmdline.len) {
    return errg(err_invalid_arugment);
  }
  if (!ipc) {
    return errg(err_null_pointer);
  }
  error err = mem(ipc, 1, sizeof(struct ipc));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  **ipc = (struct ipc){
      .options =
          {
              .userdata = opt->userdata,
              .on_update_editing_image_state = opt->on_update_editing_image_state,
              .on_export_faview_slider = opt->on_export_faview_slider,
              .on_export_layer_names = opt->on_export_layer_names,
          },
      .process = INVALID_HANDLE_VALUE,
      .pipe_in_w = INVALID_HANDLE_VALUE,
      .pipe_out_r = INVALID_HANDLE_VALUE,
  };
  err = scpy(&(*ipc)->options.cmdline, opt->cmdline.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  mtx_init(&(*ipc)->mtx, mtx_plain | mtx_recursive);
  cndvar_init(&(*ipc)->c2s);
  cndvar_init(&(*ipc)->s2c);

cleanup:
  if (efailed(err)) {
    ereport(sfree(&(*ipc)->options.cmdline));
    ereport(mem_free(ipc));
  }
  return err;
}

NODISCARD static error
build_environment_strings(struct wstr const *const key, struct wstr const *const value, struct wstr *const dest) {
  if (!dest) {
    return errg(err_null_pointer);
  }
  error err = eok();
  struct wstr tmp = {0};
  wchar_t *envstr = GetEnvironmentStringsW();
  if (!envstr) {
    err = emsg(err_type_generic, err_fail, &native_unmanaged_const(NSTR("GetEnvironmentStringsW failed.")));
    goto cleanup;
  }

  // copy original data to tmp but skip same key=value data.
  for (wchar_t const *src = envstr; *src != L'\0';) {
    size_t const l = wcslen(src);
    if (key && key->len && l >= key->len + 1 && wcsncmp(src, key->ptr, key->len) == 0 && src[key->len] == L'=') {
      src += l + 1;
      continue;
    }
    err = sgrow(&tmp, tmp.len + l + 1);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    memcpy(tmp.ptr + tmp.len, src, (l + 1) * sizeof(wchar_t));
    tmp.len += l + 1;
    src += l + 1;
  }
  // append key=value
  if (key && key->len) {
    size_t const l = key->len + 1 + (value ? value->len : 0);
    err = sgrow(&tmp, tmp.len + l + 1);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    memcpy(tmp.ptr + tmp.len, key->ptr, key->len * sizeof(wchar_t));
    tmp.len += key->len;
    tmp.ptr[tmp.len++] = L'=';
    if (value && value->len) {
      memcpy(tmp.ptr + tmp.len, value->ptr, value->len * sizeof(wchar_t));
      tmp.len += value->len;
    }
    tmp.ptr[tmp.len++] = L'\0';
  }
  // append endmark
  err = sgrow(&tmp, tmp.len + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp.ptr[tmp.len++] = L'\0';

  err = sgrow(dest, tmp.len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  memcpy(dest->ptr, tmp.ptr, tmp.len * sizeof(wchar_t));

cleanup:
  if (envstr) {
    if (!FreeEnvironmentStringsW(envstr)) {
      ereport(errhr(HRESULT_FROM_WIN32(GetLastError())));
    }
    envstr = NULL;
  }
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error get_working_directory(struct wstr const *const cmdline, struct wstr *const dir) {
  if (!cmdline || !cmdline->ptr || !cmdline->len) {
    return errg(err_invalid_arugment);
  }
  if (!dir) {
    return errg(err_null_pointer);
  }
  struct wstr tmp = {0};
  struct wstr tmp2 = {0};
  size_t pathlen = 0;
  error err = eok();
  if (cmdline->ptr[0] == L'"') {
    ++pathlen;
    while (pathlen < cmdline->len && cmdline->ptr[pathlen] != L'"') {
      ++pathlen;
    }
    err = sncpy(&tmp, cmdline->ptr + 1, pathlen - 1);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  } else {
    while (pathlen < cmdline->len && cmdline->ptr[pathlen] != L' ') {
      ++pathlen;
    }
    err = sncpy(&tmp, cmdline->ptr, pathlen);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  DWORD const dirlen = GetFullPathNameW(tmp.ptr, 0, NULL, NULL);
  if (dirlen == 0) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  err = sgrow(&tmp2, dirlen);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  wchar_t *fn = NULL;
  if (GetFullPathNameW(tmp.ptr, dirlen, tmp2.ptr, &fn) == 0 || fn == NULL) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  *fn = '\0';
  err = scpy(dir, tmp2.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp2));
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error read(struct ipc *const ipc, void *const buf, DWORD sz) {
  char *b = buf;
  for (DWORD read = 0; sz > 0; b += read, sz -= read) {
    if (!ReadFile(ipc->pipe_out_r, b, sz, &read, NULL)) {
      return errhr(HRESULT_FROM_WIN32(GetLastError()));
    }
  }
  return eok();
}

NODISCARD static error read_string_utf8(struct ipc *const ipc, struct str *const dest) {
  uint32_t sz = 0;
  error err = read(ipc, &sz, sizeof(sz));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sgrow(dest, sz + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read(ipc, dest->ptr, sz);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  dest->len = (size_t)sz;
cleanup:
  return err;
}

#define FOURCC(c0, c1, c2, c3) ((uint32_t)(((c3) << 24) | ((c2) << 16) | ((c1) << 8) | (c0)))

NODISCARD static error write(struct ipc *const ipc, void const *const buf, DWORD sz) {
  char const *b = buf;
  for (DWORD written = 0; sz > 0; b += written, sz -= written) {
    if (!WriteFile(ipc->pipe_in_w, b, sz, &written, NULL)) {
      return errhr(HRESULT_FROM_WIN32(GetLastError()));
    }
  }
  return eok();
}

NODISCARD static error write_string_utf8(struct ipc *const ipc, struct str const *const s) {
  error err = write(ipc, &(uint32_t){s->len}, sizeof(uint32_t));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write(ipc, s->ptr, s->len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD static error read_request_update_editing_image_state(struct ipc *const ipc) {
  struct ipc_update_editing_image_state_params params = {0};
  error err = read_string_utf8(ipc, &params.file_path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_string_utf8(ipc, &params.state_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (ipc->options.on_update_editing_image_state) {
    ipc->options.on_update_editing_image_state(ipc->options.userdata, &params);
  }
  uint32_t const r = 0x80000000;
  err = write(ipc, &r, sizeof(r));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&params.state_utf8));
  ereport(sfree(&params.file_path_utf8));
  return err;
}

NODISCARD static error read_request_export_faview_slider(struct ipc *const ipc) {
  struct ipc_export_faview_slider_params params = {0};
  error err = read_string_utf8(ipc, &params.file_path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_string_utf8(ipc, &params.slider_name_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_string_utf8(ipc, &params.names_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_string_utf8(ipc, &params.values_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read(ipc, &params.selected_index, sizeof(params.selected_index));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (ipc->options.on_export_faview_slider) {
    ipc->options.on_export_faview_slider(ipc->options.userdata, &params);
  }
  uint32_t const r = 0x80000000;
  err = write(ipc, &r, sizeof(r));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&params.values_utf8));
  ereport(sfree(&params.names_utf8));
  ereport(sfree(&params.slider_name_utf8));
  ereport(sfree(&params.file_path_utf8));
  return err;
}

NODISCARD static error read_request_export_layer_names(struct ipc *const ipc) {
  struct ipc_export_layer_names_params params = {0};
  error err = read_string_utf8(ipc, &params.file_path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_string_utf8(ipc, &params.names_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_string_utf8(ipc, &params.values_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read(ipc, &params.selected_index, sizeof(params.selected_index));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (ipc->options.on_export_layer_names) {
    ipc->options.on_export_layer_names(ipc->options.userdata, &params);
  }
  uint32_t const r = 0x80000000;
  err = write(ipc, &r, sizeof(r));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&params.values_utf8));
  ereport(sfree(&params.names_utf8));
  ereport(sfree(&params.file_path_utf8));
  return err;
}

NODISCARD static error read_request_unknown_command(struct ipc *const ipc) {
  struct str const result = str_unmanaged_const("invalid request command");
  uint32_t const r = (uint32_t)(result.len | 0x80000000);
  error err = write(ipc, &r, sizeof(r));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write(ipc, result.ptr, result.len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD static error read_request(struct ipc *const ipc, uint32_t id) {
  enum request_cmdid {
    request_update_editing_image_state = FOURCC('E', 'D', 'I', 'S'),
    request_export_faview_slider = FOURCC('E', 'X', 'F', 'S'),
    request_export_layer_names = FOURCC('E', 'X', 'L', 'N'),
  };
  error err = eok();
  switch (id) {
  case request_update_editing_image_state:
    err = read_request_update_editing_image_state(ipc);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  case request_export_faview_slider:
    err = read_request_export_faview_slider(ipc);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  case request_export_layer_names:
    err = read_request_export_layer_names(ipc);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  default:
    err = read_request_unknown_command(ipc);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
cleanup:
  return err;
}

NODISCARD static error read_reply(struct ipc *const ipc, uint32_t size) {
  struct str tmp = {0};
  struct wstr tmp2 = {0};
  error err = eok(), rep = eok();
  if (!size) {
    goto cleanup;
  }
  err = sgrow(&tmp, size + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read(ipc, tmp.ptr, size);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp.len = (size_t)size;
  err = from_utf8(&tmp, &tmp2);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  rep = error_add_(
      NULL, err_type_generic, err_fail, &tmp2, &(struct ov_filepos){.file = "remote process", .func = __func__});
  tmp2.cap = 0;
cleanup:
  cndvar_lock(&ipc->s2c);
  ipc->reply = rep;
  cndvar_signal(&ipc->s2c, 1);
  cndvar_unlock(&ipc->s2c);
  cndvar_lock(&ipc->c2s);
  cndvar_wait_while(&ipc->c2s, 0);
  ipc->c2s.var = 0;
  cndvar_unlock(&ipc->c2s);
  mtx_unlock(&ipc->mtx);
  ereport(sfree(&tmp2));
  ereport(sfree(&tmp));
  return err;
}

static int read_worker(void *userdata) {
  struct ipc *const ipc = userdata;
  error err = eok();
  while (1) {
    uint32_t sz = 0;
    err = read(ipc, &sz, sizeof(sz));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (!(sz & 0x80000000)) {
#ifndef NDEBUG
      ods(L"S request %c%c%c%c", sz & 0xff, (sz >> 8) & 0xff, (sz >> 16) & 0xff, (sz >> 24) & 0xff);
#endif
      err = read_request(ipc, sz);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      continue;
    }
    err = read_reply(ipc, sz & 0x7fffffff);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }

cleanup:
  if (efailed(err)) {
    if (eis_hr(err, HRESULT_FROM_WIN32(ERROR_BROKEN_PIPE))) {
      efree(&err);
    } else {
      ereport(err);
    }
    return 1;
  }
  return 0;
}

NODISCARD static error start_process(struct ipc *ipc) {
  if (!ipc) {
    return errg(err_invalid_arugment);
  }
  if (ipc->process != INVALID_HANDLE_VALUE || ipc->pipe_in_w != INVALID_HANDLE_VALUE ||
      ipc->pipe_out_r != INVALID_HANDLE_VALUE) {
    return errg(err_unexpected);
  }
  struct wstr envvar = {0};
  struct wstr path = {0};
  struct wstr dir = {0};
  HANDLE in_r = INVALID_HANDLE_VALUE;
  HANDLE in_w = INVALID_HANDLE_VALUE;
  HANDLE out_r = INVALID_HANDLE_VALUE;
  HANDLE out_w = INVALID_HANDLE_VALUE;
  HANDLE err_r = INVALID_HANDLE_VALUE;
  HANDLE err_w = INVALID_HANDLE_VALUE;
  PROCESS_INFORMATION pi = {
      .hProcess = INVALID_HANDLE_VALUE,
      .hThread = INVALID_HANDLE_VALUE,
  };
  error err = eok();

  SECURITY_ATTRIBUTES sa = {
      .nLength = sizeof(SECURITY_ATTRIBUTES),
      .bInheritHandle = TRUE,
  };
  if (!CreatePipe(&in_r, &in_w, &sa, 0)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  if (!SetHandleInformation(in_w, HANDLE_FLAG_INHERIT, 0)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  if (!CreatePipe(&out_r, &out_w, &sa, 0)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  if (!SetHandleInformation(out_r, HANDLE_FLAG_INHERIT, 0)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  if (!CreatePipe(&err_r, &err_w, &sa, 0)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  if (!SetHandleInformation(err_r, HANDLE_FLAG_INHERIT, 0)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }

  err = build_environment_strings(&wstr_unmanaged_const(L""), &wstr_unmanaged_const(L""), &envvar);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  // have to copy this buffer because CreateProcessW may modify path string.
  err = scpy(&path, ipc->options.cmdline.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = get_working_directory(&path, &dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  STARTUPINFOW si = {
      .cb = sizeof(STARTUPINFOW),
      .dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW,
      .wShowWindow = SW_SHOWDEFAULT,
      .hStdInput = in_r,
      .hStdOutput = out_w,
      .hStdError = err_w,
  };
  if (!CreateProcessW(NULL,
                      path.ptr,
                      NULL,
                      NULL,
                      TRUE,
                      CREATE_NO_WINDOW | CREATE_UNICODE_ENVIRONMENT,
                      envvar.ptr,
                      dir.ptr,
                      &si,
                      &pi)) {
    HRESULT hr = HRESULT_FROM_WIN32(GetLastError());
    if (hr == HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND)) {
      err = err(err_type_ptk, err_ptk_ipc_target_not_found);
    } else if (hr == HRESULT_FROM_WIN32(ERROR_ACCESS_DENIED)) {
      err = err(err_type_ptk, err_ptk_ipc_target_access_denied);
    } else {
      err = emsg(err_type_hresult, (int)hr, &native_unmanaged_const(NSTR("外部プロセスの起動に失敗しました。")));
    }
    goto cleanup;
  }

  ipc->process = pi.hProcess;
  ipc->pipe_in_w = in_w;
  ipc->pipe_out_r = out_r;

  if (thrd_create(&ipc->thread, read_worker, ipc) != thrd_success) {
    err = errg(err_fail);
    goto cleanup;
  }
  ipc->thread_running = true;

cleanup:
  if (efailed(err)) {
    ipc->process = INVALID_HANDLE_VALUE;
    ipc->pipe_in_w = INVALID_HANDLE_VALUE;
    ipc->pipe_out_r = INVALID_HANDLE_VALUE;
  } else {
    pi.hProcess = INVALID_HANDLE_VALUE;
    in_w = INVALID_HANDLE_VALUE;
    out_r = INVALID_HANDLE_VALUE;
  }
  if (pi.hThread != INVALID_HANDLE_VALUE) {
    CloseHandle(pi.hThread);
    pi.hThread = INVALID_HANDLE_VALUE;
  }
  if (pi.hProcess != INVALID_HANDLE_VALUE) {
    CloseHandle(pi.hProcess);
    pi.hProcess = INVALID_HANDLE_VALUE;
  }
  if (err_r != INVALID_HANDLE_VALUE) {
    CloseHandle(err_r);
    err_r = INVALID_HANDLE_VALUE;
  }
  if (err_w != INVALID_HANDLE_VALUE) {
    CloseHandle(err_w);
    err_w = INVALID_HANDLE_VALUE;
  }
  if (out_r != INVALID_HANDLE_VALUE) {
    CloseHandle(out_r);
    out_r = INVALID_HANDLE_VALUE;
  }
  if (out_w != INVALID_HANDLE_VALUE) {
    CloseHandle(out_w);
    out_w = INVALID_HANDLE_VALUE;
  }
  if (in_r != INVALID_HANDLE_VALUE) {
    CloseHandle(in_r);
    in_r = INVALID_HANDLE_VALUE;
  }
  if (in_w != INVALID_HANDLE_VALUE) {
    CloseHandle(in_w);
    in_w = INVALID_HANDLE_VALUE;
  }
  ereport(sfree(&dir));
  ereport(sfree(&path));
  ereport(sfree(&envvar));
  return err;
}

static bool is_process_running(struct ipc const *const ipc) {
  return ipc && ipc->process != INVALID_HANDLE_VALUE && WaitForSingleObject(ipc->process, 0) == WAIT_TIMEOUT;
}

static void close_process(struct ipc *const ipc) {
  if (ipc->pipe_in_w != INVALID_HANDLE_VALUE) {
    CloseHandle(ipc->pipe_in_w);
    ipc->pipe_in_w = INVALID_HANDLE_VALUE;
  }
  if (ipc->pipe_out_r != INVALID_HANDLE_VALUE) {
    CloseHandle(ipc->pipe_out_r);
    ipc->pipe_out_r = INVALID_HANDLE_VALUE;
  }
  if (ipc->process != INVALID_HANDLE_VALUE) {
    CloseHandle(ipc->process);
    ipc->process = INVALID_HANDLE_VALUE;
  }
  if (ipc->thread_running) {
    thrd_join(ipc->thread, NULL);
    ipc->thread_running = false;
  }
}

void ipc_exit(struct ipc **const ipc) {
  if (is_process_running(*ipc)) {
    close_process(*ipc);
  }
  cndvar_exit(&(*ipc)->s2c);
  cndvar_exit(&(*ipc)->c2s);
  mtx_destroy(&(*ipc)->mtx);
  ereport(sfree(&(*ipc)->options.cmdline));
  ereport(mem_free(ipc));
}

NODISCARD static error start_command_process(struct ipc *const ipc, uint32_t const msgid) {
  mtx_lock(&ipc->mtx);
  error err = eok();
  if (!is_process_running(ipc)) {
    close_process(ipc);
    err = start_process(ipc);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
#ifndef NDEBUG
  ods(L"send %c%c%c%c", msgid & 0xff, (msgid >> 8) & 0xff, (msgid >> 16) & 0xff, (msgid >> 24) & 0xff);
#endif
  err = write(ipc, &msgid, sizeof(msgid));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD static error get_reply(struct ipc *const ipc) {
  cndvar_lock(&ipc->s2c);
  cndvar_wait_while(&ipc->s2c, 0);
  ipc->s2c.var = 0;
  error err = ipc->reply;
  ipc->reply = eok();
  cndvar_unlock(&ipc->s2c);
  return err;
}

NODISCARD static error finish_command_process(struct ipc *const ipc, error e) {
  cndvar_lock(&ipc->c2s);
  cndvar_signal(&ipc->c2s, 1);
  cndvar_unlock(&ipc->c2s);
  mtx_unlock(&ipc->mtx);
  return e;
}

NODISCARD error ipc_add_file(struct ipc *const ipc, struct str const *const path_utf8, uint32_t const tag) {
  error err = start_command_process(ipc, FOURCC('A', 'D', 'D', 'F'));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_string_utf8(ipc, path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write(ipc, &tag, sizeof(tag));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_reply(ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return finish_command_process(ipc, err);
}

NODISCARD error ipc_clear_files(struct ipc *const ipc) {
  error err = start_command_process(ipc, FOURCC('C', 'L', 'R', 'F'));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_reply(ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return finish_command_process(ipc, err);
}

NODISCARD error ipc_deserialize(struct ipc *const ipc, struct str const *const src_utf8) {
  error err = start_command_process(ipc, FOURCC('D', 'S', 'L', 'Z'));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_string_utf8(ipc, src_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_reply(ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  uint32_t r = 0;
  err = read(ipc, &r, sizeof(r));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return finish_command_process(ipc, err);
}

NODISCARD error ipc_draw(struct ipc *const ipc,
                         int32_t const id,
                         struct str const *const path_utf8,
                         void *const p,
                         int32_t const width,
                         int32_t const height) {
  error err = start_command_process(ipc, FOURCC('D', 'R', 'A', 'W'));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write(ipc, &id, sizeof(id));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_string_utf8(ipc, path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write(ipc, &width, sizeof(width));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write(ipc, &height, sizeof(height));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_reply(ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  uint32_t sz = 0;
  err = read(ipc, &sz, sizeof(sz));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (sz > (DWORD)(width * 4 * height)) {
    err = errg(err_not_sufficient_buffer);
    goto cleanup;
  }
  err = read(ipc, p, sz);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return finish_command_process(ipc, err);
}

NODISCARD error ipc_get_layer_names(struct ipc *const ipc,
                                    int32_t const id,
                                    struct str const *const path_utf8,
                                    struct str *const dest_utf8) {
  error err = start_command_process(ipc, FOURCC('L', 'N', 'A', 'M'));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write(ipc, &id, sizeof(id));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_string_utf8(ipc, path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_reply(ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_string_utf8(ipc, dest_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return finish_command_process(ipc, err);
}

NODISCARD error ipc_serialize(struct ipc *const ipc, struct str *const dest_utf8) {
  error err = start_command_process(ipc, FOURCC('S', 'R', 'L', 'Z'));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_reply(ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_string_utf8(ipc, dest_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return finish_command_process(ipc, err);
}

NODISCARD error ipc_set_properties(struct ipc *const ipc,
                                   int32_t const id,
                                   struct str const *const path_utf8,
                                   struct ipc_set_properties_options *options,
                                   struct ipc_set_properties_results *rets) {
  error err = start_command_process(ipc, FOURCC('P', 'R', 'O', 'P'));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write(ipc, &id, sizeof(id));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_string_utf8(ipc, path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  enum {
    propid_endmark = 0,
    propid_layer = 1,
    propid_scale = 2,
    propid_offset_x = 3,
    propid_offset_y = 4,
    propid_tag = 5,
  };
  if (options->tag) {
    err = write(ipc, &(int32_t){propid_tag}, sizeof(int32_t));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = write(ipc, options->tag, sizeof(uint32_t));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  if (options->layer_utf8) {
    err = write(ipc, &(int32_t){propid_layer}, sizeof(int32_t));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = write_string_utf8(ipc, options->layer_utf8);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  if (options->scale) {
    err = write(ipc, &(int32_t){propid_scale}, sizeof(int32_t));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = write(ipc, options->scale, sizeof(float));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  if (options->offset_x) {
    err = write(ipc, &(int32_t){propid_offset_x}, sizeof(int32_t));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = write(ipc, options->offset_x, sizeof(int32_t));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  if (options->offset_y) {
    err = write(ipc, &(int32_t){propid_offset_y}, sizeof(int32_t));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = write(ipc, options->offset_y, sizeof(int32_t));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = write(ipc, &(int32_t){propid_endmark}, sizeof(int32_t));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_reply(ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  int32_t modifiled = 0, width = 0, height = 0;
  uint32_t cache_key = 0;
  err = read(ipc, &modifiled, sizeof(int32_t));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read(ipc, &cache_key, sizeof(uint32_t));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read(ipc, &width, sizeof(int32_t));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read(ipc, &height, sizeof(int32_t));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *rets = (struct ipc_set_properties_results){
      .modifiled = modifiled != 0,
      .cache_key = cache_key,
      .width = width,
      .height = height,
  };
cleanup:
  return finish_command_process(ipc, err);
}

NODISCARD error ipc_show_gui(struct ipc *const ipc, uint64_t *const window) {
  error err = start_command_process(ipc, FOURCC('S', 'G', 'U', 'I'));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_reply(ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read(ipc, window, sizeof(uint64_t));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return finish_command_process(ipc, err);
}

NODISCARD error ipc_update_current_project_path(struct ipc *const ipc, struct str const *const path_utf8) {
  error err = start_command_process(ipc, FOURCC('U', 'P', 'D', 'P'));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = write_string_utf8(ipc, path_utf8);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_reply(ipc);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return finish_command_process(ipc, err);
}
