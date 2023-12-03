#include "bdx_cache.h"

#include <ovarray.h>
#include <ovthreads.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "../i18n.h"

struct model {
  char *name;
  struct budouxc *model;
  struct timespec used_at;
};

static struct model g_caches[4] = {0};

static bool a_is_old_than_b(struct timespec const *const a, struct timespec const *const b) {
  return a->tv_sec < b->tv_sec || (a->tv_sec == b->tv_sec && a->tv_nsec < b->tv_nsec);
}

NODISCARD error bdx_cache_get(char const *const name, struct budouxc **const model) {
  if (!name || !model) {
    return errg(err_invalid_arugment);
  }

  size_t oldest = 0;
  for (size_t i = 0; i < sizeof(g_caches) / sizeof(g_caches[0]); ++i) {
    if (g_caches[i].name && strcmp(g_caches[i].name, name) == 0) {
      timespec_get(&g_caches[i].used_at, TIME_UTC);
      *model = g_caches[i].model;
      return eok();
    }
    if (a_is_old_than_b(&g_caches[i].used_at, &g_caches[oldest].used_at)) {
      oldest = i;
    }
  }

  error err = eok();
  HANDLE h = INVALID_HANDLE_VALUE;
  char *json = NULL;

  if (g_caches[oldest].model) {
    budouxc_destroy(g_caches[oldest].model);
    g_caches[oldest].model = NULL;
  }

  char errormsg[128];
  if (strcmp(name, "ja") == 0) {
    g_caches[oldest].model = budouxc_init_embedded_ja(NULL, errormsg);
  } else if (strcmp(name, "zh_hans") == 0) {
    g_caches[oldest].model = budouxc_init_embedded_zh_hans(NULL, errormsg);
  } else if (strcmp(name, "zh_hant") == 0) {
    g_caches[oldest].model = budouxc_init_embedded_zh_hant(NULL, errormsg);
  } else {
    h = CreateFileA(name, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (h == INVALID_HANDLE_VALUE) {
      err =
          emsg_i18n(err_type_hresult, HRESULT_FROM_WIN32(GetLastError()), gettext("failed to open BudouX model file."));
      goto cleanup;
    }
    LARGE_INTEGER sz;
    if (!GetFileSizeEx(h, &sz)) {
      err = emsg_i18n(err_type_hresult, HRESULT_FROM_WIN32(GetLastError()), gettext("failed to get file size."));
      goto cleanup;
    }
    if (sz.HighPart != 0) {
      err = emsg_i18n(err_type_generic, err_unexpected, gettext("file size is too large."));
      goto cleanup;
    }
    err = mem(&json, sz.LowPart, 1);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (!ReadFile(h, json, sz.LowPart, &sz.LowPart, NULL)) {
      err = emsg_i18n(err_type_hresult, HRESULT_FROM_WIN32(GetLastError()), gettext("failed to read file."));
      goto cleanup;
    }
    g_caches[oldest].model = budouxc_init(NULL, json, sz.LowPart, errormsg);
  }
  if (!g_caches[oldest].model) {
    err = emsg_i18n(err_type_generic, err_fail, errormsg);
    goto cleanup;
  }
  err = OV_ARRAY_GROW(&g_caches[oldest].name, strlen(name) + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  strcpy(g_caches[oldest].name, name);
  timespec_get(&g_caches[oldest].used_at, TIME_UTC);
  *model = g_caches[oldest].model;
cleanup:
  if (h != INVALID_HANDLE_VALUE) {
    CloseHandle(h);
  }
  if (json) {
    ereport(mem_free(json));
  }
  return err;
}

void bdx_cache_cleanup(void) {
  for (size_t i = 0; i < sizeof(g_caches) / sizeof(g_caches[0]); ++i) {
    if (g_caches[i].name) {
      OV_ARRAY_DESTROY(&g_caches[i].name);
    }
    if (g_caches[i].model) {
      budouxc_destroy(g_caches[i].model);
      g_caches[i].model = NULL;
    }
  }
}
