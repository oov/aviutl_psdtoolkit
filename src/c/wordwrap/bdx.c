#include "bdx.h"

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

struct bdx_marker_context {
  struct line_reader const *lr;
  size_t nextpos;
  size_t **boundaries;
};

static char32_t get_char(void *const userdata) {
  struct bdx_marker_context *const bmctx = userdata;
  size_t const pos = line_reader_find_right(bmctx->lr, bmctx->nextpos);
  if (pos == SIZE_MAX) {
    return 0;
  }
  bmctx->nextpos = pos + 1;

#ifdef WW_DEBUG
  wchar_t buf[128];
  wsprintfW(buf, L"get_char: %lc", bmctx->lr->text->ptr[bmctx->lr->glyphs[pos].pos]);
  OutputDebugStringW(buf);
#endif

  return bmctx->lr->text->ptr[bmctx->lr->glyphs[pos].pos];
}

static bool add_boundary(size_t const boundary, void *const userdata) {
  struct bdx_marker_context *const bmctx = userdata;
  error err = OV_ARRAY_PUSH(bmctx->boundaries, boundary);
  if (efailed(err)) {
    ereport(err);
    return false;
  }
  return true;
}

/**
 * @brief Writes markers to text boundaries.
 *
 * @param lr Pointer to the line_reader.
 * @param model Pointer to the BudouX model.
 * @param boundaries Pointer to an array storing boundary positions.
 * @return Error code or eok() if successful.
 *
 * Parses the text using the BudouX model to find word boundaries and writes markers to lr->glyphs.
 * The boundaries array reuses memory to avoid frequent malloc/free calls.
 * Returns an error if memory allocation fails.
 */
NODISCARD error bdx_write_markers(struct line_reader const *const lr,
                                  struct budouxc *const model,
                                  size_t **const boundaries) {
  if (!lr || !model || !boundaries) {
    return errg(err_invalid_arugment);
  }
  OV_ARRAY_SET_LENGTH(*boundaries, 0);
  bool const r = budouxc_parse_boundaries_callback(model,
                                                   get_char,
                                                   add_boundary,
                                                   &(struct bdx_marker_context){
                                                       .lr = lr,
                                                       .nextpos = lr->linehead,
                                                       .boundaries = boundaries,
                                                   });
  if (!r) {
    return errg(err_out_of_memory);
  }

  // set a flag to indicate detection is complete.
  lr->glyphs[lr->linehead].flags |= gt_budoux_marked;

  size_t pos = lr->linehead - 1;
  size_t nch = 0;
  for (size_t i = 0; i < OV_ARRAY_LENGTH(*boundaries); ++i) {
    size_t const boundary = (*boundaries)[i];
    while (nch <= boundary) {
      pos = line_reader_find_right(lr, pos + 1);
      if (pos == SIZE_MAX) {
        break;
      }
      ++nch;
    }
    if (pos == SIZE_MAX) {
      break;
    }

#ifdef WW_DEBUG
    wchar_t buf[128];
    wsprintfW(buf, L"bdx boundary: %d pos: %d", boundary, pos);
    OutputDebugStringW(buf);
#endif
    lr->glyphs[pos].flags |= gt_budoux_breakable;
  }
  return eok();
}
