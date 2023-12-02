#include "text_cache.h"

#include <ovthreads.h>

struct text_cache_item {
  struct text_cache c;
  struct timespec used_at;
};

static struct text_cache_item g_caches[8] = {0};

static bool a_is_old_than_b(struct timespec const *const a, struct timespec const *const b) {
  return a->tv_sec < b->tv_sec || (a->tv_sec == b->tv_sec && a->tv_nsec < b->tv_nsec);
}

struct text_cache *text_cache_get(uint64_t const hash) {
  size_t oldest = 0;
  for (size_t i = 0; i < sizeof(g_caches) / sizeof(g_caches[0]); ++i) {
    if (g_caches[i].c.hash == hash) {
      timespec_get(&g_caches[i].used_at, TIME_UTC);
      return &g_caches[i].c;
    }
    if (a_is_old_than_b(&g_caches[i].used_at, &g_caches[oldest].used_at)) {
      oldest = i;
    }
  }
  timespec_get(&g_caches[oldest].used_at, TIME_UTC);
  return &g_caches[oldest].c;
}

void text_cache_cleanup(void) {
  for (size_t i = 0; i < sizeof(g_caches) / sizeof(g_caches[0]); ++i) {
    if (g_caches[i].c.text.ptr) {
      eignore(sfree(&g_caches[i].c.text));
    }
  }
}
