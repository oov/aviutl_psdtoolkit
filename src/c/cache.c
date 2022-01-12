#include "cache.h"

#include "ovthreads.h"

struct cache {
  struct hmap store;
  thrd_t thread;
  mtx_t mtx;
  struct cndvar cv;
};

static struct cache g_cache = {0};

struct cache_item {
  char *key;
  size_t key_len;
  void *value;
  size_t value_len;
  struct timespec *used_at;
};

struct gc_scan_data {
  struct cache *cache;
  struct timespec *t;
};

static bool gc_scan(void const *const item, void *const userdata) {
  enum {
    lifetime = 60,
  };
  struct gc_scan_data *const d = userdata;
  struct cache_item const *const ci = item;
  if (d->t->tv_sec >= ci->used_at->tv_sec + lifetime ||
      (d->t->tv_sec == ci->used_at->tv_sec + lifetime && d->t->tv_nsec >= ci->used_at->tv_nsec)) {
    struct cache_item *deleted = NULL;
    ereport(hmdelete(&d->cache->store, ci, &deleted));
    if (deleted) {
      ereport(mem_free(&deleted->key));
      ereport(mem_free(&deleted->value));
      ereport(mem_free(&deleted->used_at));
    }
  }
  return true;
}

static int gc(void *userdata) {
  enum {
    gc_interval = 30,
  };
  struct cache *const cache = userdata;
  struct timespec t = {0};
  timespec_get(&t, TIME_UTC);
  t.tv_sec += gc_interval;
  for (;;) {
    cndvar_lock(&cache->cv);
    t.tv_sec += gc_interval;
    int r = cndvar_timedwait_while(&cache->cv, 0, &t);
    cache->cv.var = 0;
    cndvar_unlock(&cache->cv);
    if (r == thrd_success) {
      return 0;
    }
    if (r == thrd_error) {
      return 1;
    }
    mtx_lock(&cache->mtx);
    ereport(hmscan(&cache->store,
                   gc_scan,
                   &((struct gc_scan_data){
                       .cache = cache,
                       .t = &t,
                   })));
    mtx_unlock(&cache->mtx);
  }
}

static void get_key(void const *const item, void const **const key, size_t *const key_bytes) {
  struct cache_item const *const ci = item;
  *key = ci->key;
  *key_bytes = ci->key_len;
}

NODISCARD error cache_init(void) {
  mtx_init(&g_cache.mtx, mtx_plain);
  cndvar_init(&g_cache.cv);
  error err = hmnewd(&g_cache.store, sizeof(struct cache_item), 1, get_key);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (thrd_create(&g_cache.thread, gc, &g_cache) != thrd_success) {
    err = errg(err_fail);
    goto cleanup;
  }
cleanup:
  if (efailed(err)) {
    ereport(hmfree(&g_cache.store));
    cndvar_exit(&g_cache.cv);
    mtx_destroy(&g_cache.mtx);
  }
  return err;
}

static bool cache_scan_free(void const *const item, void *const userdata) {
  (void)userdata;
  struct cache_item *const ci = ovbase_deconster_(item);
  ereport(mem_free(&ci->key));
  ci->key_len = 0;
  ereport(mem_free(&ci->value));
  ci->value_len = 0;
  ereport(mem_free(&ci->used_at));
  return true;
}

void cache_exit(void) {
  cndvar_lock(&g_cache.cv);
  cndvar_signal(&g_cache.cv, 1);
  cndvar_unlock(&g_cache.cv);
  thrd_join(g_cache.thread, NULL);
  mtx_lock(&g_cache.mtx);
  ereport(hmscan(&g_cache.store, cache_scan_free, NULL));
  ereport(hmfree(&g_cache.store));
  cndvar_exit(&g_cache.cv);
  mtx_destroy(&g_cache.mtx);
}

NODISCARD error cache_put(struct str const *const key, void *const value, size_t const value_len) {
  if (!key || !value) {
    return errg(err_invalid_arugment);
  }
  bool locked = false;
  struct cache_item *old_item = NULL;
  struct cache_item ci = {
      .key_len = key->len,
      .value = value,
      .value_len = value_len,
  };
  error err = mem(&ci.key, key->len, sizeof(char));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = mem(&ci.used_at, 1, sizeof(struct timespec));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  timespec_get(ci.used_at, TIME_UTC);
  memcpy(ci.key, key->ptr, key->len);
  mtx_lock(&g_cache.mtx);
  locked = true;
  err = hmset(&g_cache.store, &ci, &old_item);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  ci.key = NULL;
  ci.used_at = NULL;
cleanup:
  if (locked) {
    mtx_unlock(&g_cache.mtx);
  }
  ereport(mem_free(&ci.key));
  ereport(mem_free(&ci.used_at));
  if (old_item) {
    ereport(mem_free(&old_item->key));
    ereport(mem_free(&old_item->value));
    ereport(mem_free(&old_item->used_at));
  }
  return err;
}

NODISCARD error cache_get(struct str const *const key, void **const value, size_t *const value_len) {
  if (!key) {
    return errg(err_invalid_arugment);
  }
  if (!value || !value_len) {
    return errg(err_null_pointer);
  }
  bool locked = false;
  struct cache_item *ci = NULL;
  mtx_lock(&g_cache.mtx);
  locked = true;
  error err = hmget(&g_cache.store,
                    &((struct cache_item const){
                        .key = key->ptr,
                        .key_len = key->len,
                    }),
                    &ci);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (ci == NULL) {
    err = errg(err_not_found);
    goto cleanup;
  }
  *value = ci->value;
  *value_len = ci->value_len;
  timespec_get(ci->used_at, TIME_UTC);
cleanup:
  if (locked) {
    mtx_unlock(&g_cache.mtx);
  }
  return err;
}
