#include "speak.h"

#include <math.h>

#include "ovthreads.h"

enum {
  audio_frequency = 24000,
  audio_channels = 1,
  audio_rdft_size = 256,
};

struct item {
  void *h;
  struct str filepath;
  struct timespec used_at;
};

struct speak {
  mtx_t mtx;
  thrd_t thread;
  struct cndvar cv;
  struct speak_callbacks cbs;
  struct item items[8];
  int16_t *buf;
  float *fft_buf;
  float *fft_wnd;
  int *fft_iw;
  float *fft_fw;
};

void rdft(int n, int isgn, float *a, int *ip, float *w);

static void close(struct speak *spk, size_t idx) {
  if (!spk || idx >= sizeof(spk->items) / sizeof(spk->items[0])) {
    return;
  }
  struct item *const item = spk->items + idx;
  if (item->h) {
    spk->cbs.close(spk->cbs.userdata, item->h);
    item->h = NULL;
  }
  if (item->filepath.ptr != NULL) {
    item->filepath.ptr[0] = L'\0';
    item->filepath.len = 0;
  }
}

NODISCARD static error open(struct speak *const spk, struct str const *const filepath, size_t *const idx) {
  if (!spk || !filepath || !filepath->ptr) {
    return errg(err_invalid_arugment);
  }
  if (!idx) {
    return errg(err_null_pointer);
  }
  void *h = NULL;
  error err = eok();
  size_t const n = sizeof(spk->items) / sizeof(spk->items[0]);
  size_t unused = n, oldest = n;
  for (size_t i = 0; i < n; ++i) {
    struct item *item = spk->items + i;
    if (item->h == NULL) {
      if (unused == n) {
        unused = i;
      }
      continue;
    }
    if (strcmp(item->filepath.ptr, filepath->ptr) != 0) {
      struct item const *const o = spk->items + oldest;
      if (oldest == n || o->used_at.tv_sec > item->used_at.tv_sec ||
          (o->used_at.tv_sec == item->used_at.tv_sec && o->used_at.tv_nsec > item->used_at.tv_nsec)) {
        oldest = i;
      }
      continue;
    }
    // found
    *idx = i;
    goto cleanup;
  }

  err = spk->cbs.open(spk->cbs.userdata, filepath, audio_frequency, audio_channels, &h);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  // need to free oldest one before set.
  if (unused == n) {
    close(spk, oldest);
    unused = oldest;
  }

  struct item *item = spk->items + unused;
  err = scpy(&item->filepath, filepath->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (timespec_get(&item->used_at, TIME_UTC) != TIME_UTC) {
    err = errg(err_fail);
    goto cleanup;
  }
  item->h = h;
  h = NULL;

cleanup:
  if (h) {
    spk->cbs.close(spk->cbs.userdata, h);
    h = NULL;
  }
  return err;
}

NODISCARD static error read(struct speak *const spk,
                            struct str const *const filepath,
                            float const pos,
                            float const low_cut,
                            float const high_cut,
                            float *const level) {
  size_t idx = 0;
  error err = open(spk, filepath, &idx);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  struct item *const item = spk->items + idx;
  size_t const sample_pos = (size_t)(truncf(pos * audio_frequency));
  int16_t *const buf = spk->buf;
  size_t samples = 0;
  err = spk->cbs.read(spk->cbs.userdata, item->h, sample_pos, audio_rdft_size, buf, &samples);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (timespec_get(&item->used_at, TIME_UTC) != TIME_UTC) {
    err = errg(err_unexpected);
    goto cleanup;
  }
  float *const fft_buf = spk->fft_buf;
  float *const fft_wnd = spk->fft_wnd;
  for (size_t i = 0; i < samples; ++i) {
    fft_buf[i] = (float)(buf[i]) * fft_wnd[i];
  }
  for (size_t i = samples; i < audio_rdft_size; ++i) {
    fft_buf[i] = 0.f;
  }
  rdft(audio_rdft_size, 1, fft_buf, spk->fft_iw, spk->fft_fw);
  float const d = audio_frequency / audio_rdft_size;
  float s = 0, n = 0, x = 0, y = 0, hz = 0;
  for (size_t i = 0, hf = audio_rdft_size / 2; i < hf; ++i) {
    hz = (float)(i)*d;
    if (hz < low_cut) {
      continue;
    }
    if (high_cut < hz) {
      break;
    }
    x = fft_buf[i * 2];
    y = fft_buf[i * 2 + 1];
    s += sqrtf(x * x + y * y);
    n += 1;
  }
  *level = s / n;
cleanup:
  return err;
}

static int gc(void *const userdata) {
  enum {
    gc_interval = 1,
    lifetime = 2,
  };
  struct speak *const spk = userdata;
  struct timespec t = {0};
  timespec_get(&t, TIME_UTC);
  t.tv_sec += gc_interval;
  for (;;) {
    cndvar_lock(&spk->cv);
    t.tv_sec += gc_interval;
    int r = cndvar_timedwait_while(&spk->cv, 0, &t);
    spk->cv.var = 0;
    cndvar_unlock(&spk->cv);
    if (r == thrd_success) {
      return 0;
    }
    if (r == thrd_error) {
      return 1;
    }
    mtx_lock(&spk->mtx);
    size_t const n = sizeof(spk->items) / sizeof(spk->items[0]);
    for (size_t i = 0; i < n; ++i) {
      struct item *const item = spk->items + i;
      if (!item->h) {
        continue;
      }
      if (t.tv_sec >= item->used_at.tv_sec + lifetime ||
          (t.tv_sec == item->used_at.tv_sec + lifetime && t.tv_nsec >= item->used_at.tv_nsec)) {
        close(spk, i);
      }
    }
    mtx_unlock(&spk->mtx);
  }
}

NODISCARD error speak_init(struct speak_callbacks const *const cb, struct speak **const dest) {
  if (!cb || !cb->open || !cb->read || !cb->close || !dest) {
    return errg(err_invalid_arugment);
  }
  if (*dest != NULL) {
    return errg(err_unexpected);
  }
  int16_t *buf = NULL;
  float *fft_buf = NULL, *fft_wnd = NULL, *fft_fw = NULL;
  int *fft_iw = NULL;
  error err = mem(&fft_buf, (audio_rdft_size * 5) / 2, sizeof(float));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  fft_wnd = fft_buf + audio_rdft_size;
  fft_fw = fft_wnd + audio_rdft_size;
  err = mem(&fft_iw, 2 + (size_t)(sqrtf(audio_rdft_size / 2)), sizeof(int));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  // The buffer size we really need is only audio_rdft_size,
  // but it seems that avi_file_read_audio_sample function may need more buffer than requested.
  // Therefore we have to reserve large buffer enough for that.
  err = mem(&buf, audio_frequency / 10, sizeof(int16_t));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = mem(dest, 1, sizeof(struct speak));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  struct speak *const spk = *dest;
  *spk = (struct speak){
      .cbs = *cb,
      .buf = buf,
      .fft_buf = fft_buf,
      .fft_wnd = fft_wnd,
      .fft_fw = fft_fw,
      .fft_iw = fft_iw,
  };
  fft_iw[0] = 0;
  for (size_t i = 0; i < audio_rdft_size; ++i) {
    fft_wnd[i] =
        (1.f / 32768.f) * (0.54f - 0.46f * cosf(2.f * 3.14159265358979323846f * (float)i / (float)audio_rdft_size));
  }
  buf = NULL;
  fft_buf = NULL;
  fft_iw = NULL;
  mtx_init(&spk->mtx, mtx_plain);
  cndvar_init(&spk->cv);
  if (thrd_create(&spk->thread, gc, spk) != thrd_success) {
    err = errg(err_fail);
    speak_exit(dest);
    goto cleanup;
  }
cleanup:
  if (buf) {
    ereport(mem_free(&buf));
  }
  if (fft_buf) {
    ereport(mem_free(&fft_buf));
  }
  if (fft_iw) {
    ereport(mem_free(&fft_iw));
  }
  return err;
}

NODISCARD error speak_get_level(struct speak *spk,
                                struct str const *const filepath,
                                float const pos,
                                float const low_cut,
                                float const high_cut,
                                float *const level) {
  mtx_lock(&spk->mtx);
  error err = read(spk, filepath, pos, low_cut, high_cut, level);
  mtx_unlock(&spk->mtx);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

void speak_exit(struct speak **dest) {
  if (!dest) {
    return;
  }
  struct speak *const spk = *dest;
  if (!spk) {
    return;
  }
  cndvar_lock(&spk->cv);
  cndvar_signal(&spk->cv, 1);
  cndvar_unlock(&spk->cv);
  thrd_join(spk->thread, NULL);
  mtx_lock(&spk->mtx);
  size_t const n = sizeof(spk->items) / sizeof(spk->items[0]);
  for (size_t i = 0; i < n; ++i) {
    struct item *const item = spk->items + i;
    if (item->h) {
      spk->cbs.close(spk->cbs.userdata, item->h);
    }
    ereport(sfree(&item->filepath));
  }
  cndvar_exit(&spk->cv);
  mtx_destroy(&spk->mtx);
  ereport(mem_free(&spk->buf));
  ereport(mem_free(&spk->fft_iw));
  ereport(mem_free(&spk->fft_buf));
  ereport(mem_free(dest));
}

#ifdef __GNUC__
#  ifndef __has_warning
#    define __has_warning(x) 0
#  endif
#  pragma GCC diagnostic push
#  if __has_warning("-Wimplicit-float-conversion")
#    pragma GCC diagnostic ignored "-Wimplicit-float-conversion"
#  endif
#  if __has_warning("-Wmissing-prototypes")
#    pragma GCC diagnostic ignored "-Wmissing-prototypes"
#  endif
#endif // __GNUC__
#include "3rd/fft/fftsg.c"
#ifdef __GNUC__
#  pragma GCC diagnostic pop
#endif // __GNUC__
