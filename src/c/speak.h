#pragma once

#include "3rd/base.c/include/base.h"

struct speak;
struct speak_callbacks {
  void *userdata;
  NODISCARD
  error (*open)(void *const userdata, struct str const *const filepath, int const freq, int const ch, void **const h);
  NODISCARD
  error (*read)(void *const userdata,
                void *const h,
                size_t const offset,
                size_t const length,
                int16_t *const buf,
                size_t *const samples);
  void (*close)(void *const userdata, void *h);
};

NODISCARD error speak_init(struct speak_callbacks const *const cb, struct speak **const dest);
NODISCARD error speak_get_level(struct speak *spk,
                                struct str const *const filepath,
                                float const pos,
                                float const low_cut,
                                float const high_cut,
                                float *const level);
void speak_exit(struct speak **dest);
