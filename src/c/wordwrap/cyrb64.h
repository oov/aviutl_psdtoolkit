#pragma once

#include <stdint.h>

struct cyrb64 {
  uint32_t h1;
  uint32_t h2;
};

static inline void cyrb64_init(struct cyrb64 *const ctx, uint32_t const seed) {
  ctx->h1 = 0x91eb9dc7 ^ seed;
  ctx->h2 = 0x41c6ce57 ^ seed;
}

static inline void cyrb64_update(struct cyrb64 *const ctx, uint32_t const *const src, size_t const len) {
  for (size_t i = 0; i < len; ++i) {
    ctx->h1 = (ctx->h1 ^ src[i]) * 2654435761;
    ctx->h2 = (ctx->h2 ^ src[i]) * 1597334677;
  }
}

static inline uint64_t cyrb64_final(struct cyrb64 const *const ctx) {
  uint32_t h1 = ctx->h1, h2 = ctx->h2;
  h1 = ((h1 ^ (h1 >> 16)) * 2246822507) ^ ((h2 ^ (h2 >> 13)) * 3266489909);
  h2 = ((h2 ^ (h2 >> 16)) * 2246822507) ^ ((h1 ^ (h1 >> 13)) * 3266489909);
  return (((uint64_t)h2) << 32) | ((uint64_t)h1);
}
