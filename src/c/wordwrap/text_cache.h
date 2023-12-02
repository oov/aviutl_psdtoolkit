#pragma once

#include <ovbase.h>

struct text_cache {
  uint64_t hash;
  struct str text;
};

struct text_cache *text_cache_get(uint64_t const hash);
void text_cache_cleanup(void);
