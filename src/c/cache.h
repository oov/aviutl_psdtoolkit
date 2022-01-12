#pragma once

#include "ovbase.h"

NODISCARD error cache_init(void);
void cache_exit(void);
NODISCARD error cache_put(struct str const *const key, void *const value, size_t const value_len);
NODISCARD error cache_get(struct str const *const key, void **const value, size_t *const value_len);
