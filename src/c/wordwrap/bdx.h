#pragma once

#include <budoux-c.h>
#include <ovbase.h>

#include "line_reader.h"

/**
 * @brief Retrieve or load a BudouX model.
 *
 * @param name Model name or file path.
 * @param model Pointer to the model.
 * @return Success status.
 *
 * This function retrieves a model from the cache or loads a new one if not present.
 * "ja", "zh_hans", "zh_hant" are treated as embedded models, others as file paths.
 * If cache is full and a new model is needed, the oldest model is replaced.
 */
NODISCARD error bdx_cache_get(char const *const name, struct budouxc **const model);
void bdx_cache_cleanup(void);

NODISCARD error bdx_write_markers(struct line_reader const *const lr,
                                  struct budouxc *const model,
                                  size_t **const boundaries);
